(* Unison file synchronizer: src/fsmonitoring/watchercommon.ml *)
(* Copyright 2012, Benjamin C. Pierce 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

let debug = ref false

let _ =
  if Sys.os_type = "Unix" then
    ignore(Sys.set_signal Sys.sigpipe Sys.Signal_ignore)

module StringMap = Map.Make(String)
(*FIX: temporary workaround, as there is no StringMap.filter function
  in OCaml 3.11 *)
let stringmap_filter f m =
  StringMap.fold (fun k v m' -> if f k v then StringMap.add k v m' else m')
    m StringMap.empty
module StringSet = Set.Make(String)
module IntSet =
  Set.Make
    (struct type t = int let compare (x : int) (y : int) = compare x y end)

let disallowed_char c =
  match c with
    'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~'
  | '!' | '*' | '\'' | '(' | ')' | ';' | ':' | '@' | '&'
  | '=' | '+' | '$' | ',' | '/' | '?' | '#' | '[' | ']' ->
    false
  | _ ->
    true

let quote s =
  let l = String.length s in
  let n = ref 0 in
  for i = 0 to l - 1 do if disallowed_char s.[i] then incr n done;
  if !n = 0 then s else begin
    let q = String.create (l + 2 * !n) in
    let j = ref 0 in
    let hex = "0123456789ABCDEF" in
    for i = 0 to l - 1 do
      let c = s.[i] in
      if disallowed_char s.[i] then begin
        q.[!j] <- '%';
        q.[!j + 1] <- hex.[Char.code c lsr 4];
        q.[!j + 2] <- hex.[Char.code c land 15];
        j := !j + 3
      end else begin
        q.[!j] <- c;
        incr j
      end
    done;
    q
  end

let unquote s =
  let l = String.length s in
  let n = ref 0 in
  for i = 0 to l - 1 do if s.[i] = '%' then incr n done;
  if !n = 0 then s else begin
    let hex_char c =
      match c with
        '0'..'9' -> Char.code c - Char.code '0'
      | 'a'..'f' -> Char.code c - Char.code 'a' + 10
      | 'A'..'F' -> Char.code c - Char.code 'A' + 10
      | _        -> invalid_arg "unquote"
    in
    let u = String.create (l - 2 * !n) in
    let j = ref 0 in
    for i = 0 to l - 2 * !n - 1 do
      let c = s.[!j] in
      if c = '%' then begin
        u.[i] <- Char.chr ((hex_char s.[!j + 1]) lsl 4 + hex_char s.[!j + 2]);
        j := !j + 3
      end else begin
        u.[i] <- c;
        incr j
      end
    done;
    u
  end

let split_on_space s =
  try
    let i = String.index s ' ' in
    (String.sub s 0 i,
     String.sub s (i + 1) (String.length s - i - 1))
  with Not_found ->
    (s, "")

let (>>=) = Lwt.bind

let rec really_write o s pos len =
  Lwt_unix.write o s pos len >>= fun l ->
  if l = len then
    Lwt.return ()
  else
    really_write o s (pos + l) (len - l)

let format_exc e =
  match e with
    Unix.Unix_error (code, funct, arg) ->
      Format.sprintf "%s [%s%s]%s@."
        (Unix.error_message code) funct
        (if String.length arg > 0 then "(" ^ arg ^ ")" else "")
        (match code with
            Unix.EUNKNOWNERR n -> Format.sprintf " (code %d)" n
        | _                    -> "")
  | _ ->
     Format.sprintf "uncaugth exception %s@." (Printexc.to_string e)

(****)

let _in = (*Lwt_unix.stdin*) Lwt_unix.of_unix_file_descr Unix.stdin
let _out = (*Lwt_unix.stdout*) Lwt_unix.of_unix_file_descr Unix.stdout

let printf fmt =
  Printf.ksprintf (fun s -> really_write _out s 0 (String.length s)) fmt

let read_line =
  let b = Buffer.create 160 in
  let buf = String.create 160 in
  let start = ref 0 in
  let last = ref 0 in
  let rec read_line () =
    begin if !start = !last then begin
      Lwt_unix.read _in buf 0 160 >>= fun l ->
      if l = 0 then raise End_of_file;
      start := 0; last := l;
      Lwt.return ()
    end else
      Lwt.return ()
    end >>= fun () ->
    try
      let i = String.index_from buf !start '\n' in
      if i >= !last then raise Not_found;
      Buffer.add_substring b buf !start (i - !start);
      start := i + 1;
      let s = Buffer.contents b in
      Buffer.clear b;
      Lwt.return s
    with Not_found ->
      Buffer.add_substring b buf !start (!last - !start);
      start := 0; last := 0;
      read_line ()
  in
  read_line

let error msg =
  Lwt_unix.run (printf "ERROR %s\n" (quote msg));
  exit 1

(****)

module F (M : sig type watch end) = struct
include M

type status = Modified | Created

type t =
  { id : int; mutable gen : int;
    mutable watch : watch option;
    mutable subdirs : t StringMap.t;
    parent : parent;
    archive_hash : string;
    mutable changed : bool;
    mutable changed_children : (status * float ref) StringMap.t }

and parent = Root of string * string | Parent of string * t

let get_id file = file.id
let get_watch file = file.watch
let set_watch file watch = file.watch <- watch
let get_subdirs file = file.subdirs

let current_gen = ref 0

let file_by_id = Hashtbl.create 16
let roots = Hashtbl.create 16

let concat fspath path =
  if path = "" then fspath else Filename.concat fspath path

let is_root file =
  match file.parent with
    Root _   -> true
  | Parent _ -> false

let rec dir_path dir path =
  match dir.parent with
    Root (fspath, path') -> concat fspath (concat path' path)
  | Parent (name, dir)   -> dir_path dir (concat name path)

(****)

let delay = 0.5

let changes = ref StringMap.empty

let waiting_for_changes = ref StringSet.empty
let active_wait = ref false

let change_table hash =
  try
    StringMap.find hash !changes
  with Not_found ->
    let h = Hashtbl.create 1024 in
    changes := StringMap.add hash h !changes;
    h

let signal_changes replicas_with_changes =
  waiting_for_changes := StringSet.empty;
  printf "CHANGES %s\n"
    (String.concat " "
       (List.map quote (StringSet.elements replicas_with_changes)))

let signal_immediate_changes hash =
  if StringSet.mem hash !waiting_for_changes then begin
    waiting_for_changes := StringSet.empty;
    printf "CHANGES %s\n" (quote hash)
  end else
    Lwt.return ()

let replicas_with_changes watched_replicas =
  let time = Unix.gettimeofday () in
  let changed = ref StringSet.empty in
  Hashtbl.iter
    (fun (hash', _, _) r ->
       if
         r.changed &&
         not (StringSet.mem hash' !changed) &&
         StringSet.mem hash' watched_replicas
       then
         changed := StringSet.add hash' !changed)
    roots;
  StringSet.iter
    (fun hash ->
       if not (StringSet.mem hash !changed) then
         try
           Hashtbl.iter
             (fun _ time_ref -> if time -. !time_ref > delay then raise Exit)
             (change_table hash)
         with Exit ->
           changed := StringSet.add hash !changed)
    watched_replicas;
  !changed

let has_impending_changes watched_replicas =
  try
    StringSet.iter
      (fun hash -> Hashtbl.iter (fun _ _ -> raise Exit) (change_table hash))
      watched_replicas;
    false
  with Exit ->
    true

let rec wait_for_changes watched_replicas =
  if not (StringSet.is_empty watched_replicas) then begin
    let changed = replicas_with_changes watched_replicas in
    if not (StringSet.is_empty changed) then signal_changes changed else
    if has_impending_changes watched_replicas then signal_impending_changes ()
    else Lwt.return ()
  end else
    Lwt.return ()

and signal_impending_changes () =
  if not (StringSet.is_empty !waiting_for_changes || !active_wait) then begin
    active_wait := true;
    Lwt_unix.sleep delay >>= fun () ->
    active_wait := false;
    wait_for_changes !waiting_for_changes
  end else
    Lwt.return ()

let wait hash =
  waiting_for_changes := StringSet.add hash !waiting_for_changes;
  ignore (wait_for_changes (StringSet.singleton hash))

let add_change dir nm time =
  Hashtbl.replace (change_table dir.archive_hash) (dir.id, nm) time;
  if !time = 0. then
    ignore (signal_immediate_changes dir.archive_hash)
  else
    ignore (signal_impending_changes ())
let remove_change dir nm =
  Hashtbl.remove (change_table dir.archive_hash) (dir.id, nm)
let clear_change_table hash =
  changes := StringMap.remove hash !changes

let rec clear_changes hash time =
  let rec clear_rec f =
    f.changed_children <-
      stringmap_filter
        (fun nm (_, time_ref) ->
           if time -. !time_ref <= delay then true else begin
             remove_change f nm;
             false
           end)
        f.changed_children;
    StringMap.iter (fun _ f' -> clear_rec f') f.subdirs
  in
  Hashtbl.iter
    (fun (hash', _, _) f ->
       if hash' = hash then begin
         f.changed <- false;
         clear_rec f
       end)
    roots

(****)

let rec signal_change time dir nm_opt kind =
  match nm_opt with
    Some nm ->
      begin try
        let (st, _) = StringMap.find nm dir.changed_children in
        if
          st = Created && kind = `DEL &&
          not (StringMap.mem nm dir.subdirs)
        then begin
          if !debug then Format.eprintf "Deleted: %s@." (dir_path dir nm);
          dir.changed_children <- StringMap.remove nm dir.changed_children;
          remove_change dir nm
        end else begin
          dir.changed_children <-
            StringMap.add nm (st, time) dir.changed_children;
          add_change dir nm time
        end
      with Not_found ->
        if kind = `CREAT && dir.gen <> !current_gen then begin
          if !debug then Format.eprintf "Created: %s@." (dir_path dir nm);
          dir.changed_children <-
            StringMap.add nm (Created, time) dir.changed_children;
          add_change dir nm time
        end else begin
          if !debug then Format.eprintf "Modified: %s@." (dir_path dir nm);
          dir.changed_children <-
            StringMap.add nm (Modified, time) dir.changed_children;
          add_change dir nm time
        end
      end
  | None ->
      match dir.parent with
        Root _ ->
          dir.changed <- true;
          ignore (signal_immediate_changes dir.archive_hash)
      | Parent (nm, parent_dir) ->
          signal_change time parent_dir (Some nm) kind

let signal_overflow () =
  Hashtbl.iter (fun _ r -> r.changed <- true) roots;
  ignore (signal_changes !waiting_for_changes)

(****)

module type S = sig
  val add_watch : string -> t -> unit
  val release_watch : t -> unit
  val watch : unit -> unit
  val clear_event_memory : unit -> unit
end

module F (M : S) = struct
include M

let gather_changes hash time =
  clear_event_memory ();
  let rec gather_rec path r l =
    let c =
      stringmap_filter (fun _ (_, time_ref) -> time -. !time_ref > delay)
        r.changed_children
    in
    let l = StringMap.fold (fun nm _ l -> concat path nm :: l) c l in
    StringMap.fold
      (fun nm r' l ->
         if StringMap.mem nm c then l else
         gather_rec (concat path nm) r' l)
      r.subdirs l
  in
  List.rev
    (Hashtbl.fold
       (fun (hash', _, path) r l ->
          if hash' <> hash then l else
          (* If this path is not watched (presumably, it does not exist),
             we report that it should be scanned again. On the other hand,
             this is not reported as a change by the WAIT function, so that
             Unison does not loop checking this path. *)
          if r.changed && r.watch = None then path :: l else            
          gather_rec path r l)
       roots [])

let rec find_root hash fspath path =
  if Hashtbl.mem roots (hash, fspath, path) then
    Some (fspath, path)
  else
    try
      let i = String.rindex path '/' in
      find_root hash fspath (String.sub path 0 i)
    with Not_found ->
      if path = "" then
        None
      else
        find_root hash fspath ""

let last_file_id = ref 0

let new_file hash parent =
  let f =
    { id = !last_file_id; watch = None; gen = -1;
      parent = parent; archive_hash = hash; subdirs = StringMap.empty;
      changed = false; changed_children = StringMap.empty }
  in
  incr last_file_id;
  Hashtbl.add file_by_id f.id f;
  f

let new_root hash fspath path =
  if !debug then Format.eprintf "ROOT %s %s@." fspath path;
  let r = new_file hash (Root (fspath, path)) in
  Hashtbl.add roots (hash, fspath, path) r;
  r

let dir_child dir name =
  try
    StringMap.find name dir.subdirs
  with Not_found ->
    let d = new_file dir.archive_hash (Parent (name, dir)) in
    dir.subdirs <- StringMap.add name d dir.subdirs;
    d

let rec follow_path dir path pos =
  if path = "" then dir else
  try
    let i = String.index_from path pos '/' in
    try
      let dir = StringMap.find (String.sub path pos (i - pos)) dir.subdirs in
      follow_path dir path (i + 1)
    with Not_found ->
      assert false
  with Not_found ->
    dir_child dir (String.sub path pos (String.length path - pos))

let rec follow_fspath hash fspath dir path pos =
  if path = "" then dir else
  try
    let i = String.index_from path pos '/' in
    try
      let dir = StringMap.find (String.sub path pos (i - pos)) dir.subdirs in
      follow_fspath hash fspath dir path (i + 1)
    with Not_found ->
      new_root hash fspath path
  with Not_found ->
    dir_child dir (String.sub path pos (String.length path - pos))

let find_start hash fspath path =
  match find_root hash fspath path with
    None ->
      new_root hash fspath path
  | Some (root_fspath, root_path) ->
      let root = Hashtbl.find roots (hash, root_fspath, root_path) in
      if fspath = root_fspath && path = root_path then
        root
      else
        follow_fspath hash fspath root path
          (if root_path = "" then 0 else String.length root_path + 1)

let clear_file_changes file =
  StringMap.iter (fun nm _ -> remove_change file nm) file.changed_children;
  file.changed_children <- StringMap.empty;
  file.changed <- false

let rec remove_file file =
  if !debug then Format.eprintf "REMOVING %s@." (dir_path file "");
  StringMap.iter (fun _ f -> remove_file f) file.subdirs;
  Hashtbl.remove file_by_id file.id;
  release_watch file;
  match file.parent with
    Root _         -> ()
  | Parent (nm, p) -> p.subdirs <- StringMap.remove nm p.subdirs

let rec remove_old_files file =
  if file.gen <> !current_gen then remove_file file else begin
    StringMap.iter (fun _ f -> remove_old_files f) file.subdirs;
    if
      file.watch = None && StringMap.is_empty file.subdirs &&
      not (is_root file)
    then
      remove_file file
  end

let print_ack () = printf "OK\n"

let start_watching hash fspath path =
  let start_file = find_start hash fspath path in
  clear_file_changes start_file;
  start_file.gen <- !current_gen;
  let fspath = concat fspath path in
(*Format.eprintf ">>> %s@." fspath;*)
  if is_root start_file then add_watch fspath start_file;
  print_ack () >>= fun () ->
  let rec add_directories () =
    read_line () >>= fun l ->
    let (cmd, path) = split_on_space l in
    let path = unquote path in
    match cmd with
      "DIR" ->
(*Format.eprintf "DIR %s@." path;*)
        let fullpath = concat fspath path in
        let file = follow_path start_file path 0 in
        clear_file_changes file;
        file.gen <- !current_gen;
(*Format.eprintf "%s@." fullpath;*)
        add_watch fullpath file;
        print_ack () >>= fun () ->
        add_directories ()
    | "LINK" ->
(*Format.eprintf "LINK %s@." path;*)
        let fullpath = concat fspath path in
        let file = follow_path start_file path 0 in
        clear_file_changes file;
        file.gen <- !current_gen;
(*Format.eprintf "%s@." fullpath;*)
        add_watch fullpath file;
        print_ack () >>= fun () ->
        add_directories ()
    | "DONE" ->
        Lwt.return ()
    | _ ->
        error (Format.sprintf "unknown command '%s'" cmd)
  in
  add_directories () >>= fun () ->
  (* We remove any file which is not watched anymore,
     as well as files which are not in fact watched. *)
  remove_old_files start_file;
  incr current_gen;
  Lwt.return ()

(****)

let reset hash =
  let l = ref [] in
  Hashtbl.iter
    (fun ((hash', _, _) as key) f ->
       if hash' = hash then begin
         l := key :: !l;
         remove_file f
       end)
    roots;
  List.iter (fun key -> Hashtbl.remove roots key) !l;
  clear_change_table hash

(****)

let rec lazy_fold_right f l accu =
  match l with
    [] -> accu ()
  | a::l -> f a (fun () -> lazy_fold_right f l accu)

let output_changes hash =
  let time = Unix.gettimeofday () in
  let lst = gather_changes hash time in
  clear_changes hash time;
  lazy_fold_right (fun p cont -> printf "RECURSIVE %s\n" (quote p) >>= cont)
    lst (fun () -> printf "DONE\n")

let rec loop () =
  read_line () >>= fun l ->
  (* Cancel any wait when receiving a command *)
  let (cmd, args) = split_on_space l in
  if cmd <> "WAIT" then waiting_for_changes := StringSet.empty;
  match cmd with
    "VERSION" ->
      loop ()
  | "DEBUG" ->
      debug := true;
      loop ()
  | "START" ->
      let (hash, rem) = split_on_space args in
      let (fspath, path) = split_on_space rem in
      start_watching (unquote hash) (unquote fspath) (unquote path)
        >>= fun () ->
      loop ()
  | "WAIT" ->
      wait (unquote args);
      loop ()
  | "CHANGES" ->
      output_changes (unquote args) >>= fun () ->
      loop ()
  | "RESET" ->
      reset (unquote args);
      loop ()
  | _ ->
      error (Format.sprintf "unknown command '%s'" cmd)

let _ =
watch ();
Lwt_unix.run
  (printf "VERSION 1\n" >>= fun () ->
   Lwt.catch (fun () -> loop ())
     (fun e ->
        match e with
          End_of_file | Unix.Unix_error (Unix.EPIPE, _, _) ->
            Lwt.return ()
        | _ ->
            if !debug then Format.eprintf "%s@." (format_exc e);
            error ("error while communicating with Unison: " ^ format_exc e)))

end
end
