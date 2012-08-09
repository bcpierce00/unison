(* Unison file synchronizer: src/fsmonitoring/linux/watcher.ml *)
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

(*
LIMITATIONS
- we do not detect when a directory below a path is moved;
- same limitation for the directories containing symlinked files;
- do not watch chains of symlinks (only the first symlink and the
  final target are watched)
- we do not watch non-existent roots

POSSIBLE IMPROVEMENTS
- there could be a special case for directory attribute changes

Maybe we should ignore Unison temporary files
*)

let (>>=) = Lwt.bind

module M = Watchercommon.F(struct type watch = Inotify.wd end)
include M

(****)

module Linux = struct

let print_opt_path f p =
  match p with
    Some p -> Format.fprintf f " \"%s\"" p
  | None   -> ()

let print_event path_of_id (wd, evl, id, p) =
  Format.eprintf "%02d %s%a"
    (Inotify.int_of_wd wd) (path_of_id wd) print_opt_path p;
  List.iter (fun ev -> Format.eprintf " %s" (Inotify.string_of_event ev)) evl;
  if id <> 0l then Format.eprintf " %08lx" id;
  Format.eprintf "@."

let action_kind ev =
  Inotify.
    (match ev with
     | Access        -> `OTHER
     | Attrib        -> `MODIF
     | Close_write   -> `OTHER
     | Close_nowrite -> `OTHER
     | Create        -> `CREAT
     | Delete        -> `DEL
     | Delete_self   -> `DEL
     | Modify        -> `MODIF
     | Move_self     -> `DEL
     | Moved_from    -> `DEL
     | Moved_to      -> `MODIF
     | Open          -> `OTHER
     | Ignored       -> `OTHER
     | Isdir         -> `OTHER
     | Q_overflow    -> `OTHER
     | Unmount       -> `DEL)

let event_kind (_, evl, _, _) =
  List.fold_left (fun k act -> if k = `OTHER then action_kind act else k)
    `OTHER evl

let is_change ev =
  Inotify.
    (match ev with
     | Access        -> false
     | Attrib        -> true
     | Close_write   -> false
     | Close_nowrite -> false
     | Create        -> true
     | Delete        -> true
     | Delete_self   -> true
     | Modify        -> true
     | Move_self     -> true
     | Moved_from    -> true
     | Moved_to      -> true
     | Open          -> false
     | Ignored       -> false
     | Isdir         -> false
     | Q_overflow    -> false
     | Unmount       -> true)

let is_creation ev = ev = Inotify.Create

let is_deletion ev =
  Inotify.
    (match ev with
     | Access        -> false
     | Attrib        -> false
     | Close_write   -> false
     | Close_nowrite -> false
     | Create        -> false
     | Delete        -> true
     | Delete_self   -> true
     | Modify        -> false
     | Move_self     -> true
     | Moved_from    -> true
     | Moved_to      -> false
     | Open          -> false
     | Ignored       -> false
     | Isdir         -> false
     | Q_overflow    -> false
     | Unmount       -> true)

let is_immediate ev =
  Inotify.
    (match ev with
     | Access        -> false
     | Attrib        -> false
     | Close_write   -> false
     | Close_nowrite -> false
     | Create        -> false
     | Delete        -> true
     | Delete_self   -> true
     | Modify        -> false
     | Move_self     -> true
     | Moved_from    -> true
     | Moved_to      -> true
     | Open          -> false
     | Ignored       -> false
     | Isdir         -> false
     | Q_overflow    -> false
     | Unmount       -> true)

let event_is_change (_, evl, _, _) = List.exists is_change evl
let event_is_creation (_, evl, _, _) = List.exists is_creation evl
let event_is_deletion (_, evl, _, _) = List.exists is_deletion evl
let event_is_immediate (_, evl, _, _) = List.exists is_immediate evl

let st = Lwt_inotify.init ()

module IntSet =
  Set.Make
    (struct type t = int let compare (x : int) (y : int) = compare x y end)

let watcher_by_id = Hashtbl.create 16

let path_of_id id =
  try
  dir_path
    (Hashtbl.find file_by_id (IntSet.choose (Hashtbl.find watcher_by_id id)))
    ""
  with Not_found ->
    Format.sprintf "????"

let previous_event = ref None
let time_ref = ref (ref 0.)

let clear_event_memory () = previous_event := None

let rec watch_rec () =
  Lwt_inotify.read st >>= fun ((wd, evl, _, nm_opt) as ev) ->
  let time = Unix.gettimeofday () in
  if !previous_event <> Some ev then begin
    previous_event := Some ev;
    if !Watchercommon.debug then print_event path_of_id ev;
    time_ref := ref time;
    let kind = event_kind ev in
    if kind <> `OTHER then begin
      try
        let files = Hashtbl.find watcher_by_id wd in
        let event_time = if event_is_immediate ev then ref 0. else !time_ref in
        IntSet.iter
          (fun file ->
             signal_change
               event_time (Hashtbl.find file_by_id file) nm_opt kind)
          files
      with Not_found ->
        ()
    end else if List.mem Inotify.Q_overflow evl then begin
      if !Watchercommon.debug then Format.eprintf "OVERFLOW@.";
      signal_overflow ()
    end
  end else
    !time_ref := time;
  watch_rec ()

let watch () =
  ignore
    (Lwt.catch (fun () -> watch_rec ())
       (fun e ->
          Watchercommon.error
            ("error while handling events: " ^ Watchercommon.format_exc e)))

let release_watch file =
  match get_watch file with
    None ->
      ()
  | Some id ->
      set_watch file None;
      let s = IntSet.remove (get_id file) (Hashtbl.find watcher_by_id id) in
      if IntSet.is_empty s then begin
        begin try
          Lwt_inotify.rm_watch st id
          (* Will fail with EINVAL if the file has been deleted... *)
        with Inotify.Error (_, no) ->
          ()
        end;
        Hashtbl.remove watcher_by_id id
      end else
        Hashtbl.replace watcher_by_id id s

let selected_events =
  Inotify.([S_Attrib; S_Modify; S_Delete_self; S_Move_self;
            S_Create; S_Delete; S_Modify; S_Moved_from; S_Moved_to])

let add_watch path file =
  try
    let id = Lwt_inotify.add_watch st path selected_events in
    begin match get_watch file with
      Some id' when id = id' ->
        ()
    | _ ->
        release_watch file;
        let s =
          try Hashtbl.find watcher_by_id id with Not_found -> IntSet.empty in
        Hashtbl.replace watcher_by_id id (IntSet.add (get_id file) s);
        set_watch file (Some id)
    end
  with Inotify.Error (_, no) ->
    release_watch file;
    match no with
      2 | 13 | 20 | 28 | 40 ->
        ()
    | _ ->
        Watchercommon.error
          (Format.sprintf "unexpected error %d while adding a watcher" no)

end

(****)

include F(Linux)
