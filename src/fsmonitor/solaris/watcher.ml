(* Unison file synchronizer: src/fsmonitor/solaris/watcher.ml *)
(* Copyright 2021, Benjamin C. Pierce

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

(* A brief overview of the File Event Notification (FEN) interface
 *
 * Events are delivered via ports. A port is created by [port_create].
 * Each file or directory to be watched must be individually associated
 * with the port by [port_associate].
 *
 * When associating an object with a port, the stat times of the object
 * can be passed in. During association, the FEN system will compare these
 * times with their current values to detect if there has been a change
 * between the stat() call and the association. If yes, then an event is
 * delivered immediately. If the times have not changed, or they are all
 * passed in as zero, then the next event is monitored and delivered when
 * it occurs.
 *
 * Objects are associated and events monitored per vnode. It is due to this
 * approach that all files must be watched individually. For example, while
 * adding and deleting a file will be detected as a modification event on the
 * parent directory, modifying an existing file within the same directory
 * will not be detected by watching the directory.
 *
 * For each association, only one event is delivered. The object is then
 * automatically dissociated (equivalent of [port_dissociate]) and must
 * be associated again to receive further events.
 *
 * Events are polled and retrieved by [port_get].
 *
 * A port has a limit to the number of objects associated with it. As all
 * files must be watched individually, the number of objects can grow very
 * large. This implementation maintains a pool of ports and automatically
 * creates new ports as needed, and closes ports that are no longer needed.
 *)

let (>>=) = Lwt.bind

let () = Gc.set { (Gc.get()) with space_overhead = 40 }

(****)

type port = int

(* event_objects are allocated in C heap and are not GC'd by OCaml. Care must
 * be taken to free each event_object explicitly and not use them after having
 * been freed.
 *
 * event_objects have released their backing system resources and must be freed
 * with [free_event_object] in the following cases:
 *  - after [port_get] when having received an event and event_object is not
 *    re-associated, and
 *  - after calling [port_dissociate] (exceptions here must be fatal or
 *    ensure that [free_event_object] still gets called).
 *
 * Current implementation builds on the following assumption:
 *  - A Watchercommon watch (= value that keeps track of live event_objects) is
 *    not discarded without explicitly closing it.
 *
 *    All watches are closed explicitly by either [release_watch] or
 *    in [cleanup_watch]. These functions take care of releasing the resources
 *    if needed and freeing the objects properly after they've been released.
 *    This also means that exceptions must be fatal or take care not to discard
 *    event_objects without properly releasing and freeing them.
 *
 * If the above assumption changes or can no longer be guaranteed then the
 * implementation may have to be changed. There are a few ways to make
 * event_objects GC'd. All of these solutions carry a rather high memory
 * overhead as FEN requires each file to be monitored individually.
 *
 * One possible way is to change the type event_object to nativeint and use
 * [Gc.finalise] to attach a [free_event_object] to the returned event_object
 * after every successful [port_associate].
 *
 * The other is to in C stub wrap the event object in a Custom block with
 * [free_event_object] as the finalizer. Ultimately, this could be the safest
 * solution, as in addition to enabling GC on event_objects, it makes it
 * possible to prevent use-after-free by setting the value to NULL after free.
 *)

type event_object = int

let string_of_eo eo = Format.sprintf "%#x" (eo * 2)

type assocs = (event_object, string) Hashtbl.t

type watch_t = (port, assocs) Hashtbl.t * bool

module M = Watchercommon.F (struct type watch = watch_t end)
include M

(****)

module Solaris = struct

let clear_event_memory () = ()

(****)

type cookie = int

type fen_event =
  | FILE_ACCESS | FILE_MODIFIED | FILE_ATTRIB | FILE_DELETE | FILE_RENAME_TO
  | FILE_RENAME_FROM | FILE_TRUNC | FILE_NOFOLLOW | UNMOUNTED | MOUNTEDOVER

let print_event ev =
  let print_ev ev =
    let s = match ev with
    | FILE_ACCESS -> "FILE_ACCESS"
    | FILE_MODIFIED -> "FILE_MODIFIED"
    | FILE_ATTRIB -> "FILE_ATTRIB"
    | FILE_DELETE -> "FILE_DELETE"
    | FILE_RENAME_TO -> "FILE_RENAME_TO"
    | FILE_RENAME_FROM -> "FILE_RENAME_FROM"
    | FILE_TRUNC -> "FILE_TRUNC"
    | FILE_NOFOLLOW -> "FILE_NOFOLLOW"
    | UNMOUNTED -> "UNMOUNTED"
    | MOUNTEDOVER -> "MOUNTEDOVER"
    in
    Format.eprintf "%s " s
  in
  List.iter print_ev ev;
  Format.eprintf "@."

let event_kind =
  let kind = function
  | FILE_ACCESS -> `OTHER
  | FILE_MODIFIED -> `MODIF
  | FILE_ATTRIB -> `MODIF
  | FILE_DELETE -> `DEL
  | FILE_RENAME_TO -> `CREAT
  | FILE_RENAME_FROM -> `DEL
  | FILE_TRUNC -> `MODIF
  | FILE_NOFOLLOW -> `OTHER
  | UNMOUNTED -> `OTHER
  | MOUNTEDOVER -> `OTHER
  in
  List.fold_left (fun k v -> if k = `OTHER then kind v else k) `OTHER

let event_is_exceptional =
  let is_ex = function
  | FILE_ACCESS -> false
  | FILE_MODIFIED -> false
  | FILE_ATTRIB -> false
  | FILE_DELETE -> true
  | FILE_RENAME_TO -> true
  | FILE_RENAME_FROM -> true
  | FILE_TRUNC -> false
  | FILE_NOFOLLOW -> false
  | UNMOUNTED -> true
  | MOUNTEDOVER -> true
  in
  List.fold_left (fun k v -> if not k then is_ex v else k) false

(* FIXME: should be equal to [event_is_exceptional] here? *)
let event_is_immediate ev = false

(****)

external port_create : unit -> port = "unsn_port_create"
external port_close : port -> unit = "unsn_port_close"
external port_associate : port -> string -> bool -> cookie -> event_object = "unsn_port_associate"
external port_reassociate : port -> event_object -> bool -> bool = "unsn_port_reassociate"
external port_dissociate : port -> event_object -> unit = "unsn_port_dissociate"
external port_get : port -> (port * event_object * cookie * (fen_event list)) list = "unsn_port_get"
external free_event_object : event_object -> unit = "unsn_free_event_object"

(****)

let max_ev_per_port = 65000 (* A safe max. The OS limit should be at 64k. *)
     (* The number of ports is limited at 8k per process, so not a worry. *)

let ports = ref []

let allocate_port () =
  let avail_port, _ =
    try
      List.find (fun (_, count) -> count < max_ev_per_port) !ports
    with Not_found ->
      let p = port_create (), 0 in ports := p :: !ports; p
  in
  ports := List.map (fun p' ->
    let port, count = p' in
    if port <> avail_port then
      p'
    else
      port, count + 1
  ) !ports;
  avail_port

let release_port p =
  ports := List.fold_left (fun nl p' ->
    let port, count = p' in
    if p <> port then
      p' :: nl
    else begin
      if count > 1 then
        (port, count - 1) :: nl
      else begin
        let () = port_close port in
        nl
      end
    end
  ) [] !ports

(****)

let is_directory path follow =
  let st = match follow with
  | false -> Unix.lstat path
  | true -> begin
      try
        Unix.stat path
      with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
        Watchercommon.error (Format.sprintf
          "Unable to follow link '%s' because its target is missing" path)
    end
  in
  Unix.S_DIR = st.st_kind

let associate is_child wh id follow absname name =
  if not is_child || not (is_directory absname follow) then begin
    let port = allocate_port () in
    let wh_p = try Hashtbl.find wh port with Not_found ->
      let wh_p = Hashtbl.create (if is_child then 1 else 1024) in
      Hashtbl.add wh port wh_p;
      wh_p
    in
    let eo = port_associate port absname follow id in
    Hashtbl.add wh_p eo name
  end

let add_watch_children path assoc_f =
  let rec loop dir =
    match Unix.readdir dir with
    | exception End_of_file -> ()
    | "." | ".." -> loop dir
    | name ->
        let () = assoc_f name in
        loop dir
  in
  let dir = Unix.opendir path in
  try
    let () = loop dir in
    Unix.closedir dir
  with Unix.Unix_error _ as e ->
    begin try
      Unix.closedir dir
    with Unix.Unix_error _ -> () end;
    raise e

let rec add_watch path file follow =
  match get_watch file with
  | Some (_, follow') when follow = follow' ->
      ()
  | Some _ ->
      release_watch file;
      add_watch path file follow
  | None ->
      let id = get_id file
      and wh = Hashtbl.create 1 in
      let () = set_watch file (Some (wh, follow)) in
      try
        let () = associate false wh id follow path "" in
        if is_directory path follow then add_watch_children path
          (fun nm -> associate true wh id follow (Filename.concat path nm) nm)
      with
      | Unix.Unix_error (EACCES, _, _)
      | Unix.Unix_error (ENOTDIR, _, _)
      | Unix.Unix_error (ELOOP, _, _) ->
          (* These are handled well by Unison *)
          ()
      | Unix.Unix_error _ as e ->
          Watchercommon.error
            (Format.sprintf
              "Error while starting to watch for changes: [%s] %s"
              path (Watchercommon.format_exc e))

and release_watch file =
  match get_watch file with
  | None -> ()
  | Some (wh, _) ->
      set_watch file None;
      let unwatch port eo name =
        port_dissociate port eo;
        free_event_object eo;
        release_port port
      in
      Hashtbl.iter (fun port wh_p -> Hashtbl.iter (unwatch port) wh_p) wh

(* Once an event is delivered, the FEN automatically dissociates the object.
 *
 * The object must be re-associated in the following cases:
 *  - It was not requested by [add_watch] but was implicitly added by
 *    [add_watch_children]. In other words, the name is not "".
 *
 * When the object is not to be re-associated or re-association did not
 * succeed then the following must be done:
 *  - The associated port must be released.
 *  - The event object must be freed and then discarded (event object must
 *    no longer be referenced or used in any way).
 *  - The watch must be released completely by calling [release_watch],
 *    even if it was an implicitly added child that failed re-association.
 *
 * Unison and Watchercommon will associate the path again if and when needed.
 *
 * This releasing and associating can potentially be terrible for performance
 * on large directories (with several tens or hundreds of thousands of files)
 * but it is the easiest way to guarantee that all children in a directory are
 * watched.
 *)
let cleanup_watch file name port eo id ev =
  match get_watch file with
  | None -> ()
  | Some (wh, follow) ->
      let reassoc =
        try
          let wh_p = Hashtbl.find wh port in
          let r =
            match name with
            | "" -> false
            | _ -> port_reassociate port eo follow
          in
          if not r then begin
            Hashtbl.remove wh_p eo;
            free_event_object eo;
            release_port port
          end;
          r
        with Not_found -> false
      in
      if not reassoc then release_watch file
      (* [release_watch] here is safe because even if some events within the
       * watch may not have been processed yet, all event objects in a watch
       * will be dissociated, freed and the entire watch discarded.
       *
       * Dissocating an already dissociated object is a noop.
       *
       * Since the watch is discarded, there will not be any use-after-free
       * or double free possible as event objects are always looked up from
       * a watch before any processing. *)

let process_ev time ((file, name), (port, eo, id, ev)) =
  if !Watchercommon.debug then begin
    Format.eprintf " %i: [%s] %s \"%s\": " port (string_of_eo eo)
      (dir_path file "") name;
    print_event ev
  end;
  let () = cleanup_watch file name port eo id ev in
  let event_time = if event_is_immediate ev then ref 0. else ref time
  and name = match name with
  | "" -> None
  | _ -> Some name
  in
  signal_change event_time file name (event_kind ev)

(* Always process events on children first and on parents last because
 * the cleanup procedure clears out children together with the parent. *)
let compare_event e e' =
  match e, e' with
  | ((_, ""), _), ((_, ""), _) -> 0
  | ((_, ""), _), ((_, n), _) -> 1
  | ((_, n), _), ((_, ""), _) -> -1
  | ((_, n), _), ((_, n'), _) -> 0

let process_ev_list ev_list =
  let time = Unix.gettimeofday () in
  let ev_list = List.fold_left
    (fun k ((port, eo, id, _) as o) ->
      try
        let file = Hashtbl.find file_by_id id in
        match get_watch file with
        | None ->
            k
        | Some (wh, _) ->
            let wh_p = Hashtbl.find wh port in
            let name = Hashtbl.find wh_p eo in
            ((file, name), o) :: k
      with Not_found ->
        k
    ) [] ev_list
  in
  let ev_list = List.sort compare_event ev_list in
  List.iter (process_ev time) ev_list

let rec read_events () =
  (* FIXME: List.concat_map is available since OCaml 4.10.0 *)
  let ev_list = List.map (fun (port, _) -> port_get port) !ports in
  let ev_list = Safelist.concat ev_list in
  if List.length ev_list > 0 then
    Lwt_unix.yield () >>= fun () ->
    Lwt.return ev_list
  else
    Lwt_unix.sleep 1.5 >>=
    read_events

let watch () =
  let rec watch_rec () =
    read_events () >>= fun ev_list ->
    let () = process_ev_list ev_list in
    watch_rec ()
  in
  ignore
    (Lwt.catch watch_rec
       (fun e ->
          Watchercommon.error
            ("error while handling events: " ^ Watchercommon.format_exc e)))

end

(****)

include F(Solaris)
