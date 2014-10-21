(* Unison file synchronizer: src/monitoring/windows/watcher.ml *)
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
- we do not watch non-existent or non-directory roots

REMARK
ReadDirectoryChangesW fails with ERROR_INVALID_PARAMETER when
we are not on a directory, and ERROR_ACCESS_DENIED when the directory
is removed.

Maybe we should ignore Unison temporary files
*)

let (>>=) = Lwt.bind

module StringMap = Watchercommon.StringMap

type watch_def =
  { mutable handle : Lwt_win.directory_handle option;
    mutable longname : string StringMap.t;
    mutable shortname : string StringMap.t }

module M = Watchercommon.F (struct type watch = watch_def end)
include M

(****)

module Windows = struct

let print_event (nm, act) =
  Format.eprintf "%s %d@." nm (Obj.magic act : int)

let event_is_immediate (_, act) =
  match act with
    Lwt_win.FILE_ACTION_ADDED
  | Lwt_win.FILE_ACTION_MODIFIED         -> false
  | Lwt_win.FILE_ACTION_REMOVED
  | Lwt_win.FILE_ACTION_RENAMED_OLD_NAME 
  | Lwt_win.FILE_ACTION_RENAMED_NEW_NAME -> true

let event_kind (_, act) =
  match act with
    Lwt_win.FILE_ACTION_ADDED            -> `CREAT
  | Lwt_win.FILE_ACTION_MODIFIED         -> `MODIF
  | Lwt_win.FILE_ACTION_RENAMED_NEW_NAME -> `MOVED
  | Lwt_win.FILE_ACTION_REMOVED
  | Lwt_win.FILE_ACTION_RENAMED_OLD_NAME -> `DEL

let long_name dir nm =
  match get_watch dir with
    None   -> nm
  | Some w -> try StringMap.find nm w.longname with Not_found -> nm

let rec follow_win_path dir path pos =
  try
    let i = String.index_from path pos '\\' in
    let nm = String.sub path pos (i - pos) in
    try
      let dir = StringMap.find (long_name dir nm) (get_subdirs dir) in
      follow_win_path dir path (i + 1)
    with Not_found ->
      if !Watchercommon.debug then
        Format.eprintf "Ignored directory %s (%s) in path %s@."
          nm (long_name dir nm) path;
      None
  with Not_found ->
    Some (dir, String.sub path pos (String.length path - pos))

let previous_event = ref None
let time_ref = ref (ref 0.)

let clear_event_memory () = previous_event := None

let flags =
  Lwt_win.([FILE_NOTIFY_CHANGE_FILE_NAME; FILE_NOTIFY_CHANGE_DIR_NAME;
            FILE_NOTIFY_CHANGE_ATTRIBUTES; (*FILE_NOTIFY_CHANGE_SIZE;*)
            FILE_NOTIFY_CHANGE_LAST_WRITE; FILE_NOTIFY_CHANGE_CREATION;
            (*FILE_NOTIFY_CHANGE_SECURITY*)])

let watch_root_directory path dir =
  let h = Lwt_win.open_directory path in
  let rec loop () =
    Lwt_win.readdirectorychanges h true flags >>= fun l ->
    let time = Unix.gettimeofday () in
    List.iter
      (fun ((ev_path, _) as ev) ->
         if !previous_event <> Some ev then begin
           time_ref := ref time;
           previous_event := Some ev;
           if !Watchercommon.debug then print_event ev;
           match follow_win_path dir ev_path 0 with
             None ->
               ()
           | Some (subdir, nm) ->
               let event_time =
                 if event_is_immediate ev then ref 0. else !time_ref in
               let kind = event_kind ev in
               let nm =
                 match kind, get_watch subdir with
                   (`CREAT | `MOVED), Some w ->
                     begin try
                       match
                         Shortnames.of_file (Filename.concat path ev_path)
                       with
                         Some (l, s) ->
                           if !Watchercommon.debug then
                             Format.eprintf "New mapping: %s -> %s@." l s;
                           (* First remove a previous binding, if any *)
                           begin try
                             w.longname <-
                               StringMap.remove (StringMap.find l w.shortname)
                                 w.longname
                           with Not_found -> () end;
                           begin try
                             w.shortname <-
                               StringMap.remove (StringMap.find s w.longname)
                                 w.shortname
                           with Not_found -> () end;
                           w.shortname <- StringMap.add l s w.shortname;
                           w.longname <- StringMap.add s l w.longname;
                           l
                       | None ->
                           long_name subdir nm
                     with Unix.Unix_error _ as e ->
                       if !Watchercommon.debug then
                         Format.eprintf
                           "Error while getting file short name: %s@."
                           (Watchercommon.format_exc e);
                       long_name subdir nm
                     end
                 | `DEL, Some w ->
                     let l = long_name subdir nm in
                     begin try
                       let s = StringMap.find l w.shortname in
                       w.shortname <- StringMap.remove l w.shortname;
                       w.longname <- StringMap.remove s w.longname
                     with Not_found -> () end;
                     l
                 | _ ->
                     long_name subdir nm
               in
               if
                 not (kind = `MODIF && StringMap.mem nm (get_subdirs subdir))
               then
                 signal_change event_time subdir (Some nm) kind
         end else
           !time_ref := time)
      l;
    if l = [] then begin
      if !Watchercommon.debug then Format.eprintf "OVERFLOW@.";
      signal_overflow ()
    end;
    loop ()
  in
  ignore (Lwt.catch loop
            (fun e ->
               set_watch dir None;
               begin try Lwt_win.close_dir h with Unix.Unix_error _ -> () end;
               if !Watchercommon.debug then
                 Format.eprintf "Error while reading directory changes: %s@."
                   (Watchercommon.format_exc e); Lwt.return ()));
  h

let add_watch path file =
  if get_watch file = None then begin
    let watch_info =
      { handle = None;
        longname = StringMap.empty; shortname = StringMap.empty } in
    set_watch file (Some watch_info);
    if is_root file then begin
      try
        watch_info.handle <- Some (watch_root_directory path file)
      with Unix.Unix_error _ as e ->
        Watchercommon.error
          (Format.sprintf
             "Error while starting to watch for changes: %s@."
             (Watchercommon.format_exc e))
    end;
    let mapping =
      try Shortnames.in_directory path with Unix.Unix_error _ -> [] in
    watch_info.longname <-
      List.fold_left
        (fun m (l, s) ->
           if !Watchercommon.debug then Format.eprintf "%s -> %s@." l s;
           StringMap.add s l m)
        StringMap.empty mapping;
    watch_info.shortname <-
      List.fold_left (fun m (l, s) -> StringMap.add l s m)
        StringMap.empty mapping
  end

let release_watch file =
  match get_watch file with
    Some {handle = Some h} ->
      set_watch file None;
      begin try Lwt_win.close_dir h with Unix.Unix_error _ -> () end
  | _ ->
      set_watch file None

let watch () = ()  (* No global loop under Windows... *)

end

(****)

include F(Windows)
