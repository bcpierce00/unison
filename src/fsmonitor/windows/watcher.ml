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

type watch_def = { mutable handle : Lwt_win.directory_handle option }

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

let rec follow_win_path dir path pos =
  try
    let i = String.index_from path pos '\\' in
    let nm = String.sub path pos (i - pos) in
    try
      let dir = StringMap.find nm (get_subdirs dir) in
      follow_win_path dir path (i + 1)
    with Not_found ->
      if !Watchercommon.debug then
        Format.eprintf "Ignored directory %s in path %s@." nm path;
      None
  with Not_found ->
    Some (dir, Some (String.sub path pos (String.length path - pos)))

let rec follow_win_path_parent root dir path pos =
  try
    let i = String.index_from path pos '\\' in
    let nm = String.sub path pos (i - pos) in
    let getn nm =
      let dir = StringMap.find nm (get_subdirs dir) in
      follow_win_path_parent (root ^ "\\" ^ nm) dir path (i + 1)
    in
    try getn nm with Not_found -> getn (Lwt_win.longpathname root nm)
  with Not_found ->
    Some (dir, None)

let get_win_path root dir ((ev_path, act) as ev) =
  (* Blindly expand the event path to long names form. If event path
     is not found among the watched patchs then try to find the nearest
     parent directory and report a modification on it. MSDN states the
     following: "If there is both a short and long name for the file,
     [Lwt_win.readdirectorychanges] will return one of these names,
     but it is unspecified which one." *)
  let p = if event_kind ev = `DEL then None else
    follow_win_path dir (Lwt_win.longpathname root ev_path) 0 in
  match p with
  | Some _ as pathnm -> (pathnm, ev)
  | None ->
    (* If path is not found or event is a deletion then look up the
       parent directory and report a modification on it. It is not
       possible to expand the name of the deleted file or directory
       (it doesn't exist). *)
      (follow_win_path_parent root dir ev_path 0,
        (ev_path, Lwt_win.FILE_ACTION_MODIFIED))

let previous_event = ref None

let clear_event_memory () = previous_event := None

let flags =
  Lwt_win.([FILE_NOTIFY_CHANGE_FILE_NAME; FILE_NOTIFY_CHANGE_DIR_NAME;
            FILE_NOTIFY_CHANGE_ATTRIBUTES; (*FILE_NOTIFY_CHANGE_SIZE;*)
            FILE_NOTIFY_CHANGE_LAST_WRITE; FILE_NOTIFY_CHANGE_CREATION;
            (*FILE_NOTIFY_CHANGE_SECURITY*)])

let watch_root_directory path dir =
  let h = Lwt_win.open_directory path in
  let path = Lwt_win.longpathname "" path in
  let path =
    if String.sub path 0 4 = "\\\\?\\" then begin
      let n = String.sub path 4 (String.length path - 4) in
      if String.sub n 0 3 = "UNC" then
        "\\" ^ String.sub n 3 (String.length n - 3)
      else
        n
    end else
      path
  in
  let rec loop () =
    Lwt_win.readdirectorychanges h true flags >>= fun l ->
    let time = Unix.gettimeofday () in
    List.iter
      (fun ((ev_path, _) as ev) ->
         if !previous_event <> Some ev then begin
           previous_event := Some ev;
           if !Watchercommon.debug then print_event ev;
           let pathnm, ev = get_win_path path dir ev in
           match pathnm with
             None ->
               ()
           | Some (subdir, nm) ->
               let event_time =
                 if event_is_immediate ev then 0. else time in
               let kind = event_kind ev in
               signal_change event_time subdir nm kind
         end)
      l;
    if l = [] && get_watch dir <> None then begin
      if !Watchercommon.debug then Format.eprintf "OVERFLOW@.";
      signal_overflow ()
    end;
    if get_watch dir <> None then loop ()
    else Lwt.return ()
  in
  ignore (Lwt.catch loop
            (fun e ->
               set_watch dir None;
               begin try Lwt_win.close_dir h with Unix.Unix_error _ -> () end;
               if !Watchercommon.debug then
                 Format.eprintf "Error while reading directory changes: %s@."
                   (Watchercommon.format_exc e); Lwt.return ()));
  h

let add_watch path file _ =
  if get_watch file = None then begin
    let watch_info = { handle = None } in
    set_watch file (Some watch_info);
    if is_root file then
      try
        watch_info.handle <- Some (watch_root_directory path file)
      with Unix.Unix_error _ as e ->
        Watchercommon.error
          (Format.sprintf
             "Error while starting to watch for changes: %s@."
             (Watchercommon.format_exc e))
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
