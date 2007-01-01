(* Unison file synchronizer: src/fileinfo.ml *)
(* Copyright 1999-2007 (see COPYING for details) *)

let debug = Util.debug "fileinfo"

type typ = [ `ABSENT | `FILE | `DIRECTORY | `SYMLINK ]

let type2string = function
    `ABSENT    -> "nonexistent"
  | `FILE      -> "file"
  | `DIRECTORY -> "dir"
  | `SYMLINK   -> "symlink"

type t = { typ : typ; inode : int; ctime : float;
           desc : Props.t; osX : Osx.info}

(* Stat function that pays attention to pref for following links             *)
let statFn fromRoot fspath path =
  let fullpath = Fspath.concat fspath path in
  let stats = Fspath.lstat fullpath in
  if stats.Unix.LargeFile.st_kind = Unix.S_LNK 
     && fromRoot 
     && Path.followLink path
  then 
    try Fspath.stat fullpath 
    with Unix.Unix_error((Unix.ENOENT | Unix.ENOTDIR),_,_) ->
      raise (Util.Transient (Printf.sprintf
        "Path %s is marked 'follow' but its target is missing"
        (Fspath.toString fullpath)))
  else
    stats

let get fromRoot fspath path =
  Util.convertUnixErrorsToTransient
  "querying file information"
    (fun () ->
       try
         let stats = statFn fromRoot fspath path in
         let typ =
           match stats.Unix.LargeFile.st_kind with
             Unix.S_REG -> `FILE
           | Unix.S_DIR -> `DIRECTORY
           | Unix.S_LNK -> `SYMLINK
           | _ ->
               raise (Util.Transient
                        ("path " ^
                         (Fspath.concatToString fspath path) ^
                         " has unknown file type"))
         in
         let osxInfos = Osx.getFileInfos fspath path typ in
         { typ = typ;
           inode    = stats.Unix.LargeFile.st_ino;
           ctime    = stats.Unix.LargeFile.st_ctime;
           desc     = Props.get stats osxInfos;
           osX      = osxInfos }
       with
         Unix.Unix_error((Unix.ENOENT | Unix.ENOTDIR),_,_) ->
         { typ = `ABSENT;
           inode    = 0;
           ctime    = 0.0;
           desc     = Props.dummy;
           osX      = Osx.getFileInfos fspath path `ABSENT })

let check fspath path props =
  Props.check fspath path (statFn false fspath path) props

let set fspath path action newDesc =
  let (kind, p) =
    match action with
      `Set defDesc ->
        (* Set the permissions and maybe the other properties                *)
        `Set, Props.override defDesc newDesc
    | `Copy oldPath ->
        (* Set the permissions (using the permissions of the file at         *)
        (* [oldPath] as a default) and maybe the other properties            *)
        `Set, Props.override (get false fspath oldPath).desc newDesc
    | `Update oldDesc ->
        (* Update the different properties (only if necessary)               *)
        `Update,
        Props.override
          (get false fspath path).desc (Props.diff oldDesc newDesc)
  in
  Props.set fspath path kind p;
  check fspath path p

type stamp =
    InodeStamp of int         (* inode number, for Unix systems *)
  | CtimeStamp of float       (* creation time, for windows systems *)

let pretendLocalOSIsWin32 =
  Prefs.createBool "pretendwin" false
    "Use creation times for detecting updates"
    ("When set to true, this preference makes Unison use Windows-style "
  ^ "fast update detection (using file creation times as "
  ^ "``pseudo-inode-numbers''), even when running on a Unix system.  This "
  ^ "switch should be used with care, as it is less safe than the standard "
  ^ "update detection method, but it can be useful for synchronizing VFAT "
  ^ "filesystems (which do not support inode numbers) mounted on Unix "
  ^ "systems.  The {\\tt fastcheck} option should also be set to true.")

let stamp info =
  if Prefs.read pretendLocalOSIsWin32 then CtimeStamp info.ctime else
  match Util.osType with
    `Unix  -> InodeStamp info.inode
  | `Win32 -> CtimeStamp info.ctime

let ressStamp info = Osx.stamp info.osX

let unchanged fspath path info =
  (* The call to [Util.time] must be before the call to [get] *)
  let t0 = Util.time () in
  let info' = get true fspath path in
  let dataUnchanged =
    Props.same_time info.desc info'.desc
      &&
    stamp info = stamp info'
      &&
    if Props.time info'.desc = t0 then begin
      Unix.sleep 1;
      false
    end else
      true
  in
  (info', dataUnchanged,
   Osx.ressUnchanged info.osX.Osx.ressInfo info'.osX.Osx.ressInfo
     (Some t0) dataUnchanged)
