(* Unison file synchronizer: src/fileinfo.ml *)
(* Copyright 1999-2020, Benjamin C. Pierce

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


let debugV = Util.debug "fileinfo+"

let allowSymlinks =
  Prefs.createBoolWithDefault "links"
    ~category:(`Advanced `Sync)
    "allow the synchronization of symbolic links (true/false/default)"
    ("When set to {\\tt true}, this flag causes Unison to synchronize \
      symbolic links.  When the flag is set to {\\tt false}, symbolic \
      links will be ignored during update detection.  \
      Ordinarily, when the flag is set to {\\tt default}, symbolic \
      links are synchronized except when one of the hosts is running \
      Windows.  On a Windows client, Unison makes an attempt to detect \
      if symbolic links are supported and allowed by user privileges.  \
      You may have to get elevated privileges to create symbolic links.  \
      When the flag is set to {\\tt default} and symbolic links can't be \
      synchronized then an error is produced during update detection.")

let symlinksAllowed =
  Prefs.createBool "links-aux" true
    ~category:(`Internal `Pseudo)
    "*Pseudo-preference for internal use only" ""

let init b =
  Prefs.set symlinksAllowed
    (Prefs.read allowSymlinks = `True ||
      (Prefs.read allowSymlinks = `Default &&
      (not b || System.hasSymlink ())))

type typ = [ `ABSENT | `FILE | `DIRECTORY | `SYMLINK ]

let mtyp = Umarshal.(sum4 unit unit unit unit
                       (function
                        | `ABSENT -> I41 ()
                        | `FILE -> I42 ()
                        | `DIRECTORY -> I43 ()
                        | `SYMLINK -> I44 ())
                       (function
                        | I41 () -> `ABSENT
                        | I42 () -> `FILE
                        | I43 () -> `DIRECTORY
                        | I44 () -> `SYMLINK))

let type2string = function
    `ABSENT    -> "nonexistent"
  | `FILE      -> "file"
  | `DIRECTORY -> "dir"
  | `SYMLINK   -> "symlink"

(* IMPORTANT!
   This is the 2.51-compatible version of type [Fileinfo.t]. It must always
   remain exactly the same as the type [Fileinfo.t] in version 2.51.5. This
   means that if any of the types it is composed of changes then for each
   changed type also a 2.51-compatible version must be created. *)
type t251 = { typ : typ; inode : int; desc : Props.t251; osX : Osx.info}

type ('a, 'b) info = { typ : typ; inode : int; desc : 'a; osX : Osx.info }
     constraint 'a = _ Props.props
type t = (Props.t, [`WithRess]) info
type basic = (Props.basic, [`NoRess]) info
type bress = (Props.basic, [`WithRess]) info

let minfo propsm = Umarshal.(prod4 mtyp int propsm Osx.minfo
                    (fun {typ; inode; desc; osX} -> typ, inode, desc, osX)
                    (fun (typ, inode, desc, osX) -> {typ; inode; desc; osX}))

let m = minfo Props.m
let mbasic = minfo Props.mbasic

let to_compat251 (x : basic) : t251 =
  { typ = x.typ;
    inode = x.inode;
    desc = Props.to_compat251 x.desc;
    osX = x.osX }

let of_compat251 (x : t251) : basic =
  { typ = x.typ;
    inode = x.inode;
    desc = Props.of_compat251 x.desc;
    osX = x.osX }

(* Stat function that pays attention to pref for following links             *)
let statFn fromRoot fspath path =
  let fullpath = Fspath.concat fspath path in
  let stats = Fs.lstat fullpath in
  if stats.Unix.LargeFile.st_kind = Unix.S_LNK
     && fromRoot
     && Path.followLink path
  then begin
    Fswatch.followLink path;
    try Fs.stat fullpath
    with Unix.Unix_error((Unix.ENOENT | Unix.ENOTDIR),_,_) ->
      raise (Util.Transient (Printf.sprintf
        "Path %s is marked 'follow' but its target is missing"
        (Fspath.toPrintString fullpath)))
  end else
    stats

(* Warning! Do not change this string without some backwards compatibility
   code in place. This string is not only meant for humans, it is also
   processed by code. *)
let symlinkErr = " is a symbolic link"
let symlinkErrLen = String.length symlinkErr

let shouldIgnore s =
  Prefs.read allowSymlinks = `False &&
    let l = String.length s in
    if l > symlinkErrLen then
      String.sub s (l - symlinkErrLen) symlinkErrLen = symlinkErr
    else
      false

let getAux fromRoot fspath path getProps =
  Util.convertUnixErrorsToTransient
  "querying file information"
    (fun () ->
       try
         let stats = statFn fromRoot fspath path in
         debugV (fun () ->
                   Util.msg "%s: %b %f %f\n"
                     (Fspath.toDebugString (Fspath.concat fspath path))
                     fromRoot stats.Unix.LargeFile.st_ctime stats.Unix.LargeFile.st_mtime);
         let typ =
           match stats.Unix.LargeFile.st_kind with
             Unix.S_REG -> Util.debug "fileinfo+" (fun () -> Util.msg "get: FILE\n"); `FILE
           | Unix.S_DIR -> `DIRECTORY
           | Unix.S_LNK ->
               if not fromRoot || Prefs.read symlinksAllowed then
                 `SYMLINK
               else
                 raise
                   (Util.Transient
                      ("path " ^
                       (Fspath.toPrintString (Fspath.concat fspath path)) ^
                       symlinkErr))
           | _ ->
               raise (Util.Transient
                        ("path " ^
                         (Fspath.toPrintString (Fspath.concat fspath path)) ^
                         " has unknown file type"))
         in
         let osxInfos = Osx.getFileInfos fspath path typ in
         { typ = typ;
           inode    = (* The inode number is truncated so that
                         it fits in a 31 bit ocaml integer *)
                      stats.Unix.LargeFile.st_ino land 0x3FFFFFFF;
           desc     = getProps fspath path stats osxInfos;
           osX      = osxInfos }
       with
         Unix.Unix_error((Unix.ENOENT | Unix.ENOTDIR),_,_) ->
         { typ = `ABSENT;
           inode    = 0;
           desc     = Props.dummy;
           osX      = Osx.getFileInfos fspath path `ABSENT })

let getType fromRoot fspath path =
  (getAux fromRoot fspath path (fun _ _ _ _ -> Props.dummy)).typ

let getBasic fromRoot fspath path =
  getAux fromRoot fspath path (fun _ _ st _ -> Props.get' st)

let getBasicWithRess fromRoot fspath path =
  getAux fromRoot fspath path (fun _ _ st i -> Props.getWithRess st i)

let get ?(archProps = Props.dummy) fromRoot fspath path =
  let getProps fspath path stats typ =
    Props.get ~archProps fspath path stats typ in
  getAux fromRoot fspath path getProps

let basic x =
  { typ = x.typ;
    inode = x.inode;
    desc = x.desc;
    osX = x.osX }

let check fspath path props =
  Util.convertUnixErrorsToTransient
  "checking file information"
    (fun () -> Props.check fspath path (statFn false fspath path) props)

let set fspath path action newDesc =
  let (kind, p) =
    match action with
      `Set defDesc ->
        (* Set the permissions and maybe the other properties                *)
        (* BCP [Nov 2008]: Jerome, in a message to unison-hackers on
           Oct 5, 2005, suggested that this would be better as
              `Set, Props.override (get false fspath path).desc newDesc
           but this does not seem right to me (bcp): if the file was just
           created, then its permissions are something like 0x600, whereas
           the default permissions will set the world read bit, etc. *)
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

(* IMPORTANT!
   This is the 2.51-compatible version of type [Fileinfo.stamp]. It must
   always remain exactly the same as the type [Fileinfo.stamp] in version
   2.51.5. *)
type stamp251 =
    InodeStamp of int         (* inode number, for Unix systems *)
  | CtimeStamp of float       (* creation time, for windows systems *)

type stamp =
  | InodeStamp of int         (* inode number, for Unix systems *)
  | NoStamp
  | RescanStamp               (* stamp indicating file should be rescanned
                                 (perhaps because previous transfer failed) *)

let mstamp = Umarshal.(sum3 int unit unit
                         (function
                          | InodeStamp a -> I31 a
                          | NoStamp -> I32 ()
                          | RescanStamp -> I33 ())
                         (function
                          | I31 a -> InodeStamp a
                          | I32 () -> NoStamp
                          | I33 () -> RescanStamp))

let stamp_to_compat251 (st : stamp) : stamp251 =
  match st with
  | InodeStamp i -> InodeStamp i
  | NoStamp -> CtimeStamp 0.0
  | RescanStamp -> InodeStamp (-1)

let stamp_of_compat251 (st : stamp251) : stamp =
  match st with
  | InodeStamp i -> if i <> -1 then InodeStamp i else RescanStamp
  | CtimeStamp _ -> NoStamp

let ignoreInodeNumbers =
  Prefs.createBool "ignoreinodenumbers" false
    ~category:(`Advanced `Syncprocess)
    "ignore inode number changes when detecting updates"
    ("When set to true, this preference makes Unison not take advantage \
      of inode numbers during fast update detection. \
      This switch should be used with care, as it \
      is less safe than the standard update detection method, but it \
      can be useful with filesystems which do not support inode numbers.")
let _ = Prefs.alias ignoreInodeNumbers "pretendwin"

let stamp info =
  if Prefs.read ignoreInodeNumbers then NoStamp else
  if Fs.hasInodeNumbers () then InodeStamp info.inode else NoStamp

let ressStamp info = Osx.stamp info.osX

let unchanged fspath path info =
  (* The call to [Util.time] must be before the call to [get] *)
  let t0 = Util.time () in
  let info' = get ~archProps:info.desc true fspath path in
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
