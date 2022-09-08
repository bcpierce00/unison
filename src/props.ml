(* Unison file synchronizer: src/props.ml *)
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


let debug = Util.debug "props"
let debugverbose = Util.debug "props+"

module type S = sig
  type t
  val m : t Umarshal.t
  val dummy : t
  val hash : t -> int -> int
  val similar : t -> t -> bool
  val override : t -> t -> t
  val strip : t -> t
  val diff : t -> t -> t
  val toString : t -> string
  val syncedPartsToString : t -> string
  val set : Fspath.t -> t -> unit
  val get : Unix.LargeFile.stats -> t
end

(* Nb: the syncedPartsToString call is only used for archive dumping, for    *)
(* debugging purposes.  It could be deleted without losing functionality.    *)

(**** Permissions ****)

module Perm : sig
  include S
  val fileDefault : t
  val fileSafe : t
  val dirDefault : t
  val extract : t -> int
  val set : Fspath.t -> [`Set | `Update] -> t -> unit
  val check : Fspath.t -> Path.local -> Unix.LargeFile.stats -> t -> unit
  val validatePrefs : unit -> unit
  val permMask : int Prefs.t
  val dontChmod : bool Prefs.t
  val init : bool -> unit
end = struct

(* We introduce a type, Perm.t, that holds a file's permissions along with   *)
(* the operating system where the file resides. Different operating systems  *)
(* have different permission systems, so we have to take the OS into account *)
(* when comparing and setting permissions.  We also need an "impossible"     *)
(* permission that to take care of a tricky special case in update           *)
(* detection.  It can be that the archive contains a directory that has      *)
(* never been synchronized, although some of its children have been.  In     *)
(* this case, the directory's permissions have never been synchronized and   *)
(* might be different on the two replicas.  We use NullPerm for the          *)
(* permissions of such an archive entry, and ensure (in similarPerms) that   *)
(* NullPerm is never similar to any real permission.                         *)

(* NOTE: IF YOU CHANGE TYPE "PERM", THE ARCHIVE FORMAT CHANGES; INCREMENT    *)
(* "UPDATE.ARCHIVEFORMAT"                                                    *)
type t = int * int

let m = Umarshal.(prod2 int int id id)

(* This allows us to export NullPerm while keeping the type perm abstract    *)
let dummy = (0, 0)

let extract = fst

let unix_mask =
    0o7777 (* All bits *)
let wind_mask =
    0o200 (* -w------- : only the write bit can be changed in Windows *)

let permMask =
  Prefs.createInt "perms"
    (0o777 (* rwxrwxrwx *) + 0o1000 (* Sticky bit *))
    ~category:(`Basic `Sync)
    "part of the permissions which is synchronized"
    "The integer value of this preference is a mask indicating which \
     permission bits should be synchronized.  It is set by default to \
     $0o1777$: all bits but the set-uid and set-gid bits are \
     synchronised (synchronizing theses latter bits can be a security \
     hazard).  If you want to synchronize all bits, you can set the \
     value of this preference to $-1$.  If one of the replica is on \
     a FAT [Windows] filesystem, you should consider using the \
     {\\tt fat} preference instead of this preference.  If you need \
     Unison not to set permissions at all, set the value of this \
     preference to $0$ and set the preference {\\tt dontchmod} to {\\tt true}."

(* Os-specific local conventions on file permissions                         *)
let (fileDefault, dirDefault, fileSafe, dirSafe) =
  match Util.osType with
    `Win32 ->
      debug
        (fun() ->
           Util.msg "Using windows defaults for file permissions");
      ((0o600, -1), (* rw------- *)
       (0o700, -1), (* rwx------ *)
       (0o600, -1), (* rw------- *)
       (0o700, -1)) (* rwx------ *)
  | `Unix ->
      let umask =
        let u = Unix.umask 0 in
        ignore (Unix.umask u);
        debug
          (fun() ->
             Util.msg "Umask: %s" (Printf.sprintf "%o" u));
        (fun fp -> (lnot u) land fp) in
      ((umask 0o666, -1), (* rw-rw-rw- *)
       (umask 0o777, -1), (* rwxrwxrwx *)
       (umask 0o600, -1), (* rw------- *)
       (umask 0o700, -1)) (* rwx------ *)

let hash (p, m) h = Uutil.hash2 (p land m) (Uutil.hash2 m h)

let perm2fileperm (p, m) = p
let fileperm2perm p = (p, Prefs.read permMask)

(* Are two perms similar (for update detection and recon)                    *)
let similar (p1, m1) (p2, m2) =
  let m = Prefs.read permMask in
  m1 land m = m && m2 land m = m &&
  p1 land m = p2 land m

(* overrideCommonPermsIn p1 p2 : gives the perm that would result from       *)
(* propagating p2 to p1. We expect the following invariants: similarPerms    *)
(* (overrideCommonPermsIn p1 p2) p2 (whenever similarPerms p2 p2) and        *)
(* hashPerm (overrideCommonPermsIn p1 p2) = hashPerm p2                      *)
let override (p1, m1) (p2, m2) =
  let m = Prefs.read permMask land m2 in
  ((p1 land (lnot m)) lor (p2 land m), m)

let strip (p, m) = (p, m land (Prefs.read permMask))

let diff (p, m) (p', m') = (p', (p lxor p') land m land m')

let toString =
  function
    (_, 0) -> "unknown permissions"
  | (fp, _) when Prefs.read permMask = wind_mask ->
      if fp land wind_mask <> 0 then "read-write" else "read-only"
  | (fp, _) ->
     let m = Prefs.read permMask in
     let bit mb unknown off on =
       if mb land m = 0 then
         unknown
       else if fp land mb <> 0 then
         on
       else
         off
     in
     bit 0o4000 "" "-"  "S" ^
     bit 0o2000 "" "-"  "s" ^
     bit 0o1000 "?" ""  "t" ^
     bit 0o0400 "?" "-" "r" ^
     bit 0o0200 "?" "-" "w" ^
     bit 0o0100 "?" "-" "x" ^
     bit 0o0040 "?" "-" "r" ^
     bit 0o0020 "?" "-" "w" ^
     bit 0o0010 "?" "-" "x" ^
     bit 0o0004 "?" "-" "r" ^
     bit 0o0002 "?" "-" "w" ^
     bit 0o0001 "?" "-" "x"

let syncedPartsToString =
  function
    (_, 0) -> "unknown permissions"
  | (fp, m) ->
     let bit mb unknown off on =
       if mb land m = 0 then
         unknown
       else if fp land mb <> 0 then
         on
       else
         off
     in
     bit 0o4000 "" "-"  "S" ^
     bit 0o2000 "" "-"  "s" ^
     bit 0o1000 "?" ""  "t" ^
     bit 0o0400 "?" "-" "r" ^
     bit 0o0200 "?" "-" "w" ^
     bit 0o0100 "?" "-" "x" ^
     bit 0o0040 "?" "-" "r" ^
     bit 0o0020 "?" "-" "w" ^
     bit 0o0010 "?" "-" "x" ^
     bit 0o0004 "?" "-" "r" ^
     bit 0o0002 "?" "-" "w" ^
     bit 0o0001 "?" "-" "x"

let dontChmod =
  Prefs.createBool "dontchmod"
  false
  ~category:(`Advanced `Syncprocess)
  "when set, never use the chmod system call"
  (  "By default, Unison uses the 'chmod' system call to set the permission bits"
  ^ " of files after it has copied them.  But in some circumstances (and under "
  ^ " some operating systems), the chmod call always fails.  Setting this "
  ^ " preference completely prevents Unison from ever calling chmod.")

let validatePrefs () =
  if Prefs.read dontChmod && (Prefs.read permMask <> 0) then raise (Util.Fatal
    "If the 'dontchmod' preference is set, the 'perms' preference should be 0")

let set abspath kind (fp, mask) =
  (* BCP: removed "|| kind <> `Update" on 10/2005, but reinserted it on 11/2008.
     I'd removed it to make Dale Worley happy -- he wanted a way to make sure that
     Unison would never call chmod, and setting prefs to 0 seemed like a reasonable
     way to do this.  But in fact it caused new files to be created with wrong prefs.
   *)
  if (mask <> 0 || kind = `Set) && (not (Prefs.read dontChmod)) then
    Util.convertUnixErrorsToTransient
    "setting permissions"
      (fun () ->
        debug
          (fun() ->
            Util.msg "Setting permissions for %s to %s (%s)\n"
              (Fspath.toDebugString abspath) (toString (fileperm2perm fp))
              (Printf.sprintf "%o/%o" fp mask));
        try
          Fs.chmod abspath fp
        with Unix.Unix_error (Unix.EOPNOTSUPP, _, _) as e ->
          try
            Util.convertUnixErrorsToTransient "setting permissions"
              (fun () -> raise e)
          with Util.Transient msg ->
            raise (Util.Transient
                     (msg ^
                      ". You can use preference \"fat\",\
                       or else set preference \"perms\" to 0 and \
                       preference \"dontchmod\" to true to avoid this error")))

let get stats = (stats.Unix.LargeFile.st_perm, Prefs.read permMask)

let check fspath path stats (fp, mask) =
  let fp' = stats.Unix.LargeFile.st_perm in
  if fp land mask <> fp' land mask then
    raise
      (Util.Transient
         (Format.sprintf
            "Failed to set permissions of file %s to %s: \
             the permissions was set to %s instead. \
             The filesystem probably does not support all permission bits. \
             If this is a FAT filesystem, you should set the \"fat\" option \
             to true. \
             Otherwise, you should probably set the \"perms\" option to 0o%o \
             (or to 0 if you don't need to synchronize permissions)."
            (Fspath.toPrintString (Fspath.concat fspath path))
            (syncedPartsToString (fp, mask))
            (syncedPartsToString (fp', mask))
            ((Prefs.read permMask) land (lnot (fp lxor fp')))))

let init someHostIsRunningWindows =
  let mask = if someHostIsRunningWindows then wind_mask else unix_mask in
  let oldMask = Prefs.read permMask in
  let newMask = oldMask land mask in
  debug
    (fun() ->
      Util.msg "Setting permission mask to %s (%s and %s)\n"
        (Printf.sprintf "%o" newMask)
        (Printf.sprintf "%o" oldMask)
        (Printf.sprintf "%o" mask));
  Prefs.set permMask newMask

end

(* ------------------------------------------------------------------------- *)
(*                         User and group ids                                *)
(* ------------------------------------------------------------------------- *)

let numericIds =
  Prefs.createBool "numericids" false
    ~category:(`Advanced `Syncprocess)
    "don't map uid/gid values by user/group names"
    "When this flag is set to \\verb|true|, groups and users are \
     synchronized numerically, rather than by name. \n\
     \n\
     The special uid 0 and the special group 0 are never mapped via \
     user/group names even if this preference is not set."

(* For backward compatibility *)
let _ = Prefs.alias numericIds "numericIds"

module Id (M : sig
  val sync : bool Prefs.t
  val kind : string
  val to_num : string -> int
  val toString : int -> string
  val syncedPartsToString : int -> string
  val set : Fspath.t -> int -> unit
  val get : Unix.LargeFile.stats -> int
end) : sig
  include S
  val init : bool -> unit
end = struct

type t =
    IdIgnored
  | IdNamed of string
  | IdNumeric of int

let m = Umarshal.(sum3 unit string int
                    (function
                     | IdIgnored -> I31 ()
                     | IdNamed a -> I32 a
                     | IdNumeric a -> I33 a)
                    (function
                     | I31 () -> IdIgnored
                     | I32 a -> IdNamed a
                     | I33 a -> IdNumeric a))

let dummy = IdIgnored

let hash id h =
  Uutil.hash2
    (match id with
       IdIgnored   -> -1
     | IdNumeric i -> i
     | IdNamed nm  -> Uutil.hash nm)
    h

let similar id id' =
  not (Prefs.read M.sync)
    ||
  (id <> IdIgnored && id' <> IdIgnored && id = id')

let override id id' = id'

let strip id = if Prefs.read M.sync then id else IdIgnored

let diff id id' = if similar id id' then IdIgnored else id'

let toString id =
  match id with
    IdIgnored   -> ""
  | IdNumeric i -> " " ^ M.kind ^ "=" ^ string_of_int i
  | IdNamed n   -> " " ^ M.kind ^ "=" ^ n

let syncedPartsToString = toString

let tbl = Hashtbl.create 17

let extern id =
  match id with
    IdIgnored   -> -1
  | IdNumeric i -> i
  | IdNamed nm  ->
      try
        Hashtbl.find tbl nm
      with Not_found ->
        let id =
          try M.to_num nm with Not_found ->
            raise (Util.Transient ("No " ^ M.kind ^ " " ^ nm))
        in
        if id = 0 then
          raise (Util.Transient
                   (Printf.sprintf "Trying to map the non-root %s %s to %s 0"
                      M.kind nm M.kind));
        Hashtbl.add tbl nm id;
        id

let set abspath id =
  match extern id with
    -1 ->
      ()
  | id ->
      Util.convertUnixErrorsToTransient
        "setting file ownership"
        (fun () ->
           M.set abspath id)

let tbl = Hashtbl.create 17

let get stats =
  if not (Prefs.read M.sync) then IdIgnored else
  let id = M.get stats in
  if id = 0 || Prefs.read numericIds then IdNumeric id else
  try
    Hashtbl.find tbl id
  with Not_found ->
    let id' = try IdNamed (M.toString id) with Not_found -> IdNumeric id in
    Hashtbl.add tbl id id';
    id'

let init someHostIsRunningWindows =
  if someHostIsRunningWindows then
    Prefs.set M.sync false;

end

module Uid = Id (struct

let sync =
  Prefs.createBool "owner" false
    ~category:(`Basic `Sync)
    "synchronize owner"
    ("When this flag is set to \\verb|true|, the owner attributes "
     ^ "of the files are synchronized.  "
     ^ "Whether the owner names or the owner identifiers are synchronized"
     ^ "depends on the preference \\texttt{numerids}.")

let kind = "user"

let to_num nm = (Unix.getpwnam nm).Unix.pw_uid
let toString id = (Unix.getpwuid id).Unix.pw_name
let syncedPartsToString = toString

let set path id = Fs.chown path id (-1)
let get stats = stats.Unix.LargeFile.st_uid

end)

module Gid = Id (struct

let sync =
  Prefs.createBool "group" false
    ~category:(`Basic `Sync)
    "synchronize group attributes"
    ("When this flag is set to \\verb|true|, the group attributes "
     ^ "of the files are synchronized.  "
     ^ "Whether the group names or the group identifiers are synchronized "
     ^ "depends on the preference \\texttt{numerids}.")

let kind = "group"

let to_num nm = (Unix.getgrnam nm).Unix.gr_gid
let toString id = (Unix.getgrgid id).Unix.gr_name
let syncedPartsToString = toString

let set path id = Fs.chown path (-1) id
let get stats = stats.Unix.LargeFile.st_gid

end)

(* ------------------------------------------------------------------------- *)
(*                          Modification time                                *)
(* ------------------------------------------------------------------------- *)

module Time : sig
  include S
  val same : t -> t -> bool
  val extract : t -> float
  val sync : bool Prefs.t
  val replace : t -> float -> t
  val check : Fspath.t -> Path.local -> Unix.LargeFile.stats -> t -> unit
end = struct

let sync =
  Prefs.createBool "times" false
    ~category:(`Basic `Sync)
    "synchronize modification times"
    "When this flag is set to \\verb|true|, \
     file modification times (but not directory modtimes) are propagated."

type t = Synced of float | NotSynced of float

let m = Umarshal.(sum2 float float
                    (function
                     | Synced a -> I21 a
                     | NotSynced a -> I22 a)
                    (function
                     | I21 a -> Synced a
                     | I22 a -> NotSynced a))

let dummy = NotSynced 0.

let extract t = match t with Synced v -> v | NotSynced v -> v

let minus_two = Int64.of_int (-2)
let approximate t = Int64.logand (Int64.of_float t) minus_two

let oneHour = Int64.of_int 3600
let minusOneHour = Int64.neg oneHour
let moduloOneHour t =
  let v = Int64.rem t oneHour in
  if v >= Int64.zero then v else Int64.add v oneHour

(* Accept one hour differences and one second differences *)
let possible_deltas =
  [ -3601L; 3601L; -3600L; 3600L; -3599L; 3599L; -1L; 1L; 0L ]

let hash t h =
  Uutil.hash2
    (match t with
       Synced _    -> 1 (* As we are ignoring one-second differences,
                           we cannot provide a more accurate hash. *)
     | NotSynced _ -> 0)
    h

(* Times have a two-second granularity on FAT filesystems.  They are
   approximated upward under Windows, downward under Linux...
   Ignoring one-second changes also makes Unison more robust when
   dealing with systems with sub-second granularity (we have no control
   on how this is may be rounded). *)
let similar t t' =
  not (Prefs.read sync)
    ||
  match t, t' with
    Synced v, Synced v'      ->
      List.mem (Int64.sub (Int64.of_float v)  (Int64.of_float v'))
        possible_deltas
  | NotSynced _, NotSynced _ ->
      true
  | _                        ->
      false

let override t t' =
  match t, t' with
    _, Synced _ -> t'
  | Synced v, _ -> NotSynced v
  | _           -> t

let replace t v =
  match t with
    Synced _    -> Synced v
  | NotSynced _ -> NotSynced v

let strip t =
  match t with
    Synced v when not (Prefs.read sync) -> NotSynced v
  |  _                                  -> t

let diff t t' = if similar t t' then NotSynced (extract t') else t'

let toString t = Util.time2string (extract t)

let syncedPartsToString t = match t with
  Synced _    -> Format.sprintf "%s (%f)" (toString t) (extract t)
| NotSynced _ -> ""

(* FIX: Probably there should be a check here that prevents us from ever     *)
(* setting a file's modtime into the future.                                 *)
let set abspath t =
  match t with
    Synced v ->
      Util.convertUnixErrorsToTransient
        "setting modification time"
        (fun () ->
           if false then begin
             (* A special hack for Rasmus, who has a special situation that
                requires the utimes-setting program to run 'setuid root'
                (and we do not want all of Unison to run setuid, so we just
                spin off an external utility to do it). *)
             let time = Unix.localtime v in
             let tstr = Printf.sprintf
                          "%4d%02d%02d%02d%02d.%02d"
                          (time.Unix.tm_year + 1900)
                          (time.Unix.tm_mon + 1)
                          time.Unix.tm_mday
                          time.Unix.tm_hour
                          time.Unix.tm_min
                          time.Unix.tm_sec in
             let cmd = "/usr/local/bin/sudo -u root /usr/bin/touch -m -a -t "
                       ^ tstr ^ " " ^ Fspath.quotes abspath in
             Util.msg "Running external program to set utimes:\n  %s\n" cmd;
             let (r,_) = Lwt_unix.run (External.runExternalProgram cmd) in
             if r<>(Unix.WEXITED 0) then raise (Util.Transient "External time-setting command failed")
           end else
             Fs.utimes abspath (if v = 0. then 1e-12 else v) v)
             (* If atime and mtime arguments are both 0 then Unix.utimes
                will set actual atime and mtime on the file to be the
                current timestamp, which is not the desired result.
                To sync the exact mtime value of 0, atime must be non-zero.
                Setting atime to be different from zero by less than a
                nanosecond allows to achieve the desired result.
                https://github.com/bcpierce00/unison/issues/223 *)
  | _ ->
      ()

let get stats =
  let v = stats.Unix.LargeFile.st_mtime in
  if stats.Unix.LargeFile.st_kind = Unix.S_REG && Prefs.read sync then
    Synced v
  else
    NotSynced v

let check fspath path stats t =
  match t with
    NotSynced _ ->
      ()
  | Synced v ->
      let t' = Synced (stats.Unix.LargeFile.st_mtime) in
      if not (similar t t') then
        raise
          (Util.Transient
             (Format.sprintf
                "Failed to set modification time of file %s to %s: \
             the time was set to %s instead"
            (Fspath.toPrintString (Fspath.concat fspath path))
            (syncedPartsToString t)
            (syncedPartsToString t')))

(* When modification time are synchronized, we cannot update the
   archive when they are changed due to daylight saving time.  Thus,
   we have to compare then using "similar". *)
let same p p' =
  match p, p' with
    Synced _, Synced _ ->
      similar p p'
  | _                  ->
      let delta = extract p -. extract p' in
      delta = 0. || delta = 3600. || delta = -3600.

end

(* ------------------------------------------------------------------------- *)
(*                          Type and creator                                 *)
(* ------------------------------------------------------------------------- *)

module TypeCreator :
  sig
    include S
    val set : Fspath.t -> Path.local -> t -> unit
    val get : Unix.LargeFile.stats -> Osx.info -> t
  end = struct

type t = string option

let m = Umarshal.(option string)

let dummy = None

let hash t h = Uutil.hash2 (Uutil.hash t) h

let similar t t' =
  not (Prefs.read Osx.rsrc) || t = t'

let override t t' = t'

let strip t = t

let diff t t' = if similar t t' then None else t'

let zeroes = "\000\000\000\000\000\000\000\000"

let toString t =
  match t with
    Some s when String.length s > 0 && s.[0] = 'F' &&
                String.sub (s ^ zeroes) 1 8 <> zeroes ->
      let s = s ^ zeroes in
      " " ^ String.escaped (String.sub s 1 4) ^
      " " ^ String.escaped (String.sub s 5 4)
  | _ ->
      ""

let syncedPartsToString = toString

let set fspath path t =
  match t with
    None   -> ()
  | Some t -> Osx.setFileInfos fspath path t

let get stats info =
  if
    Prefs.read Osx.rsrc &&
    (stats.Unix.LargeFile.st_kind = Unix.S_REG ||
     stats.Unix.LargeFile.st_kind = Unix.S_DIR)
  then
    Some info.Osx.finfo
  else
    None

end

(* ------------------------------------------------------------------------- *)
(*                               Change time                                 *)
(* ------------------------------------------------------------------------- *)

(* ctime itself is never synchronized. It is only leveraged for faster
   metadata update detection; and stored in archive for this purpose. *)

module CTime : sig
  type t
  val m : t Umarshal.t
  val dummy : t
  val override : t -> t -> t
  val get : Unix.LargeFile.stats -> t
  val same_time : t -> t -> bool
end = struct

type t = float

let m = Umarshal.float

let dummy = -1.

(* Currently [override] does not work for ctime because the real on-disk
   ctime will inevitably change when the final props are set on disk by
   [Files.setProp] or the final rename after copying is done in [Files.copy]
   (these happen after [override]). There is no [stat] done after these
   operations, so this final ctime will not get stored in the archive.
   It is not a major issue and doesn't break anything. The only side-effect is
   that at next updates scan the entire set of metadata for this file/dir is
   scanned (as if fastcheck was disabled); which may even be a good thing.
   Not worth changing or adding the cost of an additional [stat]. But if it
   is changed in future then the proper ctime value must be extracted in
   [Props.get']. *)
let override t t' = t

let get stats = stats.Unix.LargeFile.st_ctime

let same_time t t' = System.hasCorrectCTime && t = t'

end

(* ------------------------------------------------------------------------- *)
(*                        Extended attributes (xattr)                        *)
(* ------------------------------------------------------------------------- *)

let featXattrValid = ref (fun _ _ -> None)

let featXattr =
  Features.register "Sync: xattr" ~arcFormatChange:true
  (Some (fun a b -> !featXattrValid a b))

let xattrEnabled () = Features.enabled featXattr

let syncXattrs =
  Prefs.createBool "xattrs" false
    ~category:(`Advanced `Sync)
    ~send:xattrEnabled
    "synchronize extended attributes (xattrs)"
    ("When this flag is set to \\verb|true|, the extended attributes of \
     files and directories are synchronized. System extended attributes \
     are not synchronized.")

let () = featXattrValid :=
  fun _ enabledThis ->
    if not enabledThis && Prefs.read syncXattrs then
      Some ("You have requested synchronization of extended attributes (the \
        \"xattrs\" preference) but the server does not support this.")
    else None

let xattrIgnorePred =
  Pred.create "xattrignore"
    ~category:(`Advanced `Sync)
    ~send:xattrEnabled
    (* By default ignore the Linux xattr security and trusted namespaces *)
    ~initial:["Regex !(security|trusted)[.].*"]
    ("Preference \\texttt{-xattrignore \\ARG{namespec}} causes Unison to \
     ignore extended attributes with names that match \\ARG{namespec}. \
     This can be used to exclude extended attributes that would fail \
     synchronization due to lack of permissions or technical differences \
     at replicas. The syntax of \\ARG{namespec} is the same as used \
     for path specification (described in \
     \\sectionref{pathspec}{Path Specification}); prefer the \\verb|Path| \
     and \\verb|Regex| forms over the \\verb|Name| form. The pattern is \
     applied to the {\\em name} of extended attribute, not to path. \
     {\\em On Linux}, attributes in the security and trusted namespaces \
     are ignored by default (this is achieved by pattern \\texttt{Regex \
     !(security|trusted)[.].*}). To sync attributes in one or both of \
     these namespaces, see the \\verb|xattrignorenot| preference. \
     Note that the namespace name must be prefixed with a \"!\" (applies \
     on Linux only). All names not prefixed with a \"!\" are taken \
     as strictly belonging to the user namespace and therefore the \
     \"!user.\" prefix is never used.")

let xattrIgnorenotPred =
  Pred.create "xattrignorenot"
    ~category:(`Advanced `Sync)
    ~send:xattrEnabled
    ("This preference overrides the preference \\texttt{xattrignore}. \
     It gives a list of patterns (in the same format as \
     \\verb|xattrignore|) for extended attributes that should {\\em not} \
     be ignored, whether or not they happen to match one of the \
     \\verb|xattrignore| patterns. It is possible to synchronize only \
     desired attributes by ignoring all attributes (for example, by \
     setting \\verb|xattrignore| to \\texttt{Path *} and then adding \
     \\verb|xattrignorenot| for extended attributes that should be \
     synchronized. \
     {\\em On Linux}, attributes in the security and trusted namespaces \
     are ignored by default. To sync attributes in one or both of these \
     namespaces, you may add an \\verb|xattrignorenot| pattern like \
     \\texttt{Path !security.*} to sync all attributes in the \
     security namespace, or \\texttt{Path !security.selinux} to sync \
     a specific attribute in an otherwise ignored namespace. \
     Note that the namespace name must be prefixed with a \"!\" (applies \
     on Linux only). All names not prefixed with a \"!\" are taken \
     as strictly belonging to the user namespace and therefore the \
     \"!user.\" prefix is never used.")

module Xattr : sig
  include S
  val ctimeDetect : bool
  val get : Fspath.t -> Unix.LargeFile.stats -> t
  val readAll : Fspath.t -> t -> t
  val getAll : t -> t
  val purge : t -> t
  val check : Fspath.t -> Path.local -> Unix.LargeFile.stats -> t -> unit
  module Data : Propsdata.S
end = struct

module Size = Uutil.Filesize

module Data = Propsdata.Xattr

module Cache = struct
  let get key = Data.find_opt key

  let add key value =
    (* Cache relatively small data in a relatively small quantity to keep
       the memory pressure and network traffic at updates scanning low.

       There is no cache management. Once it's full, it's full. This can be
       enhanced in future, if needed. *)
    if String.length value < 1024 && Data.length () < 200 then
      Data.add key value;
    value
end

type attrvalue =
  | String of string
  | Hash of string
  | Loaded of (string * string) (* full value, hash *)

let mattrvalue = Umarshal.(sum3 string string (prod2 string string id id)
                            (function
                             | String v -> I31 v
                             | Hash v -> I32 v
                             | Loaded v -> I33 v)
                            (function
                             | I31 v -> String v
                             | I32 v -> Hash v
                             | I33 v -> Loaded v))

type attrlist = (string * attrvalue) list

let mattrlist = Umarshal.(list (prod2 string mattrvalue id id))

type sizeandattrs = attrlist * Uutil.Filesize.t

let msizeandattrs = Umarshal.(prod2 mattrlist Uutil.Filesize.m id id)

(* None indicates xattrs are not supported. This is not synchronized.
 * An empty list means xattrs are supported but there are none on the file.
 * This will be synchronized. *)
type t = sizeandattrs option

let dummy = None

let m = Umarshal.cond xattrEnabled dummy Umarshal.(option msizeandattrs)

let ctimeDetect = System.xattrUpdatesCTime

(* Since [hash] is supposed to be run after [purge] (resulting in the
   data that is stored in the archives) then we don't need to take
   into account the difference between Hash and Loaded.

   The attribute list must be sorted to get a stable hash. The list
   is sorted once, when retrieving it from fs. If sorting conditions
   are changed in future then this hash function may have to be
   changed to retain backwards compatibility. *)
let hash t h = if Prefs.read syncXattrs then Uutil.hash2 (Uutil.hash t) h else h

let attrToString = function
  | (n, String v) ->
      Printf.sprintf "Name: %s    Value: %s" n (String.escaped v)
  | (n, Hash h) ->
      Printf.sprintf "Name: %s    Fingerprint: %s" n (Digest.to_hex h)
  | (n, Loaded (_, h)) ->
      Printf.sprintf "Name: %s    Fingerprint: %s" n (Digest.to_hex h)

let toString' style = function
  | Some ([], _) -> "0 xattrs"
  | Some ([(n, _) as x], z) ->
      Printf.sprintf "1 xattr (%s bytes)%s" (Size.toString z)
        (match style with
        | `Summary -> ""
        | `Simple -> ": " ^ n
        | `Verbose -> ": " ^ attrToString x)
  | Some (l, z) ->
      Printf.sprintf "%u xattrs (%s bytes)%s" (Safelist.length l) (Size.toString z)
        (match style with
        | `Summary -> ""
        | `Simple -> ": " ^ (String.concat ", " (Safelist.map (fun (n, _) -> n) l))
        | `Verbose -> "\n  " ^ (String.concat "\n  " (Safelist.map attrToString l)))
  | None -> ""

let toString = function
  | None -> ""
  | t -> " " ^ toString' `Summary t

let syncedPartsToString t = " " ^ toString' `Simple t

let toDebugString t = toString' `Simple t

let toStringVerb t = toString' `Verbose t

let attrEqual (n, v) (n', v') =
  String.equal n n' &&
  match v, v' with
  | String a, String b
  | String a, Loaded (b, _)
  | Hash a, Hash b
  | Hash a, Loaded (_, b)
  | Loaded (a, _), String b
  | Loaded (_, a), Hash b
  | Loaded (_, a), Loaded (_, b) -> String.equal a b
  | String s, Hash h
  | Hash h, String s -> String.equal h (Digest.string s)

let rec attrlist_mem x = function
  | [] -> false
  | a :: l -> attrEqual a x || attrlist_mem x l

let similar t t' =
  not (Prefs.read syncXattrs)
    ||
  match t, t' with
  | None, None -> true
  | Some (l, z), Some (l', z') ->
      Int64.equal (Size.toInt64 z) (Size.toInt64 z') &&
      Safelist.length l = Safelist.length l' &&
        Safelist.for_all (fun m -> attrlist_mem m l') l
  | _ -> false

let override t t' = t'

let strip t = if Prefs.read syncXattrs then t else None

let diff t t' = if similar t t' then None else t'

let wrapFail default f =
  try f () with
  | Fs.XattrNotSupported -> default
  | Failure msg ->
      raise (Util.Transient (msg ^
        ". You can set preference \"xattrs\" to false to avoid this error."))

let optMap f = function None -> None | Some x -> Some (f x)
let optAttrsMap f = optMap (fun (l, z) -> (Safelist.map f l, z))

let purge t =
  optAttrsMap (function (n, Loaded (_, h)) -> (n, Hash h) | x -> x) t

let readAll path t =
  let f = function
    | (n, Hash h) ->
        debugverbose (fun () ->
          Util.msg "Reading xattr %s for %s\n" n (Fspath.toDebugString path));
        let v' =
          match Cache.get h with
          | Some v ->
              debugverbose (fun () -> Util.msg "Read xattr %s from cache\n" n);
              v
          | None ->
              let v = Fs.xattr_get path n in
              if Digest.string v <> h then
                raise (Util.Transient (
                  Printf.sprintf "The value of extended attribute '%s' has \
                    changed on source file %s" n (Fspath.toPrintString path)))
              else
                Cache.add h v
        in
        (n, Loaded (v', h))
    | x -> x
  in
  if Prefs.read syncXattrs then
    wrapFail t (fun () -> optAttrsMap f t)
  else
    t

let getAll t =
  let f = function
    | (n, Hash h) ->
        begin match Cache.get h with
        | Some v ->
            debugverbose (fun () -> Util.msg "Got xattr %s from cache\n" n);
            (n, Loaded (v, h))
        | None -> raise Not_found
        end
    | x -> x
  in
  if Prefs.read syncXattrs then
    wrapFail t (fun () -> optAttrsMap f t)
  else
    t

let skipIgnoredXattr l =
  Safelist.filter (fun (n, _) ->
    let keep =
      not (Pred.test xattrIgnorePred n) || (Pred.test xattrIgnorenotPred n) in
    debugverbose (fun () ->
      Util.msg "Xattr: attribute %s %s\n" n
        (if keep then "not ignored" else "IGNORED by user request"));
    keep) l

let getXattrs path =
  let sumSize total (_, len) = total + len in (* No fear of overflow *)
  let xattrNameCompare (a, _) (b, _) = String.compare a b in
  let sortXattrs = Safelist.sort xattrNameCompare in
  let readXattr (n, len) =
    if len > 16777211 then (* Max length of strings on 32-bit OCaml *)
      failwith ("The value of extended attribute '" ^ n ^
        "' is larger than 16 MB. This is currently not supported") else
    let v = Fs.xattr_get path n in
    let value =
      if len <= 32 then String v
      else
        let h = Digest.string v in
        let _ = Cache.add h v in
        Hash h
    in
    (n, value)
  in
  wrapFail None (fun () ->
    let names = Fs.xattr_list path |> skipIgnoredXattr |> sortXattrs in
    let size = Size.ofInt (Safelist.fold_left sumSize 0 names) in
    Some (Safelist.map readXattr names, size))

let setXattrs path t =
  match t with
  | Some (l, _) -> begin
      match getXattrs path with
      | Some (xattrs0, _) -> begin
          try
            let xattrs = skipIgnoredXattr l in
            xattrs |> Safelist.iter (fun ((n, v) as m) ->
              if not (attrlist_mem m xattrs0) then
              begin
                debugverbose (fun () -> Util.msg "Writing xattr: %s\n" n);
                match v with
                | String x | Loaded (x, _) -> Fs.xattr_set path n x
                | Hash _ -> () (* This should not happen; just skip it *)
              end);
            xattrs0 |> Safelist.iter (fun (n, _) ->
              if not (Safelist.exists (fun (n', _) -> n' = n) xattrs) then
              begin
                debugverbose (fun () -> Util.msg "Removing xattr: %s\n" n);
                Fs.xattr_remove path n
              end)
          with
          | Fs.XattrNotSupported ->
              raise (Util.Transient ("Extended attributes are not supported. \
                       You can set preference \"xattrs\" to false \
                       to avoid this error."))
          | Failure msg ->
              raise (Util.Transient (msg ^
                       ". You can set preference \"xattrs\" to false \
                       to avoid this error. You can add a 'debug' preference \
                       with value \"props+\" to see more details."))
        end
      | _ -> ()
    end
  | _ -> ()

let set abspath t =
  match t with
  | Some _ when Prefs.read syncXattrs ->
      debug (fun () ->
        Util.msg "Setting xattrs for %s (%s)\n"
          (Fspath.toDebugString abspath) (toDebugString t));
      setXattrs abspath t
  | _ -> ()

let get abspath stats =
  if Prefs.read syncXattrs &&
    (stats.Unix.LargeFile.st_kind = Unix.S_REG ||
     stats.Unix.LargeFile.st_kind = Unix.S_DIR)
    (* Theoretically could sync xattrs on symlinks (if C stubs are
       enhanced accordingly). However, in the current implementation
       there are no props stored for symlinks in the archive. *)
  then
    let xattrs = getXattrs abspath in
    debug (fun () ->
      Util.msg "Xattr: got %s for %s\n"
        (toDebugString xattrs) (Fspath.toDebugString abspath));
    xattrs
  else
    None

let check fspath path stats t =
  match t with
  | None -> ()
  | Some _ ->
      let abspath = Fspath.concat fspath path in
      let t' = get abspath stats in
      if not (similar t t') then
        let msg = Format.sprintf ("Failed to set requested extended attributes \
          on %s.\nThe following attributes were requested to be set:\n%s\n\
          Actual attributes after setting:\n%s")
          (Fspath.toPrintString abspath) (toStringVerb t) (toStringVerb t') in
        raise (Util.Transient msg)

end

(* ------------------------------------------------------------------------- *)
(*                                   ACL                                     *)
(* ------------------------------------------------------------------------- *)

let featACLValid = ref (fun _ _ -> None)

let featACL =
  Features.register "Sync: ACL" ~arcFormatChange:true
  (Some (fun a b -> !featACLValid a b))

let aclEnabled () = Features.enabled featACL

let syncACL =
  Prefs.createBool "acl" false
    ~category:(`Advanced `Sync)
    ~send:aclEnabled
    "synchronize ACLs"
    ("When this flag is set to \\verb|true|, the ACLs of files and \
     directories are synchronized. The type of ACLs depends on the \
     platform and filesystem support. On Unix-like platforms it \
     can be NFSv4 ACLs, for example.")

let () = featACLValid :=
  fun _ enabledThis ->
    if not enabledThis && Prefs.read syncACL then
      Some ("You have requested synchronization of ACLs (the \
        \"acl\" preference) but the server does not support this.")
    else None

module ACL : sig
  include S
  val get : Fspath.t -> Unix.LargeFile.stats -> t
  val check : Fspath.t -> Path.local -> Unix.LargeFile.stats -> t -> unit
  module Data : sig
    include Propsdata.S
    val keep : t -> unit
  end
end = struct

module Data = struct
  include Propsdata.ACL

  let keep = function
    | None | Some "" -> ()
    | Some s -> keep s (* [keep] of Propsdata.ACL *)
end

(* The result value of this function must be deterministic for its input
   (over both roots, and over time, as long as it is the same archive). *)
let deflate acl =
  if acl = "" then acl
  else begin
    let key = Digest.string acl in
    Data.add key acl;
    key
  end

let inflate t = if t = "" then t else Data.find t

(* None indicates ACLs are not supported. This is not synchronized.
   An empty string represents a trivial/removed ACL. This will be
   synchronized. *)
type t = string option

let dummy = None

let m = Umarshal.cond aclEnabled dummy Umarshal.(option string)

let hash t h = if Prefs.read syncACL then Uutil.hash2 (Uutil.hash t) h else h

let toString = function
  | Some "" -> " <trivial ACL>"
  | Some s -> " A=" ^ (inflate s)
  | None -> if not (Prefs.read syncACL) then "" else " !No ACL support!"

let syncedPartsToString = toString

let aclIds = Str.regexp
  "\\(\\(user\\|group\\):\\)[^:]+:\\([^:]+:[^:]+:[^:]+:[0-9]+\\($\\|,\\)\\)"
let removeAclNames s =
  Str.global_replace aclIds "\\1\\3" (inflate s)

let similar2 t t' =
  Prefs.read numericIds
    &&
  (* Try to strip out the user/group names and compare only numeric ids.
     Format of ACE is expected to be as follows:
       user:name:rw------------:------I:allow:1300 *)
  String.equal (removeAclNames t) (removeAclNames t')

let similar t t' =
  not (Prefs.read syncACL)
    ||
  (* This is a direct string comparison. It does not take into account
     changes in ACE ordering because ACE ordering is considered to be
     significant and different ordering means different ACL. *)
  let result =
    match t, t' with
    | None, None -> true
    | Some acl, Some acl' when String.equal acl acl' -> true
    | Some acl, Some acl' -> similar2 acl acl'
    | _ -> false in
  debugverbose (fun () ->
    Util.msg "Comparing ACLs |%s| and |%s| => %s%s\n"
      (toString t) (toString t')
      (match result with true -> "same" | false -> "different")
      (if Prefs.read numericIds then
        " (comparing numeric user/group ids)" else ""));
  result

let override t t' = t'

let strip t = if Prefs.read syncACL then t else None

let diff t t' = if similar t t' then None else t'

let wrapFail f =
  try f () with
  | Failure msg ->
      raise (Util.Transient (msg ^
        ". You can set preference \"acl\" to false to avoid this error."))

let getACLAsText path =
  wrapFail (fun () ->
    match Fs.acl_get_text path with
    | "-1" -> None (* "-1" is used as a special code for no ACL support *)
    | acl -> Some (deflate acl))

let setACLFromText path t =
  match t with
  | Some acl -> wrapFail (fun () -> Fs.acl_set_text path (inflate acl))
  | _ -> ()

let set abspath t =
  match t with
  | Some _ when Prefs.read syncACL ->
      debug (fun () ->
        Util.msg "Setting ACL for %s from text |%s|\n"
          (Fspath.toDebugString abspath) (toString t));
      setACLFromText abspath t
  | _ -> ()

let get abspath stats =
  if Prefs.read syncACL &&
    (stats.Unix.LargeFile.st_kind = Unix.S_REG ||
     stats.Unix.LargeFile.st_kind = Unix.S_DIR)
    (* Theoretically could sync ACLs on symlinks (if C stubs are
       enhanced accordingly). However, in the current implementation
       there are no props stored for symlinks in the archive. *)
  then
    let acltext = getACLAsText abspath in
    debug (fun () ->
      Util.msg "Got text ACL |%s| for %s\n"
        (toString acltext) (Fspath.toDebugString abspath));
    acltext
  else
    None

let check fspath path stats acl =
  match acl with
  | None -> ()
  | Some _ ->
      let abspath = Fspath.concat fspath path in
      let acl' = get abspath stats in
      if not (similar acl acl') then
        let msg = Format.sprintf
          "Failed to set ACL of file %s to\n%s\n\
          The ACL was instead set to\n%s\n\
          The filesystem probably does not have full ACL support or \
          the synchronized ACL is of different type, or there \
          are other incompatibilities between systems. \
          If this is a filesystem without correct ACL support, you \
          should set the \"acl\" preference to false.%s"
          (Fspath.toPrintString abspath) (toString acl) (toString acl')
          (if Prefs.read numericIds then "" else " Or, you may want to \
             try setting the \"numericids\" preference to true if the \
             user/group names don't match on both systems.") in
        raise (Util.Transient msg)

end

(* ------------------------------------------------------------------------- *)
(*                           Properties                                      *)
(* ------------------------------------------------------------------------- *)

(* IMPORTANT!
   This is the 2.51-compatible version of type [Props.t]. It must always remain
   exactly the same as the type [Props.t] in version 2.51.5. This means that if
   any of the types it is composed of changes then for each changed type also a
   2.51-compatible version must be created. *)
type t251 =
  { perm : Perm.t;
    uid : Uid.t;
    gid : Gid.t;
    time : Time.t;
    typeCreator : TypeCreator.t;
    length : Uutil.Filesize.t }

type t =
  { perm : Perm.t;
    uid : Uid.t;
    gid : Gid.t;
    time : Time.t;
    typeCreator : TypeCreator.t;
    length : Uutil.Filesize.t;
    ctime : CTime.t;
    xattr : Xattr.t;
    acl : ACL.t;
  }

type _ props = t
type basic = [`Basic] props
type x = [`ExtLoaded] props

let m = Umarshal.(prod4
                    (prod6 Perm.m Uid.m Gid.m Time.m TypeCreator.m Uutil.Filesize.m id id)
                    (cond (fun () -> xattrEnabled () || aclEnabled ()) CTime.dummy CTime.m)
                    Xattr.m
                    ACL.m
                    (fun {perm; uid; gid; time; typeCreator; length; ctime; xattr; acl} ->
                       ((perm, uid, gid, time, typeCreator, length), ctime, xattr, acl))
                    (fun ((perm, uid, gid, time, typeCreator, length), ctime, xattr, acl) ->
                       {perm; uid; gid; time; typeCreator; length; ctime; xattr; acl}))

let mbasic = m
let mx = m

let to_compat251 (p : t) : t251 =
  { perm = p.perm;
    uid = p.uid;
    gid = p.gid;
    time = p.time;
    typeCreator = p.typeCreator;
    length = p.length }

let of_compat251 (p : t251) : t =
  { perm = p.perm;
    uid = p.uid;
    gid = p.gid;
    time = p.time;
    typeCreator = p.typeCreator;
    length = p.length;
    ctime = CTime.dummy;
    xattr = Xattr.dummy;
    acl = ACL.dummy;
  }

let template perm =
  { perm = perm; uid = Uid.dummy; gid = Gid.dummy;
    time = Time.dummy; typeCreator = TypeCreator.dummy;
    length = Uutil.Filesize.dummy;
    ctime = CTime.dummy;
    xattr = Xattr.dummy;
    acl = ACL.dummy;
  }

let dummy = template Perm.dummy

let hash p h =
  h
  |> ACL.hash p.acl
  |> Xattr.hash p.xattr
  |> TypeCreator.hash p.typeCreator
  |> Time.hash p.time
  |> Gid.hash p.gid
  |> Uid.hash p.uid
  |> Perm.hash p.perm

(* IMPORTANT!
   This is the 2.51-compatible version of [hash]. It must always produce exactly
   the same result as the [hash] in version 2.51.5.
   If code changes elsewhere make this function produce a different result then
   it must be updated accordingly to again return the 2.51-compatible result. *)
let hash251 (p : t251) h =
  Perm.hash p.perm
    (Uid.hash p.uid
       (Gid.hash p.gid
          (Time.hash p.time
             (TypeCreator.hash p.typeCreator h))))

let similar p p' =
  Perm.similar p.perm p'.perm
    &&
  Uid.similar p.uid p'.uid
    &&
  Gid.similar p.gid p'.gid
    &&
  Time.similar p.time p'.time
    &&
  TypeCreator.similar p.typeCreator p'.typeCreator
    &&
  Xattr.similar p.xattr p'.xattr
    &&
  ACL.similar p.acl p'.acl

let override p p' =
  { perm = Perm.override p.perm p'.perm;
    uid = Uid.override p.uid p'.uid;
    gid = Gid.override p.gid p'.gid;
    time = Time.override p.time p'.time;
    typeCreator = TypeCreator.override p.typeCreator p'.typeCreator;
    length = p'.length;
    ctime = CTime.override p.ctime p'.ctime;
    xattr = Xattr.override p.xattr p'.xattr;
    acl = ACL.override p.acl p'.acl;
  }

let strip p =
  { perm = Perm.strip p.perm;
    uid = Uid.strip p.uid;
    gid = Gid.strip p.gid;
    time = Time.strip p.time;
    typeCreator = TypeCreator.strip p.typeCreator;
    length = p.length;
    ctime = p.ctime;
    xattr = Xattr.strip p.xattr;
    acl = ACL.strip p.acl;
  }

let toString p =
  Printf.sprintf
    "modified on %s  size %-9.0f %s%s%s%s%s%s"
    (Time.toString p.time)
    (Uutil.Filesize.toFloat p.length)
    (Perm.toString p.perm)
    (Uid.toString p.uid)
    (Gid.toString p.gid)
    (Xattr.toString p.xattr)
    (TypeCreator.toString p.typeCreator)
    (ACL.toString p.acl)

let syncedPartsToString p =
  let tm = Time.syncedPartsToString p.time in
  Printf.sprintf
    "%s%s  size %-9.0f %s%s%s%s%s%s"
    (if tm = "" then "" else "modified at ")
    tm
    (Uutil.Filesize.toFloat p.length)
    (Perm.syncedPartsToString p.perm)
    (Uid.syncedPartsToString p.uid)
    (Gid.syncedPartsToString p.gid)
    (Xattr.syncedPartsToString p.xattr)
    (TypeCreator.syncedPartsToString p.typeCreator)
    (ACL.syncedPartsToString p.acl)

let diff p p' =
  { perm = Perm.diff p.perm p'.perm;
    uid = Uid.diff p.uid p'.uid;
    gid = Gid.diff p.gid p'.gid;
    time = Time.diff p.time p'.time;
    typeCreator = TypeCreator.diff p.typeCreator p'.typeCreator;
    length = p'.length;
    ctime = p'.ctime;
    xattr = Xattr.diff p.xattr p'.xattr;
    acl = ACL.diff p.acl p'.acl;
  }

let get' stats =
  { perm = Perm.get stats;
    uid = Uid.get stats;
    gid = Gid.get stats;
    time = Time.get stats;
    typeCreator = TypeCreator.dummy;
    length =
      if stats.Unix.LargeFile.st_kind = Unix.S_REG then
        Uutil.Filesize.fromStats stats
      else
        Uutil.Filesize.zero;
    ctime = CTime.dummy;
    xattr = Xattr.dummy;
    acl = ACL.dummy;
  }

(* Important note about [fspath] and [path] arguments to [get]:
   If the path points to a symlink then the [stats] argument may be the
   result of either stat(2) or lstat(2) on said path. When this distinction
   is important then it can be easily checked by seeing if [stats.st_kind]
   is S_LNK or not. If it is not S_LNK then any syscalls/functions on this
   path are expected to follow symlinks (and not follow otherwise). *)
let get ?(archProps = dummy) fspath path stats infos =
  let abspath = Fspath.concat fspath path in
  (* Note for future: ctime could very well be included in [get'] but it
     does not seem necessary at the moment. See the comment at
     [CTime.override]. *)
  let ctime = CTime.get stats in
  let ctimeChanged = not (CTime.same_time ctime archProps.ctime) in
  let props = get' stats in
  { props with
    typeCreator = TypeCreator.get stats infos;
    ctime;
    xattr =
      if ctimeChanged || not Xattr.ctimeDetect then Xattr.get abspath stats
      else archProps.xattr;
    acl =
      if ctimeChanged then ACL.get abspath stats
      else archProps.acl;
  }

let getWithRess stats osXinfo =
  let props = get' stats in
  { props with
    typeCreator = TypeCreator.get stats osXinfo;
  }

let set fspath path kind p =
  let abspath = Fspath.concat fspath path in
  Uid.set abspath p.uid;
  Gid.set abspath p.gid;
  TypeCreator.set fspath path p.typeCreator;
  Xattr.set abspath p.xattr;
  Time.set abspath p.time;
  Perm.set abspath kind p.perm;
  (* ACLs must always be set after chmod,
   * otherwise chmod may replace the ACL. *)
  ACL.set abspath p.acl

(* Paranoid checks *)
let check fspath path stats p =
  ACL.check fspath path stats p.acl;
  Xattr.check fspath path stats p.xattr;
  Time.check fspath path stats p.time;
  Perm.check fspath path stats p.perm

let init someHostIsRunningWindows =
  Perm.init someHostIsRunningWindows;
  Uid.init someHostIsRunningWindows;
  Gid.init someHostIsRunningWindows

let fileDefault = template Perm.fileDefault
let fileSafe = template Perm.fileSafe
let dirDefault = template Perm.dirDefault

let same_time p p' = Time.same p.time p'.time
let same_ctime p p' = CTime.same_time p.ctime p'.ctime
let length p = p.length
let setLength p l = {p with length=l}

let time p = Time.extract p.time
let setTime p p' = {p with time = Time.replace p.time (time p'); ctime = p'.ctime}

let perms p = Perm.extract p.perm

let syncModtimes = Time.sync
let permMask = Perm.permMask
let dontChmod = Perm.dontChmod

let validatePrefs = Perm.validatePrefs

let loadExtData fspath path p =
  let abspath = Fspath.concat fspath path in
  { p with
    xattr = Xattr.readAll abspath p.xattr;
  }

let purgeExtData p =
  { p with
    xattr = Xattr.purge p.xattr;
  }

let withExtData p =
  { p with
    xattr = Xattr.getAll p.xattr;
  }

(* ------------------------------------------------------------------------- *)
(*                          Shared data for props                            *)
(* ------------------------------------------------------------------------- *)

module Data = struct

  type e = string * (string * string) list
  type d = e list

  let m = Umarshal.(list (prod2 string (list (prod2 string string id id)) id id))

  let enabled () =
    xattrEnabled () || aclEnabled ()

  let extract k pd = try Safelist.assoc k pd with Not_found -> []

  let extern kind =
    let add_nonempty k v pd =
      match v with
      | [] -> pd
      | _ -> (k, v) :: pd
    in
    []
    |> add_nonempty "xattr" (Xattr.Data.get kind)
    |> add_nonempty "ACL" (ACL.Data.get kind)

  let intern pd =
    Xattr.Data.set (extract "xattr" pd);
    ACL.Data.set (extract "ACL" pd);
    ()

  let merge pd =
    Xattr.Data.merge (extract "xattr" pd);
    ACL.Data.merge (extract "ACL" pd);
    ()

  let gcInit () =
    Xattr.Data.clear `Kept;
    ACL.Data.clear `Kept;
    ()

  let gcKeep p =
    (* Xattr data cache is not persisted *)
    ACL.Data.keep p.acl;
    ()

  let gcDone () = extern `Kept

end

(* ------------------------------------------------------------------------- *)
(*                          Directory change stamps                          *)
(* ------------------------------------------------------------------------- *)

(* We are reusing the directory length to store a flag indicating that
   the directory is unchanged *)

type dirChangedStamp = Uutil.Filesize.t

let mdirChangedStamp = Uutil.Filesize.m

let freshDirStamp () =
  let t =
    (Unix.gettimeofday () +. sqrt 2. *. float (Unix.getpid ())) *. 1000.
  in
  Uutil.Filesize.ofFloat t

let changedDirStamp = Uutil.Filesize.zero

let setDirChangeFlag p stamp inode =
  let stamp = Uutil.Filesize.add stamp (Uutil.Filesize.ofInt inode) in
  (setLength p stamp, length p <> stamp)

let dirMarkedUnchanged p stamp inode =
  let stamp = Uutil.Filesize.add stamp (Uutil.Filesize.ofInt inode) in
  stamp <> changedDirStamp && length p = stamp
