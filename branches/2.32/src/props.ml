(* Unison file synchronizer: src/props.ml *)
(* Copyright 1999-2009, Benjamin C. Pierce 

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

module type S = sig
  type t
  val dummy : t
  val hash : t -> int -> int
  val similar : t -> t -> bool
  val override : t -> t -> t
  val strip : t -> t
  val diff : t -> t -> t
  val toString : t -> string
  val syncedPartsToString : t -> string
  val set : Fspath.t -> Path.local -> [`Set | `Update] -> t -> unit
  val get : Unix.LargeFile.stats -> Osx.info -> t
  val init : bool -> unit
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
  val check : Fspath.t -> Path.local -> Unix.LargeFile.stats -> t -> unit
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
    "part of the permissions which is synchronized"
    "The integer value of this preference is a mask indicating which \
     permission bits should be synchronized.  It is set by default to \
     $0o1777$: all bits but the set-uid and set-gid bits are \
     synchronised (synchronizing theses latter bits can be a security \
     hazard).  If you want to synchronize all bits, you can set the \
     value of this preference to $-1$."

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
     bit 0o1000 "" ""  "t" ^
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
     bit 0o1000 "" ""  "t" ^
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
  "!When set, never use the chmod system call"
  ("By default, Unison uses the 'chmod' system call to set the permission bits"
  ^ " of files after it has copied them.  But in some circumstances (and under "
  ^ " some operating systems), the chmod call always fails.  Setting this "
  ^ " preference completely prevents Unison from ever calling chmod.")

let set fspath path kind (fp, mask) =
  (* BCP: removed "|| kind <> `Update" on 10/2005, but reinserted it on 11/2008.
     I'd removed it to make Dale Worley happy -- he wanted a way to make sure that
     Unison would never call chmod, and setting prefs to 0 seemed like a reasonable
     way to do this.  But in fact it caused new files to be created with wrong prefs.
   *)
  if (mask <> 0 || kind = `Set) && (not (Prefs.read dontChmod)) then
    Util.convertUnixErrorsToTransient
    "setting permissions"
      (fun () ->
        let abspath = Fspath.concatToString fspath path in
        debug
          (fun() ->
            Util.msg "Setting permissions for %s to %s (%s)\n"
              abspath (toString (fileperm2perm fp))
              (Printf.sprintf "%o/%o" fp mask));
        Unix.chmod abspath fp)

let get stats _ = (stats.Unix.LargeFile.st_perm, Prefs.read permMask)

let check fspath path stats (fp, mask) =
  let fp' = stats.Unix.LargeFile.st_perm in
  if fp land mask <> fp' land mask then
    raise
      (Util.Transient
         (Format.sprintf
            "Failed to set permissions of file %s to %s: \
             the permissions was set to %s instead. \
             The filesystem probably does not support all permission bits. \
             You should probably set the \"perms\" option to 0o%o \
             (or to 0 if you don't need to synchronize permissions)."
            (Fspath.concatToString fspath path)
            (syncedPartsToString (fp, mask))
            (syncedPartsToString (fp', mask))
            (mask land (lnot (fp lxor fp')))))

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
    "!don't map uid/gid values by user/group names"
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
  val set : string -> int -> unit
  val get : Unix.LargeFile.stats -> int
end) : S = struct

type t =
    IdIgnored
  | IdNamed of string
  | IdNumeric of int

let dummy = IdIgnored

let hash id h =
  Uutil.hash2
    (match id with
       IdIgnored   -> -1
     | IdNumeric i -> i
     | IdNamed nm  -> Hashtbl.hash nm)
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

let set fspath path kind id =
  match extern id with
    -1 ->
      ()
  | id ->
      Util.convertUnixErrorsToTransient
        "setting file ownership"
        (fun () ->
           let abspath = Fspath.concatToString fspath path in
           M.set abspath id)

let tbl = Hashtbl.create 17

let get stats _ =
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
  Prefs.createBool "owner"
    false "synchronize owner"
    ("When this flag is set to \\verb|true|, the owner attributes "
     ^ "of the files are synchronized.  "
     ^ "Whether the owner names or the owner identifiers are synchronized"
     ^ "depends on the preference \texttt{numerids}.")

let kind = "user"

let to_num nm = (Unix.getpwnam nm).Unix.pw_uid
let toString id = (Unix.getpwuid id).Unix.pw_name
let syncedPartsToString = toString

let set path id = Unix.chown path id (-1)
let get stats = stats.Unix.LargeFile.st_uid

end)

module Gid = Id (struct

let sync =
  Prefs.createBool "group"
    false "synchronize group attributes"
    ("When this flag is set to \\verb|true|, the group attributes "
     ^ "of the files are synchronized.  "
     ^ "Whether the group names or the group identifiers are synchronized"
     ^ "depends on the preference \\texttt{numerids}.")

let kind = "group"

let to_num nm = (Unix.getgrnam nm).Unix.gr_gid
let toString id = (Unix.getgrgid id).Unix.gr_name
let syncedPartsToString = toString

let set path id = Unix.chown path (-1) id
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
  Prefs.createBool "times"
    false "synchronize modification times"
    "When this flag is set to \\verb|true|, \
     file modification times (but not directory modtimes) are propagated."

type t = Synced of float | NotSynced of float

let dummy = NotSynced 0.

let extract t = match t with Synced v -> v | NotSynced v -> v

let minus_two = Int64.of_int (-2)
let approximate t = Int64.logand (Int64.of_float t) minus_two

let oneHour = Int64.of_int 3600
let minusOneHour = Int64.neg oneHour
let moduloOneHour t =
  let v = Int64.rem t oneHour in
  if v >= Int64.zero then v else Int64.add v oneHour

let hash t h =
  Uutil.hash2
    (match t with
       Synced f    -> Hashtbl.hash (moduloOneHour (approximate f))
     | NotSynced _ -> 0)
    h

let similar t t' =
  not (Prefs.read sync)
    ||
  match t, t' with
    Synced v, Synced v'      ->
      let delta = Int64.sub (approximate v) (approximate v') in
      delta = Int64.zero || delta = oneHour || delta = minusOneHour
  | NotSynced _, NotSynced _ ->
      true
  | _                        ->
      false

(* Accept one hour differences and one second differences *)
let possible_deltas =
  [ -3601L; 3601L; -3600L; 3600L; -3599L; 3599L; -1L; 1L; 0L ]

(* FIX: this is the right similar function (dates are approximated
   on FAT filesystems upward under Windows, downward under Linux).
   The hash function needs to be updated as well *)
let similar_correct t t' =
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
    Synced _    -> t
  | NotSynced _ -> NotSynced v

let strip t =
  match t with
    Synced v when not (Prefs.read sync) -> NotSynced v
  |  _                                  -> t

let diff t t' = if similar t t' then NotSynced (extract t') else t'

let toString t = Util.time2string (extract t)

let syncedPartsToString t = match t with
  Synced _    -> toString t
| NotSynced _ -> ""

let iCanWrite p =
  try
    Unix.access p [Unix.W_OK];
    true
  with
    Unix.Unix_error _ -> false

(* FIX: Probably there should be a check here that prevents us from ever     *)
(* setting a file's modtime into the future.                                 *)
let set fspath path kind t =
  match t with
    Synced v ->
      Util.convertUnixErrorsToTransient
        "setting modification time"
        (fun () ->
           let abspath = Fspath.concatToString fspath path in
           if Util.osType = `Win32 && not (iCanWrite abspath) then
             begin
              (* Nb. This workaround was proposed by Dmitry Bely, to
                 work around the fact that Unix.utimes fails on readonly
                 files under windows.  I'm [bcp] a little bit uncomfortable
                 with it for two reasons: (1) if we crash in the middle,
                 the permissions might be left in a bad state, and (2) I
                 don't understand the Win32 permissions model enough to
                 know whether it will always work -- e.g., what if the
                 UID of the unison process is not the same as that of the
                 file itself (under Unix, this case would fail, but we
                 certainly don't want to make it WORLD-writable, even
                 briefly!). *)
               let oldPerms =
                 (Unix.LargeFile.lstat abspath).Unix.LargeFile.st_perm in
               Util.finalize
                 (fun()->
                    Unix.chmod abspath 0o600;
                    Unix.utimes abspath v v)
                 (fun()-> Unix.chmod abspath oldPerms)
             end
           else if false then begin
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
                       ^ tstr ^ " '" ^ abspath ^ "'" in
             Util.msg "Running external program to set utimes:\n  %s\n" cmd;
             let (r,_) = External.runExternalProgram cmd in
             if r<>(Unix.WEXITED 0) then raise (Util.Transient "External time-setting command failed")
           end else
             Unix.utimes abspath v v)
  | _ ->
      ()

let get stats _ =
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
      if not (similar_correct t t') then
        raise
          (Util.Transient
             (Format.sprintf
                "Failed to set modification time of file %s to %s: \
             the time was set to %s instead"
            (Fspath.concatToString fspath path)
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

let init _ = ()

end

(* ------------------------------------------------------------------------- *)
(*                          Type and creator                                 *)
(* ------------------------------------------------------------------------- *)

module TypeCreator : S = struct

type t = string option

let dummy = None

let hash t h = Uutil.hash2 (Hashtbl.hash t) h

let similar t t' =
  not (Prefs.read Osx.rsrc) || t = t'

let override t t' = t'

let strip t = t

let diff t t' = if similar t t' then None else t'

let zeroes = "\000\000\000\000\000\000\000\000"

let toString t =
  match t with
    Some s when s.[0] = 'F' && String.sub (s ^ zeroes) 1 8 <> zeroes ->
      let s = s ^ zeroes in
      " " ^ String.escaped (String.sub s 1 4) ^
      " " ^ String.escaped (String.sub s 5 4)
  | _ ->
      ""

let syncedPartsToString = toString

let set fspath path kind t =
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

let init _ = ()

end

(* ------------------------------------------------------------------------- *)
(*                           Properties                                      *)
(* ------------------------------------------------------------------------- *)

type t =
  { perm : Perm.t;
    uid : Uid.t;
    gid : Gid.t;
    time : Time.t;
    typeCreator : TypeCreator.t;
    length : Uutil.Filesize.t }

let template perm =
  { perm = perm; uid = Uid.dummy; gid = Gid.dummy;
    time = Time.dummy; typeCreator = TypeCreator.dummy;
    length = Uutil.Filesize.dummy }

let dummy = template Perm.dummy

let hash p h =
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

let override p p' =
  { perm = Perm.override p.perm p'.perm;
    uid = Uid.override p.uid p'.uid;
    gid = Gid.override p.gid p'.gid;
    time = Time.override p.time p'.time;
    typeCreator = TypeCreator.override p.typeCreator p'.typeCreator;
    length = p'.length }

let strip p =
  { perm = Perm.strip p.perm;
    uid = Uid.strip p.uid;
    gid = Gid.strip p.gid;
    time = Time.strip p.time;
    typeCreator = TypeCreator.strip p.typeCreator;
    length = p.length }

let toString p =
  Printf.sprintf
    "modified on %s  size %-9.f %s%s%s%s"
    (Time.toString p.time)
    (Uutil.Filesize.toFloat p.length)
    (Perm.toString p.perm)
    (Uid.toString p.uid)
    (Gid.toString p.gid)
    (TypeCreator.toString p.typeCreator)

let syncedPartsToString p =
  let tm = Time.syncedPartsToString p.time in
  Printf.sprintf
    "%s%s  size %-9.f %s%s%s%s"
    (if tm = "" then "" else "modified at ")
    tm
    (Uutil.Filesize.toFloat p.length)
    (Perm.syncedPartsToString p.perm)
    (Uid.syncedPartsToString p.uid)
    (Gid.syncedPartsToString p.gid)
    (TypeCreator.syncedPartsToString p.typeCreator)

let diff p p' =
  { perm = Perm.diff p.perm p'.perm;
    uid = Uid.diff p.uid p'.uid;
    gid = Gid.diff p.gid p'.gid;
    time = Time.diff p.time p'.time;
    typeCreator = TypeCreator.diff p.typeCreator p'.typeCreator;
    length = p'.length }

let get stats infos =
  { perm = Perm.get stats infos;
    uid = Uid.get stats infos;
    gid = Gid.get stats infos;
    time = Time.get stats infos;
    typeCreator = TypeCreator.get stats infos;
    length =
      if stats.Unix.LargeFile.st_kind = Unix.S_REG then
        Uutil.Filesize.fromStats stats
      else
        Uutil.Filesize.zero }

let set fspath path kind p =
  Uid.set fspath path kind p.uid;
  Gid.set fspath path kind p.gid;
  TypeCreator.set fspath path kind p.typeCreator;
  Time.set fspath path kind p.time;
  Perm.set fspath path kind p.perm

(* Paranoid checks *)
let check fspath path stats p =
  Time.check fspath path stats p.time;
  Perm.check fspath path stats p.perm

let init someHostIsRunningWindows =
  Perm.init someHostIsRunningWindows;
  Uid.init someHostIsRunningWindows;
  Gid.init someHostIsRunningWindows;
  Time.init someHostIsRunningWindows;
  TypeCreator.init someHostIsRunningWindows

let fileDefault = template Perm.fileDefault
let fileSafe = template Perm.fileSafe
let dirDefault = template Perm.dirDefault

let same_time p p' = Time.same p.time p'.time
let length p = p.length
let setLength p l = {p with length=l}

let time p = Time.extract p.time
let setTime p t = {p with time = Time.replace p.time t}

let perms p = Perm.extract p.perm

let syncModtimes = Time.sync
