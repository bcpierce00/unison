
(* Transfer a file from a replica to the other *)
val file :
    Common.root         (* root of source *)
 -> Path.local          (* path of source *)
 -> Common.root         (* root of target *)
 -> Fspath.t            (* fspath of target *)
 -> Path.local          (* path of target *)
 -> Path.local          (* path of "real" [original] target *)
 -> [`Update of (Uutil.Filesize.t * Uutil.Filesize.t) | `Copy]
 -> Props.t             (* permissions for new file *)
 -> Os.fullfingerprint  (* fingerprint of file *)
 -> Osx.ressStamp       (* ressource info of file *)
 -> Uutil.File.t        (* file's index in UI (for progress bars) *)
 -> unit Lwt.t

val localFile :
    Fspath.t             (* fspath of source *)
 -> Path.local           (* path of source *)
 -> Fspath.t             (* fspath of target *)
 -> Path.local           (* path of target *)
 -> Path.local           (* path of "real" [original] target *)
 -> [`Update of (Uutil.Filesize.t * Uutil.Filesize.t) | `Copy]
 -> Props.t              (* permissions for new file *)
 -> Uutil.Filesize.t     (* fork length *)
 -> Uutil.File.t option  (* file's index in UI (for progress bars), as appropriate *)
 -> unit
