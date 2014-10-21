
(* Transfer a file from one replica to the other *)
val file :
    Common.root         (* root of source *)
 -> Path.local          (* path of source *)
 -> Common.root         (* root of target *)
 -> Fspath.t            (* fspath of target *)
 -> Path.local          (* path of target (temp location) *)
 -> Path.local          (* path of "real" (original) target *)
 -> [`Update of (Uutil.Filesize.t * Uutil.Filesize.t) | `Copy]
 -> Props.t             (* permissions for new file *)
 -> Os.fullfingerprint  (* fingerprint of file *)
 -> Fileinfo.stamp option (* source file stamp, if available *)
 -> Osx.ressStamp       (* resource info of file *)
 -> Uutil.File.t        (* file's index in UI (for progress bars) *)
 -> Fileinfo.t Lwt.t    (* information regarding the transferred file *)

val localFile :
    Fspath.t             (* fspath of source *)
 -> Path.local           (* path of source *)
 -> Fspath.t             (* fspath of target *)
 -> Path.local           (* path of target *)
 -> Path.local           (* path of "real" [original] target *)
 -> [`Update of (Uutil.Filesize.t * Uutil.Filesize.t) | `Copy]
 -> Props.t              (* permissions for new file *)
 -> Uutil.Filesize.t     (* fork length *)
 -> Uutil.File.t option  (* file's index in UI (for progress bars), if appropriate *)
 -> unit

val recursively :
    Fspath.t             (* fspath of source *)
 -> Path.local           (* path of source *)
 -> Fspath.t             (* fspath of target *)
 -> Path.local           (* path of target *)
 -> unit
