(* $I1: Unison file synchronizer: src/lock.ml $ *)
(* $I2: Last modified by vouillon on Fri, 05 Nov 2004 10:12:27 -0500 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

let rename oldFile newFile =
  begin try Unix.link oldFile newFile with Unix.Unix_error _ -> () end;
  let res = try (Unix.LargeFile.stat oldFile).Unix.LargeFile.st_nlink = 2
            with Unix.Unix_error _ -> false
  in
  Unix.unlink oldFile;
  res

let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL]
let create name mode =
  try
    Unix.close (Unix.openfile name flags mode);
    true
  with Unix.Unix_error (Unix.EEXIST, _, _) ->
    false

let rec unique name i mode =
  let nm = name ^ string_of_int i in
  if create nm mode then nm else
    (* highly unlikely *)
    unique name (i + 1) mode

let acquire name =
  Util.convertUnixErrorsToTransient
    "Lock.acquire"
    (fun () ->
       match Util.osType with
         `Unix -> (* O_EXCL is broken under NFS... *)
           rename (unique name (Unix.getpid ()) 0o600) name
       | _ ->
           create name 0o600)

let release name = try Unix.unlink name with Unix.Unix_error _ -> ()

let is_locked name =
  Util.convertUnixErrorsToTransient
    "Lock.test"
    (fun () -> Sys.file_exists name)
