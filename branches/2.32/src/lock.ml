(* Unison file synchronizer: src/lock.ml *)
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
