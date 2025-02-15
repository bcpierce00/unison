(* Unison file synchronizer: src/fs.ml *)
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

type fspath = Fspath.t
let mfspath = Fspath.m
type dir_handle = System.dir_handle
                = { readdir : unit -> string; closedir : unit -> unit }

let path p = Fspath.toString p |> System.extendedPath

(****)

let symlink l f = System.symlink l (path f)

let readlink f = System.readlink (path f)

let chown f usr grp = System.chown (path f) usr grp

let chmod f mode = System.chmod (path f) mode

let utimes f t1 t2 = System.utimes (path f) t1 t2

let unlink f = System.unlink (path f)

let rmdir f = System.rmdir (path f)

let mkdir f mode = System.mkdir (path f) mode

let rename f f' = System.rename (path f) (path f')

let stat f = System.stat (path f)

let lstat f = System.lstat (path f)

let openfile f flags perms = System.openfile (path f) flags perms

let opendir f = System.opendir (path f)

let open_in_gen flags mode f =
  System.open_in_gen flags mode (path f)

let open_out_gen flags mode f =
  System.open_out_gen flags mode (path f)

(****)

let open_in_bin f = System.open_in_bin (path f)

let file_exists f =
  try
    ignore (stat f); true
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
    false

(****)

let clone_path f1 f2 = System.clone_path (path f1) (path f2)
let clone_file = System.clone_file
let copy_file = System.copy_file

(****)

exception XattrNotSupported = System.XattrNotSupported

let xattr_list f = System.xattr_list (path f)
let xattr_get f n = System.xattr_get (path f) n
let xattr_set f n v = System.xattr_set (path f) n v
let xattr_remove f n = System.xattr_remove (path f) n

let xattrUpdatesCTime = System.xattrUpdatesCTime

(****)

let acl_get_text f = System.acl_get_text (path f)
let acl_set_text f acl = System.acl_set_text (path f) acl

(****)

let hasInodeNumbers () = System.hasInodeNumbers ()
let hasSymlink () = System.hasSymlink ()
let hasCorrectCTime = System.hasCorrectCTime
