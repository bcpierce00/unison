(* Unison file synchronizer: src/fs.ml *)
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

module System = System_impl.Fs

type fspath = Fspath.t
type dir_handle = System.dir_handle
                = { readdir : unit -> string; closedir : unit -> unit }

let symlink l f = System.symlink l (Fspath.toString f)

let readlink f = System.readlink (Fspath.toString f)

let chown f usr grp = System.chown (Fspath.toString f) usr grp

let chmod f mode = System.chmod (Fspath.toString f) mode

let utimes f t1 t2 = System.utimes (Fspath.toString f) t1 t2

let unlink f = System.unlink (Fspath.toString f)

let rmdir f = System.rmdir (Fspath.toString f)

let mkdir f mode = System.mkdir (Fspath.toString f) mode

let rename f f' = System.rename (Fspath.toString f) (Fspath.toString f')

let stat f = System.stat (Fspath.toString f)

let lstat f = System.lstat (Fspath.toString f)

let openfile f flags perms = System.openfile (Fspath.toString f) flags perms

let opendir f : dir_handle = System.opendir (Fspath.toString f)

let open_in_gen flags mode f =
  System.open_in_gen flags mode (Fspath.toString f)

let open_out_gen flags mode f =
  System.open_out_gen flags mode (Fspath.toString f)

(****)

let open_in_bin f = open_in_gen [Open_rdonly; Open_binary] 0 f

let file_exists f =
  try
    ignore (stat f); true
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
    false

(****)

let fingerprint f = System.fingerprint (Fspath.toString f)

let canSetTime f = System.canSetTime (Fspath.toString f)
let hasInodeNumbers () = System.hasInodeNumbers ()

let setUnicodeEncoding = System.setUnicodeEncoding
