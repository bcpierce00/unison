(* Unison file synchronizer: src/system/win/system_impl.ml *)
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

module System = System_win.M (struct let useLongUNCPaths = false end)

module Fs = struct

  let unicode = ref false

  let setUnicodeEncoding u = unicode := u

  let c1 f1 f2 v1 = if !unicode then f1 v1 else f2 v1
  let c2 f1 f2 v1 v2 = if !unicode then f1 v1 v2 else f2 v1 v2
  let c3 f1 f2 v1 v2 v3 = if !unicode then f1 v1 v2 v3 else f2 v1 v2 v3

  module G = System_generic
  module W = System_win.M (struct let useLongUNCPaths = true end)

  type fspath = string

  let fspathConcat v1 v2 = c2 W.fspathConcat G.fspathConcat v1 v2
  let fspathDirname v = c1 W.fspathDirname G.fspathDirname v

  type dir_handle = Unix.dir_handle

  let symlink v1 v2 = c2 W.symlink G.symlink v1 v2
  let readlink v = c1 W.readlink G.readlink v
  let chown v1 v2 v3 = c3 W.chown G.chown v1 v2 v3
  let chmod v1 v2 = c2 W.chmod G.chmod v1 v2
  let utimes v1 v2 v3 = c3 W.utimes G.utimes v1 v2 v3
  let unlink v = c1 W.unlink G.unlink v
  let rmdir v = c1 W.rmdir G.rmdir v
  let mkdir v1 v2 = c2 W.mkdir G.mkdir v1 v2
  let rename v1 v2 = c2 W.rename G.rename v1 v2
  let stat v = c1 W.stat G.stat v
  let lstat v = c1 W.lstat G.lstat v
  let opendir v = c1 W.opendir G.opendir v
  let readdir v = c1 W.readdir G.readdir v
  let closedir v = c1 W.closedir G.closedir v
  let openfile v1 v2 v3 = c3 W.openfile G.openfile v1 v2 v3
  let open_in_gen v1 v2 v3 = c3 W.open_in_gen G.open_in_gen v1 v2 v3
  let open_out_gen v1 v2 v3 = c3 W.open_out_gen G.open_out_gen v1 v2 v3
  let getcwd v = c1 W.getcwd G.getcwd v
  let chdir v = c1 W.chdir G.chdir v
  let readlink v = c1 W.readlink G.readlink v

  let canSetTime v = c1 W.canSetTime G.canSetTime v
  let hasInodeNumbers v = c1 W.hasInodeNumbers G.hasInodeNumbers v
end
