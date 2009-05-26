(* Unison file synchronizer: src/system/system_intf.ml *)
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

module type Core = sig

type fspath
type dir_handle

val symlink : string -> fspath -> unit
val readlink : fspath -> string
val chown : fspath -> int -> int -> unit
val chmod : fspath -> int -> unit
val utimes : fspath -> float -> float -> unit
val unlink : fspath -> unit
val rmdir : fspath -> unit
val mkdir : fspath -> Unix.file_perm -> unit
val rename : fspath -> fspath -> unit
val stat : fspath -> Unix.LargeFile.stats
val lstat : fspath -> Unix.LargeFile.stats
val opendir : fspath -> dir_handle
val readdir : dir_handle -> string
val closedir : dir_handle -> unit
val openfile :
  fspath -> Unix.open_flag list -> Unix.file_perm -> Unix.file_descr

(****)

val open_out_gen : open_flag list -> int -> fspath -> out_channel
val open_in_bin : fspath -> in_channel
val file_exists : fspath -> bool

end

module type Full = sig

include Core

val putenv : string -> string -> unit
val getenv : string -> string
val argv : unit -> string array

val fspathFromString : string -> fspath
val fspathToPrintString : fspath -> string
val fspathToDebugString : fspath -> string
val fspathToString : fspath -> string
val fspathConcat : fspath -> string -> fspath
val fspathDirname : fspath -> fspath
val fspathAddSuffixToFinalName : fspath -> string -> fspath

val open_in_gen : open_flag list -> int -> fspath -> in_channel

val link : fspath -> fspath -> unit
val chdir : fspath -> unit
val getcwd : unit -> fspath

val create_process :
  string -> string array ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int
val open_process_in : string -> in_channel
val open_process_out : string -> out_channel
val open_process_full :
  string -> in_channel * out_channel * in_channel
val close_process_in : in_channel -> Unix.process_status
val close_process_out : out_channel -> Unix.process_status
val close_process_full :
  in_channel * out_channel * in_channel -> Unix.process_status

end
