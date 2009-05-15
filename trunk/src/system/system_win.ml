(* Unison file synchronizer: src/system/system_win.ml *)
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

(*XXXX

We have to propagate the encoding mode when canonizing roots
===> new major version

TO CONVERT
==========
Unix.open_process_in
Unix.open_process_out
Unix.create_process
Unix.execvp
Lwt_unix.open_process_full
Lwt_unix.open_process_in

- Use SetConsoleOutputCP/SetConsoleCP in text mode ???
*)

type fspath = string

let fspathFromString f = f
let fspathToPrintString f = f
let fspathToString f = f
let fspathToDebugString f = String.escaped f

let fspathConcat = Filename.concat
let fspathDirname = Filename.dirname
let fspathAddSuffixToFinalName f suffix = f ^ suffix

(****)

let fixPath f =
  for i = 0 to String.length f - 1 do
    if f.[i] = '/' then f.[i] <- '\\'
  done;
  f
let winRootRx = Rx.rx "[a-zA-Z]:[/\\].*"
let winUncRx = Rx.rx "//[^/]+/[^/]+/.*"
(* FIX: we could also handle UNC paths *)
let extendedPath f =
  if Rx.match_string winRootRx f then
    fixPath ("\\\\?\\" ^ f)
  else
    f

let utf16 s = Unicode.to_utf_16 s
let utf8 s = Unicode.from_utf_16 s
let path16 = utf16
let epath f = utf16 (extendedPath f)

let sys_error e =
  match e with
    Unix.Unix_error (err, _, "") ->
      raise (Sys_error (Unix.error_message err))
  | Unix.Unix_error (err, _, s) ->
      raise (Sys_error (Format.sprintf "%s: %s" s (Unix.error_message err)))
  | _ ->
      raise e

(****)

external getenv_impl : string -> string = "win_getenv"
external putenv_impl : string -> string -> string -> unit = "win_putenv"
external argv_impl : unit -> string array = "win_argv"

let getenv nm = utf8 (getenv_impl (utf16 nm))
let putenv nm v = putenv_impl nm (utf16 nm) (utf16 v)
let argv () = Array.map utf8 (argv_impl ())

(****)

type dir_entry = Dir_empty | Dir_read of string | Dir_toread
type dir_handle = Unix.dir_handle
type dir_handle' =
  { handle : int; mutable entry_read: dir_entry }

external stat_impl : string -> string -> Unix.LargeFile.stats = "win_stat"
external rmdir_impl : string -> string -> unit = "win_rmdir"
external mkdir_impl : string -> string -> unit = "win_mkdir"
external unlink_impl : string -> string -> unit = "win_unlink"
external rename_impl : string -> string -> string -> unit = "win_rename"
external chmod_impl : string -> string -> int -> unit = "win_chmod"
external utimes_impl :
  string -> string -> float -> float -> unit = "win_utimes"
external open_impl :
  string -> string -> Unix.open_flag list -> Unix.file_perm -> Unix.file_descr = "win_open"
external chdir_impl : string -> string -> unit = "win_chdir"
external getcwd_impl : unit -> string = "win_getcwd"
external findfirst : string -> string * int = "win_findfirstw"
external findnext : int -> string = "win_findnextw"
external findclose : int -> unit = "win_findclosew"

let stat f = stat_impl f (epath f)
let lstat = stat
let rmdir f = rmdir_impl f (epath f)
let mkdir f perms = mkdir_impl f (epath f)
let unlink f = unlink_impl f (epath f)
let rename f1 f2 = rename_impl f1 (epath f1) (epath f2)
let chmod f perm = chmod_impl f (epath f) perm
let chown _ _ _ = raise (Unix.Unix_error (Unix.ENOSYS, "chown", ""))
let utimes f t1 t2 = utimes_impl f (epath f) t1 t2
let link _ _ = raise (Unix.Unix_error (Unix.ENOSYS, "link", ""))
let openfile f flags perm = open_impl f (epath f) flags perm
let readlink _ = raise (Unix.Unix_error (Unix.ENOSYS, "readlink", ""))
let symlink _ _ = raise (Unix.Unix_error (Unix.ENOSYS, "symlink", ""))

let chdir f =
  try
    chdir_impl f (path16 f) (* Better not to use [epath] here *)
  with e -> sys_error e
let getcwd () =
  try
    utf8 (getcwd_impl ())
  with e -> sys_error e

let badFileRx = Rx.rx ".*[?*].*"

let ud : dir_handle' -> dir_handle = Obj.magic
let du : dir_handle -> dir_handle' = Obj.magic

let opendir d =
  if Rx.match_string badFileRx d then
    raise (Unix.Unix_error (Unix.ENOENT, "opendir", d));
  try
    let (first_entry, handle) = findfirst (epath (fspathConcat d "*")) in
    ud { handle = handle; entry_read = Dir_read first_entry }
  with End_of_file ->
    ud { handle = 0; entry_read = Dir_empty }
let readdir d =
  let d = du d in
  match d.entry_read with
    Dir_empty -> raise End_of_file
  | Dir_read name -> d.entry_read <- Dir_toread; utf8 name
  | Dir_toread -> utf8 (findnext d.handle)
let closedir d =
  let d = du d in
  match d.entry_read with
    Dir_empty -> ()
  | _         -> findclose d.handle

let rec conv_flags fl =
  match fl with
    Open_rdonly :: rem   -> Unix.O_RDONLY :: conv_flags rem
  | Open_wronly :: rem   -> Unix.O_WRONLY :: conv_flags rem
  | Open_append :: rem   -> Unix.O_APPEND :: conv_flags rem
  | Open_creat :: rem    -> Unix.O_CREAT :: conv_flags rem
  | Open_trunc :: rem    -> Unix.O_TRUNC :: conv_flags rem
  | Open_excl :: rem     -> Unix.O_EXCL :: conv_flags rem
  | Open_binary :: rem   -> conv_flags rem
  | Open_text :: rem     -> conv_flags rem
  | Open_nonblock :: rem -> Unix.O_NONBLOCK :: conv_flags rem
  | []                   -> []

let open_in_gen flags perms f =
  try
    Unix.in_channel_of_descr (openfile f (conv_flags flags) perms)
  with e ->
    sys_error e
let open_out_gen flags perms f =
  try
    Unix.out_channel_of_descr (openfile f (conv_flags flags) perms)
  with e ->
    sys_error e

(****)

let file_exists f =
  try
    ignore (stat f); true
  with
    Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
      false
  | e ->
      sys_error e

let open_in_bin f = open_in_gen [Open_rdonly; Open_binary] 0 f
