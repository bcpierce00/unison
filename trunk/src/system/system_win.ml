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

- Use SetConsoleOutputCP/SetConsoleCP in text mode ???
http://www.codeproject.com/KB/cpp/unicode_console_output.aspx?display=Print

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

let utf8 = Unicode.from_utf_16
let utf16 = Unicode.to_utf_16
let path8 = Unicode.from_utf_16(*_filename*)
let path16 = Unicode.to_utf_16(*_filename*)
let epath f = path16 (extendedPath f)

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
external link_impl : string -> string -> string -> unit = "win_link"
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
let link f1 f2 = link_impl f1 (epath f1) (epath f2)
let openfile f flags perm = open_impl f (epath f) flags perm
let readlink _ = raise (Unix.Unix_error (Unix.ENOSYS, "readlink", ""))
let symlink _ _ = raise (Unix.Unix_error (Unix.ENOSYS, "symlink", ""))

let chdir f =
  try
    chdir_impl f (path16 f) (* Better not to use [epath] here *)
  with e -> sys_error e
let getcwd () =
  try
    path8 (getcwd_impl ())
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
  | Dir_read name -> d.entry_read <- Dir_toread; path8 name
  | Dir_toread -> path8 (findnext d.handle)
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

(****)

external win_create_process :
  string -> string -> string ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int
  = "w_create_process" "w_create_process_native"

let make_cmdline args =
  let maybe_quote f =
    if String.contains f ' ' || String.contains f '\"'
    then Filename.quote f
    else f in
  String.concat " " (List.map maybe_quote (Array.to_list args))

let create_process prog args fd1 fd2 fd3 =
  win_create_process
    prog (utf16 prog) (utf16 (make_cmdline args)) fd1 fd2 fd3

(****)

(* The following is by Xavier Leroy and Pascal Cuoq,
   projet Cristal, INRIA Rocquencourt.
   Taken from the Objective Caml win32unix library. *)

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
  | Process_full of in_channel * out_channel * in_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd proc input output error =
  let shell =
    try getenv "COMSPEC"
    with Not_found -> raise(Unix.Unix_error(Unix.ENOEXEC, "open_proc", cmd)) in
  let pid =
    win_create_process
      shell (utf16 shell) (utf16 (shell ^ " /c " ^ cmd)) input output error in
  Hashtbl.add popen_processes proc pid

let open_process_in cmd =
  let (in_read, in_write) = Unix.pipe() in
  Unix.set_close_on_exec in_read;
  let inchan = Unix.in_channel_of_descr in_read in
  open_proc cmd (Process_in inchan) Unix.stdin in_write Unix.stderr;
  Unix.close in_write;
  inchan

let open_process_out cmd =
  let (out_read, out_write) = Unix.pipe() in
  Unix.set_close_on_exec out_write;
  let outchan = Unix.out_channel_of_descr out_write in
  open_proc cmd (Process_out outchan) out_read Unix.stdout Unix.stderr;
  Unix.close out_read;
  outchan

let open_process_full cmd =
  let (in_read, in_write) = Unix.pipe() in
  let (out_read, out_write) = Unix.pipe() in
  let (err_read, err_write) = Unix.pipe() in
  Unix.set_close_on_exec in_read;
  Unix.set_close_on_exec out_write;
  Unix.set_close_on_exec err_read;
  let inchan = Unix.in_channel_of_descr in_read in
  let outchan = Unix.out_channel_of_descr out_write in
  let errchan = Unix.in_channel_of_descr err_read in
  open_proc cmd (Process_full(inchan, outchan, errchan))
                out_read in_write err_write;
  Unix.close out_read; Unix.close in_write; Unix.close err_write;
  (inchan, outchan, errchan)

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise(Unix.Unix_error(Unix.EBADF, fun_name, ""))

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  snd(Unix.waitpid [] pid)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  snd(Unix.waitpid [] pid)

let close_process_full (inchan, outchan, errchan) =
  let pid =
    find_proc_id "close_process_full"
                 (Process_full(inchan, outchan, errchan)) in
  close_in inchan; close_out outchan; close_in errchan;
  snd(Unix.waitpid [] pid)

(****)

(* The new implementation of utimes does not have the limitation of
   the standard one *)
let canSetTime f = true

(* We provide some kind of inode numbers *)
let hasInodeNumbers () = true
