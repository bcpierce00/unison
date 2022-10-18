(* Unison file synchronizer: src/system/system_generic.ml *)
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

type fspath = string

let mfspath = Umarshal.string

let extendedPath f = f

let fspathToDebugString f = String.escaped f

(****)

let getenv = Sys.getenv
let putenv = Unix.putenv
let argv () = Sys.argv

(****)

type dir_handle = { readdir : unit -> string; closedir : unit -> unit }

let stat = Unix.LargeFile.stat
let lstat = Unix.LargeFile.lstat
let rmdir = Unix.rmdir
let mkdir = Unix.mkdir
let unlink = Unix.unlink
let rename = Unix.rename
let open_in_gen = open_in_gen
let open_out_gen = open_out_gen
let chmod = Unix.chmod
let chown = Unix.chown
let utimes = Unix.utimes
let link s d = Unix.link s d
let openfile = Unix.openfile
let opendir f =
  let h = Unix.opendir f in
  { readdir =  (fun () -> Unix.readdir h);
    closedir = (fun () -> Unix.closedir h) }

let readdir = Unix.readdir
let closedir = Unix.closedir
let readlink = Unix.readlink
(* BCP 5/16: Eta-expand for backward compatibility with OCaml <=4.02 *)
let symlink s1 s2 = Unix.symlink s1 s2
let chdir = Sys.chdir
let getcwd = Sys.getcwd

(****)

let file_exists = Sys.file_exists
let open_in_bin = open_in_bin

(****)

let create_process = Unix.create_process
let open_process_in = Unix.open_process_in
let open_process_out = Unix.open_process_out
let open_process_full cmd = Unix.open_process_full cmd (Unix.environment ())
let process_in_pid = Unix.process_in_pid
let process_out_pid = Unix.process_out_pid
let process_full_pid = Unix.process_full_pid
let close_process_in = Unix.close_process_in
let close_process_out = Unix.close_process_out
let close_process_full = Unix.close_process_full

(****)

let isNotWindows = Sys.os_type <> "Win32"

(* Note that Cygwin provides some kind of inode numbers, but we only
   have access to the lower 32 bits on 32bit systems... *)
(* Best effort inode numbers are provided in Windows since OCaml 4.03 *)
(* However, these inode numbers are not usable on FAT filesystems, as
   renaming a file "b" over a file "a" does not change the inode
   number of "a". *)
let hasInodeNumbers () = true

let hasSymlink = Unix.has_symlink

(* Cygwin can apparently provide correct ctime.
 *
 * With current OCaml Unix library, ctime is not correct on Win32.
 * This can change in future, in which case [hasCorrectCTime] should
 * be made dependent on OCaml version. *)
let hasCorrectCTime = isNotWindows

(****)

type terminalStateFunctions =
  { defaultTerminal : unit -> unit; rawTerminal : unit -> unit;
    startReading : unit -> unit; stopReading : unit -> unit }

let terminalStateFunctions () =
  let oldState = Unix.tcgetattr Unix.stdin in
  { defaultTerminal =
      (fun () -> Unix.tcsetattr Unix.stdin Unix.TCSANOW oldState);
    rawTerminal =
      (fun () ->
         let newState =
           { oldState with Unix.c_icanon = false; Unix.c_echo = false;
                           Unix.c_vmin = 1 }
         in
         Unix.tcsetattr Unix.stdin Unix.TCSANOW newState);
    startReading = (fun () -> ());
    stopReading = (fun () -> ()) }

let has_stdout ~info:_ = true
let has_stderr ~info:_ = true

(****)

let fingerprint f =
  let ic = open_in_bin f in
  let d = Digest.channel ic (-1) in
  close_in ic;
  d

(****)

exception XattrNotSupported
let _ = Callback.register_exception "XattrNotSupported" XattrNotSupported

external xattr_list : string -> (string * int) list = "unison_xattrs_list"
external xattr_get_ : string -> string -> string = "unison_xattr_get"
external xattr_set_ : string -> string -> string -> unit = "unison_xattr_set"
external xattr_remove_ : string -> string -> unit = "unison_xattr_remove"
external xattr_updates_ctime : unit -> bool = "unison_xattr_updates_ctime"

let xattrUpdatesCTime = xattr_updates_ctime ()

let xattr_get p n =
  try xattr_get_ p n with
  | Failure e -> failwith ("(attr: " ^ n ^ ") " ^ e)

let xattr_set p n v =
  try xattr_set_ p n v with
  | Failure e -> failwith ("(attr: " ^ n ^ ") " ^ e)

let xattr_remove p n =
  try xattr_remove_ p n with
  | Failure e -> failwith ("(attr: " ^ n ^ ") " ^ e)

(****)

external acl_get_text : string -> string = "unison_acl_to_text"
external acl_set_text : string -> string -> unit = "unison_acl_from_text"
