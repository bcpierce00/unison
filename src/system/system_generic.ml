(* Unison file synchronizer: src/system/system_generic.ml *)
(* Copyright 1999-2014, Benjamin C. Pierce 

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

let fspathFromString f = f
let fspathToPrintString f = f
let fspathToString f = f
let fspathToDebugString f = String.escaped f

let fspathConcat = Filename.concat
let fspathDirname = Filename.dirname
let fspathAddSuffixToFinalName f suffix = f ^ suffix

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
let link = Unix.link
let openfile = Unix.openfile
let opendir f =
  let h = Unix.opendir f in
  { readdir =  (fun () -> Unix.readdir h);
    closedir = (fun () -> Unix.closedir h) }

let readdir = Unix.readdir
let closedir = Unix.closedir
let readlink = Unix.readlink
let symlink = Unix.symlink
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
let close_process_in = Unix.close_process_in
let close_process_out = Unix.close_process_out
let close_process_full = Unix.close_process_full

(****)

let isNotWindows = Sys.os_type <> "Win32"

let canSetTime f =
  isNotWindows ||
  try
    Unix.access f [Unix.W_OK];
    true
  with
    Unix.Unix_error _ -> false

(* Note that Cygwin provides some kind of inode numbers, but we only
   have access to the lower 32 bits on 32bit systems... *)
let hasInodeNumbers () = isNotWindows

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

(****)

let fingerprint f =
  let ic = open_in_bin f in
  let d = Digest.channel ic (-1) in
  close_in ic;
  d
