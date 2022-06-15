(* Unison file synchronizer: src/system/system_win.ml *)
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

(*XXXX

- Use SetConsoleOutputCP/SetConsoleCP in text mode ???
http://www.codeproject.com/KB/cpp/unicode_console_output.aspx?display=Print

*)

type fspath = string

let mfspath = Umarshal.string

let fspathFromString f = f
let fspathToPrintString f = f
let fspathToString f = f
let fspathToDebugString f = String.escaped f

let fspathConcat = Filename.concat
let fspathDirname = Filename.dirname
let fspathAddSuffixToFinalName f suffix = f ^ suffix

(****)

let fixPath f = String.map (function '/' -> '\\' | c -> c) f
let winRootRx = Rx.rx "[a-zA-Z]:[/\\].*"
let winUncRx = Rx.rx "[/\\][/\\][^?/\\]+[/\\][^/\\]+[/\\].*"
let extendedPath f =
  if Rx.match_string winRootRx f then
    fixPath ("\\\\?\\" ^ f)
  else if Rx.match_string winUncRx f then
    fixPath ("\\\\?\\UNC" ^ String.sub f 1 (String.length f - 1))
  else
    f

let encodingError p =
  raise
    (Sys_error
       (Format.sprintf "The file path '%s' is not encoded in Unicode." p))

let path8 = Unicode.from_utf_16(*_filename*)
let epath f =
  try
    Unicode.to_utf_16(*_filename*) (extendedPath f)
  with
    Unicode.Invalid -> encodingError f

let sys_error e =
  match e with
    Unix.Unix_error (err, _, "") ->
      raise (Sys_error (Unix.error_message err))
  | Unix.Unix_error (err, _, s) ->
      raise (Sys_error (Format.sprintf "%s: %s" s (Unix.error_message err)))
  | _ ->
      raise e

(****)

let getenv = Sys.getenv
let putenv = Unix.putenv
let argv () = Sys.argv

(****)

type dir_handle = System_generic.dir_handle
                = { readdir : unit -> string; closedir : unit -> unit }

external stat_impl : string -> string -> bool -> Unix.LargeFile.stats = "win_stat"
let stat f = stat_impl f (epath f) false
let lstat f = stat_impl f (epath f) true
let rmdir = Unix.rmdir
let mkdir = Unix.mkdir
let unlink = Unix.unlink
let rename f1 f2 =
  (* Comment from original C stub implementation:
     Windows Unicode API: when a file cannot be renamed due to a sharing
     violation error or an access denied error, retry for up to 1 second,
     in case the file is temporarily opened by an indexer or an anti-virus. *)
  let rec ren_aux delay =
    try
      Unix.rename f1 f2
    with
    | (Unix.Unix_error ((Unix.EACCES | Unix.EUNKNOWNERR (-32)), _, _)) as e ->
                                       (* ERROR_SHARING_VIOLATION *)
        if (delay < 1.) then begin
          Unix.sleepf delay;
          ren_aux (delay *. 2.)
        end else
          raise e
    | e -> raise e
  in
  ren_aux 0.01
let chmod = Unix.chmod
let chown _ _ _ = raise (Unix.Unix_error (Unix.ENOSYS, "chown", ""))
let utimes = Unix.utimes
let link s d = Unix.link s d
let openfile f flags perm =
  let fd = Unix.openfile f flags perm in
  (* Comment from original C stub implementation:
     Windows: implement somewhat the O_APPEND flag, so that appending
     lines to a profile (ignored files, for instance) works instead of
     overwriting the beginning of the file (the file pointer is moved to
     the end when the file is opened, rather that each time something is
     written, which is good enough here) *)
  if List.mem Unix.O_APPEND flags then
    ignore (Unix.LargeFile.lseek fd 0L Unix.SEEK_END);
  fd

let readlink f =
  (* Windows apparently mangles the link values if the value is an absolute
     path. With [readlink] you're not getting back the same value you set
     with [symlink] (except if it was a relative path). It's not clear if
     this happens always or under certain circumstances only.

     It's unclear how this mangling works, but it appears to convert the link
     value to an NT namespace path under the \?? directory (with \DosDevices
     being a symlink to it?). For regular DOS paths with a drive letter, this
     is usually ok in terms of nearly-preserving the original link value, as it
     only adds the \??\ prefix. For \\server\share\ network paths, it changes
     the prefix to \??\UNC\server\share\.

     https://docs.microsoft.com/en-us/windows-hardware/drivers/kernel/introduction-to-ms-dos-device-names
     https://docs.microsoft.com/en-us/windows-hardware/drivers/kernel/object-directories
     https://docs.microsoft.com/en-us/windows-hardware/drivers/kernel/object-names

     This conversion happens to all(?) absolute paths to targets, whether they
     were originally in the common DOS format, UNC, or already in Win32 file
     namespace format (with \\?\ prefix).

     Since we don't know what was the link value set by [symlink], we do as
     little modification as possible to the output of [readlink]. This means
     changing the prefix to \\?\ (because that's at least somewhat known to
     user-space and something we can deal with) and hoping that the resulting
     path is correct.  Without this change the path will be rejected by some
     (all?) filesystem syscalls. *)
  let l = Unix.readlink f in
  let len = String.length l in
  if len > 3 && l.[0] = '\\' && l.[1] = '?' && l.[2] = '?' && l.[3] = '\\' then
    "\\\\?\\" ^ (String.sub l 4 (len - 4))
  else l

let symlink f t = Unix.symlink f t

let chdir = Sys.chdir
external long_name : string -> string = "win_long_path_name"
let getcwd () =
  try
    (* Normalize the path *)
    let s = long_name (Sys.getcwd ()) in
    (* Convert the drive letter to uppercase *)
    match s.[0] with
    | 'a' .. 'z' -> String.capitalize_ascii s
    | _ -> s
  with e -> sys_error e

let badFileRx = Rx.rx ".*[?*].*"
let winFileNsPathRx = Rx.rx "[/\\][/\\][?][/\\].+"

let opendir d =
  (* Windows uses wildcards to retrieve the list of files in a directory.
     It is not possible to list files in a directory when the path name
     itself contains the wildcards "*" or "?". *)
  let d' = if Rx.match_string winFileNsPathRx d then String.sub d 4 (String.length d - 4) else d in
  if Rx.match_string badFileRx d' then
    raise (Unix.Unix_error (Unix.ENOENT, "opendir", d));
  let h = Unix.opendir d in
  { readdir =  (fun () -> Unix.readdir h);
    closedir = (fun () -> Unix.closedir h) }

let open_in_gen = open_in_gen
let open_out_gen = open_out_gen

(****)

let file_exists = Sys.file_exists
let open_in_bin = open_in_bin

(****)

let create_process = Unix.create_process

(****)

let open_process_in = Unix.open_process_in
let open_process_out = Unix.open_process_out
let open_process_full cmd = Unix.open_process_full cmd (Unix.environment ())
let close_process_in = Unix.close_process_in
let close_process_out = Unix.close_process_out
let close_process_full = Unix.close_process_full

(****)

(* Works in Windows since OCaml 4.07 *)
let canSetTime f = true

(* Best effort inode numbers are provided in Windows since OCaml 4.03 *)
(* We provide some kind of inode numbers *)
(* However, these inode numbers are not usable on FAT filesystems, as
   renaming a file "b" over a file "a" does not change the inode
   number of "a". *)
let hasInodeNumbers () = true

let hasSymlink = Unix.has_symlink

external hasCorrectCTime_impl : unit -> bool = "win_has_correct_ctime"

let hasCorrectCTime = hasCorrectCTime_impl ()

(****)

type fdopt = Unix.file_descr option
external initConsole : unit -> fdopt * fdopt * fdopt = "win_init_console"
external getConsoleMode : unit -> int = "win_get_console_mode"
external setConsoleMode : int -> unit = "win_set_console_mode"
external getConsoleOutputCP : unit -> int = "win_get_console_output_cp"
external setConsoleOutputCP : int -> unit = "win_set_console_output_cp"

type terminalStateFunctions =
  { defaultTerminal : unit -> unit; rawTerminal : unit -> unit;
    startReading : unit -> unit; stopReading : unit -> unit }

let terminalStateFunctions () =
  (* First, allocate a console in case we don't already have one.
     Unix.stdin/out/err have bogus handles if they weren't redirected by
     the user and there was no console at startup. We must restore them
     if a console was allocated. The fd numbers for the handles are
     hardcoded as 0, 1, 2, and must be redirected as well because these
     fds are not restored automatically by Windows. The stdin/out/err
     channels in Stdlib do not need to be restored separately because
     they operate by same hardcoded fd numbers, which will be restored
     when Unix.stdin/out/err are restored.*)
  let redirect (in', out', err') =
    let safe_redirect fd1' fd2 =
      match fd1' with Some fd1 -> Unix.dup2 fd1 fd2 | None -> ()
    in
    safe_redirect in' Unix.stdin;
    safe_redirect out' Unix.stdout;
    safe_redirect err' Unix.stderr
    (* in', out', err' must not be closed after dup2 because they are set as
       the std handles in Win32 API and something might break when they are
       closed (in fact, most everything that does not use hardcoded fd numbers
       0, 1, 2, which are the ones restored by these redirections). *)
  in
  let () = redirect (initConsole ()) in
  let oldstate = getConsoleMode () in
  let oldcp = getConsoleOutputCP () in
  (* Ctrl-C does not interrupt a call to ReadFile when
     ENABLE_LINE_INPUT is not set, so we handle Ctr-C
     as a character when reading from the console.
     We still want Ctrl-C to generate an exception when not reading
     from the console in order to be able to interrupt Unison at any
     time.  *)
  { defaultTerminal = (fun () -> setConsoleMode oldstate;
                                 setConsoleOutputCP oldcp);
    rawTerminal = (fun () -> setConsoleMode 0x19; setConsoleOutputCP 65001);
    startReading = (fun () -> setConsoleMode 0x18);
    stopReading = (fun () -> setConsoleMode 0x19) }

(****)

let fingerprint f =
  let ic = open_in_bin f in
  let d = Digest.channel ic (-1) in
  close_in ic;
  d
