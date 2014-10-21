(* Unison file synchronizer: src/fswatch.ml *)
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

(*
Protocol description
====================

  The file monitoring process receives commands from stdin and
  responds to stdout. Commands and responds are single lines composed
  of an identifier followed by a single space and a space separated
  list of arguments. Arguments are percent-encoded. At the minimum,
  spaces and newlines must be escaped. The two processes should accept
  any other escaped character.

  Unison and the child process starts by indicating the protocol
  version they support.  At the moment, they should just output the
  line 'VERSION 1'.

  Debugging is enabled by the 'DEBUG' command.

  At any time, the child process can signal an error by sending an
  "ERROR msg" message.

  When Unison start scanning a part of the replica, it emits command:
  'START hash fspath path', thus indicating the archive hash (that
  uniquely determines the replica) the replica's fspath and the path
  where the scanning process starts. The child process should start
  monitoring this location, then acknowledge the command by sending an
  'OK' response.
  When Unison starts scanning a directory, it emits the command
  'DIR path1', where 'path1' is relative to the path given by the
  START command (the location of the directory can be obtained by
  concatenation of 'fspath', 'path', and 'path1'). The child process
  should then start monitoring the directory, before sending an 'OK'
  response.
  When Unison encounters a followed link, it emits the command
  'LINK path1'. The child process is expected to start monitoring
  the link target before replying by 'OK'.
  Unison signals that it is done scanning the part of the replica
  described by the START process by emitting the 'DONE' command. The
  child process should not respond to this command.

  Unison can ask for a list of paths containing changes in a given
  replica by sending the 'CHANGES hash' command. The child process
  responds by a sequence of 'RECURSIVE path' responses, followed by a
  'DONE' response. These paths should be relative to the replica
  'fspath'. The child process will not have to report this changes any
  more: it can consider that Unison has taken this information into
  account once and for all. Thus, it is expected to thereafter report
  only further changes.

  Unison can wait for changes in a replica by emitting a 'WAIT hash'
  command. It can watch several replicas by sending a serie of these
  commands. The child process is expected to respond once, by a
  'CHANGE hash1 ... hash2' response that lists the changed replicas
  among those included in a 'WAIT' command, when changes are
  available. It should cancel pending waits when any other command is
  received.

  Finally, the command 'RESET hash' tells the child process to stop
  watching the given replica. In particular, it can discard any
  pending change information for this replica.
*)

let debug = Util.debug "fswatch"
let debugverbose = Trace.debug "fswatch+"

let (>>=) = Lwt.bind

let rec really_write o s pos len =
  Lwt_unix.write o s pos len >>= fun l ->
  if l = len then
    Lwt.return ()
  else
    really_write o s (pos + l) (len - l)

let split_on_space s =
  try
    let i = String.index s ' ' in
    (String.sub s 0 i,
     String.sub s (i + 1) (String.length s - i - 1))
  with Not_found ->
    (s, "")

let disallowed_char c =
  match c with
    'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~'
  | '!' | '*' | '\'' | '(' | ')' | ';' | ':' | '@' | '&'
  | '=' | '+' | '$' | ',' | '/' | '?' | '#' | '[' | ']' ->
    false
  | _ ->
    true

let quote s =
  let l = String.length s in
  let n = ref 0 in
  for i = 0 to l - 1 do if disallowed_char s.[i] then incr n done;
  if !n = 0 then s else begin
    let q = String.create (l + 2 * !n) in
    let j = ref 0 in
    let hex = "0123456789ABCDEF" in
    for i = 0 to l - 1 do
      let c = s.[i] in
      if disallowed_char s.[i] then begin
        q.[!j] <- '%';
        q.[!j + 1] <- hex.[Char.code c lsr 4];
        q.[!j + 2] <- hex.[Char.code c land 15];
        j := !j + 3
      end else begin
        q.[!j] <- c;
        incr j
      end
    done;
    q
  end

let unquote s =
  let l = String.length s in
  let n = ref 0 in
  for i = 0 to l - 1 do if s.[i] = '%' then incr n done;
  if !n = 0 then s else begin
    let hex_char c =
      match c with
        '0'..'9' -> Char.code c - Char.code '0'
      | 'a'..'f' -> Char.code c - Char.code 'a' + 10
      | 'A'..'F' -> Char.code c - Char.code 'A' + 10
      | _        -> invalid_arg "unquote"
    in
    let u = String.create (l - 2 * !n) in
    let j = ref 0 in
    for i = 0 to l - 2 * !n - 1 do
      let c = s.[!j] in
      if c = '%' then begin
        u.[i] <- Char.chr ((hex_char s.[!j + 1]) lsl 4 + hex_char s.[!j + 2]);
        j := !j + 3
      end else begin
        u.[i] <- c;
        incr j
      end
    done;
    u
  end

module Cond = struct
  type t = unit Lwt.t list ref
  let make () = ref []
  let signal s =
    let wl = !s in
    s := [];
    List.iter (fun w -> Lwt.wakeup w ()) wl
  let wait s =
    let t = Lwt.wait () in
    s := t :: !s;
    t
end

(****)

let useWatcher =
  Prefs.createBool "watch" true
    "!when set, use a file watcher process to detect changes"
    "Unison uses a file watcher process, when available, to detect filesystem \
     changes; this is used to speed up update detection, and for continuous \
     synchronization (\\verb|-repeat watch| preference. Setting this flag to \
     false disable the use of this process." 

let printf o fmt =
  Printf.ksprintf
    (fun s ->
       debugverbose (fun () -> Util.msg "<< %s" s);
       Util.convertUnixErrorsToFatal
         "sending command to filesystem watcher"
         (fun () -> Lwt_unix.run (really_write o s 0 (String.length s))))
    fmt

let read_line i =
  let b = Buffer.create 160 in
  let buf = String.create 160 in
  let start = ref 0 in
  let last = ref 0 in
  let rec read () =
    begin
      if !start = !last then begin
        Lwt_unix.read i buf 0 160 >>= fun l ->
        if l = 0 then
          raise (Util.Fatal "Filesystem watcher died unexpectively");
        start := 0; last := l;
        Lwt.return ()
      end else
        Lwt.return ()
    end >>= fun () ->
    try
      let i = String.index_from buf !start '\n' in
      if i >= !last then raise Not_found;
      Buffer.add_substring b buf !start (i - !start);
      start := i + 1;
      let s = Buffer.contents b in
      Buffer.clear b;
      debugverbose (fun() -> Util.msg ">> %s\n" s);
      Lwt.return s
    with Not_found ->
      Buffer.add_substring b buf !start (!last - !start);
      start := 0; last := 0;
      read ()
  in
  read
  
(****)

let path =
  List.map System.fspathFromString
    (try
       Str.split (Str.regexp (if Util.osType = `Win32 then ";" else ":"))
         (Sys.getenv "PATH")
     with Not_found ->
       [])

let search_in_path ?(path = path) name =
  System.fspathConcat
    (List.find (fun dir -> System.file_exists (System.fspathConcat dir name))
       path)
    name

let exec_path = [System.fspathFromString Sys.executable_name]
(*
  try
    (* Linux *)
    [System.fspathFromString (Unix.readlink "/proc/self/exe")]
  with Unix.Unix_error _ | Invalid_argument _ ->
    let name = (System.argv ()).(0) in
    if not (Filename.is_relative name) then
      [System.fspathFromString name]
    else if Filename.is_implicit name then
      try
        [search_in_path name]
      with Not_found ->
        []
    else
      [System.fspathConcat (System.getcwd ()) name]
*)

let exec_dir = List.map System.fspathDirname exec_path

let watcher =
  lazy
    (let suffix = if Util.osType = `Win32 then ".exe" else "" in
     System.fspathToString
       (try
          search_in_path ~path:(exec_dir @ path)
            ("unison-fsmonitor-" ^ Uutil.myMajorVersion ^ suffix)
        with Not_found ->
          search_in_path ~path:(exec_dir @ path)
            ("unison-fsmonitor" ^ suffix)))

type 'a exn_option = Value of 'a | Exn of exn | Nothing

type conn =
  { output : Lwt_unix.file_descr;
    has_changes : Cond.t;
    has_line : Cond.t;
    line_read : Cond.t;
    mutable last_line : string exn_option }

let conn = ref None

let rec reader conn read_line =
  read_line () >>= fun l ->
  Cond.signal conn.has_changes;
  if fst (split_on_space l) = "CHANGES" then begin
    reader conn read_line
  end else begin
    conn.last_line <- Value l;
    Cond.signal conn.has_line;
    Cond.wait conn.line_read >>= fun () ->
    reader conn read_line
   end

let safeClose fd = try Lwt_unix.close fd with Unix.Unix_error _ -> ()

let currentConnection () =
  match !conn with
    Some c -> c
  | None   -> raise (Util.Fatal ("File monitoring helper program not running"))

let closeConnection () =
  match !conn with
    Some c -> conn := None; safeClose c.output
  | None   -> ()

let connected () = !conn <> None

let startProcess () =
  try
    let w = Lazy.force watcher in
    let (i1,o1) = Lwt_unix.pipe_out () in
    let (i2,o2) = Lwt_unix.pipe_in () in
    Lwt_unix.set_close_on_exec i2;
    Lwt_unix.set_close_on_exec o1;
    Util.convertUnixErrorsToFatal "starting filesystem watcher" (fun () ->
      ignore (System.create_process w [|w|] i1 o2 Unix.stderr));
    Unix.close i1; Unix.close o2;
    let c =
      { output = o1;
        has_changes = Cond.make ();
        has_line = Cond.make ();
        line_read = Cond.make ();
        last_line = Nothing }
    in
    ignore
      (Lwt.catch (fun () -> reader c (read_line i2))
         (fun e ->
            closeConnection (); safeClose i2;
            Cond.signal c.has_changes;
            c.last_line <- Exn e; Cond.signal c.has_line;
            Lwt.return ()));
    conn := Some c;
    true
  with Not_found ->
    false

let emitCmd fmt =
  let c = currentConnection () in
  try
    printf c.output fmt
  with e ->
    closeConnection ();
    raise e
 
let rec readLine () =
  let c = currentConnection () in
  match c.last_line with
    Nothing -> Lwt_unix.run (Cond.wait c.has_line); readLine ()
  | Value l -> c.last_line <- Nothing; Cond.signal c.line_read; l
  | Exn e   -> raise e

let badResponse cmd args expected =
  closeConnection ();
  if cmd = "ERROR" then
    raise (Util.Fatal ("Filesystem watcher error: " ^ (unquote args) ^ "\n\
                        The watcher can be disabled by setting preference \
                        'watch' to false"))
  else
    raise
      (Util.Fatal
         (Format.sprintf
            "Unexpected response '%s %s' from the filesystem watcher \
             (expected %s)" cmd args expected))

let readAck () =
  let (cmd, args) = split_on_space (readLine ()) in
  if cmd <> "OK" then badResponse cmd args "OK"

let readVersion () =
  let (cmd, args) = split_on_space (readLine ()) in
  if cmd <> "VERSION" then badResponse cmd args "VERSION"

let exchangeVersions () =
  let res = startProcess () in
  if res then begin
    emitCmd "VERSION 1\n";
    debug (fun () -> Util.msg "debugging enabled\n"; emitCmd "DEBUG\n");
    readVersion ()
  end;
  res

(****)

type archiveHash = string

let scanning = ref false
let start_path = ref ""

let relpath path =
  let s2 = Path.toString path in
  let l1 = String.length !start_path in
  let l2 = String.length s2 in
  if l1 = 0 then begin
    s2
  end else if l1 = l2 then begin
    assert (s2 = !start_path);
    ""
  end else begin
    assert
      ((l2 >= l1 + 1) && String.sub s2 0 l1 = !start_path && s2.[l1] = '/');
    String.sub s2 (l1 + 1) (l2 - l1 - 1)
  end

let startScanning hash fspath path =
  if connected () then begin
    emitCmd "START %s %s %s\n"
      (quote hash)
      (quote (Fspath.toString fspath)) (quote (Path.toString path));
    readAck ();
    scanning := true;
    start_path := Path.toString path
  end

let scanDirectory path =
  if !scanning then begin
    emitCmd "DIR %s\n" (quote (relpath path));
    readAck ()
  end

let followLink path =
  if !scanning then begin
    emitCmd "LINK %s\n" (quote (relpath path));
    readAck ()
  end

let stopScanning () =
  if !scanning then begin
    scanning := false;
    emitCmd "DONE\n"
  end

let start hash =
  if not (Prefs.read useWatcher) then
    false
  else if not (connected ()) then
    exchangeVersions ()
  else begin
    emitCmd "RESET %s\n" (quote hash);
    true
  end

let wait hash =
  let c = currentConnection () in
  let res = Cond.wait c.has_changes in
  emitCmd "WAIT %s\n" (quote hash);
  res

(****)

let rec parseChanges l =
  let (cmd, args) = split_on_space (readLine ()) in
  match cmd with
    "CHANGES" ->
      parseChanges l
  | "RECURSIVE" ->
      parseChanges (Path.fromString (unquote args) :: l)
  | "DONE" ->
      List.rev l
  | other ->
      badResponse other args "RECURSIVE or DONE"

let getChanges hash =
  if connected () then begin
    emitCmd "CHANGES %s\n" (quote hash);
    parseChanges []
  end else
    raise (Util.Fatal "No file monitoring helper program found")
