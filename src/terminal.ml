(* Unison file synchronizer: src/terminal.ml *)
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

(* Parsing messages from OpenSSH *)
(* Examples.

"tjim@saul.cis.upenn.edu's password: " (to stdout)


"Permission denied, please try again." (to stderr ...)
"tjim@saul.cis.upenn.edu's password: " (... to stdout)


"Permission denied (publickey,gssapi,password,hostbased)." (to stderr)


"The authenticity of host 'saul.cis.upenn.edu (158.130.12.4)' can't be established.
RSA key fingerprint is d1:d8:5e:08:8c:ae:56:15:66:af:4b:55:53:2a:bc:38.
Are you sure you want to continue connecting (yes/no)? " (to stdout)


"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@    WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!     @
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
IT IS POSSIBLE THAT SOMEONE IS DOING SOMETHING NASTY!
Someone could be eavesdropping on you right now (man-in-the-middle attack)!
It is also possible that the RSA host key has just been changed.
The fingerprint for the RSA key sent by the remote host is
d1:d8:5e:08:8c:ae:56:15:66:af:4b:55:53:2a:bc:38.
Please contact your system administrator.
Add correct host key in /Users/trevor/.ssh/known_hosts to get rid of this message.
Offending key in /Users/trevor/.ssh/known_hosts:22
RSA host key for saul.cis.upenn.edu has changed and you have requested strict checking.
Host key verification failed." (to stderr)
*)

let passwordRx =
  Rx.rx ".*assword:[ ]*"
let passphraseRx =
  Rx.rx "Enter passphrase for key.*"
let authenticityRx =
  Rx.rx "The authenticity of host .* continue connecting \\(yes/no\\)\\? "
let password s = Rx.match_string passwordRx s
let passphrase s = Rx.match_string passphraseRx s
let authenticity s = Rx.match_string authenticityRx s

(* Create a new process with a new controlling terminal, useful for
   SSH password interaction.
*)

(* Implemented in file pty.c *)
type pty
external win_openpty : unit -> (Unix.file_descr * Unix.file_descr)
  * pty * (Unix.file_descr * Unix.file_descr) = "win_openpty"
external win_closepty : pty -> unit = "win_closepty"
let win_openpty () = try Some (win_openpty ()) with Unix.Unix_error _ -> None

external dumpFd : Unix.file_descr -> int = "%identity"
external setControllingTerminal : Unix.file_descr -> unit =
  "setControllingTerminal"
external c_openpty : unit -> Unix.file_descr * Unix.file_descr =
  "c_openpty"
let openpty() = try Some (c_openpty ()) with Unix.Unix_error _ -> None

(* Utility functions copied from ocaml's unix.ml because they are not exported :-| *)
(* Duplicate [fd] if needed to make sure it isn't one of the
   standard descriptors (stdin, stdout, stderr).
   Note that this function always leaves the standard descriptors open,
   the caller must take care of closing them if needed.
   The "cloexec" mode doesn't matter, because
   the descriptor returned by [dup] will be closed before the [exec],
   and because no other thread is running concurrently
   (we are in the child process of a fork).
 *)
let rec file_descr_not_standard fd =
  if dumpFd fd >= 3 then fd else file_descr_not_standard (Unix.dup fd)

let safe_close fd = try Unix.close fd with Unix.Unix_error _ -> ()

let perform_redirections new_stdin new_stdout new_stderr =
  let new_stdin = file_descr_not_standard new_stdin in
  let new_stdout = file_descr_not_standard new_stdout in
  let new_stderr = file_descr_not_standard new_stderr in
  (*  The three dup2 close the original stdin, stdout, stderr,
      which are the descriptors possibly left open
      by file_descr_not_standard *)
  Unix.dup2 ~cloexec:false new_stdin Unix.stdin;
  Unix.dup2 ~cloexec:false new_stdout Unix.stdout;
  Unix.dup2 ~cloexec:false new_stderr Unix.stderr;
  safe_close new_stdin;
  safe_close new_stdout;
  safe_close new_stderr

let rec safe_waitpid pid =
  (* This function is intentionally synchronous so that it can be run during
     cleanup code when Lwt threads might be stopped or otherwise be in an
     unreliable state. *)
  let kill_noerr si = try Unix.kill pid si with Unix.Unix_error _ -> () in
  let t = Unix.gettimeofday () in
  let rec aux st =
    match Unix.waitpid [Unix.WNOHANG] pid with
    | (0, _) ->
        Unix.sleepf 0.002;
        let dt = Unix.gettimeofday () -. t in
        if dt >= 0.5 && st = 0 then begin
          kill_noerr Sys.(if os_type = "Win32" then sigkill else sigterm);
          aux 1
        end else if dt >= 2.0 && st = 1 then begin
          kill_noerr Sys.sigkill;
          aux 2
        end else
          aux st
    | (_, r) -> r
    | exception Unix.Unix_error (EINTR, _, _) -> aux st
  in
  aux 0

let term_sessions = Hashtbl.create 3

external win_create_process_pty :
  string -> string -> pty ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int
  = "w_create_process_pty" "w_create_process_pty_native"

let make_cmdline args =
  let maybe_quote f =
    if String.contains f ' ' || String.contains f '\"'
    then Filename.quote f
    else f in
  String.concat " " (List.map maybe_quote (Array.to_list args))

let create_process_pty prog args pty fd1 fd2 fd3 =
  win_create_process_pty prog (make_cmdline args) pty fd1 fd2 fd3

let protect f g =
  try f () with Sys_error _ | Unix.Unix_error _ as e ->
    begin try g () with Sys_error _  | Unix.Unix_error _ -> () end;
    raise e

let finally f g =
  try let r = f () in g (); r with Sys_error _ | Unix.Unix_error _ as e ->
    begin try g () with Sys_error _  | Unix.Unix_error _ -> () end;
    raise e

external win_alloc_console : unit -> Unix.file_descr option = "win_alloc_console"

let fallback_session cmd args new_stdin new_stdout new_stderr =
  if Sys.os_type = "Win32" then begin
    (* OCaml's [Unix.create_process] hides the Windows console window of
       the child process unless the parent process already has a console.
       This is unsuitable for running interactive child processes like
       the ssh client. To make it possible to use the ssh client without pty,
       we have to open a Windows console window before launching the child
       process. Unfortunately, we can't know if the ssh client (or any other
       remote shell client) requires user interaction via the Windows console
       or not.

       Ignore any errors because it is almost certain that the error indicates
       that a console already exists (and we can't do anything about other
       errors anyway).

       If a new console was allocated and [Unix.stderr] is invalid (which
       will happen in Windows for GUI without a console unless stderr is
       redirected elsewhere; this is checked in the C stub) then also
       redirect [Unix.stderr] to the new console. [new_stderr] is most likely
       [Unix.stderr] and will therefore be associated with the new console. *)
    try
      match win_alloc_console () with
      | None -> ()
      | Some fd -> try Unix.dup2 fd Unix.stderr with Unix.Unix_error _ -> ()
    with Unix.Unix_error _ -> ()
  end;
  let childPid =
    System.create_process cmd args new_stdin new_stdout new_stderr in
  Hashtbl.add term_sessions childPid (fun () -> ignore (safe_waitpid childPid));
  (None, childPid)

let win_create_session cmd args new_stdin new_stdout new_stderr =
  match win_openpty () with
  | None -> fallback_session cmd args new_stdin new_stdout new_stderr
  | Some ((masterIn, masterOut), pty, (conIn, conOut)) ->
      safe_close conIn;
      let create_proc () =
        create_process_pty cmd args pty new_stdin new_stdout conOut in
      let childPid =
        protect (fun () -> finally create_proc
                                   (fun () -> safe_close conOut))
                (fun () -> safe_close masterOut;
                           safe_close masterIn)
      in
      let fdIn = Lwt_unix.of_unix_file_descr masterIn
      and fdOut = Lwt_unix.of_unix_file_descr masterOut in
      let ret = Some (fdIn, fdOut) in
      Hashtbl.add term_sessions childPid
        (fun () -> finally (fun () -> win_closepty pty)
                           (fun () -> finally (fun () -> Lwt_unix.close fdOut)
                                              (fun () -> Lwt_unix.close fdIn)));
      (ret, childPid)

(* Like Unix.create_process except that we also try to set up a
   controlling terminal for the new process.  If successful, a file
   descriptor for the master end of the controlling terminal is
   returned. *)
let unix_create_session cmd args new_stdin new_stdout new_stderr =
  match openpty () with
    None -> fallback_session cmd args new_stdin new_stdout new_stderr
  | Some (masterFd, slaveFd) ->
      Unix.set_close_on_exec masterFd;
      Unix.set_close_on_exec slaveFd;
      flush_all (); (* Clear buffers to avoid risk of double flushing by child.
        Even this is not sufficient, strictly speaking, as there is a window
        of opportunity to fill the buffer between flushing and calling fork. *)
      begin match Unix.fork () with
        0 ->
          begin try
            (* Child process stderr must redirected as early as possible to
               make sure all error output is captured and visible in GUI. *)
            Unix.dup2 ~cloexec:false slaveFd Unix.stderr;
            (* new_stderr will be used by parent process only. *)
            if new_stderr <> Unix.stderr then safe_close new_stderr;
            Unix.close masterFd;
            ignore (Unix.setsid ());
            setControllingTerminal slaveFd;
            (* WARNING: SETTING ECHO TO FALSE! *)
            let tio = Unix.tcgetattr slaveFd in
            tio.Unix.c_echo <- false;
            Unix.tcsetattr slaveFd Unix.TCSANOW tio;
            (* Redirect ssh authentication errors to controlling terminal,
               instead of new_stderr, so that they can be captured by GUI.
               This will inevitably also redirect the remote stderr to GUI
               as ssh's own error output is mixed with remote stderr output. *)
            perform_redirections new_stdin new_stdout slaveFd;
            Unix.execvp cmd args (* never returns *)
          with Unix.Unix_error (e, s1, s2) ->
            Printf.eprintf "Error in create_session child: [%s] (%s) %s\n"
              s1 s2 (Unix.error_message e);
            flush stderr;
            (* FIXME: this should be Unix._exit (available from OCaml 4.12)
               which doesn't flush buffers (or run other exit handlers).
               When [_exit] is eventually used then to _completely_ avoid risk
               of double flushing, [Unix.write Unix.stderr] should be used
               above instead of [eprintf]. Using [_exit] and not using any
               [Stdlib.out_channel] will avoid all buffering and exit handler
               issues. *)
            exit 127
          end
      | childPid ->
          (* Keep a file descriptor so that we do not get EIO errors
             when the OpenSSH 5.6 child process closes the file
             descriptor before opening /dev/tty. *)
          (* Unix.close slaveFd; *)
          let fd = Lwt_unix.of_unix_file_descr masterFd in
          let ret = Some (fd, fd) in
          Hashtbl.add term_sessions childPid
            (fun () -> safe_close slaveFd;
                       finally (fun () -> Lwt_unix.close fd)
                               (fun () -> ignore (safe_waitpid childPid)));
          (ret, childPid)
      end

let create_session =
  match Sys.os_type with
  | "Win32" -> win_create_session
  | _       -> unix_create_session

let close_session pid =
  try
    let cleanup = Hashtbl.find term_sessions pid in
    Hashtbl.remove term_sessions pid;
    cleanup ()
  with Not_found ->
    raise (Unix.Unix_error (Unix.ESRCH, "Terminal.close_session", ""))

let (>>=) = Lwt.bind

(* OpenSSH on Windows is known to produce at least the following escape
   sequences. Examples of raw output with OCaml string escapes, starting from
   beginning of line and ending at end of line, newline excluded:

\027[2J\027[m\027[H\027]0;C:\\WINDOWS\\System32\\OpenSSH\\ssh.exe\007\027[?25h

The authenticity of host 'example.com (127.0.0.1)' can't be established.\r\nECDSA key fingerprint is SHA256:CxGGHIVL7YDoSAtAzkIJNNaheGW7dDa7m7H+antMzDv.  \r\nAre you sure you want to continue connecting (yes/no/[fingerprint])?\027[10X\027[1C

   Most of these sequences are clearly useless for Unison and can be safely
   ignored. The final sequence CSI 10 X CSI 1 C is a bit weird. In this
   context, CSI 1 C can be interpreted as 1 space, although this is not
   universal.

   Some versions may have also emitted CSI ! p (VT220 soft reset) but this
   no longer seems to be the case. *)

type controlSt = No | Escape | EscapeSeq | CSI | OSC | StringSeq | OSCEsc | StringEsc

(* A very primitive and minimal parser of ANSI X3.64/ECMA-48 control sequences.
   It parses 7-bit control characters (C0) only. 8-bit control characters (C1)
   are intentionally not parsed.
   The vast majority of sequences are just ignored. *)
let parseCtrlSeq s =
  let s' = Buffer.create (String.length s) in
  let add_char = Buffer.add_char s' in
  let params = Buffer.create 32 in
  let params_add_char = Buffer.add_char params in
  let st = ref No in
  let state x = st := x in
  let parseEsc ch =
    Buffer.clear params;
    match ch with
    | '\032'..'\047' -> state EscapeSeq
    | '[' -> state CSI
    | ']' -> state OSC
    | 'X' | '^' | '_' -> state StringSeq
    | _ -> state No
  in
  let parseCh ch =
    match !st with
    | No when ch = '\027' -> state Escape
    | No -> add_char ch
    | Escape -> parseEsc ch
    | EscapeSeq ->
        begin
          match ch with
          | '\024' | '\026' -> state No (* CAN, SUB *)
          | '\000'..'\025' -> add_char ch (* Control charaters (roughly) *)
          | '\027' -> state Escape
          | '\048'..'\126' -> state No (* Final *)
          | '\127'..'\255' -> state No (* Invalid *)
          | _ -> ()
        end
    | CSI ->
        begin
          match ch with
          | '\024' | '\026' -> state No (* CAN, SUB *)
          | '\000'..'\025' -> add_char ch (* Control charaters (roughly) *)
          | '\027' -> state Escape
          | '\064'..'\126' -> (* Final *)
              begin
                state No;
                match ch with
                | 'C' -> (* cursor forward *)
                    let n =
                      try int_of_string (Buffer.contents params)
                      with Failure _ -> 1 in
                    for _ = 1 to n do add_char ' ' done
                | _ -> ()
              end
          | '\127'..'\255' -> state No (* Invalid *)
          | _ -> params_add_char ch
        end
    | OSC ->
        begin
          match ch with
          | '\024' | '\026' -> state No (* CAN, SUB *)
          | '\007' -> state No (* BEL *)
          | '\000'..'\025' -> add_char ch (* Control charaters (roughly) *)
          | '\027' -> state OSCEsc
          | _ -> ()
        end
    | OSCEsc ->
        begin
          match ch with
          | '\\' -> state No (* String terminator *)
          | _ -> parseEsc ch
        end
    | StringSeq ->
        begin
          match ch with
          | '\024' | '\026' -> state No (* CAN, SUB *)
          | '\000'..'\025' -> add_char ch (* Control charaters (roughly) *)
          | '\027' -> state StringEsc
          | _ -> ()
        end
    | StringEsc ->
        begin
          match ch with
          | '\\' -> state No (* String terminator *)
          | _ -> parseEsc ch
        end
  in
  String.iter parseCh s;
  Buffer.contents s'

let processEscapes s =
  parseCtrlSeq s

(* Wait until there is input. If there is terminal input s,
   return Some s. Otherwise, return None. *)
let rec termInput (fdTerm, _) fdInput =
  let buf = Bytes.create 10000 in
  let rec readPrompt () =
    Lwt_unix.read fdTerm buf 0 10000 >>= fun len ->
    if len = 0 then
      (* The remote end is dead *)
      Lwt.return None
    else
      let query = Bytes.sub_string buf 0 len in
      if query = "\r\n" || query = "\n" || query = "\r" then
        readPrompt ()
      else
        Lwt.return (Some (processEscapes query))
  in
  let connectionEstablished () =
    Lwt_unix.wait_read fdInput >>= fun () -> Lwt.return None
  in
  Lwt_unix.run
    (Lwt.choose
       [readPrompt (); connectionEstablished ()])

type termInteract = {
  userInput : string -> (string -> unit) -> unit;
  endInput : unit -> unit }

(* Read messages from the terminal and use the callback to get an answer *)
let handlePasswordRequests (fdIn, fdOut) {userInput; endInput} isReady =
  let scrollback = Buffer.create 32 in
  let extract () =
    let s = Buffer.contents scrollback in
    let () = Buffer.clear scrollback in
    s
  in
  let blen = 10000 in
  let buf = Bytes.create blen in
  let ended = ref false in
  let closeInput () =
    ended := true;
    endInput ()
  in
  let terminalError loc e =
    closeInput ();
    Util.encodeException loc `Fatal e
  in
  let sendResponse s =
    Lwt.catch
      (fun () ->
        if isReady () || !ended then Lwt.return 0
        else Lwt_unix.write_substring fdOut (s ^ "\n") 0 (String.length s + 1))
      (terminalError "writing to shell terminal")
  in
  let promptUser () =
    let query = extract () in
    if query = "\r\n" || query = "\n" || query = "\r" then ()
    else
      (* There is a tiny, almost non-existent risk of a broken escape sequence
         at the very beginning or the very end of the buffer (this can happen
         if bytes read from the pty end in the middle of a sequence and before
         reading any further we charge ahead with processing what we've read).
         Given that it's almost certainly ssh we're dealing with, this risk can
         safely be ignored. *)
      let querytext = processEscapes query in
      if querytext = "" || String.trim querytext = "" then ()
      else
        userInput querytext (fun s -> Lwt.ignore_result (sendResponse s))
  in
  let rec loop () =
    (* When reading from a pty, the reading loop will not stop even when the
       remote shell process dies. The reading will end (return 0 or an error)
       when the pty is closed.
       The only way to stop the reading loop without closing the pty is to
       signal [isReady]. *)
    Lwt.catch
      (fun () -> Lwt_unix.read fdIn buf 0 blen)
      (fun ex -> if isReady () || !ended then Lwt.return 0 else Lwt.fail ex)
    >>= function
    | 0 -> Lwt.return ()
    | len ->
        Buffer.add_string scrollback (Bytes.sub_string buf 0 len);
        if isReady () then begin (* The shell connection has been established *)
          closeInput ();
          Lwt.return ()
        end else begin
          Lwt.ignore_result (Lwt_unix.sleep 0.05 >>= fun () -> (* Give time for connection checks *)
            Lwt.return (if not !ended && not (isReady ()) then promptUser ()));
          loop ()
        end
  in
  let readTerm = Lwt.catch loop (terminalError "reading from shell terminal") in
  let handleReqs = readTerm >>= fun () -> Lwt.return (extract ()) in
  let extractRemainingOutput () =
    closeInput ();
    (* Give a final chance of reading the error output from the ssh process. *)
    let timeout = Lwt_unix.sleep 0.3 in
    Lwt.choose [readTerm; timeout] >>= fun () ->
    Lwt.return (Util.trimWhitespace (processEscapes (extract ())))
  in
  (handleReqs, extractRemainingOutput)
