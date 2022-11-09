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

(*
let a1 = [|'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'P';'Q';'R';'S';'T'|]
let a2 = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'a';'b';'c';'d';'e';'f'|]
exception Break of (Unix.file_descr * string) option
let ptyMasterOpen () =
  if not(Osx.isMacOSX or Osx.isLinux) then None else
  try
    (* Adapted from Stevens' Advanced Programming in Unix *)
    let x = "/dev/pty--" in
    for i = 0 to Array.length a1 do
      x.[8] <- a1.(i);
      for j = 0 to Array.length a2 do
        x.[9] <- a2.(j);
        let fdOpt =
          try Some(Unix.openfile x [Unix.O_RDWR] 0)
          with Unix.Unix_error _ -> None in
        match fdOpt with None -> ()
        | Some fdMaster ->
          x.[5] <- 't';
          raise (Break(Some(fdMaster,x)))
      done
    done;
    None
  with Break z -> z

let ptySlaveOpen = function
    None -> None
  | Some(fdMaster,ttySlave) ->
      let slave =
        try Some (Unix.openfile ttySlave [Unix.O_RDWR] 0o600)
        with Unix.Unix_error _ -> None in
      (try Unix.close fdMaster with Unix.Unix_error(_,_,_) -> ());
      slave

let printTermAttrs fd = (* for debugging *)
  let tio = Unix.tcgetattr fd in
  let boolPrint name x d =
    if x then Printf.printf "%s is ON (%s)\n" name d
    else Printf.printf "%s is OFF (%s)\n" name d in
  let intPrint name x d =
    Printf.printf "%s = %d (%s)\n" name x d in
  let charPrint name x d =
    Printf.printf "%s = '%c' (%s)\n" name x d in
  boolPrint "c_ignbrk" tio.Unix.c_ignbrk   "Ignore the break condition.";
  boolPrint "c_brkint" tio.Unix.c_brkint   "Signal interrupt on break condition.";
  boolPrint "c_ignpar" tio.Unix.c_ignpar   "Ignore characters with parity errors.";
  boolPrint "c_parmrk" tio.Unix.c_parmrk   "Mark parity errors.";
  boolPrint "c_inpck" tio.Unix.c_inpck     "Enable parity check on input.";
  boolPrint "c_istrip" tio.Unix.c_istrip   "Strip 8th bit on input characters.";
  boolPrint "c_inlcr" tio.Unix.c_inlcr     "Map NL to CR on input.";
  boolPrint "c_igncr" tio.Unix.c_igncr     "Ignore CR on input.";
  boolPrint "c_icrnl" tio.Unix.c_icrnl     "Map CR to NL on input.";
  boolPrint "c_ixon" tio.Unix.c_ixon       "Recognize XON/XOFF characters on input.";
  boolPrint "c_ixoff" tio.Unix.c_ixoff     "Emit XON/XOFF chars to control input flow.";
  boolPrint "c_opost" tio.Unix.c_opost     "Enable output processing.";
  intPrint "c_obaud" tio.Unix.c_obaud      "Output baud rate (0 means close connection).";
  intPrint "c_ibaud" tio.Unix.c_ibaud      "Input baud rate.";
  intPrint "c_csize" tio.Unix.c_csize      "Number of bits per character (5-8).";
  intPrint "c_cstopb" tio.Unix.c_cstopb    "Number of stop bits (1-2).";
  boolPrint "c_cread" tio.Unix.c_cread     "Reception is enabled.";
  boolPrint "c_parenb" tio.Unix.c_parenb   "Enable parity generation and detection.";
  boolPrint "c_parodd" tio.Unix.c_parodd   "Specify odd parity instead of even.";
  boolPrint "c_hupcl" tio.Unix.c_hupcl     "Hang up on last close.";
  boolPrint "c_clocal" tio.Unix.c_clocal   "Ignore modem status lines.";
  boolPrint "c_isig" tio.Unix.c_isig       "Generate signal on INTR, QUIT, SUSP.";
  boolPrint "c_icanon" tio.Unix.c_icanon   "Enable canonical processing (line buffering and editing)";
  boolPrint "c_noflsh" tio.Unix.c_noflsh   "Disable flush after INTR, QUIT, SUSP.";
  boolPrint "c_echo" tio.Unix.c_echo       "Echo input characters.";
  boolPrint "c_echoe" tio.Unix.c_echoe     "Echo ERASE (to erase previous character).";
  boolPrint "c_echok" tio.Unix.c_echok     "Echo KILL (to erase the current line).";
  boolPrint "c_echonl" tio.Unix.c_echonl   "Echo NL even if c_echo is not set.";
  charPrint "c_vintr" tio.Unix.c_vintr     "Interrupt character (usually ctrl-C).";
  charPrint "c_vquit" tio.Unix.c_vquit     "Quit character (usually ctrl-\\).";
  charPrint "c_verase" tio.Unix.c_verase   "Erase character (usually DEL or ctrl-H).";
  charPrint "c_vkill" tio.Unix.c_vkill     "Kill line character (usually ctrl-U).";
  charPrint "c_veof" tio.Unix.c_veof       "End-of-file character (usually ctrl-D).";
  charPrint "c_veol" tio.Unix.c_veol       "Alternate end-of-line char. (usually none).";
  intPrint "c_vmin" tio.Unix.c_vmin        "Minimum number of characters to read before the read request is satisfied.";
  intPrint "c_vtime" tio.Unix.c_vtime      "Maximum read wait (in 0.1s units).";
  charPrint "c_vstart" tio.Unix.c_vstart   "Start character (usually ctrl-Q).";
  charPrint "c_vstop" tio.Unix.c_vstop      "Stop character (usually ctrl-S)."
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
let rec safe_dup fd =
  let new_fd = Unix.dup fd in
  if dumpFd new_fd >= 3 then
    new_fd
  else begin
    let res = safe_dup fd in
    Unix.close new_fd;
    res
  end
let safe_close fd = try Unix.close fd with Unix.Unix_error _ -> ()
let perform_redirections new_stdin new_stdout new_stderr =
  let newnewstdin = safe_dup new_stdin in
  let newnewstdout = safe_dup new_stdout in
  let newnewstderr = safe_dup new_stderr in
  safe_close new_stdin;
  safe_close new_stdout;
  safe_close new_stderr;
  Unix.dup2 newnewstdin Unix.stdin; Unix.close newnewstdin;
  Unix.dup2 newnewstdout Unix.stdout; Unix.close newnewstdout;
  Unix.dup2 newnewstderr Unix.stderr; Unix.close newnewstderr

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
(*
      Printf.printf "openpty returns %d--%d\n" (dumpFd fdM) (dumpFd fdS); flush stdout;
      Printf.printf "new_stdin=%d, new_stdout=%d, new_stderr=%d\n"
        (dumpFd new_stdin) (dumpFd new_stdout) (dumpFd new_stderr) ; flush stdout;
*)
      begin match Unix.fork () with
        0 ->
          begin try
            Unix.close masterFd;
            ignore (Unix.setsid ());
            setControllingTerminal slaveFd;
            (* WARNING: SETTING ECHO TO FALSE! *)
            let tio = Unix.tcgetattr slaveFd in
            tio.Unix.c_echo <- false;
            Unix.tcsetattr slaveFd Unix.TCSANOW tio;
            (* Redirect ssh authentication errors to controlling terminal,
               instead of new_stderr, so that they can be captured by GUI.
               This will also redirect the remote stderr to GUI. *)
            safe_close new_stderr;
            perform_redirections new_stdin new_stdout slaveFd;
            Unix.execvp cmd args (* never returns *)
          with Unix.Unix_error (e, s1, s2) ->
            Printf.eprintf "Error in create_session child: [%s] (%s) %s\n"
              s1 s2 (Unix.error_message e);
            flush stderr;
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

let escRemove = Str.regexp
   ("\\(\\(.\\|[\n\r]\\)+\027\\[[12]J\\)" (* Clear screen *)
  ^ "\\|\\(\027\\[[0-2]?J\\)" (* Clear screen *)
  ^ "\\|\\(\027\\[!p\\)" (* Soft reset *)
  ^ "\\|\\(\027\\][02];[^\007]*\007\\)" (* Set console window title *)
  ^ "\\|\\(\027\\[\\?25[hl]\\)" (* Show/hide cursor *)
  ^ "\\|\\(\027\\[[0-9;]*m\\)" (* Formatting *)
  ^ "\\|\\(\027\\[H\\)") (* Home *)

let escSpace = Str.regexp "\027\\[\\([0-9]*\\)C"

let processEscapes s =
  let whitesp s =
    try String.make (min 1 (int_of_string (Str.replace_matched "\\1" s))) ' '
    with Failure _ -> " "
  in
  Str.global_replace escRemove "" s
  |> Str.global_substitute escSpace whitesp

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

(* Read messages from the terminal and use the callback to get an answer *)
let handlePasswordRequests (fdIn, fdOut) callback isReady =
  let scrollback = Buffer.create 32 in
  let extract () =
    let s = Buffer.contents scrollback in
    let () = Buffer.clear scrollback in
    s
  in
  let buf = Bytes.create 10000 in
  let ended = ref false in
  let time = ref (Unix.gettimeofday ()) in
  let rec loop () =
    Lwt.catch
      (fun () -> Lwt_unix.read fdIn buf 0 10000)
      (fun ex -> if isReady () || !ended then Lwt.return 0 else Lwt.fail ex)
    >>= function
    | 0 -> Lwt.return None (* The remote end is dead *)
    | len ->
        time := Unix.gettimeofday ();
        Buffer.add_string scrollback (Bytes.sub_string buf 0 len);
        if !ended then (* The session ended before establishing a connection *)
          Lwt.return None
        else if isReady () then (* The shell connection has been established *)
          Lwt.return (Some (extract ()))
        else
          loop ()
  in
  let delay = 0.2 in
  let rec prompt () =
    if isReady () || !ended then
      Lwt.return ()
    else
      let d = (Unix.gettimeofday ()) -. !time in
      if d < delay
        || Buffer.length scrollback = 0 then
      (* HACK: Delay briefly (0.2 s, noticable to a human but not terrible
         either since some delay from the network is expected anyway) to allow
         time for output to be generated and read in as a whole. Otherwise, it
         may happen that 'Invalid password' and 'Please re-enter password'
         prompts are received separately, and this is not what we want.
         Loop by 0.01 s to let other threads run while not introducing
         noticable latency to the user. *)
      Lwt_unix.sleep (max (delay -. (max 0. d)) 0.01) >>= prompt
    else
      let query = extract () in
      if query = "\r\n" || query = "\n" || query = "\r" then
        Lwt_unix.yield () >>= prompt
      else
        let querytext = processEscapes query in
        if querytext = "" || String.trim querytext = "" then
          Lwt_unix.yield () >>= prompt
        else
          let response = callback querytext in
          Lwt_unix.write_substring fdOut
            (response ^ "\n") 0 (String.length response + 1) >>= fun _ ->
          prompt ()
  in
  let getErr () =
    ended := true;
    (* Yield a couple of times to give one final chance of reading the error
       output from the ssh process. *)
    Lwt_unix.yield () >>=
    Lwt_unix.yield >>= fun () ->
    Lwt.return (Util.trimWhitespace (processEscapes (extract ())))
  in
  let () = Lwt.ignore_result (Lwt.catch prompt
    (Util.encodeException "writing to shell terminal" `Fatal))
  and readTerm = Lwt.catch loop
    (Util.encodeException "reading from shell terminal" `Fatal)
  in
  (readTerm, getErr)
