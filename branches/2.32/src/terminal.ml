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
          with _ -> None in
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
        with _ -> None in
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

(* Like Unix.create_process except that we also try to set up a
   controlling terminal for the new process.  If successful, a file
   descriptor for the master end of the controlling terminal is
   returned. *)
let create_session cmd args new_stdin new_stdout new_stderr =
  match openpty () with
    None ->
      (None,
       Unix.create_process cmd args new_stdin new_stdout new_stderr)
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
            perform_redirections new_stdin new_stdout new_stderr;
            Unix.execvp cmd args (* never returns *)
          with _ ->
            Printf.eprintf "Some error in create_session child\n";
            flush stderr;
            exit 127
          end
      | childPid ->
          Unix.close slaveFd;
          (Some masterFd, childPid)
      end

let rec select a b c d =
  try Unix.select a b c d
  with Unix.Unix_error(Unix.EINTR,_,_) -> select a b c d

(* Wait until there is input. If there is terminal input s,
   return Some s. Otherwise, return None. *)
let rec termInput fdTerm fdInput =
  let (ready,_,_) = select [fdTerm;fdInput] [] [] (-1.0) in
  if not(Safelist.exists (fun x -> x=fdTerm) ready) then None else
  (* there's input waiting on the terminal *)
  (* read a line of input *)
  let msg =
    let n = 1024 in (* Assume length of input from terminal < n *)
    let s = String.create n in
    let howmany =
      let rec loop() =
        try Unix.read fdTerm s 0 n
        with Unix.Unix_error(Unix.EINTR,_,_) -> loop() in
      loop() in
    if howmany <= 0 then "" else
    String.sub s 0 howmany in
  let len = String.length msg in
  if len = 0 then None (* the terminal has been closed *)
  else if len = 2 && msg.[0] = '\r' && msg.[1] = '\n' then
    termInput fdTerm fdInput
  else Some msg

let (>>=) = Lwt.bind

(* Read messages from the terminal and use the callback to get an answer *)
let handlePasswordRequests fdTerm callback =
  Unix.set_nonblock fdTerm;
  let buf = String.create 10000 in
  let rec loop () =
    Lwt_unix.read fdTerm buf 0 10000 >>= (fun len ->
      if len = 0 then
        (* The remote end is dead *)
        Lwt.return ()
      else
        let query = String.sub buf 0 len in
        if query = "\r\n" then
          loop ()
        else begin
          let response = callback query in
          Lwt_unix.write fdTerm
            (response ^ "\n") 0 (String.length response + 1)
              >>= (fun _ ->
          loop ())
        end)
  in
  ignore (loop ())
