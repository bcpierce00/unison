(*
Non-blocking I/O and select does not (fully) work under Windows.
The libray therefore does not use them under Windows, and will
therefore have the following limitations:
- No read will be performed while there are some threads ready to run
  or waiting to write;
- When a read is pending, everything else will be blocked: [sleep]
  will not terminate and other reads will not be performed before
  this read terminates;
- A write on a socket or a pipe can block the execution of the program
  if the data are never consumed at the other end of the connection.
  In particular, if both ends use this library and write at the same
  time, this could result in a dead-lock.
- [connect] is blocking
*)
let windows_hack = Sys.os_type <> "Unix"

module SleepQueue =
  Pqueue.Make (struct
    type t = float * int * unit Lwt.t
    let compare (t, i, _) (t', i', _) =
      let c = compare t t' in
      if c = 0 then i - i' else c
  end)
let sleep_queue = ref SleepQueue.empty

let event_counter = ref 0

let sleep d =
  let res = Lwt.wait () in
  incr event_counter;
  let t = if d <= 0. then 0. else Unix.gettimeofday () +. d in
  sleep_queue :=
    SleepQueue.add (t, !event_counter, res) !sleep_queue;
  res

let yield () = sleep 0.

let get_time t =
  if !t = -1. then t := Unix.gettimeofday ();
  !t

let in_the_past now t =
  t = 0. || t <= get_time now

let rec restart_threads imax now =
  match
    try Some (SleepQueue.find_min !sleep_queue) with Not_found -> None
  with
    Some (time, i, thr) when in_the_past now time && i - imax <= 0 ->
      sleep_queue := SleepQueue.remove_min !sleep_queue;
      Lwt.wakeup thr ();
      restart_threads imax now
  | _ ->
      ()

let inputs = ref []
let outputs = ref []
let wait_children = ref []

let child_exited = ref false
let _ =
  if not windows_hack then
    ignore(Sys.signal Sys.sigchld (Sys.Signal_handle (fun _ -> child_exited := true)))

let bad_fd fd =
  try ignore (Unix.LargeFile.fstat fd); false with
    Unix.Unix_error (_, _, _) ->
      true

let wrap_syscall queue fd cont syscall =
  let res =
    try
      Some (syscall ())
    with
      Exit
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
        (* EINTR because we are catching SIG_CHLD hence the system call
           might be interrupted to handle the signal; this lets us restart
           the system call eventually. *)
        None
    | e ->
        queue := List.remove_assoc fd !queue;
        Lwt.wakeup_exn cont e;
        None
  in
  match res with
    Some v ->
      queue := List.remove_assoc fd !queue;
      Lwt.wakeup cont v
  | None ->
      ()

let rec run thread =
  match Lwt.poll thread with
    Some v ->
      v
  | None ->
      let next_event =
        try
          let (time, _, _) = SleepQueue.find_min !sleep_queue in Some time
        with Not_found ->
          None
      in
      let now = ref (-1.) in
      let delay =
        match next_event with
          None      -> -1.
        | Some 0.   -> 0.
        | Some time -> max 0. (time -. get_time now)
      in
      let infds = List.map fst !inputs in
      let outfds = List.map fst !outputs in
      let (readers, writers, _) =
        if windows_hack then
          let writers = outfds in
          let readers =
            if delay = 0. || writers <> [] then [] else infds in
          (readers, writers, [])
        else if infds = [] && outfds = [] && delay = 0. then
          ([], [], [])
        else
          try
            let res = Unix.select infds outfds [] delay in
            if delay > 0. && !now <> -1. then now := !now +. delay;
            res
          with
            Unix.Unix_error (Unix.EINTR, _, _) ->
              ([], [], [])
          | Unix.Unix_error (Unix.EBADF, _, _) ->
              (List.filter bad_fd infds, List.filter bad_fd outfds, [])
      in
      restart_threads !event_counter now;
      List.iter
        (fun fd ->
           match List.assoc fd !inputs with
             `Read (buf, pos, len, res) ->
                wrap_syscall inputs fd res
                  (fun () -> Unix.read fd buf pos len)
           | `Accept res ->
                wrap_syscall inputs fd res
                  (fun () ->
                     let (s, _) as v = Unix.accept fd in
                     if not windows_hack then Unix.set_nonblock s;
                     v)
           | `Wait res ->
                wrap_syscall inputs fd res (fun () -> ()))
        readers;
      List.iter
        (fun fd ->
           match List.assoc fd !outputs with
             `Write (buf, pos, len, res) ->
                wrap_syscall outputs fd res
                  (fun () -> Unix.write fd buf pos len)
           | `CheckSocket res ->
                wrap_syscall outputs fd res
                  (fun () ->
                     try ignore (Unix.getpeername fd) with
                       Unix.Unix_error (Unix.ENOTCONN, _, _) ->
                         ignore (Unix.read fd " " 0 1))
           | `Wait res ->
                wrap_syscall inputs fd res (fun () -> ()))
        writers;
      if !child_exited then begin
        child_exited := false;
        List.iter
          (fun (id, (res, flags, pid)) ->
             wrap_syscall wait_children id res
               (fun () ->
                  let (pid', _) as v = Unix.waitpid flags pid in
                  if pid' = 0 then raise Exit;
                  v))
          !wait_children
      end;
      run thread

(****)

let wait_read ch =
  let res = Lwt.wait () in
  inputs := (ch, `Wait res) :: !inputs;
  res

let wait_write ch =
  let res = Lwt.wait () in
  outputs := (ch, `Wait res) :: !outputs;
  res

let read ch buf pos len =
  try
    if windows_hack then raise (Unix.Unix_error (Unix.EAGAIN, "", ""));
    Lwt.return (Unix.read ch buf pos len)
  with
    Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      let res = Lwt.wait () in
      inputs := (ch, `Read (buf, pos, len, res)) :: !inputs;
      res
  | e ->
      Lwt.fail e

let write ch buf pos len =
  try
    Lwt.return (Unix.write ch buf pos len)
  with
    Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      let res = Lwt.wait () in
      outputs := (ch, `Write (buf, pos, len, res)) :: !outputs;
      res
  | e ->
      Lwt.fail e

let pipe () =
  let (out_fd, in_fd) as fd_pair = Unix.pipe() in
  if not windows_hack then begin
    Unix.set_nonblock in_fd;
    Unix.set_nonblock out_fd
  end;
  Lwt.return fd_pair

let socket dom typ proto =
  let s = Unix.socket dom typ proto in
  if not windows_hack then Unix.set_nonblock s;
  Lwt.return s

let socketpair dom typ proto =
  let (s1, s2) as spair = Unix.socketpair dom typ proto in
  if not windows_hack then begin
    Unix.set_nonblock s1; Unix.set_nonblock s2
  end;
  Lwt.return spair

let accept ch =
  let res = Lwt.wait () in
  inputs := (ch, `Accept res) :: !inputs;
  res

let check_socket ch =
  let res = Lwt.wait () in
  outputs := (ch, `CheckSocket res) :: !outputs;
  res

let connect s addr =
  try
    Unix.connect s addr;
    Lwt.return ()
  with
    Unix.Unix_error
      ((Unix.EINPROGRESS | Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
        check_socket s
  | e ->
      Lwt.fail e

let ids = ref 0
let new_id () = incr ids; !ids

let _waitpid flags pid =
  try
    Lwt.return (Unix.waitpid flags pid)
  with e ->
    Lwt.fail e

let waitpid flags pid =
  if List.mem Unix.WNOHANG flags || windows_hack then
    _waitpid flags pid
  else
    let flags = Unix.WNOHANG :: flags in
    Lwt.bind (_waitpid flags pid) (fun ((pid', _) as res) ->
    if pid' <> 0 then
      Lwt.return res
    else
      let res = Lwt.wait () in
      wait_children := (new_id (), (res, flags, pid)) :: !wait_children;
      res)

let wait () = waitpid [] (-1)

let system cmd =
  match Unix.fork () with
     0 -> begin try
            Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |];
            assert false
          with _ ->
            exit 127
          end
  | id -> Lwt.bind (waitpid [] id) (fun (pid, status) -> Lwt.return status)

(****)

type lwt_in_channel = in_channel
type lwt_out_channel = out_channel

let wait_inchan ic = wait_read (Unix.descr_of_in_channel ic)
let wait_outchan oc = wait_write (Unix.descr_of_out_channel oc)

let rec input_char ic =
  try
    Lwt.return (Pervasives.input_char ic)
  with
    Sys_blocked_io ->
      Lwt.bind (wait_inchan ic) (fun () -> input_char ic)
  | e ->
      Lwt.fail e

let rec input ic s ofs len =
  try
    Lwt.return (Pervasives.input ic s ofs len)
  with
    Sys_blocked_io ->
      Lwt.bind (wait_inchan ic) (fun () -> input ic s ofs len)
  | e ->
      Lwt.fail e

let rec unsafe_really_input ic s ofs len =
  if len <= 0 then
    Lwt.return ()
  else begin
    Lwt.bind (input ic s ofs len) (fun r ->
    if r = 0
    then Lwt.fail End_of_file
    else unsafe_really_input ic s (ofs+r) (len-r))
  end

let really_input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > String.length s - len
  then Lwt.fail (Invalid_argument "really_input")
  else unsafe_really_input ic s ofs len

let input_line ic =
  let buf = ref (String.create 128) in
  let pos = ref 0 in
  let rec loop () =
    if !pos = String.length !buf then begin
      let newbuf = String.create (2 * !pos) in
      String.blit !buf 0 newbuf 0 !pos;
      buf := newbuf
    end;
    Lwt.bind (input_char ic) (fun c ->
    if c = '\n' then
      Lwt.return ()
    else begin
      !buf.[!pos] <- c;
      incr pos;
      loop ()
    end)
  in
  Lwt.bind
    (Lwt.catch loop
       (fun e ->
          match e with
            End_of_file when !pos <> 0 ->
              Lwt.return ()
          | _ ->
              Lwt.fail e))
    (fun () ->
       let res = String.create !pos in
       String.blit !buf 0 res 0 !pos;
       Lwt.return res)

(****)

type popen_process =
    Process of in_channel * out_channel
  | Process_in of in_channel
  | Process_out of out_channel
  | Process_full of in_channel * out_channel * in_channel

let popen_processes = (Hashtbl.create 7 : (popen_process, int) Hashtbl.t)

let open_proc cmd proc input output toclose =
  match Unix.fork () with
     0 -> if input <> Unix.stdin then begin
            Unix.dup2 input Unix.stdin;
            Unix.close input
          end;
          if output <> Unix.stdout then begin
            Unix.dup2 output Unix.stdout;
            Unix.close output
          end;
          List.iter Unix.close toclose;
          Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |];
          exit 127
  | id -> Hashtbl.add popen_processes proc id

let open_process_in cmd =
  Lwt.bind (pipe ()) (fun (in_read, in_write) ->
  let inchan = Unix.in_channel_of_descr in_read in
  open_proc cmd (Process_in inchan) Unix.stdin in_write [in_read];
  Unix.close in_write;
  Lwt.return inchan)

let open_process_out cmd =
  Lwt.bind (pipe ()) (fun (out_read, out_write) ->
  let outchan = Unix.out_channel_of_descr out_write in
  open_proc cmd (Process_out outchan) out_read Unix.stdout [out_write];
  Unix.close out_read;
  Lwt.return outchan)

let open_process cmd =
  Lwt.bind (pipe ()) (fun (in_read, in_write) ->
  Lwt.bind (pipe ()) (fun (out_read, out_write) ->
  let inchan = Unix.in_channel_of_descr in_read in
  let outchan = Unix.out_channel_of_descr out_write in
  open_proc cmd (Process(inchan, outchan)) out_read in_write
                                           [in_read; out_write];
  Unix.close out_read;
  Unix.close in_write;
  Lwt.return (inchan, outchan)))

(* FIX: Subprocesses that use /dev/tty to print things on the terminal
   will NOT have this output captured and returned to the caller of this
   function.  There's an argument that this is correct, but if we are
   running from a GUI the user may not be looking at any terminal and it
   will appear that the process is just hanging.  This can be fixed, in
   principle, by writing a little C code that opens /dev/tty and then uses
   the TIOCNOTTY ioctl control to detach the terminal. *)

let open_proc_full cmd env proc output input error toclose =
  match Unix.fork () with
     0 -> Unix.dup2 input Unix.stdin; Unix.close input;
          Unix.dup2 output Unix.stdout; Unix.close output;
          Unix.dup2 error Unix.stderr; Unix.close error;
          List.iter Unix.close toclose;
          Unix.execve "/bin/sh" [| "/bin/sh"; "-c"; cmd |] env;
          exit 127
  | id -> Hashtbl.add popen_processes proc id

let open_process_full cmd env =
  Lwt.bind (pipe ()) (fun (in_read, in_write) ->
  Lwt.bind (pipe ()) (fun (out_read, out_write) ->
  Lwt.bind (pipe ()) (fun (err_read, err_write) ->
  let inchan = Unix.out_channel_of_descr in_write in
  let outchan = Unix.in_channel_of_descr out_read in
  let errchan = Unix.in_channel_of_descr err_read in
  open_proc_full cmd env (Process_full(outchan, inchan, errchan))
                 out_write in_read err_write [in_write; out_read; err_read];
  Unix.close out_write;
  Unix.close in_read;
  Unix.close err_write;
  Lwt.return (outchan, inchan, errchan))))

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise (Unix.Unix_error (Unix.EBADF, fun_name, ""))

let close_process_in inchan =
  let pid = find_proc_id "close_process_in" (Process_in inchan) in
  close_in inchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)

let close_process_out outchan =
  let pid = find_proc_id "close_process_out" (Process_out outchan) in
  close_out outchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)

let close_process (inchan, outchan) =
  let pid = find_proc_id "close_process" (Process(inchan, outchan)) in
  close_in inchan; close_out outchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)

let close_process_full (outchan, inchan, errchan) =
  let pid =
    find_proc_id "close_process_full"
                 (Process_full(outchan, inchan, errchan)) in
  close_out inchan; close_in outchan; close_in errchan;
  Lwt.bind (waitpid [] pid) (fun (_, status) -> Lwt.return status)
