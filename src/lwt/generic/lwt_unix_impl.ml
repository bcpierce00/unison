(*
Non-blocking I/O and select does not (fully) work under Windows.
The library therefore does not use them under Windows, and will
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
let windows_hack = Sys.win32

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

type file_descr = Unix.file_descr

let of_unix_file_descr fd = if not windows_hack then Unix.set_nonblock fd; fd

let inputs = ref []
let outputs = ref []

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
        if infds = [] && outfds = [] && delay = 0. then
          ([], [], [])
        else
          try
            let (readers, writers, _) as res =
              Unix.select infds outfds [] delay in
            if delay > 0. && !now <> -1. && readers = [] && writers = [] then
              now := !now +. delay;
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
           try
             match List.assoc fd !inputs with
               `Read (buf, pos, len, res) ->
                  wrap_syscall inputs fd res
                    (fun () -> Unix.read fd buf pos len)
             | `Accept res ->
                  wrap_syscall inputs fd res
                    (fun () ->
                       let (s, _) as v = Unix.accept ~cloexec:true fd in
                       if not windows_hack then Unix.set_nonblock s;
                       v)
             | `Wait res ->
                  wrap_syscall inputs fd res (fun () -> ())
           with Not_found ->
             ())
        readers;
      List.iter
        (fun fd ->
           try
             match List.assoc fd !outputs with
               `Write (buf, pos, len, res) ->
                  wrap_syscall outputs fd res
                    (fun () -> Unix.write fd buf pos len)
             | `WriteSubstring (buf, pos, len, res) ->
                  wrap_syscall outputs fd res
                    (fun () -> Unix.write_substring fd buf pos len)
             | `CheckSocket res ->
                  wrap_syscall outputs fd res
                    (fun () ->
                       try ignore (Unix.getpeername fd) with
                         Unix.Unix_error (Unix.ENOTCONN, _, _) ->
                           ignore (Unix.read fd (Bytes.create 1) 0 1))
             | `Wait res ->
                  wrap_syscall inputs fd res (fun () -> ())
           with Not_found ->
             ())
        writers;
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
    if windows_hack then raise (Unix.Unix_error (Unix.EAGAIN, "", ""));
    Lwt.return (Unix.write ch buf pos len)
  with
    Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      let res = Lwt.wait () in
      outputs := (ch, `Write (buf, pos, len, res)) :: !outputs;
      res
  | e ->
      Lwt.fail e

let write_substring ch buf pos len =
  try
    if windows_hack then raise (Unix.Unix_error (Unix.EAGAIN, "", ""));
    Lwt.return (Unix.write_substring ch buf pos len)
  with
    Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      let res = Lwt.wait () in
      outputs := (ch, `WriteSubstring (buf, pos, len, res)) :: !outputs;
      res
  | e ->
      Lwt.fail e

let pipe_in ?cloexec () =
  let (in_fd, out_fd) as fd_pair = Unix.pipe ?cloexec () in
  if not windows_hack then
    Unix.set_nonblock in_fd;
  fd_pair

let pipe_out ?cloexec () =
  let (in_fd, out_fd) as fd_pair = Unix.pipe ?cloexec () in
  if not windows_hack then
    Unix.set_nonblock out_fd;
  fd_pair

let socket ?cloexec dom typ proto =
  let s = Unix.socket ?cloexec dom typ proto in
  if not windows_hack then Unix.set_nonblock s;
  s

let socketpair dom typ proto =
  let (s1, s2) as spair = Unix.socketpair dom typ proto in
  if not windows_hack then begin
    Unix.set_nonblock s1; Unix.set_nonblock s2
  end;
  Lwt.return spair

let bind = Unix.bind
let setsockopt = Unix.setsockopt
let listen = Unix.listen
let close = Unix.close
let set_close_on_exec = Unix.set_close_on_exec

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

(****)

type lwt_in_channel = in_channel
type lwt_out_channel = out_channel

let intern_in_channel ch =
  Unix.set_nonblock (Unix.descr_of_in_channel ch); ch
let intern_out_channel ch =
  Unix.set_nonblock (Unix.descr_of_out_channel ch); ch


let wait_inchan ic = wait_read (Unix.descr_of_in_channel ic)
let wait_outchan oc = wait_write (Unix.descr_of_out_channel oc)

let stdlib_input_char = input_char
let rec input_char ic =
  try
    Lwt.return (stdlib_input_char ic)
  with
    Sys_blocked_io ->
      Lwt.bind (wait_inchan ic) (fun () -> input_char ic)
  | e ->
      Lwt.fail e

let input_line ic =
  let buf = ref (Bytes.create 128) in
  let pos = ref 0 in
  let rec loop () =
    if !pos = Bytes.length !buf then begin
      let newbuf = Bytes.create (2 * !pos) in
      Bytes.blit !buf 0 newbuf 0 !pos;
      buf := newbuf
    end;
    Lwt.bind (input_char ic) (fun c ->
    if c = '\n' then
      Lwt.return ()
    else begin
      Bytes.set !buf !pos c;
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
       let res = Bytes.create !pos in
       Bytes.blit !buf 0 res 0 !pos;
       Lwt.return (Bytes.to_string res))
