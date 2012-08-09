(*
- should check all events before looping again for avoiding race
  conditions...
  (we have the first, scan the subsequent ones)
*)

let no_overlapped_io = false
let d = ref false

(****)

type buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let buffer_create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

external unsafe_blit_string_to_buffer :
  string -> int -> buffer -> int -> int -> unit = "ml_blit_string_to_buffer"
external unsafe_blit_buffer_to_string :
  buffer -> int -> string -> int -> int -> unit = "ml_blit_buffer_to_string"

let buffer_length = Bigarray.Array1.dim

let blit_string_to_buffer s i a j l =
  if l < 0 || i < 0 || i > String.length s - l
           || j < 0 || j > buffer_length a - l
  then invalid_arg "Lwt_unix.blit_string_to_buffer"
  else unsafe_blit_string_to_buffer s i a j l

let blit_buffer_to_string a i s j l =
  if l < 0 || i < 0 || i > buffer_length a - l
           || j < 0 || j > String.length s - l
  then invalid_arg "Lwt_unix.blit_buffer_to_string"
  else unsafe_blit_buffer_to_string a i s j l

let buffer_size = 16384

let avail_buffers = ref []

let acquire_buffer () =
  match !avail_buffers with
    []     -> buffer_create buffer_size
  | b :: r -> avail_buffers := r; b

let release_buffer b = avail_buffers := b :: !avail_buffers

(****)

let last_id = ref 0
let free_list = ref (Array.init 1 (fun i -> i))

let acquire_id () =
  let len = Array.length !free_list in
  if !last_id = len then begin
    let a = Array.init (len * 2) (fun i -> i) in
    Array.blit !free_list 0 a 0 len;
    free_list := a
  end;
  let i = !free_list.(!last_id) in
  incr last_id;
  i

let release_id i =
  decr last_id;
  !free_list.(!last_id) <- i

(****)

let completionEvents = ref []

let actionCompleted id len errno name =
  completionEvents := (id, len, errno, name) :: !completionEvents

external init_lwt :
  (int -> int -> Unix.error -> string -> unit) -> int = "init_lwt"

let max_event_count = init_lwt actionCompleted

let event_count = ref 0
let free_list = Array.init max_event_count (fun i -> i)

let acquire_event nm =
  if !event_count = max_event_count then
    raise (Unix.Unix_error (Unix.EAGAIN, nm, ""));
  let i = free_list.(!event_count) in
  incr event_count;
  i

let release_event i =
  decr event_count;
  free_list.(!event_count) <- i

(****)

type helpers
type file_descr = { fd : Unix.file_descr; helpers : helpers }

external of_unix_file_descr : Unix.file_descr -> file_descr = "win_wrap_fd"

external win_wrap_async : Unix.file_descr -> file_descr = "win_wrap_overlapped"

let wrap_async =
  if no_overlapped_io then of_unix_file_descr else win_wrap_async

(****)

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
if !d then Format.eprintf "RESTART@.";
      Lwt.wakeup thr ();
if !d then Format.eprintf "RESTART...DONE@.";
      restart_threads imax now
  | _ ->
      ()

module IntTbl =
  Hashtbl.Make
    (struct type t = int let equal (x : int) y = x = y let hash x = x end)

let ioInFlight = IntTbl.create 17
let connInFlight = IntTbl.create 17

let handleCompletionEvent (id, len, errno, name) =
if !d then Format.eprintf "Handling event %d (len %d)@." id len;
  let (action, buf, res) =
    try IntTbl.find ioInFlight id with Not_found -> assert false
  in
  begin match action with
    `Write         -> ()
  | `Read (s, pos) -> if len > 0 then blit_buffer_to_string buf 0 s pos len
  | `Readdirectorychanges -> ()
  end;
  IntTbl.remove ioInFlight id;
  release_id id;
  release_buffer buf;
  if len = -1 then
    Lwt.wakeup_exn res (Unix.Unix_error (errno, name, ""))
  else
    Lwt.wakeup res len

type kind = CONNECT | ACCEPT

external win_wait : int -> int -> int = "win_wait"

external win_register_wait :
  Unix.file_descr -> kind -> int -> unit = "win_register_wait"

external win_check_connection :
  Unix.file_descr -> kind -> int -> unit = "win_check_connection"

let handle_wait_event i ch kind cont action =
if !d then prerr_endline "MMM";
  let res =
    try
      Some (action ())
    with
      Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
if !d then prerr_endline "NNN";
        win_register_wait ch.fd kind i;
        None
    | e ->
if !d then prerr_endline "OOO";
        release_event i;
        IntTbl.remove connInFlight i;
        Lwt.wakeup_exn cont e;
        None
  in
  match res with
    Some v ->
if !d then prerr_endline "PPP";
      release_event i;
      IntTbl.remove connInFlight i;
      Lwt.wakeup cont v
  | None ->
      ()

let rec run thread =
if !d then Format.eprintf "Main loop@.";
  match Lwt.poll thread with
    Some v ->
if !d then Format.eprintf "DONE!@.";
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
if !d then Format.eprintf "vvv@.";
      let i =
        try
          win_wait (truncate (ceil (delay *. 1000.))) !event_count
        with e -> assert false
      in
if !d then Format.eprintf "^^^@.";
      if i = -1 then now := !now +. delay;
      restart_threads !event_counter now;
if !d then Format.eprintf "threads restarted@.";
      let ev = !completionEvents in
      completionEvents := [];
      List.iter handleCompletionEvent (List.rev ev);
      if i >= 0 then begin
        let (kind, ch) =
          try IntTbl.find connInFlight i with Not_found -> assert false in
        match kind with
          `CheckSocket res  ->
if !d then prerr_endline "CHECK CONN";
            handle_wait_event i ch CONNECT res
               (fun () -> win_check_connection ch.fd CONNECT i)
        | `Accept res ->
if !d then prerr_endline "ACCEPT";
            handle_wait_event i ch ACCEPT res
              (fun () ->
                 win_check_connection ch.fd ACCEPT i;
                 let (v, info) = Unix.accept ch.fd in
                 (wrap_async v, info))
      end;
(*
      let infds = List.map fst !inputs in
      let outfds = List.map fst !outputs in
      let (readers, writers, _) =
        if windows_hack && not recent_ocaml then
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
          | Unix.Unix_error (Unix.EPIPE, _, _)
            when windows_hack && recent_ocaml ->
            (* Workaround for a bug in Ocaml 3.11: select fails with an
               EPIPE error when the file descriptor is remotely closed *)
              (infds, [], [])
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
                       let (s, i) = Unix.accept fd.fd in
                       if not windows_hack then Unix.set_nonblock s;
                       (wrap_async s, i))
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
             | `Wait res ->
                  wrap_syscall inputs fd res (fun () -> ())
           with Not_found ->
             ())
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
*)
      run thread

(****)

let wait_read ch = assert false

let wait_write ch = assert false

external start_read :
  file_descr -> buffer -> int -> int -> int -> unit = "win_read"
external start_write :
  file_descr -> buffer -> int -> int -> int -> unit = "win_write"

let read ch s pos len =
if !d then Format.eprintf "Start reading@.";
  let id = acquire_id () in
  let buf = acquire_buffer () in
  let len = if len > buffer_size then buffer_size else len in
  let res = Lwt.wait () in
  IntTbl.add ioInFlight id (`Read (s, pos), buf, res);
  start_read ch buf 0 len id;
if !d then Format.eprintf "Reading started@.";
  res

let write ch s pos len =
if !d then Format.eprintf "Start writing@.";
  let id = acquire_id () in
  let buf = acquire_buffer () in
  let len = if len > buffer_size then buffer_size else len in
  blit_string_to_buffer s pos buf 0 len;
  let res = Lwt.wait () in
  IntTbl.add ioInFlight id (`Write, buf, res);
  start_write ch buf 0 len id;
if !d then Format.eprintf "Writing started@.";
  res

external win_pipe_in :
  unit -> Unix.file_descr * Unix.file_descr = "win_pipe_in"
external win_pipe_out :
  unit -> Unix.file_descr * Unix.file_descr = "win_pipe_out"

let pipe_in () =
  let (i, o) = if no_overlapped_io then Unix.pipe () else win_pipe_in () in
  (wrap_async i, o)
let pipe_out () =
  let (i, o) = if no_overlapped_io then Unix.pipe () else win_pipe_out () in
  (i, wrap_async o)

external win_socket :
  Unix.socket_domain -> Unix.socket_type -> int -> Unix.file_descr =
  "win_socket"

let socket d t p =
  let s = if no_overlapped_io then Unix.socket d t p else win_socket d t p in
  Unix.set_nonblock s;
  wrap_async s

let bind ch addr = Unix.bind ch.fd addr
let setsockopt ch opt v = Unix.setsockopt ch.fd opt v
let listen ch n = Unix.listen ch.fd n
let set_close_on_exec ch = Unix.set_close_on_exec ch.fd

external kill_threads : file_descr -> unit = "win_kill_threads"

let close ch = Unix.close ch.fd; kill_threads ch

let accept ch =
  let res = Lwt.wait () in
  let i = acquire_event "accept" in
  IntTbl.add connInFlight i (`Accept res, ch);
  win_register_wait ch.fd ACCEPT i;
  res

let check_socket ch =
  let res = Lwt.wait () in
  let i = acquire_event "connect" in
  IntTbl.add connInFlight i (`CheckSocket res, ch);
  win_register_wait ch.fd CONNECT i;
  res

let connect s addr =
  try
    Unix.connect s.fd addr;
if !d then prerr_endline "AAA";
    Lwt.return ()
  with
    Unix.Unix_error
      ((Unix.EINPROGRESS | Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
if !d then prerr_endline "BBB";
        check_socket s
  | e ->
if !d then prerr_endline "CCC";
      Lwt.fail e

(*
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
     0 -> Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
  | id -> Lwt.bind (waitpid [] id) (fun (pid, status) -> Lwt.return status)
*)

(****)
(*
type lwt_in_channel = in_channel
type lwt_out_channel = out_channel

let intern_in_channel ch =
  Unix.set_nonblock (Unix.descr_of_in_channel ch); ch
let intern_out_channel ch =
  Unix.set_nonblock (Unix.descr_of_out_channel ch); ch


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
*)
(****)

(*
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
          Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
  | id -> Hashtbl.add popen_processes proc id

let open_process_in cmd =
  let (in_read, in_write) = pipe_in () in
  let inchan = Unix.in_channel_of_descr in_read in
  open_proc cmd (Process_in inchan) Unix.stdin in_write [in_read];
  Unix.close in_write;
  Lwt.return inchan

let open_process_out cmd =
  let (out_read, out_write) = pipe_out () in
  let outchan = Unix.out_channel_of_descr out_write in
  open_proc cmd (Process_out outchan) out_read Unix.stdout [out_write];
  Unix.close out_read;
  Lwt.return outchan

let open_process cmd =
  let (in_read, in_write) = pipe_in () in
  let (out_read, out_write) = pipe_out () in
  let inchan = Unix.in_channel_of_descr in_read in
  let outchan = Unix.out_channel_of_descr out_write in
  open_proc cmd (Process(inchan, outchan)) out_read in_write
                                           [in_read; out_write];
  Unix.close out_read;
  Unix.close in_write;
  Lwt.return (inchan, outchan)

(* FIX: Subprocesses that use /dev/tty to print things on the terminal
   will NOT have this output captured and returned to the caller of this
   function.  There's an argument that this is correct, but if we are
   running from a GUI the user may not be looking at any terminal and it
   will appear that the process is just hanging.  This can be fixed, in
   principle, by writing a little C code that opens /dev/tty and then uses
   the TIOCNOTTY ioctl control to detach the terminal. *)

let open_proc_full cmd env proc input output error toclose =
  match Unix.fork () with
     0 -> Unix.dup2 input Unix.stdin; Unix.close input;
          Unix.dup2 output Unix.stdout; Unix.close output;
          Unix.dup2 error Unix.stderr; Unix.close error;
          List.iter Unix.close toclose;
          Unix.execve "/bin/sh" [| "/bin/sh"; "-c"; cmd |] env
  | id -> Hashtbl.add popen_processes proc id

let open_process_full cmd env =
  let (in_read, in_write) = pipe_in () in
  let (out_read, out_write) = pipe_out () in
  let (err_read, err_write) = pipe_in () in
  let inchan = Unix.in_channel_of_descr in_read in
  let outchan = Unix.out_channel_of_descr out_write in
  let errchan = Unix.in_channel_of_descr err_read in
  open_proc_full cmd env (Process_full(inchan, outchan, errchan))
                 out_read in_write err_write [in_write; out_read; err_read];
  Unix.close out_read;
  Unix.close in_write;
  Unix.close err_write;
  Lwt.return (inchan, outchan, errchan)

let find_proc_id fun_name proc =
  try
    let pid = Hashtbl.find popen_processes proc in
    Hashtbl.remove popen_processes proc;
    pid
  with Not_found ->
    raise (Unix.Unix_error (Unix.EBADF, fun_name, ""))
*)
(*
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
*)

type lwt_in_channel
let input_line _ = assert false (*XXXXX*)
let intern_in_channel _ = assert false (*XXXXX*)

(***)

type directory_handle = Unix.file_descr

external open_dir : string -> string -> directory_handle = "win_open_directory"
let open_directory f = open_dir f (System_impl.Fs.W.epath f)

type notify_filter_flag =
    FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME
  | FILE_NOTIFY_CHANGE_ATTRIBUTES | FILE_NOTIFY_CHANGE_SIZE
  | FILE_NOTIFY_CHANGE_LAST_WRITE | FILE_NOTIFY_CHANGE_LAST_ACCESS
  | FILE_NOTIFY_CHANGE_CREATION | FILE_NOTIFY_CHANGE_SECURITY

external start_read_dir_changes :
  directory_handle -> buffer -> bool -> notify_filter_flag list -> int -> unit =
  "win_readdirtorychanges"

type file_action =
    FILE_ACTION_ADDED | FILE_ACTION_REMOVED
  | FILE_ACTION_MODIFIED | FILE_ACTION_RENAMED_OLD_NAME
  | FILE_ACTION_RENAMED_NEW_NAME

external parse_directory_changes : buffer -> (string * file_action) list
  = "win_parse_directory_changes"

let readdirectorychanges ch recursive flags =
if !d then Format.eprintf "Start reading directory changes@.";
  let id = acquire_id () in
  let buf = acquire_buffer () in
  let res = Lwt.wait () in
  IntTbl.add ioInFlight id (`Readdirectorychanges, buf, res);
  start_read_dir_changes ch buf recursive flags id;
if !d then Format.eprintf "Reading started@.";
  Lwt.bind res (fun len ->
  if len = 0 then
    Lwt.return []
  else
    Lwt.return (List.rev_map (fun (nm, act) ->
                                (System_impl.Fs.W.path8 nm, act))
                        (parse_directory_changes buf)))

let close_dir = Unix.close
