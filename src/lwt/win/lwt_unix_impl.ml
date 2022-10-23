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
external unsafe_blit_bytes_to_buffer :
  bytes -> int -> buffer -> int -> int -> unit = "ml_blit_bytes_to_buffer"
external unsafe_blit_buffer_to_bytes :
  buffer -> int -> bytes -> int -> int -> unit = "ml_blit_buffer_to_bytes"

let buffer_length = Bigarray.Array1.dim

let blit_string_to_buffer s i a j l =
  if l < 0 || i < 0 || i > String.length s - l
           || j < 0 || j > buffer_length a - l
  then invalid_arg "Lwt_unix.blit_string_to_buffer"
  else unsafe_blit_string_to_buffer s i a j l

let blit_bytes_to_buffer s i a j l =
  if l < 0 || i < 0 || i > Bytes.length s - l
           || j < 0 || j > buffer_length a - l
  then invalid_arg "Lwt_unix.blit_bytes_to_buffer"
  else unsafe_blit_bytes_to_buffer s i a j l

let blit_buffer_to_bytes a i s j l =
  if l < 0 || i < 0 || i > buffer_length a - l
           || j < 0 || j > Bytes.length s - l
  then invalid_arg "Lwt_unix.blit_buffer_to_bytes"
  else unsafe_blit_buffer_to_bytes a i s j l

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

let acquire_event l nm =
  if List.length l = max_event_count then
    raise (Unix.Unix_error (Unix.EAGAIN, nm, ""))

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

let handleCompletionEvent (id, len, errno, name) =
if !d then Format.eprintf "Handling event %d (len %d)@." id len;
  let (action, buf, res) =
    try IntTbl.find ioInFlight id with Not_found -> assert false
  in
  begin match action with
    `Write         -> ()
  | `Read (s, pos) -> if len > 0 then blit_buffer_to_bytes buf 0 s pos len
  | `Readdirectorychanges -> ()
  end;
  IntTbl.remove ioInFlight id;
  release_id id;
  release_buffer buf;
  if len = -1 then
    Lwt.wakeup_exn res (Unix.Unix_error (errno, name, ""))
  else
    Lwt.wakeup res len

type handle

let connInFlight = ref []

type kind = CONNECT | ACCEPT

external win_wait : int -> handle list -> int = "win_wait"

external win_register_wait :
  Unix.file_descr -> kind -> handle = "win_register_wait"

external win_check_connection :
  Unix.file_descr -> kind -> handle -> unit = "win_check_connection"

let handle_wait_event h ch kind cont action =
if !d then prerr_endline "MMM";
  let res =
    try
      Some (action ())
    with
      Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
if !d then prerr_endline "NNN";
        let h' = win_register_wait ch.fd kind in
        connInFlight := List.map (fun el -> if fst el <> h then el else (h', snd el)) !connInFlight;
        None
    | e ->
if !d then prerr_endline "OOO";
        connInFlight := List.filter (fun (h', _) -> h' <> h) !connInFlight;
        Lwt.wakeup_exn cont e;
        None
  in
  match res with
    Some v ->
if !d then prerr_endline "PPP";
      connInFlight := List.filter (fun (h', _) -> h' <> h) !connInFlight;
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
          win_wait (truncate (ceil (delay *. 1000.))) (List.map fst !connInFlight)
        with
          Sys.Break as e -> raise e
        | _              -> assert false
      in
if !d then Format.eprintf "^^^@.";
      if i = -1 then now := !now +. delay;
      restart_threads !event_counter now;
if !d then Format.eprintf "threads restarted@.";
      let ev = !completionEvents in
      completionEvents := [];
      List.iter handleCompletionEvent (List.rev ev);
      if i >= 0 then begin
        let (h, (kind, ch)) =
          try List.nth !connInFlight i with Failure _ -> assert false in
        match kind with
          `CheckSocket res  ->
if !d then prerr_endline "CHECK CONN";
            handle_wait_event h ch CONNECT res
               (fun () -> win_check_connection ch.fd CONNECT h)
        | `Accept res ->
if !d then prerr_endline "ACCEPT";
            handle_wait_event h ch ACCEPT res
              (fun () ->
                 win_check_connection ch.fd ACCEPT h;
                 let (v, info) = Unix.accept ~cloexec:true ch.fd in
                 (wrap_async v, info))
      end;
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
  blit_bytes_to_buffer s pos buf 0 len;
  let res = Lwt.wait () in
  IntTbl.add ioInFlight id (`Write, buf, res);
  start_write ch buf 0 len id;
if !d then Format.eprintf "Writing started@.";
  res

let write_substring ch s pos len =
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
  ?cloexec:bool -> unit -> Unix.file_descr * Unix.file_descr = "win_pipe_in"
external win_pipe_out :
  ?cloexec:bool -> unit -> Unix.file_descr * Unix.file_descr = "win_pipe_out"

let pipe_in ?cloexec () =
  let (i, o) = if no_overlapped_io then Unix.pipe () else win_pipe_in ?cloexec () in
  (wrap_async i, o)
let pipe_out ?cloexec () =
  let (i, o) = if no_overlapped_io then Unix.pipe () else win_pipe_out ?cloexec () in
  (i, wrap_async o)

external win_socket : ?cloexec:bool ->
  Unix.socket_domain -> Unix.socket_type -> int -> Unix.file_descr =
  "win_socket"

let socket ?cloexec d t p =
  let s = if no_overlapped_io then Unix.socket ?cloexec d t p
          else win_socket ?cloexec d t p in
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
  let () = acquire_event !connInFlight "accept" in
  let h = win_register_wait ch.fd ACCEPT in
  connInFlight := (h, (`Accept res, ch)) :: !connInFlight;
  res

let check_socket ch =
  let res = Lwt.wait () in
  let () = acquire_event !connInFlight "connect" in
  let h = win_register_wait ch.fd CONNECT in
  connInFlight := (h, (`CheckSocket res, ch)) :: !connInFlight;
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


type lwt_in_channel
let input_line _ = assert false (*XXXXX*)
let intern_in_channel _ = assert false (*XXXXX*)

(***)

type directory_handle = Unix.file_descr

external open_dir : string -> directory_handle = "win_open_directory"
let open_directory f = open_dir (System_win.extendedPath f)

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
    Lwt.return (List.rev (parse_directory_changes buf)))

let close_dir = Unix.close

external long_name : string -> string = "win_long_path_name"

let longpathname root path =
  (* Parameter [path] can be relative. Result value must then also be relative.
     Input parameter to [long_name] must always be absolute path. *)
  let epath = System_win.extendedPath (Filename.concat root path)
  and root = System_win.extendedPath (Filename.concat root "") in
  let start = String.length root
  and ln = long_name epath in
  try
    (* The assumption is that [root] does not change in [long_name]. The
       Windows fsmonitor operates under this assumption, so it is ok here.
       To remove this assumption, we'd have to pass [root] separately through
       [long_name]. *)
    String.sub ln start (String.length ln - start)
  with
  | Invalid_argument _ -> ln
