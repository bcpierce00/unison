(* Unison file synchronizer: src/remote.ml *)
(* Copyright 1999-2009, Benjamin C. Pierce 

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

let (>>=) = Lwt.bind

let debug = Trace.debug "remote"
let debugV = Trace.debug "remote_emit+"
let debugE = Trace.debug "remote+"
let debugT = Trace.debug "remote+"

(* BCP: The previous definitions of the last two were like this:
     let debugE = Trace.debug "remote_emit"
     let debugT = Trace.debug "thread"
   But that resulted in huge amounts of output from '-debug all'.
*)

let windowsHack = Sys.os_type <> "Unix"
let recent_ocaml =
  Scanf.sscanf Sys.ocaml_version "%d.%d"
    (fun maj min -> (maj = 3 && min >= 11) || maj > 3)

(*
   Flow-control mechanism (only active under Windows).
   Only one side is allowed to send messages at any given time.
   Once it has finished sending messages, a special message is sent
   meaning that the destination is now allowed to send messages.

   Threads behave in a very controlled way: they only perform possibly
   blocking I/Os through the remote module, and never call
   Lwt_unix.yield.  This mean that when one side gives up its right to
   write, we know that no longer how much we wait, it would not have
   any thing to write.  This ensures that there will be no deadlock.
   A more robust protocol would be to give up write permission
   whenever idle (not just after having sent at least one message).
   But then, there is the risk that the two sides exchange spurious
   messages.
*)
let needFlowControl = windowsHack
let readOrWrite = needFlowControl && not recent_ocaml

(****)

let intSize = 5

let intHash x = ((x * 791538121) lsr 23 + 17) land 255

let encodeInt m =
  let int_buf = Bytearray.create intSize in
  int_buf.{0} <- Char.chr ( m         land 0xff);
  int_buf.{1} <- Char.chr ((m lsr 8)  land 0xff);
  int_buf.{2} <- Char.chr ((m lsr 16) land 0xff);
  int_buf.{3} <- Char.chr ((m lsr 24) land 0xff);
  int_buf.{4} <- Char.chr (intHash m);
  (int_buf, 0, intSize)

let decodeInt int_buf i =
  let b0 = Char.code (int_buf.{i + 0}) in
  let b1 = Char.code (int_buf.{i + 1}) in
  let b2 = Char.code (int_buf.{i + 2}) in
  let b3 = Char.code (int_buf.{i + 3}) in
  let m = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
  if Char.code (int_buf.{i + 4}) <> intHash m then
    raise (Util.Fatal
             "Protocol error: corrupted message received;\n\
              if it happens to you in a repeatable way, \n\
              please post a report on the unison-users mailing list.");
  m

(*************************************************************************)
(*                           LOW-LEVEL IO                                *)
(*************************************************************************)

let lostConnection () =
  Lwt.fail (Util.Fatal "Lost connection with the server")

let catchIoErrors th =
  Lwt.catch th
    (fun e ->
       match e with
         Unix.Unix_error(Unix.ECONNRESET, _, _)
       | Unix.Unix_error(Unix.EPIPE, _, _)
         (* Windows may also return the following errors... *)
       | Unix.Unix_error(Unix.EINVAL, _, _) ->
         (* Client has closed its end of the connection *)
           lostConnection ()
       | _ ->
           Lwt.fail e)

(****)

let receivedBytes = ref 0.
let emittedBytes = ref 0.

(****)

(* I/O buffers *)

type ioBuffer =
  { channel : Unix.file_descr;
    buffer : string;
    mutable length : int }

let bufferSize = 16384
(* No point in making this larger, as the Ocaml Unix library uses a
   buffer of this size *)

let makeBuffer ch =
  { channel = ch; buffer = String.create bufferSize; length = 0 }

(****)

(* Low-level inputs *)

let fillInputBuffer conn =
  assert (conn.length = 0);
  catchIoErrors
    (fun () ->
       Lwt_unix.read conn.channel conn.buffer 0 bufferSize >>= fun len ->
       debugV (fun() ->
         if len = 0 then
           Util.msg "grab: EOF\n"
         else
           Util.msg "grab: %s\n"
             (String.escaped (String.sub conn.buffer 0 len)));
       if len = 0 then
         lostConnection ()
       else begin
         receivedBytes := !receivedBytes +. float len;
         conn.length <- len;
         Lwt.return ()
       end)

let rec grabRec conn s pos len =
  if conn.length = 0 then begin
    fillInputBuffer conn >>= fun () ->
    grabRec conn s pos len
  end else begin
    let l = min (len - pos) conn.length in
    Bytearray.blit_from_string conn.buffer 0 s pos l;
    conn.length <- conn.length - l;
    if conn.length > 0 then
      String.blit conn.buffer l conn.buffer 0 conn.length;
    if pos + l < len then
      grabRec conn s (pos + l) len
    else
      Lwt.return ()
  end

let grab conn s len =
  assert (len > 0);
  assert (Bytearray.length s <= len);
  grabRec conn s 0 len

let peekWithoutBlocking conn =
  String.sub conn.buffer 0 conn.length

(****)

(* Low-level outputs *)

let rec sendOutput conn =
  catchIoErrors
    (fun () ->
       Lwt_unix.write conn.channel conn.buffer 0 conn.length >>= fun len ->
       debugV (fun() ->
         Util.msg "dump: %s\n"
           (String.escaped (String.sub conn.buffer 0 len)));
       emittedBytes := !emittedBytes +. float len;
       conn.length <- conn.length - len;
       if conn.length > 0 then
         String.blit
           conn.buffer len conn.buffer 0 conn.length;
       Lwt.return ())

let rec fillBuffer2 conn s pos len =
  if conn.length = bufferSize then
    sendOutput conn >>= fun () ->
    fillBuffer2 conn s pos len
  else begin
    let l = min (len - pos) (bufferSize - conn.length) in
    Bytearray.blit_to_string s pos conn.buffer conn.length l;
    conn.length <- conn.length + l;
    if pos + l < len then
      fillBuffer2 conn s (pos + l) len
    else
      Lwt.return ()
  end

let rec fillBuffer conn l =
  match l with
    (s, pos, len) :: rem ->
      assert (pos >= 0);
      assert (len >= 0);
      assert (pos <= Bytearray.length s - len);
      fillBuffer2 conn s pos len >>= fun () ->
      fillBuffer conn rem
  | [] ->
      Lwt.return ()

let rec flushBuffer conn =
  if conn.length > 0 then
    sendOutput conn >>= fun () ->
    flushBuffer conn
  else
    Lwt.return ()

(****)

(* Output scheduling *)

type kind = Normal | Idle | Last | Urgent

type outputQueue =
  { mutable available : bool;
    mutable canWrite : bool;
    mutable flowControl : bool;
    writes : (kind * (unit -> unit Lwt.t) * unit Lwt.t) Queue.t;
    urgentWrites : (kind * (unit -> unit Lwt.t) * unit Lwt.t) Queue.t;
    idleWrites : (kind * (unit -> unit Lwt.t) * unit Lwt.t) Queue.t;
    flush : outputQueue -> unit Lwt.t }

let rec performOutputRec q (kind, action, res) =
  action () >>= fun () ->
  if kind = Last then begin
    assert (q.canWrite);
    if q.flowControl then q.canWrite <- false
  end;
  Lwt.wakeup res ();
  popOutputQueues q

and popOutputQueues q =
  if not (Queue.is_empty q.urgentWrites) then
    performOutputRec q (Queue.take q.urgentWrites)
  else if not (Queue.is_empty q.writes) && q.canWrite then
    performOutputRec q (Queue.take q.writes)
  else if not (Queue.is_empty q.idleWrites) && q.canWrite then
    performOutputRec q (Queue.take q.idleWrites)
  else begin
    q.available <- true;
    (* Flush asynchronously the output *)
    Lwt.ignore_result (q.flush q);
    Lwt.return ()
  end

(* Perform an output action in an atomic way *)
let performOutput q kind action =
  if q.available && (kind = Urgent || q.canWrite) then begin
    q.available <- false;
    performOutputRec q (kind, action, Lwt.wait ())
  end else begin
    let res = Lwt.wait () in
    Queue.add (kind, action, res)
      (match kind with
         Urgent -> q.urgentWrites
       | Normal -> q.writes
       | Idle   -> q.idleWrites
       | Last   -> assert false);
    res
  end

let allowWrites q =
  assert (not q.canWrite);
  q.canWrite <- true;
  q.available <- false;
  (* We yield to let the receiving thread restart and to let some time
     to the requests to be processed *)
  Lwt.ignore_result (Lwt_unix.yield () >>= fun () -> popOutputQueues q)

let disableFlowControl q =
  q.flowControl <- false;
  if not q.canWrite then allowWrites q

let outputQueueIsEmpty q = q.available

let makeOutputQueue isServer flush =
  { available = true; canWrite = isServer; flowControl = true;
    writes = Queue.create (); urgentWrites = Queue.create ();
    idleWrites = Queue.create ();
    flush = flush }

(****)

type connection =
  { inputBuffer : ioBuffer;
    outputBuffer : ioBuffer;
    outputQueue : outputQueue;
    receiver :  (unit -> unit Lwt.t) option ref }

let maybeFlush receiver pendingFlush q buf =
  (* We return immediately if a flush is already scheduled, or if the
     output buffer is already empty. *)
  (* If we are doing flow control and we can write, we need to send
     a write token even when the buffer is empty. *)
  if
    !pendingFlush || (buf.length = 0 && not (q.flowControl && q.canWrite))
  then
    Lwt.return ()
  else begin
    pendingFlush := true;
    (* Wait a bit, in case there are some new requests being processed *)
    Lwt_unix.yield () >>= fun () ->
    pendingFlush := false;
    (* If there are other writes scheduled, we do not flush yet *)
    if outputQueueIsEmpty q then begin
      performOutput q Last
        (fun () ->
           if q.flowControl then begin
             debugE (fun() -> Util.msg "Sending write token\n");
             fillBuffer buf [encodeInt 0] >>= fun () ->
             flushBuffer buf
           end else
             flushBuffer buf) >>= fun () ->
      assert (not (q.flowControl && q.canWrite));
      (* Restart the reader thread if needed *)
      match !receiver with
        None   -> Lwt.return ()
      | Some f -> f ()
    end else
      Lwt.return ()
  end

let makeConnection isServer inCh outCh =
  let pendingFlush = ref false in
  let receiver = ref None in
  let outputBuffer = makeBuffer outCh in
    { inputBuffer = makeBuffer inCh;
      outputBuffer = outputBuffer;
      outputQueue =
        makeOutputQueue isServer
          (fun q -> maybeFlush receiver pendingFlush q outputBuffer);
      receiver = receiver }

(* Send message [l] *)
let dump conn l =
  performOutput
    conn.outputQueue Normal (fun () -> fillBuffer conn.outputBuffer l)

(* Send message [l] when idle *)
let dumpIdle conn l =
  performOutput
    conn.outputQueue Idle (fun () -> fillBuffer conn.outputBuffer l)

(* Send message [l], even if write are disabled.  This is used for
   aborting rapidly a stream.  This works as long as only one small
   message is written at a time (the write will succeed as the pipe
   will not be full) *)
let dumpUrgent conn l =
  performOutput conn.outputQueue Urgent
    (fun () ->
       fillBuffer conn.outputBuffer l >>= fun () ->
       flushBuffer conn.outputBuffer)

(****)

(* Initialize the connection *)
let setupIO isServer inCh outCh =
  if not windowsHack then begin
    Unix.set_nonblock inCh;
    Unix.set_nonblock outCh
  end;
  makeConnection isServer inCh outCh

(* XXX *)
module Thread = struct

  let unwindProtect f cleanup =
    Lwt.catch f
      (fun e ->
         match e with
           Util.Transient err | Util.Fatal err ->
             debugT
               (fun () ->
                  Util.msg
                    "Exception caught by Thread.unwindProtect: %s\n" err);
             Lwt.catch (fun () -> cleanup e) (fun e' ->
               Util.encodeException "Thread.unwindProtect" `Fatal e')
                 >>= (fun () ->
             Lwt.fail e)
         | _ ->
             Lwt.fail e)

end

(*****************************************************************************)
(*                              MARSHALING                                   *)
(*****************************************************************************)

type tag = Bytearray.t

type 'a marshalFunction =
  'a -> (Bytearray.t * int * int) list -> (Bytearray.t * int * int) list
type 'a unmarshalFunction = Bytearray.t -> 'a
type 'a marshalingFunctions = 'a marshalFunction * 'a unmarshalFunction

let registeredSet = ref Util.StringSet.empty

let rec first_chars len msg =
  match msg with
    [] ->
      ""
  | (s, p, l) :: rem ->
      if l < len then
        Bytearray.sub s p l ^ first_chars (len - l) rem
      else
        Bytearray.sub s p len

let safeMarshal marshalPayload tag data rem =
  let (rem', length) = marshalPayload data rem in
  let l = Bytearray.length tag in
  debugE (fun() ->
            let start = first_chars (min length 10) rem' in
            let start = if length > 10 then start ^ "..." else start in
            let start = String.escaped start in
            Util.msg "send [%s] '%s' %d bytes\n"
              (Bytearray.to_string tag) start length);
  (encodeInt (l + length) :: (tag, 0, l) :: rem')

let safeUnmarshal unmarshalPayload tag buf =
  let taglength = Bytearray.length tag in
  if Bytearray.prefix tag buf 0 then
    unmarshalPayload buf taglength
  else
    let identifier =
      String.escaped
        (Bytearray.sub buf 0 (min taglength (Bytearray.length buf))) in
    raise (Util.Fatal
             (Printf.sprintf "[safeUnmarshal] expected '%s' but got '%s'"
                (String.escaped (Bytearray.to_string tag)) identifier))

let registerTag string =
  if Util.StringSet.mem string !registeredSet then
    raise (Util.Fatal (Printf.sprintf "tag %s is already registered" string))
  else
    registeredSet := Util.StringSet.add string !registeredSet;
  Bytearray.of_string string

let defaultMarshalingFunctions =
  (fun data rem ->
     let s = Bytearray.marshal data [Marshal.No_sharing] in
     let l = Bytearray.length s in
     ((s, 0, l) :: rem, l)),
  (fun buf pos -> Bytearray.unmarshal buf pos)

let makeMarshalingFunctions payloadMarshalingFunctions string =
  let (marshalPayload, unmarshalPayload) = payloadMarshalingFunctions in
  let tag = registerTag string in
  let marshal (data : 'a) rem = safeMarshal marshalPayload tag data rem in
  let unmarshal buf = (safeUnmarshal unmarshalPayload tag buf : 'a) in
  (marshal, unmarshal)

(*****************************************************************************)
(*                              SERVER SETUP                                 *)
(*****************************************************************************)

(* BCPFIX: Now that we've beefed up the clroot data structure, shouldn't
   these be part of it too? *)
let sshCmd =
  Prefs.createString "sshcmd" "ssh"
    ("!path to the ssh executable")
    ("This preference can be used to explicitly set the name of the "
     ^ "ssh executable (e.g., giving a full path name), if necessary.")

let rshCmd =
  Prefs.createString "rshcmd" "rsh"
    ("*path to the rsh executable")
    ("This preference can be used to explicitly set the name of the "
     ^ "rsh executable (e.g., giving a full path name), if necessary.")

let rshargs =
  Prefs.createString "rshargs" ""
    "*other arguments (if any) for remote shell command"
    ("The string value of this preference will be passed as additional "
     ^ "arguments (besides the host name and the name of the Unison "
     ^ "executable on the remote system) to the \\verb|rsh| "
     ^ "command used to invoke the remote server. "
     )

let sshargs =
  Prefs.createString "sshargs" ""
    "!other arguments (if any) for remote shell command"
    ("The string value of this preference will be passed as additional "
     ^ "arguments (besides the host name and the name of the Unison "
     ^ "executable on the remote system) to the \\verb|ssh| "
     ^ "command used to invoke the remote server. "
     )

let serverCmd =
  Prefs.createString "servercmd" ""
    ("!name of " ^ Uutil.myName ^ " executable on remote server")
    ("This preference can be used to explicitly set the name of the "
     ^ "Unison executable on the remote server (e.g., giving a full "
     ^ "path name), if necessary.")

let addversionno =
  Prefs.createBool "addversionno" false
    ("!add version number to name of " ^ Uutil.myName ^ " on server")
    ("When this flag is set to {\\tt true}, Unison "
     ^ "will use \\texttt{unison-\\ARG{currentversionnumber}} instead of "
     ^ "just \\verb|unison| as the remote server command.  This allows "
     ^ "multiple binaries for different versions of unison to coexist "
     ^ "conveniently on the same server: whichever version is run "
     ^ "on the client, the same version will be selected on the server.")

(* List containing the connected hosts and the file descriptors of
   the communication. *)
let connectionsByHosts = ref []

(* Gets the Read/Write file descriptors for a host;
   the connection must have been set up by canonizeRoot before calling *)
let hostConnection host =
  try Safelist.assoc host !connectionsByHosts
  with Not_found ->
    raise(Util.Fatal "Remote.hostConnection")

(* connectedHosts is a list of command-line roots and their corresponding
   canonical host names.
   Local command-line roots are not in the list.
   Although there can only be one remote host per sync, it's possible
   connectedHosts to hold more than one hosts if more than one sync is
   performed.
   It's also possible for there to be two connections open for the
   same canonical root.
*)
let connectedHosts = ref []

(**********************************************************************
                       CLIENT/SERVER PROTOCOLS
 **********************************************************************)

(*
Each protocol has a name, a client side, and a server side.

The server remembers the server side of each protocol in a table
indexed by protocol name. The function of the server is to wait for
the client to invoke a protocol, and carry out the appropriate server
side.

Protocols are invoked on the client with arguments for the server side.
The result of the protocol is the result of the server side. In types,

  serverSide : 'a -> 'b

That is, the server side takes arguments of type 'a from the client,
and returns a result of type 'b.

A protocol is started by the client sending a Request packet and then a
packet containing the protocol name to the server.  The server looks
up the server side of the protocol in its table.

Next, the client sends a packet containing marshaled arguments for the
server side.

The server unmarshals the arguments and invokes the server side with
the arguments from the client.

When the server side completes it gives a result. The server marshals
the result and sends it to the client.  (Instead of a result, the
server may also send back either a Transient or a Fatal error packet).
Finally, the client can receive the result packet from the server and
unmarshal it.

The protocol is fully symmetric, so the server may send a Request
packet to invoke a function remotely on the client.  In this case, the
two switch roles.)
*)

let receivePacket conn =
  (* Get the length of the packet *)
  let int_buf = Bytearray.create intSize in
  grab conn.inputBuffer int_buf intSize >>= (fun () ->
  let length = decodeInt int_buf 0 in
  assert (length >= 0);
  (* Get packet *)
  let buf = Bytearray.create length in
  grab conn.inputBuffer buf length >>= (fun () ->
  (debugE (fun () ->
             let start =
               if length > 10 then (Bytearray.sub buf 0 10) ^ "..."
               else Bytearray.sub buf 0 length in
             let start = String.escaped start in
             Util.msg "receive '%s' %d bytes\n" start length);
   Lwt.return buf)))

type servercmd =
  connection -> Bytearray.t ->
  ((Bytearray.t * int * int) list -> (Bytearray.t * int * int) list) Lwt.t
let serverCmds = ref (Util.StringMap.empty : servercmd Util.StringMap.t)

type serverstream =
  connection -> Bytearray.t -> unit
let serverStreams = ref (Util.StringMap.empty : serverstream Util.StringMap.t)

type header =
    NormalResult
  | TransientExn of string
  | FatalExn of string
  | Request of string
  | Stream of string
  | StreamAbort

let ((marshalHeader, unmarshalHeader) : header marshalingFunctions) =
  makeMarshalingFunctions defaultMarshalingFunctions "rsp"

let processRequest conn id cmdName buf =
  let cmd =
    try Util.StringMap.find cmdName !serverCmds
    with Not_found -> raise (Util.Fatal (cmdName ^ " not registered!"))
  in
  Lwt.try_bind (fun () -> cmd conn buf)
    (fun marshal ->
       debugE (fun () -> Util.msg "Sending result (id: %d)\n" (decodeInt id 0));
       dump conn ((id, 0, intSize) :: marshalHeader NormalResult (marshal [])))
    (function
       Util.Transient s ->
         debugE (fun () ->
           Util.msg "Sending transient exception (id: %d)\n" (decodeInt id 0));
         dump conn ((id, 0, intSize) :: marshalHeader (TransientExn s) [])
     | Util.Fatal s ->
         debugE (fun () ->
           Util.msg "Sending fatal exception (id: %d)\n" (decodeInt id 0));
         dump conn ((id, 0, intSize) :: marshalHeader (FatalExn s) [])
     | e ->
         Lwt.fail e)

let streamAbortedSrc = ref 0
let streamAbortedDst = ref false

let streamError = Hashtbl.create 7

let abortStream conn id =
  if not !streamAbortedDst then begin
    streamAbortedDst := true;
    let request = encodeInt id :: marshalHeader StreamAbort [] in
    dumpUrgent conn request
  end else
    Lwt.return ()

let processStream conn id cmdName buf =
  let id = decodeInt id 0 in
  if Hashtbl.mem streamError id then
   abortStream conn id
  else begin
    begin try
      let cmd =
        try Util.StringMap.find cmdName !serverStreams
        with Not_found -> raise (Util.Fatal (cmdName ^ " not registered!"))
      in
      cmd conn buf;
      Lwt.return ()
    with e ->
      Hashtbl.add streamError id e;
      abortStream conn id
    end
  end

(* Message ids *)
type msgId = int
module MsgIdMap = Map.Make (struct type t = msgId let compare = compare end)
(* An integer just a little smaller than the maximum representable in
   30 bits *)
let hugeint = 1000000000
let ids = ref 1
let newMsgId () = incr ids; if !ids = hugeint then ids := 2; !ids

(* Threads waiting for a response from the other side *)
let receivers = ref MsgIdMap.empty

let find_receiver id =
  let thr = MsgIdMap.find id !receivers in
  receivers := MsgIdMap.remove id !receivers;
  thr

(* Receiving thread: read a message and dispatch it to the right
   thread or create a new thread to process requests. *)
let rec receive conn =
  if readOrWrite && conn.outputQueue.canWrite then begin
    conn.receiver := Some (fun () -> receive conn); Lwt.return ()
  end else begin
    debugE (fun () -> Util.msg "Waiting for next message\n");
    (* Get the message ID *)
    let id = Bytearray.create intSize in
    grab conn.inputBuffer id intSize >>= (fun () ->
    let num_id = decodeInt id 0 in
    if num_id = 0 then begin
      debugE (fun () -> Util.msg "Received the write permission\n");
      allowWrites conn.outputQueue;
      receive conn
    end else begin
      debugE
        (fun () -> Util.msg "Message received (id: %d)\n" num_id);
      (* Read the header *)
      receivePacket conn >>= (fun buf ->
      let req = unmarshalHeader buf in
      begin match req with
        Request cmdName ->
          receivePacket conn >>= (fun buf ->
          (* We yield before starting processing the request.
             This way, the request may call [Lwt_unix.run] and this will
             not block the receiving thread. *)
          Lwt.ignore_result
            (Lwt_unix.yield () >>= (fun () ->
             processRequest conn id cmdName buf));
          receive conn)
      | NormalResult ->
          receivePacket conn >>= (fun buf ->
          Lwt.wakeup (find_receiver num_id) buf;
          receive conn)
      | TransientExn s ->
          debugV (fun() -> Util.msg "receive: Transient remote error '%s']" s);
          Lwt.wakeup_exn (find_receiver num_id) (Util.Transient s);
          receive conn
      | FatalExn s ->
          debugV (fun() -> Util.msg "receive: Fatal remote error '%s']" s);
          Lwt.wakeup_exn (find_receiver num_id) (Util.Fatal ("Server: " ^ s));
          receive conn
      | Stream cmdName ->
          receivePacket conn >>= fun buf ->
          processStream conn id cmdName buf >>= fun () ->
          receive conn
      | StreamAbort ->
          streamAbortedSrc := num_id;
          receive conn
      end)
    end)
  end

let wait_for_reply id =
  let res = Lwt.wait () in
  receivers := MsgIdMap.add id res !receivers;
  (* We yield to let the receiving thread restart.  This way, the
     thread may call [Lwt_unix.run] and this will not block the
     receiving thread. *)
  Lwt.catch
    (fun () ->
       res >>= (fun v -> Lwt_unix.yield () >>= (fun () -> Lwt.return v)))
    (fun e -> Lwt_unix.yield () >>= (fun () -> Lwt.fail e))

let registerSpecialServerCmd
    (cmdName : string)
    marshalingFunctionsArgs
    marshalingFunctionsResult
    (serverSide : connection -> 'a -> 'b Lwt.t)
    =
  (* Check that this command name has not already been bound *)
  if (Util.StringMap.mem cmdName !serverCmds) then
    raise (Util.Fatal (cmdName ^ " already registered!"));
  (* Create marshaling and unmarshaling functions *)
  let ((marshalArgs,unmarshalArgs) : 'a marshalingFunctions) =
    makeMarshalingFunctions marshalingFunctionsArgs (cmdName ^ "-args") in
  let ((marshalResult,unmarshalResult) : 'b marshalingFunctions) =
    makeMarshalingFunctions marshalingFunctionsResult (cmdName ^ "-res") in
  (* Create a server function and remember it *)
  let server conn buf =
    let args = unmarshalArgs buf in
    serverSide conn args >>= (fun answer ->
    Lwt.return (marshalResult answer))
  in
  serverCmds := Util.StringMap.add cmdName server !serverCmds;
  (* Create a client function and return it *)
  let client conn serverArgs =
    let id = newMsgId () in (* Message ID *)
    assert (id >= 0); (* tracking down an assert failure in receivePacket... *)
    let request =
      encodeInt id ::
      marshalHeader (Request cmdName) (marshalArgs serverArgs [])
    in
    let reply = wait_for_reply id in
    debugE (fun () -> Util.msg "Sending request (id: %d)\n" id);
    dump conn request >>= (fun () ->
    reply >>= (fun buf ->
    Lwt.return (unmarshalResult buf)))
  in
  client

let registerServerCmd name f =
  registerSpecialServerCmd
    name defaultMarshalingFunctions defaultMarshalingFunctions f

(* RegisterHostCmd is a simpler version of registerClientServer [registerServerCmd?].
   It is used to create remote procedure calls: the only communication
   between the client and server is the sending of arguments from
   client to server, and the sending of the result from the server
   to the client. Thus, server side does not need the file descriptors
   for communication with the client.

   RegisterHostCmd recognizes the case where the server is the local
   host, and it avoids socket communication in this case.
*)
let registerHostCmd cmdName cmd =
  let serverSide = (fun _ args -> cmd args) in
  let client0 =
    registerServerCmd cmdName serverSide in
  let client host args =
    let conn = hostConnection host in
    client0 conn args in
  (* Return a function that runs either the proxy or the local version,
     depending on whether the call is to the local host or a remote one *)
  fun host args ->
    match host with
      "" -> cmd args
    | _  -> client host args

let hostOfRoot root =
  match root with
    (Common.Local, _)       -> ""
  | (Common.Remote host, _) -> host
let connectionToRoot root = hostConnection (hostOfRoot root)

(* RegisterRootCmd is like registerHostCmd but it indexes connections by
   root instead of host. *)
let registerRootCmd (cmdName : string) (cmd : (Fspath.t * 'a) -> 'b) =
  let r = registerHostCmd cmdName cmd in
  fun root args -> r (hostOfRoot root) ((snd root), args)

let registerRootCmdWithConnection
  (cmdName : string) (cmd : connection -> 'a -> 'b) =
  let client0 = registerServerCmd cmdName cmd in
  (* Return a function that runs either the proxy or the local version,
     depending on whether the call is to the local host or a remote one *)
  fun localRoot remoteRoot args ->
    match (hostOfRoot localRoot) with
      "" -> let conn = hostConnection (hostOfRoot remoteRoot) in
            cmd conn args
    | _  -> let conn = hostConnection (hostOfRoot localRoot) in
            client0 conn args

let streamReg = Lwt_util.make_region 1

let streamingActivated =
  Prefs.createBool "stream" true
    ("!use a streaming protocol for transferring file contents")
    "When this preference is set, Unison will use an experimental \
     streaming protocol for transferring file contents more efficiently. \
     The default value is \\texttt{true}."

let registerStreamCmd
    (cmdName : string)
    marshalingFunctionsArgs
    (serverSide : connection -> 'a -> unit)
    =
  let cmd =
    registerSpecialServerCmd
      cmdName marshalingFunctionsArgs defaultMarshalingFunctions
      (fun conn v -> serverSide conn v; Lwt.return ())
  in
  let ping =
    registerServerCmd (cmdName ^ "Ping")
      (fun conn (id : int) ->
         try
           let e = Hashtbl.find streamError id in
           Hashtbl.remove streamError id;
           streamAbortedDst := false;
           Lwt.fail e
         with Not_found ->
           Lwt.return ())
  in
  (* Check that this command name has not already been bound *)
  if (Util.StringMap.mem cmdName !serverStreams) then
    raise (Util.Fatal (cmdName ^ " already registered!"));
  (* Create marshaling and unmarshaling functions *)
  let ((marshalArgs,unmarshalArgs) : 'a marshalingFunctions) =
    makeMarshalingFunctions marshalingFunctionsArgs (cmdName ^ "-str") in
  (* Create a server function and remember it *)
  let server conn buf =
    let args = unmarshalArgs buf in
    serverSide conn args
  in
  serverStreams := Util.StringMap.add cmdName server !serverStreams;
  (* Create a client function and return it *)
  let client conn id serverArgs =
    debugE (fun () -> Util.msg "Sending stream chunk (id: %d)\n" id);
    if !streamAbortedSrc = id then raise (Util.Transient "Streaming aborted");
    let request =
      encodeInt id ::
      marshalHeader (Stream cmdName) (marshalArgs serverArgs [])
    in
    dumpIdle conn request
  in
  fun conn sender ->
    if not (Prefs.read streamingActivated) then
      sender (fun v -> cmd conn v)
    else begin
      (* At most one active stream at a time *)
      let id = newMsgId () in (* Message ID *)
      Lwt.try_bind
        (fun () ->
           Lwt_util.run_in_region streamReg 1
             (fun () -> sender (fun v -> client conn id v)))
        (fun v -> ping conn id >>= fun () -> Lwt.return v)
	(fun e -> ping conn id >>= fun () -> Lwt.fail e)
    end

let commandAvailable =
  registerRootCmd "commandAvailable"
    (fun (_, cmdName) -> Lwt.return (Util.StringMap.mem cmdName !serverCmds))

(****************************************************************************
                     BUILDING CONNECTIONS TO THE SERVER
 ****************************************************************************)

let connectionHeader = "Unison " ^ Uutil.myMajorVersion ^ "\n"

let rec checkHeader conn buffer pos len =
  if pos = len then
    Lwt.return ()
  else begin
    (grab conn.inputBuffer buffer 1 >>= (fun () ->
    if buffer.{0} <> connectionHeader.[pos] then
      let prefix =
        String.sub connectionHeader 0 pos ^ Bytearray.to_string buffer in
      let rest = peekWithoutBlocking conn.inputBuffer in
      Lwt.fail
        (Util.Fatal
           ("Received unexpected header from the server:\n \
             expected \""
           ^ String.escaped (* (String.sub connectionHeader 0 (pos + 1)) *)
               connectionHeader
           ^ "\" but received \"" ^ String.escaped (prefix ^ rest) ^ "\", \n"
           ^ "which differs at \"" ^ String.escaped prefix ^ "\".\n"
           ^ "This can happen because you have different versions of Unison\n"
           ^ "installed on the client and server machines, or because\n"
           ^ "your connection is failing and somebody is printing an error\n"
           ^ "message, or because your remote login shell is printing\n"
           ^ "something itself before starting Unison."))
    else
      checkHeader conn buffer (pos + 1) len))
  end

(****)

(*
   Disable flow control if possible.
   Both hosts must use non-blocking I/O (otherwise a dead-lock is
   possible with ssh).
*)
let halfduplex =
  Prefs.createBool "halfduplex" false
    "!force half-duplex communication with the server"
    "When this flag is set to {\\tt true}, Unison network communication \
     is forced to be half duplex (the client and the server never \
     simultaneously emit data).  If you experience unstabilities with \
     your network link, this may help.  The communication is always \
     half-duplex when synchronizing with a Windows machine due to a \
     limitation of Unison current implementation that could result \
     in a deadlock."

let negociateFlowControlLocal conn () =
  if not needFlowControl then disableFlowControl conn.outputQueue;
  Lwt.return needFlowControl

let negociateFlowControlRemote =
  registerServerCmd "negociateFlowControl" negociateFlowControlLocal

let negociateFlowControl conn =
  (* Flow control negociation can be done asynchronously. *)
  if not (needFlowControl || Prefs.read halfduplex) then
    Lwt.ignore_result
      (negociateFlowControlRemote conn () >>= fun needed ->
       if not needed then
         negociateFlowControlLocal conn ()
       else
         Lwt.return true)

(****)

let initConnection in_ch out_ch =
  if not windowsHack then
    ignore(Sys.set_signal Sys.sigpipe Sys.Signal_ignore);
  let conn = setupIO false in_ch out_ch in
  checkHeader
    conn (Bytearray.create 1) 0 (String.length connectionHeader) >>= (fun () ->
  Lwt.ignore_result (receive conn);
  negociateFlowControl conn;
  Lwt.return conn)

let inetAddr host =
  let targetHostEntry = Unix.gethostbyname host in
  targetHostEntry.Unix.h_addr_list.(0)

let buildSocketConnection host port =
  Util.convertUnixErrorsToFatal "canonizeRoot" (fun () ->
    let rec loop = function
      [] ->
        raise (Util.Fatal
                 (Printf.sprintf
                    "Can't find the IP address of the server (%s:%s)" host
		    port))
    | ai::r ->
      (* create a socket to talk to the remote host *)
      let socket = Unix.socket ai.Unix.ai_family ai.Unix.ai_socktype ai.Unix.ai_protocol in
      begin try
        Unix.connect socket ai.Unix.ai_addr;
        initConnection socket socket
      with
        Unix.Unix_error (error, _, reason) ->
          (if error != Unix.EAFNOSUPPORT then
            Util.warn
              (Printf.sprintf
                    "Can't connect to server (%s:%s): %s" host port reason);
           loop r)
    end
    in loop (Unix.getaddrinfo host port [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]))

let buildShellConnection shell host userOpt portOpt rootName termInteract =
  let remoteCmd =
    (if Prefs.read serverCmd="" then Uutil.myName
     else Prefs.read serverCmd)
    ^ (if Prefs.read addversionno then "-" ^ Uutil.myMajorVersion else "")
    ^ " -server" in
  let userArgs =
    match userOpt with
      None -> []
    | Some user -> ["-l"; user] in
  let portArgs =
    match portOpt with
      None -> []
    | Some port -> ["-p"; port] in
  let shellCmd =
    (if shell = "ssh" then
      Prefs.read sshCmd
    else if shell = "rsh" then
      Prefs.read rshCmd
    else
      shell) in
  let shellCmdArgs =
    (if shell = "ssh" then
      Prefs.read sshargs
    else if shell = "rsh" then
      Prefs.read rshargs
    else
      "") in
  let preargs =
      ([shellCmd]@userArgs@portArgs@
       [host]@
       (if shell="ssh" then ["-e none"] else [])@
       [shellCmdArgs;remoteCmd]) in
  (* Split compound arguments at space chars, to make
     create_process happy *)
  let args =
    Safelist.concat
      (Safelist.map (fun s -> Util.splitIntoWords s ' ') preargs) in
  let argsarray = Array.of_list args in
  let (i1,o1) = Unix.pipe() in
  let (i2,o2) = Unix.pipe() in
  (* We need to make sure that there is only one reader and one
     writer by pipe, so that, when one side of the connection
     dies, the other side receives an EOF or a SIGPIPE. *)
  Unix.set_close_on_exec i2;
  Unix.set_close_on_exec o1;
  (* We add CYGWIN=binmode to the environment before calling
     ssh because the cygwin implementation on Windows sometimes
     puts the pipe in text mode (which does end of line
     translation).  Specifically, if unison is invoked from
     a DOS command prompt or other non-cygwin context, the pipe
     goes into text mode; this does not happen if unison is
     invoked from cygwin's bash.  By setting CYGWIN=binmode
     we force the pipe to remain in binary mode. *)
  System.putenv "CYGWIN" "binmode";
  debug (fun ()-> Util.msg "Shell connection: %s (%s)\n"
           shellCmd (String.concat ", " args));
  let term =
    Util.convertUnixErrorsToFatal "starting shell connection" (fun () ->
    match termInteract with
      None ->
        ignore (System.create_process shellCmd argsarray i1 o2 Unix.stderr);
        None
    | Some callBack ->
        fst (Terminal.create_session shellCmd argsarray i1 o2 Unix.stderr))
  in
  Unix.close i1; Unix.close o2;
  begin match term, termInteract with
  | Some fdTerm, Some callBack ->
      Terminal.handlePasswordRequests fdTerm (callBack rootName)
  | _ ->
      ()
  end;
  initConnection i2 o1

let canonizeLocally s unicode =
  (* We need to select the proper API in order to compute correctly the
     canonical fspath *)
  Fs.setUnicodeEncoding unicode;
  Fspath.canonize s

let canonizeOnServer =
  registerServerCmd "canonizeOnServer"
    (fun _ (s, unicode) ->
       Lwt.return (Os.myCanonicalHostName, canonizeLocally s unicode))

let canonize clroot = (* connection for clroot must have been set up already *)
  match clroot with
    Clroot.ConnectLocal s ->
      (Common.Local, canonizeLocally s (Case.useUnicodeAPI ()))
  | _ ->
      match
        try
          Some (Safelist.assoc clroot !connectedHosts)
        with Not_found ->
          None
      with
        None                -> raise (Util.Fatal "Remote.canonize")
      | Some (h, fspath, _) -> (Common.Remote h, fspath)

let listReplace v l = v :: Safelist.remove_assoc (fst v) l

let rec hostFspath clroot =
  try
    let (_, _, ioServer) = Safelist.assoc clroot !connectedHosts in
    Some (Lwt.return ioServer)
  with Not_found ->
    None

let canonizeRoot rootName clroot termInteract =
  let unicode = Case.useUnicodeAPI () in
  let finish ioServer s =
    (* We need to always compute the fspath as it depends on
       unicode settings *)
    canonizeOnServer ioServer (s, unicode) >>= (fun (host, fspath) ->
    connectedHosts :=
      listReplace (clroot, (host, fspath, ioServer)) !connectedHosts;
    connectionsByHosts := listReplace (host, ioServer) !connectionsByHosts;
    Lwt.return (Common.Remote host,fspath)) in
  match clroot with
    Clroot.ConnectLocal s ->
      Lwt.return (Common.Local, canonizeLocally s unicode)
  | Clroot.ConnectBySocket(host,port,s) ->
      begin match hostFspath clroot with
        Some x -> x
      | None   -> buildSocketConnection host port
      end >>= fun ioServer ->
      finish ioServer s
  | Clroot.ConnectByShell(shell,host,userOpt,portOpt,s) ->
      begin match hostFspath clroot with
        Some x -> x
      | None   -> buildShellConnection
                   shell host userOpt portOpt rootName termInteract
      end >>= fun ioServer ->
      finish ioServer s

(* A new interface, useful for terminal interaction, it should
   eventually replace canonizeRoot and buildShellConnection *)
(* A preconnection is None if there's nothing more to do, and Some if
   terminal interaction might be required (for ssh password) *)
type preconnection =
     (Unix.file_descr
     * Unix.file_descr
     * Unix.file_descr
     * Unix.file_descr
     * string option
     * Unix.file_descr option
     * Clroot.clroot
     * int)
let openConnectionStart clroot =
  match clroot with
    Clroot.ConnectLocal s ->
      None
  | Clroot.ConnectBySocket(host,port,s) ->
      Lwt_unix.run
        (begin match hostFspath clroot with
           Some x -> x
         | None   -> buildSocketConnection host port
         end >>= fun ioServer ->
         (* We need to always compute the fspath as it depends on
            unicode settings *)
         let unicode = Case.useUnicodeAPI () in
         canonizeOnServer ioServer (s, unicode) >>= fun (host, fspath) ->
         connectedHosts :=
           listReplace (clroot, (host, fspath, ioServer)) !connectedHosts;
         connectionsByHosts :=
           listReplace (host, ioServer) !connectionsByHosts;
         Lwt.return ());
      None
  | Clroot.ConnectByShell(shell,host,userOpt,portOpt,s) ->
      match hostFspath clroot with
         Some x ->
           let unicode = Case.useUnicodeAPI () in
           (* We recompute the fspath as it may have changed due to
              unicode settings *)
           Lwt_unix.run
             (x >>= fun ioServer ->
              canonizeOnServer ioServer (s, unicode) >>= fun (host, fspath) ->
              connectedHosts :=
                listReplace (clroot, (host, fspath, ioServer)) !connectedHosts;
              connectionsByHosts :=
                listReplace (host, ioServer) !connectionsByHosts;
              Lwt.return ());
           None
      | None ->
          let remoteCmd =
            (if Prefs.read serverCmd="" then Uutil.myName
             else Prefs.read serverCmd)
            ^ (if Prefs.read addversionno then "-" ^ Uutil.myMajorVersion else "")
            ^ " -server" in
          let userArgs =
            match userOpt with
              None -> []
            | Some user -> ["-l"; user] in
          let portArgs =
            match portOpt with
              None -> []
            | Some port -> ["-p"; port] in
          let shellCmd =
            (if shell = "ssh" then
              Prefs.read sshCmd
            else if shell = "rsh" then
              Prefs.read rshCmd
            else
              shell) in
          let shellCmdArgs = 
            (if shell = "ssh" then
              Prefs.read sshargs
            else if shell = "rsh" then
              Prefs.read rshargs
            else
              "") in
          let preargs =
              ([shellCmd]@userArgs@portArgs@
               [host]@
               (if shell="ssh" then ["-e none"] else [])@
               [shellCmdArgs;remoteCmd]) in
          (* Split compound arguments at space chars, to make
             create_process happy *)
          let args =
            Safelist.concat
              (Safelist.map (fun s -> Util.splitIntoWords s ' ') preargs) in
          let argsarray = Array.of_list args in
          let (i1,o1) = Unix.pipe() in
          let (i2,o2) = Unix.pipe() in
          (* We need to make sure that there is only one reader and one
             writer by pipe, so that, when one side of the connection
             dies, the other side receives an EOF or a SIGPIPE. *)
          Unix.set_close_on_exec i2;
          Unix.set_close_on_exec o1;
          (* We add CYGWIN=binmode to the environment before calling
             ssh because the cygwin implementation on Windows sometimes
             puts the pipe in text mode (which does end of line
             translation).  Specifically, if unison is invoked from
             a DOS command prompt or other non-cygwin context, the pipe
             goes into text mode; this does not happen if unison is
             invoked from cygwin's bash.  By setting CYGWIN=binmode
             we force the pipe to remain in binary mode. *)
          System.putenv "CYGWIN" "binmode";
          debug (fun ()-> Util.msg "Shell connection: %s (%s)\n"
                   shellCmd (String.concat ", " args));
          let (term,pid) =
            Terminal.create_session shellCmd argsarray i1 o2 Unix.stderr in
          (* after terminal interact, remember to close i1 and o2 *)
          Some(i1,i2,o1,o2,s,term,clroot,pid)

let openConnectionPrompt = function
    (i1,i2,o1,o2,s,Some fdTerm,clroot,pid) ->
      let x = Terminal.termInput fdTerm i2 in
      x
  | _ -> None

let openConnectionReply = function
    (i1,i2,o1,o2,s,Some fdTerm,clroot,pid) ->
    (fun response ->
      (* FIX: should loop on write, watch for EINTR, etc. *)
      ignore(Unix.write fdTerm (response ^ "\n") 0 (String.length response + 1)))
  | _ -> (fun _ -> ())

let openConnectionEnd (i1,i2,o1,o2,s,_,clroot,pid) =
      Unix.close i1; Unix.close o2;
      Lwt_unix.run
        (initConnection i2 o1 >>= fun ioServer ->
         let unicode = Case.useUnicodeAPI () in
         canonizeOnServer ioServer (s, unicode) >>= fun (host, fspath) ->
         connectedHosts :=
           listReplace (clroot, (host, fspath, ioServer)) !connectedHosts;
         connectionsByHosts :=
           listReplace (host, ioServer) !connectionsByHosts;
         Lwt.return ())

let openConnectionCancel (i1,i2,o1,o2,s,fdopt,clroot,pid) =
      try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ();
      try Unix.close i1 with Unix.Unix_error _ -> ();
      try Unix.close i2 with Unix.Unix_error _ -> ();
      try Unix.close o1 with Unix.Unix_error _ -> ();
      try Unix.close o2 with Unix.Unix_error _ -> ();
      match fdopt with
       None -> () | Some fd -> (try Unix.close fd with Unix.Unix_error _ -> ())

(****************************************************************************)
(*                     SERVER-MODE COMMAND PROCESSING LOOP                  *)
(****************************************************************************)

let showWarningOnClient =
    (registerServerCmd
       "showWarningOnClient"
       (fun _ str -> Lwt.return (Util.warn str)))

let forwardMsgToClient =
    (registerServerCmd
       "forwardMsgToClient"
       (fun _ str -> (*msg "forwardMsgToClient: %s\n" str; *)
          Lwt.return (Trace.displayMessageLocally str)))

(* This function loops, waits for commands, and passes them to
   the relevant functions. *)
let commandLoop in_ch out_ch =
  Trace.runningasserver := true;
  (* Send header indicating to the client that it has successfully
     connected to the server *)
  let conn = setupIO true in_ch out_ch in
  try
    Lwt_unix.run
      (dump conn [(Bytearray.of_string connectionHeader, 0,
                   String.length connectionHeader)]
         >>= (fun () ->
       (* Set the local warning printer to make an RPC to the client and
          show the warning there; ditto for the message printer *)
       Util.warnPrinter :=
         Some (fun str -> Lwt_unix.run (showWarningOnClient conn str));
       Trace.messageForwarder :=
         Some (fun str -> Lwt_unix.run (forwardMsgToClient conn str));
       receive conn >>=
       Lwt.wait))
(*    debug (fun () -> Util.msg "Should never happen\n") *)
  with Util.Fatal "Lost connection with the server" ->
    debug (fun () -> Util.msg "Connection closed by the client\n")

let killServer =
  Prefs.createBool "killserver" false
    "!kill server when done (even when using sockets)"
    ("When set to \\verb|true|, this flag causes Unison to kill the remote "
     ^ "server process when the synchronization is finished.  This behavior "
     ^ "is the default for \\verb|ssh| connections, so this preference is not "
     ^ "normally needed when running over \\verb|ssh|; it is provided so "
     ^ "that socket-mode servers can be killed off after a single run of "
     ^ "Unison, rather than waiting to accept future connections.  (Some "
     ^ "users prefer to start a remote socket server for each run of Unison, "
     ^ "rather than leaving one running all the time.)")

(* For backward compatibility *)
let _ = Prefs.alias killServer "killServer"

(* Used by the socket mechanism: Create a socket on portNum and wait
   for a request. Each request is processed by commandLoop. When a
   session finishes, the server waits for another request. *)
let waitOnPort hostOpt port =
  Util.convertUnixErrorsToFatal
    "waiting on port"
    (fun () ->
      let host = match hostOpt with
        Some host -> host
      | None -> "" in
      let rec loop = function
        [] -> raise (Util.Fatal
		       (if host = "" then
                        Printf.sprintf "Can't bind socket to port %s" port
                        else
                        Printf.sprintf "Can't bind socket to port %s on host %s" port host))
      | ai::r ->
        (* Open a socket to listen for queries *)
        let socket = Unix.socket ai.Unix.ai_family ai.Unix.ai_socktype
	  ai.Unix.ai_protocol in
	begin try
          (* Allow reuse of local addresses for bind *)
          Unix.setsockopt socket Unix.SO_REUSEADDR true;
          (* Bind the socket to portnum on the local host *)
	  Unix.bind socket ai.Unix.ai_addr;
          (* Start listening, allow up to 1 pending request *)
          Unix.listen socket 1;
	  socket
	with
	  Unix.Unix_error (error, _, reason) ->
            (if error != Unix.EAFNOSUPPORT then
               Util.msg
                 "Can't bind socket to port %s at address [%s]: %s\n"
                 port
                 (match ai.Unix.ai_addr with
                    Unix.ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr
                  | _                        -> assert false)
                 (Unix.error_message error);
	     loop r)
	end in
      let listening = loop (Unix.getaddrinfo host port [ Unix.AI_SOCKTYPE
        Unix.SOCK_STREAM ; Unix.AI_PASSIVE ]) in
      Util.msg "server started\n";
      while
        (* Accept a connection *)
        let (connected,_) = Os.accept listening in
        Unix.setsockopt connected Unix.SO_KEEPALIVE true;
        commandLoop connected connected;
        (* The client has closed its end of the connection *)
        begin try Unix.close connected with Unix.Unix_error _ -> () end;
        not (Prefs.read killServer)
      do () done)

let beAServer () =
  begin try
    let home = System.getenv "HOME" in
    Util.convertUnixErrorsToFatal
      "changing working directory"
      (fun () -> System.chdir (System.fspathFromString home))
  with Not_found ->
    Util.msg
      "Environment variable HOME unbound: \
       executing server in current directory\n"
  end;
  commandLoop Unix.stdin Unix.stdout
