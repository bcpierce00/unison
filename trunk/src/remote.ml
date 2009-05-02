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


(*
XXX
- Check exception handling
- Use Lwt_unix.system for the merge function
   (Unix.open_process_in for diff)
*)

let (>>=) = Lwt.bind

let debug = Trace.debug "remote"
let debugV = Trace.debug "remote+"
let debugE = Trace.debug "remote+"
let debugT = Trace.debug "remote+"

(* BCP: The previous definitions of the last two were like this:
     let debugE = Trace.debug "remote_emit"
     let debugT = Trace.debug "thread"
   But that resulted in huge amounts of output from '-debug all'.
*)

let windowsHack = Sys.os_type <> "Unix"

(****)

let encodeInt m =
  let int_buf = String.create 4 in
  String.set int_buf 0 (Char.chr ( m         land 0xff));
  String.set int_buf 1 (Char.chr ((m lsr 8)  land 0xff));
  String.set int_buf 2 (Char.chr ((m lsr 16) land 0xff));
  String.set int_buf 3 (Char.chr ((m lsr 24) land 0xff));
  int_buf

let decodeInt int_buf =
  let b0 = Char.code (String.get int_buf 0) in
  let b1 = Char.code (String.get int_buf 1) in
  let b2 = Char.code (String.get int_buf 2) in
  let b3 = Char.code (String.get int_buf 3) in
  ((b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0)

(*************************************************************************)
(*                           LOW-LEVEL IO                                *)
(*************************************************************************)

let lost_connection () =
  Lwt.fail (Util.Fatal "Lost connection with the server")

let catch_io_errors th =
  Lwt.catch th
    (fun e ->
       match e with
         Unix.Unix_error(Unix.ECONNRESET, _, _)
       | Unix.Unix_error(Unix.EPIPE, _, _)
         (* Windows may also return the following errors... *)
       | Unix.Unix_error(Unix.EINVAL, _, _) ->
         (* Client has closed its end of the connection *)
           lost_connection ()
       | _ ->
           Lwt.fail e)

(****)

type connection =
  { inputChannel : Unix.file_descr;
    inputBuffer : string;
    mutable inputLength : int;
    outputChannel : Unix.file_descr;
    outputBuffer : string;
    mutable outputLength : int;
    outputQueue : (string * int * int) list Queue.t;
    mutable pendingOutput : bool;
    mutable flowControl : bool;
    mutable canWrite : bool;
    mutable tokens : int;
    mutable reader : unit Lwt.t option }

let receivedBytes = ref 0.
let emittedBytes = ref 0.

let inputBuffer_size = 8192

let fill_inputBuffer conn =
  assert (conn.inputLength = 0);
  catch_io_errors
    (fun () ->
       Lwt_unix.read conn.inputChannel conn.inputBuffer 0 inputBuffer_size
         >>= (fun len ->
       debugV (fun() ->
         if len = 0 then
           Util.msg "grab: EOF\n"
         else
           Util.msg "grab: %s\n"
             (String.escaped (String.sub conn.inputBuffer 0 len)));
       if len = 0 then
         lost_connection ()
       else begin
         receivedBytes := !receivedBytes +. float len;
         conn.inputLength <- len;
         Lwt.return ()
       end))

let rec grab_rec conn s pos len =
  if conn.inputLength = 0 then begin
    fill_inputBuffer conn >>= (fun () ->
    grab_rec conn s pos len)
  end else begin
    let l = min (len - pos) conn.inputLength in
    String.blit conn.inputBuffer 0 s pos l;
    conn.inputLength <- conn.inputLength - l;
    if conn.inputLength > 0 then
      String.blit conn.inputBuffer l conn.inputBuffer 0 conn.inputLength;
    if pos + l < len then
      grab_rec conn s (pos + l) len
    else
      Lwt.return ()
  end

let grab conn s len =
  assert (len > 0);
  assert (String.length s <= len);
  grab_rec conn s 0 len

let peek_without_blocking conn =
  String.sub conn.inputBuffer 0 conn.inputLength

(****)

let outputBuffer_size = 8192

let rec send_output conn =
  catch_io_errors
    (fun () ->
       Lwt_unix.write
         conn.outputChannel conn.outputBuffer 0 conn.outputLength
         >>= (fun len ->
       debugV (fun() ->
         Util.msg "dump: %s\n"
           (String.escaped (String.sub conn.outputBuffer 0 len)));
       emittedBytes := !emittedBytes +. float len;
       conn.outputLength <- conn.outputLength - len;
       if conn.outputLength > 0 then
         String.blit
           conn.outputBuffer len conn.outputBuffer 0 conn.outputLength;
       Lwt.return ()))

let rec fill_buffer_2 conn s pos len =
  if conn.outputLength = outputBuffer_size then
    send_output conn >>= (fun () ->
    fill_buffer_2 conn s pos len)
  else begin
    let l = min (len - pos) (outputBuffer_size - conn.outputLength) in
    String.blit s pos conn.outputBuffer conn.outputLength l;
    conn.outputLength <- conn.outputLength + l;
    if pos + l < len then
      fill_buffer_2 conn s (pos + l) len
    else
      Lwt.return ()
  end

let rec fill_buffer conn l =
  match l with
    (s, pos, len) :: rem ->
      assert (pos >= 0);
      assert (len >= 0);
      assert (pos + len <= String.length s);
      fill_buffer_2 conn s pos len >>= (fun () ->
      fill_buffer conn rem)
  | [] ->
      Lwt.return ()

(*
   Flow-control mechanism (only active under windows).
   Only one side is allowed to send message at any given time.
   Once it has finished sending message, a special message is sent
   meaning that the destination is now allowed to send messages.
   A side is allowed to send any number of messages, but will then
   not be allowed to send before having received the same number of
   messages.
   This way, there can be no dead-lock with both sides trying
   simultaneously to send some messages.  Furthermore, multiple
   messages can still be coalesced.
*)
let needFlowControl = windowsHack

(* Loop until the output buffer is empty *)
let rec flush_buffer conn =
  if conn.tokens <= 0 && conn.canWrite then begin
    assert conn.flowControl;
    conn.canWrite <- false;
    debugE (fun() -> Util.msg "Sending write token\n");
    (* Special message allowing the other side to write *)
    fill_buffer conn [(encodeInt 0, 0, 4)] >>= (fun () ->
    flush_buffer conn) >>= (fun () ->
    if windowsHack then begin
      debugE (fun() -> Util.msg "Restarting reader\n");
      match conn.reader with
        None ->
          ()
      | Some r ->
          conn.reader <- None;
          Lwt.wakeup r ()
    end;
    Lwt.return ())
  end else if conn.outputLength > 0 then
    send_output conn >>= (fun () ->
    flush_buffer conn)
  else begin
    conn.pendingOutput <- false;
    Lwt.return ()
  end

let rec msg_length l =
  match l with
    [] -> 0
  | (s, p, l)::r -> l + msg_length r

(* Send all pending messages *)
let rec dump_rec conn =
  try
    let l = Queue.take conn.outputQueue in
    fill_buffer conn l >>= (fun () ->
    if conn.flowControl then conn.tokens <- conn.tokens - 1;
    debugE (fun () -> Util.msg "Remaining tokens: %d\n" conn.tokens);
    dump_rec conn)
  with Queue.Empty ->
    (* We wait a bit before flushing everything, so that other packets
       send just afterwards can be coalesced *)
    Lwt_unix.yield () >>= (fun () ->
    try
      ignore (Queue.peek conn.outputQueue);
      dump_rec conn
    with Queue.Empty ->
      flush_buffer conn)

(* Start the thread that write all pending messages, if this thread is
   not running at this time *)
let signalSomethingToWrite conn =
  if not conn.canWrite && conn.pendingOutput then
    debugE
      (fun () -> Util.msg "Something to write, but no write token (%d)\n"
                          conn.tokens);
  if conn.pendingOutput = false && conn.canWrite then begin
    conn.pendingOutput <- true;
    Lwt.ignore_result (dump_rec conn)
  end

(* Add a message to the output queue and schedule its emission *)
(* A message is a list of fragments of messages, represented by triplets
   (string, position in string, length) *)
let dump conn l =
  Queue.add l conn.outputQueue;
  signalSomethingToWrite conn;
  Lwt.return ()

(* Invoked when a special message is received from the other side,
   allowing this side to send messages *)
let allowWrites conn =
  if conn.flowControl then begin
    assert (conn.pendingOutput = false);
    assert (not conn.canWrite);
    conn.canWrite <- true;
    debugE (fun () -> Util.msg "Received write token (%d)\n" conn.tokens);
    (* Flush pending messages, if there are any *)
    signalSomethingToWrite conn
  end

(* Invoked when a special message is received from the other side,
   meaning that the other side does not block on write, and that
   therefore there can be no dead-lock. *)
let disableFlowControl conn =
  debugE (fun () -> Util.msg "Flow control disabled\n");
  conn.flowControl <- false;
  conn.canWrite <- true;
  conn.tokens <- 1;
  (* We are allowed to write, so we flush pending messages, if there
     are any *)
  signalSomethingToWrite conn

(****)

(* Initialize the connection *)
let setupIO in_ch out_ch =
  if not windowsHack then begin
    Unix.set_nonblock in_ch;
    Unix.set_nonblock out_ch
  end;
  { inputChannel = in_ch;
    inputBuffer = String.create inputBuffer_size;
    inputLength = 0;
    outputChannel = out_ch;
    outputBuffer = String.create outputBuffer_size;
    outputLength = 0;
    outputQueue = Queue.create ();
    pendingOutput = false;
    flowControl = true;
    canWrite = true;
    tokens = 1;
    reader = None }

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

type tag = string

type 'a marshalFunction =
  'a -> (string * int * int) list -> (string * int * int) list
type 'a unmarshalFunction =  string -> 'a
type 'a marshalingFunctions = 'a marshalFunction * 'a unmarshalFunction

let registeredSet = ref Util.StringSet.empty

let rec first_chars len msg =
  match msg with
    [] ->
      ""
  | (s, p, l) :: rem ->
      if l < len then
        String.sub s p l ^ first_chars (len - l) rem
      else
        String.sub s p len

(* An integer just a little smaller than the maximum representable in 30 bits *)
let hugeint = 1000000000

let safeMarshal marshalPayload tag data rem =
  let (rem', length) = marshalPayload data rem in
  if length > hugeint then  begin
    let start = first_chars (min length 10) rem' in
    let start = if length > 10 then start ^ "..." else start in
    let start = String.escaped start in
    Util.msg "Fatal error in safeMarshal: sending too many (%d) bytes with tag %s and contents [%s]\n" length tag start; 
    raise (Util.Fatal ((Printf.sprintf
             "Message payload too large (%d, %s, [%s]).  \n" length tag start)
             ^ "This is a bug in Unison; if it happens to you in a repeatable way, \n"
             ^ "please post a report on the unison-users mailing list."))
  end;
  let l = String.length tag in
  debugE (fun() ->
            let start = first_chars (min length 10) rem' in
            let start = if length > 10 then start ^ "..." else start in
            let start = String.escaped start in
            Util.msg "send [%s] '%s' %d bytes\n" tag start length);
  ((encodeInt (l + length), 0, 4) :: (tag, 0, l) :: rem')

let safeUnmarshal unmarshalPayload tag buf =
  let taglength = String.length tag in
  let identifier = String.sub buf 0 (min taglength (String.length buf)) in
  if identifier = tag then
    unmarshalPayload buf taglength
  else
    raise (Util.Fatal
             (Printf.sprintf "[safeUnmarshal] expected %s but got %s"
                tag identifier))

let registerTag string =
  if Util.StringSet.mem string !registeredSet then
    raise (Util.Fatal (Printf.sprintf "tag %s is already registered" string))
  else
    registeredSet := Util.StringSet.add string !registeredSet;
  string

let defaultMarshalingFunctions =
  (fun data rem ->
     try
       let s = Marshal.to_string data [Marshal.No_sharing] in
       let l = String.length s in
       ((s, 0, String.length s) :: rem, l)
     with Out_of_memory ->
       raise (Util.Fatal
                "Trying to transfer too much data in one go.\n\
                 If this happens during update detection, try to\n\
                 synchronize smaller pieces of the replica first\n\
                 using the \"path\" directive.")),
  (fun buf pos -> Marshal.from_string buf pos)

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
(*
(* Perhaps the list would be better indexed by root
     (host name [+ user name] [+ socket]) ... *)
let connectedHosts = ref []

(* Gets the Read/Write file descriptors for a host;
   the connection must have been set up by canonizeRoot before calling *)
let hostConnection host =
  try Safelist.assoc host !connectedHosts
  with Not_found ->
    raise(Util.Fatal "hostConnection")
*)

(* connectedHosts is a list of command-line roots, their corresponding
   canonical host names and canonical fspaths, and their connections.
   Local command-line roots are not in the list.
   Although there can only be one remote host per sync, it's possible
   connectedHosts to hold more than one hosts if more than one sync is
   performed.
   It's also possible for there to be two connections open for the
   same canonical root.
*)
let connectedHosts = ref []
let hostConnection host = (* host must be canonical *)
  let rec loop = function
      [] -> raise(Util.Fatal "Remote.hostConnection")
    | (cl,h,fspath,conn)::tl -> if h=host then conn else loop tl in
  loop !connectedHosts

let canonize clroot = (* connection for clroot must have been set up already *)
  match clroot with
    Clroot.ConnectLocal s -> (Common.Local, Fspath.canonize s)
  | _ ->
    let rec loop = function
        [] -> raise(Util.Fatal "Remote.canonize")
      | (cl,h,fspath,conn)::tl ->
        if cl=clroot then (Common.Remote h,fspath) else loop tl in
    loop !connectedHosts


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
  let int_buf = String.create 4 in
  grab conn int_buf 4 >>= (fun () ->
  let length = decodeInt int_buf in
  assert (length >= 0);
  (* Get packet *)
  let buf = String.create length in
  grab conn buf length >>= (fun () ->
  (debugE (fun () ->
             let start =
               if length > 10 then (String.sub buf 0 10) ^ "..."
               else String.sub buf 0 length in
             let start = String.escaped start in
             Util.msg "receive '%s' %d bytes\n" start length);
   Lwt.return buf)))

type servercmd =
  connection -> string ->
  ((string * int * int) list -> (string * int * int) list) Lwt.t
let serverCmds = ref (Util.StringMap.empty : servercmd Util.StringMap.t)

type header =
    NormalResult
  | TransientExn of string
  | FatalExn of string
  | Request of string

let ((marshalHeader, unmarshalHeader) : header marshalingFunctions) =
  makeMarshalingFunctions defaultMarshalingFunctions "rsp"

let processRequest conn id cmdName buf =
  let cmd =
    try Util.StringMap.find cmdName !serverCmds
    with Not_found -> raise (Util.Fatal (cmdName ^ " not registered!"))
  in
  Lwt.try_bind (fun () -> cmd conn buf)
    (fun marshal ->
       debugE (fun () -> Util.msg "Sending result (id: %d)\n" (decodeInt id));
       dump conn ((id, 0, 4) :: marshalHeader NormalResult (marshal [])))
    (function
       Util.Transient s ->
         debugE (fun () ->
           Util.msg "Sending transient exception (id: %d)\n" (decodeInt id));
         dump conn ((id, 0, 4) :: marshalHeader (TransientExn s) [])
     | Util.Fatal s ->
         debugE (fun () ->
           Util.msg "Sending fatal exception (id: %d)\n" (decodeInt id));
         dump conn ((id, 0, 4) :: marshalHeader (FatalExn s) [])
     | e ->
         Lwt.fail e)

(* Message ids *)
type msgId = int
module MsgIdMap = Map.Make (struct type t = msgId let compare = compare end)
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
  (if windowsHack && conn.canWrite then
     let wait = Lwt.wait () in
     assert (conn.reader = None);
     conn.reader <- Some wait;
     wait
   else
     Lwt.return ()) >>= (fun () ->
  debugE (fun () -> Util.msg "Waiting for next message\n");
  (* Get the message ID *)
  let id = String.create 4 in
  grab conn id 4 >>= (fun () ->
  let num_id = decodeInt id in
  if num_id = 0 then begin
    debugE (fun () -> Util.msg "Received the write permission\n");
    allowWrites conn;
    receive conn
  end else begin
    if conn.flowControl then conn.tokens <- conn.tokens + 1;
    debugE
      (fun () -> Util.msg "Message received (id: %d) (tokens: %d)\n"
                          num_id  conn.tokens);
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
    end)
  end))

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
      (encodeInt id, 0, 4) ::
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


(****************************************************************************
                     BUILDING CONNECTIONS TO THE SERVER
 ****************************************************************************)

let connectionHeader = "Unison " ^ Uutil.myMajorVersion ^ "\n"

let rec checkHeader conn prefix buffer pos len =
  if pos = len then
    Lwt.return ()
  else begin
    (grab conn buffer 1 >>= (fun () ->
    if buffer.[0] <> connectionHeader.[pos] then
      let rest = peek_without_blocking conn in
      Lwt.fail
        (Util.Fatal
           ("Received unexpected header from the server:\n \
             expected \""
           ^ String.escaped (* (String.sub connectionHeader 0 (pos + 1)) *)
               connectionHeader
           ^ "\" but received \"" ^ String.escaped (prefix ^ buffer ^ rest) ^ "\", \n"
           ^ "which differs at \"" ^ String.escaped (prefix ^ buffer) ^ "\".\n"
           ^ "This can happen because you have different versions of Unison\n"
           ^ "installed on the client and server machines, or because\n"
           ^ "your connection is failing and somebody is printing an error\n"
           ^ "message, or because your remote login shell is printing\n"
           ^ "something itself before starting Unison."))
    else
      checkHeader conn (prefix ^ buffer) buffer (pos + 1) len))
  end

(****)

(*
   Disable flow control if possible.
   Both hosts must use non-blocking I/O (otherwise a dead-lock is
   possible with ssh).
*)

let negociateFlowControlLocal conn () =
  if not needFlowControl then disableFlowControl conn;
  Lwt.return needFlowControl

let negociateFlowControlRemote =
  registerServerCmd "negociateFlowControl" negociateFlowControlLocal

let negociateFlowControl conn =
  if not needFlowControl then
    negociateFlowControlRemote conn () >>= (fun needed ->
    if not needed then
      negociateFlowControlLocal conn () >>= (fun _ -> Lwt.return ())
    else
      Lwt.return ())
  else
    Lwt.return ()

(****)

let initConnection in_ch out_ch =
  if not windowsHack then
    ignore(Sys.set_signal Sys.sigpipe Sys.Signal_ignore);
  let conn = setupIO in_ch out_ch in
  conn.canWrite <- false;
  checkHeader conn "" " " 0 (String.length connectionHeader) >>= (fun () ->
  Lwt.ignore_result (receive conn);
  negociateFlowControl conn >>= (fun () ->
  Lwt.return conn))

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
  Unix.putenv "CYGWIN" "binmode";
  debug (fun ()-> Util.msg "Shell connection: %s (%s)\n"
           shellCmd (String.concat ", " args));
  let term =
    match termInteract with
      None ->
        ignore (Unix.create_process shellCmd argsarray i1 o2 Unix.stderr);
        None
    | Some callBack ->
        fst (Terminal.create_session shellCmd argsarray i1 o2 Unix.stderr)
  in
  Unix.close i1; Unix.close o2;
  begin match term, termInteract with
  | Some fdTerm, Some callBack ->
      Terminal.handlePasswordRequests fdTerm (callBack rootName)
  | _ ->
      ()
  end;
  initConnection i2 o1

let canonizeOnServer =
  registerServerCmd "canonizeOnServer"
    (fun _ s -> Lwt.return (Os.myCanonicalHostName, Fspath.canonize s))

let canonizeRoot rootName clroot termInteract =
  let finish ioServer s =
    canonizeOnServer ioServer s >>= (fun (host, fspath) ->
    connectedHosts := (clroot,host,fspath,ioServer)::(!connectedHosts);
    Lwt.return (Common.Remote host,fspath)) in
  let rec hostfspath = function
         [] -> None
       | (clroot',host,fspath,_)::tl ->
         if clroot=clroot'
         then Some(Lwt.return(Common.Remote host,fspath))
         else hostfspath tl in
  match clroot with
    Clroot.ConnectLocal s ->
      Lwt.return (Common.Local, Fspath.canonize s)
  | Clroot.ConnectBySocket(host,port,s) ->
      (match hostfspath !connectedHosts with
        Some x -> x
      | None ->
          buildSocketConnection host port >>= (fun ioServer ->
            finish ioServer s))
  | Clroot.ConnectByShell(shell,host,userOpt,portOpt,s) ->
      (match hostfspath !connectedHosts with
        Some x -> x
      | None ->
          buildShellConnection
            shell host userOpt portOpt rootName termInteract >>=
          (fun ioServer -> finish ioServer s))

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
      (* This check isn't foolproof as the host in the clroot might not be canonical *)
      if (Safelist.exists (fun (clroot',_,_,_) -> clroot=clroot') !connectedHosts)
      then None
      else begin
        let ioServer = Lwt_unix.run(buildSocketConnection host port) in
        let (host,fspath) = Lwt_unix.run(canonizeOnServer ioServer s) in
        connectedHosts := (clroot,host,fspath,ioServer)::(!connectedHosts);
        None
      end
  | Clroot.ConnectByShell(shell,host,userOpt,portOpt,s) ->
      if (Safelist.exists (fun (clroot',_,_,_) -> clroot=clroot') !connectedHosts)
      then None
      else begin
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
        Unix.putenv "CYGWIN" "binmode";
        debug (fun ()-> Util.msg "Shell connection: %s (%s)\n"
                 shellCmd (String.concat ", " args));
        let (term,pid) =
          Terminal.create_session shellCmd argsarray i1 o2 Unix.stderr in
        (* after terminal interact, remember to close i1 and o2 *)
        Some(i1,i2,o1,o2,s,term,clroot,pid)
      end

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
      let ioServer = Lwt_unix.run (initConnection i2 o1) in
      let (host,fspath) = Lwt_unix.run(canonizeOnServer ioServer s) in
      connectedHosts := (clroot,host,fspath,ioServer)::(!connectedHosts)

let openConnectionCancel (i1,i2,o1,o2,s,fdopt,clroot,pid) =
      try Unix.kill pid Sys.sigkill with _ -> ();
      try Unix.close i1 with _ -> ();
      try Unix.close i2 with _ -> ();
      try Unix.close o1 with _ -> ();
      try Unix.close o2 with _ -> ();
      match fdopt with None -> () | Some fd -> (try Unix.close fd with _ -> ())

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
  let conn = setupIO in_ch out_ch in
  try
    Lwt_unix.run
      (dump conn [(connectionHeader, 0, String.length connectionHeader)]
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
    Sys.chdir (Sys.getenv "HOME")
  with Not_found ->
    Util.msg
      "Environment variable HOME unbound: \
       executing server in current directory\n"
  end;
  commandLoop Unix.stdin Unix.stdout
