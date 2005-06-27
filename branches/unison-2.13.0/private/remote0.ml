(* $I1: Unison file synchronizer: private/remote0.ml $ *)
(* $I2: Last modified by bcpierce on Tue, 16 Nov 1999 22:29:23 -0500 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

open Util
open Os
open Common
open Printf
open Trace

let debug = false        (* Controls low-level tracing of this module *)
let debugverbose = false

type 'a suspension = unit -> 'a

let unfreezeResult f =
  fun a b -> f a b ()

(*************************************************************************)
(*                        STATUS DISPLAY SUPPORT                         *)
(*************************************************************************)

(* Every time a packet it sent or received, we invoke the thunk stored
   in tickProc.  This provides a way for the user interface to redisplay
   the screen if necessary, show some status indicator that reassures
   the user that progress is being made, etc. *)

let tickProc = ref None

let tick() =
  match !tickProc with
    None -> ()
  | Some(p) -> p()

(*************************************************************************)
(*                           LOW-LEVEL IO                                *)
(*************************************************************************)

let chunkSize = 1024   (* BCP: was 4096 -- I made it smaller so that
                          tick gets called more often *)
let buf = String.create chunkSize

let encodeInt m =
  String.set buf 0 (Char.chr ( m         land 0xff));
  String.set buf 1 (Char.chr ((m lsr 8)  land 0xff));
  String.set buf 2 (Char.chr ((m lsr 16) land 0xff));
  String.set buf 3 (Char.chr ((m lsr 24) land 0xff))

let decodeInt () =
  let b0 = Char.code (String.get buf 0) in
  let b1 = Char.code (String.get buf 1) in
  let b2 = Char.code (String.get buf 2) in
  let b3 = Char.code (String.get buf 3) in
  ((b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0)

let rec dump fd buf offset howmuch =
  try
    let numWritten = Unix.write fd buf offset howmuch in
    if debugverbose then
      errmsg "[dump: %s]" (String.escaped (String.sub buf offset numWritten));
    if numWritten<0 then raise(Can'tHappen("Os","dump"));
    if numWritten<>howmuch
    then
      (* Actually this appears to be an error case, should we just
         abandon the write??? *)
      dump fd buf (offset+numWritten) (howmuch-numWritten)
    else ()
  with e -> raise (OsError (sprintf "[dump] exception '%s'"
                              (Printexc.to_string e)))

let sendPacketLength fd len =
  try
    encodeInt len;
    dump fd buf 0 4
  with e -> raise (OsError (sprintf "[sendPacketLength] exception '%s'"
                              (Printexc.to_string e)))

let sendPacket fd message =
  tick();
  try
    let len = String.length message in
    if debug then errmsg "Sending packet length %d\n" len; 
    sendPacketLength fd len;
    dump fd message 0 len;
    if debug then errmsg0 "Sent packet\n"; 
  with e -> raise (OsError (sprintf "[sendPacket] exception '%s'"
                             (Printexc.to_string e)))

exception GrabEOF

let rec grab fd buf offset howmuch =
  if howmuch<=0 then () else
  let numRead = Unix.read fd buf offset howmuch in
  if debugverbose then
    if numRead<0 then errmsg0 "[grab: error]"
    else if numRead=0 then errmsg0 "[grab: EOF]"
    else errmsg "[grab: %s]" (String.escaped(String.sub buf offset numRead));
  if numRead<0 then raise(OsError "read error");
  if numRead=0 then raise GrabEOF;
  if numRead<>howmuch
  then grab fd buf (offset+numRead) (howmuch-numRead);
  ()

let readPacketLength fd =
  let lenBuf = String.create 4 in
  grab fd buf 0 4;
  decodeInt ()

let receivePacket fileDescriptor =
  tick();
  let len = readPacketLength fileDescriptor in
  if debug then errmsg "Receiving packet length %d\n" len; 
  if len<0 then raise (Can'tHappen("Os","receivePacket"));
  let buf = String.create len in
  grab fileDescriptor buf 0 len;
  if debug then errmsg0 "Received packet\n"; 
  buf

let rec readWrite source target n doTicks =
  if doTicks then tick();
  if n<=0 then () else 
    begin
      let charsToRead = min chunkSize n in
      grab source buf 0 charsToRead;
      dump target buf 0 charsToRead;
      readWrite source target (n-charsToRead) doTicks
    end

let sendString fd string length =
  sendPacketLength fd length;
  dump fd string 0 length

(* Receive a packet from inFd into filename *) 
let receivePacketIntoFile inFd filename =
  try
    let outFd = Unix.openfile filename
                  [Unix.O_RDWR;Unix.O_CREAT;Unix.O_TRUNC] 0o666 in
    let len = readPacketLength inFd in
    if debug then 
      errmsg2 "Receiving packet, length %d, into file %s\n" len filename;
    readWrite inFd outFd len true;
    Unix.close outFd
  with e -> raise (OsError (sprintf "[receivePacketIntoFile] exception '%s'"
                              (Printexc.to_string e)))

(* Send packet from (fspath, path) to outFd *)
let sendPacketFromFile outFd fspath path =
  try
    let fileName = file2localString fspath path in
    let len = lengthOf fspath path in
    let inFd = Unix.openfile fileName [Unix.O_RDONLY] 0o444 in
    if debug then 
      errmsg2 "Sending packet, length %d, from file %s\n" len fileName;
    sendPacketLength outFd len;
    readWrite inFd outFd len true;
    Unix.close inFd
  with e -> raise (OsError (sprintf "[sendPacketFromFile] exception '%s'"
                              (Printexc.to_string e)))

(*****************************************************************************)
(*                              MARSHALING                                   *)
(*****************************************************************************)

exception MarshalError of string

type tag = string

type marshalResult =
    Buffer of int
  | TempoFile of int

let temporaryMarshalFile = fspath2localString (fileInUnisonDir "marshal")
let temporaryMarshalFspath = localString2fspath temporaryMarshalFile

type 'a marshalFunction = 'a -> marshalResult
type 'a unmarshalFunction =  string -> Unix.file_descr -> 'a
type 'a marshalingFunctions = 'a marshalFunction * 'a unmarshalFunction

let registeredSet = ref StringSet.empty

let marshalBufferSize = 4000
let marshalBuffer = String.create marshalBufferSize

let registerTag string =
  if StringSet.mem string !registeredSet then
    raise (MarshalError (sprintf "tag %s is already registered" string))
  else 
    registeredSet := StringSet.add string !registeredSet;
  string

let safeMarshalToFile tag data writeFile =
  let writeChannel = open_out_bin writeFile in
  output_string writeChannel tag;
  output_string writeChannel "\n";
  Marshal.to_channel writeChannel data [Marshal.No_sharing];
  close_out writeChannel 

let safeUnmarshalFromFile tag readFile : 'a =
  let readChannel = open_in_bin readFile in 
  let string = input_line readChannel in
  if compare tag string = 0 then 
    (let result = (Marshal.from_channel readChannel : 'a) in
    close_in readChannel;
    result)
  else (close_in readChannel;
        raise (MarshalError
                 (sprintf "[safeUnmarshalFromFile] expected %s but got %s"
                    tag string)))

(* Store a datastructure into the file (fspath, path) *)
let storeInFile tag data fspath path =
  try
    let writeFile = tempFspath fspath path in
    safeMarshalToFile tag data (fspath2localString writeFile);
    delete fspath path;
    rename writeFile emptypath fspath path
  with e -> raise (MarshalError (sprintf "[storeInFile] exception '%s'"
                                   (Printexc.to_string e)))

(* Load a datastructure from the file (fspath, path) *)
let loadFromFile tag fspath path : 'a =
  try
    let readFile = file2localString fspath path in
    safeUnmarshalFromFile tag readFile
  with e -> raise (MarshalError (sprintf "[storeInFile] exception '%s'"
                                   (Printexc.to_string e)))

let printTag tag =
  let length = String.length tag in
  for i= 0 to (length-1) do
    marshalBuffer.[i] <- tag.[i]
  done;
  length 

let showPackets =
  Prefs.createBoolPref "showpackets" false
    "*show network packets sent and received"

let safeMarshal tag fileName data =
  let pos = printTag tag in
  try
    let nbWritten = Marshal.to_buffer marshalBuffer pos 
        (marshalBufferSize - pos) (Some data) [Marshal.No_sharing] in
    Buffer (nbWritten + pos);
  with Failure _ ->
    safeMarshalToFile tag data fileName;
    let nbWritten = Marshal.to_buffer marshalBuffer pos 
        (marshalBufferSize - pos) None [Marshal.No_sharing] in
    TempoFile (nbWritten + pos)

let safeUnmarshal tag fileName fd string : 'a =
  try
    let length = String.length tag in
    let identifier = String.sub string 0 length in
    if compare identifier tag = 0 then
      let result = (Marshal.from_string string length : 'a option) in
      match result with 
        Some data -> data
      | None ->
          (if !showPackets then
            Trace.message (sprintf "[receiving file]");
            receivePacketIntoFile fd temporaryMarshalFile;
            Trace.message (sprintf "[received %d]"
                             (Unix.stat temporaryMarshalFile).Unix.st_size);
           safeUnmarshalFromFile tag fileName : 'a)
    else raise (MarshalError (sprintf "[safeUnmarshal] expected %s but got %s"
                                tag identifier))
  with Invalid_argument _ ->
    raise (MarshalError "[safeMarshal] caught invalidArgument")

let sendMarshal fd = function
   Buffer length ->
     if !showPackets then Trace.message (sprintf "[send:%d]" length);
     sendString fd marshalBuffer length
 | TempoFile length -> 
     if !showPackets then
       Trace.message (sprintf "[send file:%d]" length);
     sendString fd marshalBuffer length;
     sendPacketFromFile fd 
       (localString2fspath temporaryMarshalFile) emptypath;
     if !showPackets then
       Trace.message (sprintf "[sent]")

let fileTag = registerTag "marshalFile"

(* Very useful functions returning a pair of secured and performant marshaling
   functions for a given datatype. *)
let giveMarshalFunctions string : 'a marshalingFunctions  =
  let tag = registerTag string in
  let marshal (data : 'a) = safeMarshal tag temporaryMarshalFile data in
  let unmarshal string inFd =
    (safeUnmarshal tag temporaryMarshalFile inFd string : 'a) in
  (marshal, unmarshal)

(*****************************************************************************)
(*                              SERVER SETUP                                 *)
(*****************************************************************************)

(* This section should be revised when we change the datatype of
   hosts.  Currently all hosts must be contacted on the same protocol;
   this works because we can contact at most one host, since we either
   have one remote and one local replica, or two local replicas.  When
   the new syntax for replicas (uri.ml) is fully integrated, each host
   will come with its protocol and we will get rid of connectBySocket,
   rsh, and rshlogin.  *)

(* If this is None, then connections to remote hosts should be through
   a remote shell.  If it is Some i then connections should use port i. *)
let connectBySocket = ref None 
let rsh = ref "ssh" (* or rsh *)
let rshlogin = ref "" (* default user name *)

let rshargs =
  Prefs.createStringPref "rshargs" "" 
    "other arguments (if any) for remote shell command"

let serverCmd =
  Prefs.createStringPref "servercmd" ""
    ("name of " ^ myName ^ " executable on remote server")

(* Tries to connect to the remote server and returns the filedescriptor of
   the socket. *)
let startBySocket host portnum =
  let targetInetAddr =
    try
      let targetHostEntry = Unix.gethostbyname host in
      Array.get (targetHostEntry.Unix.h_addr_list) 0
    with _ -> raise(OsError(sprintf "can't get inet_addr for '%s'"
                              host)) in
  (* create a socket to talk to the remote host *)
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.handle_unix_error
    (fun () ->
      Unix.connect
        socket (Unix.ADDR_INET(targetInetAddr,portnum)))
    ();
  (socket,socket,None) (* None means this is a socket connection and
                          not an ssh connection *)

(* List containing the connected hosts and the file descriptors of
   the communication. *)
(* FIX: The list really should be indexed by root
   (host name [+ user name] [+ socket]) *)
let hostConnected = ref []

(* Asks for the Read/Write file descriptors of the communication and 
   creates a new connection if needed. *)
let hostConnection host =
  try List.assoc host !hostConnected
  with Not_found ->
    let inAndOut =
      begin
        match !connectBySocket with
          None -> 
            let remoteCmd = (if !serverCmd="" then myName else !serverCmd)
              ^ " -server" in
            let args =
              if !rshlogin="" then
                [| !rsh; host; !rshargs; remoteCmd |]
              else 
                [| !rsh; "-l"; !rshlogin; host; !rshargs; remoteCmd |]
            in            
            let (i1,o1) = Unix.pipe() in
            let (i2,o2) = Unix.pipe() in
            let pid = try
              Unix.create_process !rsh args i1 o2 Unix.stderr;
            with _ -> raise (OsError ("Server can't start")) in 
            (i2, o1, Some(pid))
        | Some portnum -> startBySocket host portnum
      end in
    let (fromChild, toChild, _) = inAndOut in
    hostConnected := (host, inAndOut)::(!hostConnected);
    inAndOut

(*****************************************************************************)
(*                         REMOTE FUNCTION INVOCATION                        *)
(*****************************************************************************)

(* invoke0:
      string
   -> fd
   -> fd
   -> (fd -> fd -> unit)
   -> (string -> fd -> 'a)
   -> (unit -> 'a)
*)
let invoke0 command source target send postproc =
  sendPacket target command;
  send source target;
  (* Return a future for getting the result *)
  (fun () -> 
     if Sys.file_exists temporaryMarshalFile then
       delete temporaryMarshalFspath emptypath;
     let answer = receivePacket source in
     if !showPackets then
       Trace.message (sprintf "[received:%d]" (String.length answer));
     postproc answer source)
  
(* invokeManual sends a request to the remote host using the send
   function to pass the arguments.  It returns a future for reading
   and decoding the answer. *)
let invokeManual command host send postproc =
  let source, target, _ = hostConnection host in
  invoke0 command source target send postproc

(****************************************************************************)
(*                      REGISTRATION OF SERVER FUNCTIONS                    *)
(****************************************************************************)

type servercmd = string -> Unix.file_descr -> Unix.file_descr -> marshalResult

let serverCmds = ref (StringMap.empty : servercmd StringMap.t)

let addServerCmd cmdName serverFn =
  (* Check that this command name has not already been bound *)
  try
    let _ = StringMap.find cmdName !serverCmds in
    raise (Can'tHappen("remote","Server command "^cmdName^" bound twice"))
  with Not_found -> ();
  (* Add it to the list of registered server commands *)
  serverCmds := StringMap.add cmdName serverFn !serverCmds

let registerHostCmd cmdName (cmd : 'a -> 'b) =
  (* Create marshaling and unmarshaling functions *)
  let ((marshalArgsFn,unmarshalArgsFn) : 'a marshalingFunctions) =
    giveMarshalFunctions (cmdName^"args") in
  let ((marshalResultFn,unmarshalResultFn) : 'b marshalingFunctions) =
    giveMarshalFunctions (cmdName^"results") in
  (* Create a server function and remember it *)
  let serverFn argsStr inFd outFd = 
    let args = unmarshalArgsFn argsStr inFd in
    let answer = cmd args in
    marshalResultFn answer
  in
  addServerCmd cmdName serverFn;
  (* Create a proxy function *)
  let proxyFn host args =
    invokeManual cmdName host
      (fun source target -> sendMarshal target (marshalArgsFn args))
      unmarshalResultFn
  in
  (* Return a function that runs either the proxy or the local version,
     depending on whether the call is to the local host or a remote one *)
  fun host args ->
    match host with
      "" -> (fun () -> cmd args)
    | _ -> proxyFn host args

let registerRootCmd (cmdName : string) (cmd : (fspath*'a) -> 'b) =
  let r = registerHostCmd cmdName cmd in
  fun root args ->
    match root with
      (Local,fspath) -> r "" (fspath, args) 
    | (Remote host, fspath) -> r host (fspath, args) 

(****************************************************************************)
(*             SPECIAL FUNCTIONS FOR FINDING CANONICAL HOSTNAMES            *)
(****************************************************************************)

let canonicalHostnameOnHost = 
  unfreezeResult
    (registerHostCmd
       "canonicalHostnameOnHost"
       myCanonicalHostName)

let canonicalHostnameAlist = ref []

let canonizeHost host =
  try List.assoc host (!canonicalHostnameAlist)
  with Not_found ->
    let chn = canonicalHostnameOnHost host () in
    canonicalHostnameAlist := (host,chn)::(!canonicalHostnameAlist);
    (try if chn<>host then
      hostConnected :=
         (chn,List.assoc host (!hostConnected))::(!hostConnected)
    with Not_found ->
      raise(Can'tHappen("Remote","canonizeHost")));
    (* FIX: maybe we should check to see if chn is the local host,
       and close the connection *)
    chn
  
let canonizeFspathOnHost = 
  unfreezeResult
    (registerHostCmd
       "canonizeFspathOnHost"
       canonizeFspath)

let ((marshalFspath,unmarshalFspath) : fspath marshalingFunctions) =
  giveMarshalFunctions ("fspath")

let ((marshalHostFspath,unmarshalHostFspath)
       : (string * fspath) marshalingFunctions) =
  giveMarshalFunctions ("hostfspath");;

addServerCmd "canonize"
  (fun argsStr inFd outFd -> 
     let fspath = unmarshalFspath argsStr inFd in
     marshalHostFspath(myCanonicalHostName(),canonizeFspath fspath));;

(* Instead of the above we could do

   registerHostCmd "canonize" (fun fspath -> (myCanonicalHostName(),canonizeFspath fspath))

*)

(* Invoke this with

  invoke0 "canonize" source target
    (fun source target -> sendMarshal target (marshalFspath fspath))
    unmarshalHostFspath
    ();;

  where source and target are determined by canonizeRoot.

*)



let rec canonizeRoot root =
  match root with
    (Local, fspath) ->
      (Local, canonizeFspath fspath)
  | (Remote host, fspath) ->
      let myhost = myCanonicalHostName() in
      if host=myhost then canonizeRoot(Local, fspath) else
      let host' = canonizeHost host in
      if host'=myhost then canonizeRoot(Local, fspath) else
      (Remote host', canonizeFspathOnHost host fspath)


(****************************************************************************)
(*                 SPECIAL FUNCTIONS FOR TRANSFERRING FILES                 *)
(****************************************************************************)

type where = fspath * path

let ((marshalWhere,unmarshalWhere) : where marshalingFunctions) =
  giveMarshalFunctions ("where");;

let marshalConfirmation, unmarshalConfirmation =
  (giveMarshalFunctions "confirmation" : confirmation marshalingFunctions);;

addServerCmd "send-me-a-file"
  (fun argsStr inFd outFd -> 
     let (fspathFrom,pathFrom) = unmarshalWhere argsStr inFd in
     if debug then errmsg2 "SERVER: send-me-a-file(%s,%s)\n"
                      (fspath2localString fspathFrom) (path2localString pathFrom);
     let conf = 
       try
          sendPacketFromFile outFd fspathFrom pathFrom;
          Succeeded
       with OsError err -> Failed err
       in
     marshalConfirmation conf);;

let getFile host fspathFrom pathFrom fspathTo pathTo =
  if debug then errmsg4 "getFile(%s,%s) -> (%s,%s)\n"
                   (fspath2localString fspathFrom) (path2localString pathFrom)
                   (fspath2localString fspathTo) (path2localString pathTo);
  invokeManual "send-me-a-file" host
    (fun source target ->
       sendMarshal target (marshalWhere (fspathFrom, pathFrom));
       receivePacketIntoFile source (file2localString fspathTo pathTo))
    unmarshalConfirmation
    ();;

addServerCmd "here-comes-a-file"
  (fun argsStr inFd outFd -> 
     let (fspathTo,pathTo) = unmarshalWhere argsStr inFd in
     if debug then errmsg2 "SERVER: here-comes-a-file(%s,%s)\n"
                      (fspath2localString fspathTo) (path2localString pathTo);
     let conf = 
       try
          receivePacketIntoFile inFd (file2localString fspathTo pathTo);
          Succeeded
       with OsError err -> Failed err
       in
     marshalConfirmation conf);;

let putFile host fspathFrom pathFrom fspathTo pathTo =
  if debug then errmsg4 "putFile(%s,%s) -> (%s,%s)\n"
                   (fspath2localString fspathFrom) (path2localString pathFrom)
                   (fspath2localString fspathTo) (path2localString pathTo);
  invokeManual "here-comes-a-file" host
    (fun source target ->
       sendMarshal target (marshalWhere (fspathTo, pathTo));
       sendPacketFromFile target fspathFrom pathFrom)
    unmarshalConfirmation
    ()


(****************************************************************************)
(*                     SERVER-MODE COMMAND PROCESSING LOOP                  *)
(****************************************************************************)

(* This function loops, waits for commands, and passes them to
   the relevant functions. *)
let commandLoop inFd outFd =
  while true do
    let next() = receivePacket inFd in
    let cmdName = next() in
    let args = next() in
    let marshalType =
      let cmd =
        try StringMap.find cmdName !serverCmds
        with Not_found ->
          raise (Can'tHappen ("Remote", "server: command '"
                              ^ cmdName ^ "' is unknown!")) in
      cmd args inFd outFd in
    sendMarshal outFd marshalType
  done

(* Used by the socket mechanism: Create a socket on portNum and wait
   for a request. Each request is processed by commandLoop. When a
   session finishes, the server waits for another request. *)
let waitOnPort portnum =
  (* Open a socket to listen for queries *)
  let listening = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (* Allow reuse of local addresses for bind *)
  Unix.setsockopt listening Unix.SO_REUSEADDR true;
  (* Bind the socket to portnum on the local host *)
  Unix.bind listening (Unix.ADDR_INET(Unix.inet_addr_any,portnum));
  (* Start listening, allow up to 1 pending request *)
  Unix.listen listening 1;
  while true do
    (* Accept a connection *)
    let (connected,_) = Unix.accept listening in
    begin
      try commandLoop connected connected;
      with
        Unix.Unix_error(Unix.ECONNRESET,"read",_)
      | GrabEOF ->
        (* Client has closed their end of the connection *)
          ()
      | Unix.Unix_error(error,fname,param) ->
          errmsg3 "Server.waitOnPort: got a unix error (%s,%s,%s)\n"
            (Unix.error_message error) fname param
      | exn ->
          errmsg "Server.waitOnPort: got an uncaught exception (%s)\n"
            (Printexc.to_string exn)
    end;
    try Unix.close connected
    with _ -> ()
  done

(* if positive, kill ssh-mode server after this many minutes *)
let serverTimeoutPeriod = 120  (* = two hours *)

(* This function is experimental.  When SSH is used to create the server
   process and the client is killed, it appears that the server becomes a
   zombie process that uses huge amounts of CPU time and never goes away.
   This is an attempted workaround, which simply aborts the server process
   after it has been running for a while.  Not completely satisfactory,
   but better than nothing. *)
let setupTimeoutProcess() =
  if serverTimeoutPeriod > 0 then begin
    ignore(Unix.alarm (serverTimeoutPeriod * 60));
    Sys.set_signal Sys.sigalrm
         (Sys.Signal_handle
            (fun i -> 
               let user = Sys.getenv "USER" in 
               try 
                 let cmd = "echo 'Subject: Timeout after "
                             ^ (string_of_int serverTimeoutPeriod)
                           ^ " minutes: shutting down unison server "
                             ^ (string_of_int (Unix.getpid()))
                             ^ "' | /bin/mail " ^ user in
                 ignore(Unix.system (cmd))
               with _ -> (); (* ...ignoring failures *)
               Unix.sleep 2;
               exit 0))
  end 

let killServerOnHost = 
  (registerHostCmd
     "kill"
     (fun () ->
        errmsg "Server on host %s terminating\n" (myCanonicalHostName());
        exit 0))

let killServerOnHostVerbose host = 
  try
    (* Note that we ignore the suspension returned by this command,
       in case the server has already terminated or the communication
       link is down.  If the server *is* running, the command will
       be executed even though we do not request the result. *)
    if debug then errmsg "Killing ssh server on %s\n" host;
    ignore(killServerOnHost host ())
  with
    OsError(_) -> ()
  | e -> errmsg "shutDown: error while killing server: \n%s\n"
               (Printexc.to_string e)

let beAServer() =
  setupTimeoutProcess();
  (try Sys.chdir (Sys.getenv "HOME") with Not_found ->
    errmsg0 
      "Environment variable HOME unbound: \
       executing server in current directory");
  try commandLoop Unix.stdin Unix.stdout
  with
    Unix.Unix_error(Unix.ECONNRESET,"read",_)
  | GrabEOF -> (* Client has closed their end of the connection *)
      ()

let killServer =
  Prefs.createBoolPref "killServer" false
    "Kill server when done (even when using sockets)"

(* Kills all the open communication. Useful to do before stopping the
   program (user action or fatal exception raised) *)
let shutDown ()=
  if debug then
    errmsg "Shutting down connections: hostconnected=[%s]\n"
       (concatmap ","
          (fun (h,(_,_,p)) ->
             h ^ (match p with None->"" | Some(pid) -> sprintf ":%d" pid))
          !hostConnected);
  List.iter 
    (fun (host, (fromChild, toChild, sshPid)) ->
       (match sshPid with
         None ->
           if !killServer then
             killServerOnHostVerbose host
       | Some(pid) ->
           (killServerOnHostVerbose host;
            try Unix.kill pid Sys.sigkill with Unix.Unix_error(_,_,_)->()));
       if debug then
         errmsg "Killing ssh subprocess for %s\n" host; 
       (try Unix.close fromChild with Unix.Unix_error(_,_,_)->());
       (try Unix.close toChild with Unix.Unix_error(_,_,_)->()))
    !hostConnected;
  hostConnected := []

