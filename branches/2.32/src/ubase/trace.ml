(* Unison file synchronizer: src/ubase/trace.ml *)
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


(* ---------------------------------------------------------------------- *)
(* Choosing where messages go *)

type trace_printer_choices = [`Stdout | `Stderr | `FormatStdout]

let traceprinter = ref (`Stderr : trace_printer_choices)

let redirect x = (traceprinter := x)

(* ---------------------------------------------------------------------- *)
(* Debugging messages *)

let debugmods =
  Prefs.createStringList "debug"
    "!debug module xxx ('all' -> everything, 'verbose' -> more)" 
    ("This preference is used to make Unison print various sorts of "
     ^ "information about what it is doing internally on the standard "
     ^ "error stream.  It can be used many times, each time with the name "
     ^ "of a module for which debugging information should be printed.  "
     ^ "Possible arguments for \\verb|debug| can be found "
     ^ "by looking for calls to \\verb|Util.debug| in the "
     ^ "sources (using, e.g., \\verb|grep|).  "
     ^ "Setting \\verb|-debug all| causes information from {\\em all} "
     ^ "modules to be printed (this mode of usage is the first one to try, "
     ^ "if you are trying to understand something that Unison seems to be "
     ^ "doing wrong); \\verb|-debug verbose| turns on some additional "
     ^ "debugging output from some modules (e.g., it will show exactly "
     ^ "what bytes are being sent across the network).")

let debugtimes =
  Prefs.createBool "debugtimes"
    false "*annotate debugging messages with timestamps" ""

let runningasserver = ref false

let debugging() = (Prefs.read debugmods) <> []

let enabled modname =
  let m = Prefs.read debugmods in
  let en = 
    m <> [] && (   (* tracing labeled "" is enabled if anything is *)
                   (modname = "")
                || (* '-debug verbose' enables everything *)
                   (Safelist.mem "verbose" m)
                || (* '-debug all+' likewise *)
                   (Safelist.mem "all+" m)
                || (* '-debug all' enables all tracing not marked + *)
                   (Safelist.mem "all" m && not (Util.endswith modname "+"))
                || (* '-debug m' enables m and '-debug m+' enables m+ *)
                   (Safelist.mem modname m)
                || (* '-debug m+' also enables m *)
                   (Safelist.mem (modname ^ "+") m)
               ) in
  en
    
let enable modname onoff =
  let m = Prefs.read debugmods in
  let m' = if onoff then (modname::m) else (Safelist.remove modname m) in
  Prefs.set debugmods m'

let debug modname thunk =
  if enabled modname then begin
    let s = if !runningasserver then "server: " else "" in
    let time =
      if Prefs.read debugtimes then
        let tm = Util.localtime (Util.time()) in
        Printf.sprintf "%02d:%02d:%02d"
          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      else "" in
    if time<>"" || s<>"" || modname<>"" then begin
      let time = if time="" || (s=""&&modname="") then time else time^": " in
      match !traceprinter with
      | `Stdout -> Printf.printf "[%s%s%s] " time s modname
      | `Stderr -> Printf.eprintf "[%s%s%s] " time s modname
      | `FormatStdout -> Format.printf "[%s%s%s] " time s modname
      end;
    thunk();
    flush stderr
  end

(* We set the debugPrinter variable in the Util module so that other modules
   lower down in the module dependency graph (so that they can't just
   import Trace) can also print debugging messages. *)
let _ = Util.debugPrinter := Some(debug)


(* ---------------------------------------------------------------------- *)
(* Logging *)

let logging =
  Prefs.createBool "log" true
    "!record actions in logfile"
    "When this flag is set, Unison will log all changes to the filesystems
     on a file."

let logfile =
  Prefs.createString "logfile"
    (Util.fileInHomeDir "unison.log")
    "!logfile name"
    "By default, logging messages will be appended to the file
     \\verb|unison.log| in your HOME directory.  Set this preference if
     you prefer another file."

let logch = ref None

let rec getLogch() =
  Util.convertUnixErrorsToFatal "getLogch" (fun() ->
  match !logch with
    None ->
      let file = Prefs.read logfile in
      let ch =
        open_out_gen [Open_wronly; Open_append; Open_creat] 0o600 file in
      logch := Some (ch, file);
      ch
  | Some(ch, file) ->
      if Prefs.read logfile = file then ch else begin
        close_out ch;
        logch := None; getLogch ()
      end)

let sendLogMsgsToStderr = ref true

let writeLog s =
  if !sendLogMsgsToStderr then begin
      match !traceprinter with
      | `Stdout -> Printf.printf "%s" s
      | `Stderr -> Util.msg "%s" s
      | `FormatStdout -> Format.printf "%s " s
  end else debug "" (fun() -> 
      match !traceprinter with
      | `Stdout -> Printf.printf "%s" s
      | `Stderr -> Util.msg "%s" s
      | `FormatStdout -> Format.printf "%s " s);
  if Prefs.read logging then begin
    let ch = getLogch() in
    output_string ch s;
    flush ch
  end

(* ---------------------------------------------------------------------- *)
(* Formatting and displaying messages *)

let terse =
  Prefs.createBool "terse" false "suppress status messages"
    ("When this preference is set to {\\tt true}, the user "
     ^ "interface will not print status messages.")

type msgtype = Msg | StatusMajor | StatusMinor | Log
type msg = msgtype * string

let defaultMessageDisplayer s =
  if not (Prefs.read terse) then begin
    let show() = if s<>"" then Util.msg "%s\n" s in
    if enabled "" then debug "" show
    else if not !runningasserver then show()
  end 

let messageDisplayer = ref defaultMessageDisplayer

let defaultStatusFormatter s1 s2 = s1 ^ " " ^ s2

let statusFormatter = ref defaultStatusFormatter

let statusMsgMajor = ref ""
let statusMsgMinor = ref ""

let displayMessageLocally (mt,s) = 
  let display = !messageDisplayer in
  let displayStatus() =
    display (!statusFormatter !statusMsgMajor !statusMsgMinor) in
  match mt with
    Msg -> display s
  | StatusMajor -> statusMsgMajor := s; statusMsgMinor := ""; displayStatus()
  | StatusMinor -> statusMsgMinor := s; displayStatus()
  | Log -> writeLog s

let messageForwarder = ref None

let displayMessage m =
  match !messageForwarder with
    None -> displayMessageLocally m
  | Some(f) -> f m

(* ---------------------------------------------------------------------- *)
(* Convenience functions for displaying various kinds of messages *)

let message s = displayMessage (Msg, s)

let status s =
  displayMessage (StatusMajor, s)

let statusMinor s = displayMessage (StatusMinor, s)
                      
let statusDetail s =
  let ss = if not !runningasserver then s else (Util.padto 30 s) ^ " [server]" in
  displayMessage (StatusMinor, ss)

let log s = displayMessage (Log, s)

let logverbose s =
  let temp = !sendLogMsgsToStderr in
  sendLogMsgsToStderr := !sendLogMsgsToStderr && not (Prefs.read terse);
  displayMessage (Log, s);
  sendLogMsgsToStderr := temp 

(* ---------------------------------------------------------------------- *)
(* Timing *)
    
let printTimers =
  Prefs.createBool "timers" false
    "*print timing information" ""
    
type timer = string * float

let gettime () = Unix.gettimeofday()

let startTimer desc =
  if Prefs.read(printTimers) then
    (message (desc ^ "..."); (desc, gettime()))
  else
    (desc,0.0)

let startTimerQuietly desc =
  if Prefs.read(printTimers) then
    (desc, gettime())
  else
    (desc,0.0)

let showTimer (desc, t1) =
  (* Showing timer values from the server process does not work at the moment:
     it confuses the RPC mechanism *)
  if not !runningasserver then
    if Prefs.read(printTimers) then
      let t2 = gettime() in
      message (Printf.sprintf "%s (%.2f seconds)" desc (t2 -. t1))

