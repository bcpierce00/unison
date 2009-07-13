(* Unison file synchronizer: src/transport.ml *)
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


open Common
open Lwt

let debug = Trace.debug "transport"

(*****************************************************************************)
(*                              MAIN FUNCTIONS                               *)
(*****************************************************************************)

let fileSize uiFrom uiTo =
  match uiFrom, uiTo with
    _, Updates (File (props, ContentsUpdated (_, _, ress)), _) ->
      (Props.length props, Osx.ressLength ress)
  | Updates (_, Previous (`FILE, props, _, ress)),
    (NoUpdates | Updates (File (_, ContentsSame), _)) ->
      (Props.length props, Osx.ressLength ress)
  | _ ->
      assert false

let maxthreads =
  Prefs.createInt "maxthreads" 0
    "!maximum number of simultaneous file transfers"
    ("This preference controls how much concurrency is allowed during \
      the transport phase.  Normally, it should be set reasonably high \
      to maximize performance, but when Unison is used over a \
      low-bandwidth link it may be helpful to set it lower (e.g. \
      to 1) so that Unison doesn't soak up all the available bandwidth. \
      The default is the special value 0, which mean 20 threads \
      when file content streaming is desactivated and 1000 threads \
      when it is activated.")

let actionReg = Lwt_util.make_region 50

(* Logging for a thread: write a message before and a message after the
   execution of the thread. *)
let logLwt (msgBegin: string)
    (t: unit -> 'a Lwt.t)
    (fMsgEnd: 'a -> string)
    : 'a Lwt.t =
  Trace.log msgBegin;
  Lwt.bind (t ()) (fun v ->
    Trace.log (fMsgEnd v);
    Lwt.return v)

(* [logLwtNumbered desc t] provides convenient logging for a thread given a
   description [desc] of the thread [t ()], generate pair of messages of the
   following form in the log:
 *
    [BGN] <desc>
     ...
    [END] <desc>
 **)
let rLogCounter = ref 0
let logLwtNumbered (lwtDescription: string) (lwtShortDescription: string)
    (t: unit -> 'a Lwt.t): 'a Lwt.t =
  let _ = (rLogCounter := (!rLogCounter) + 1; !rLogCounter) in
  let lwtDescription = Util.replacesubstring lwtDescription "\n " "" in 
  logLwt (Printf.sprintf "[BGN] %s\n" lwtDescription) t
    (fun _ ->
      Printf.sprintf "[END] %s\n" lwtShortDescription)

let doAction fromRoot fromPath fromContents toRoot toPath toContents id =
  (* When streaming, we can transfer many file simultaneously:
     as the contents of only one file is transferred in one direction
     at any time, little ressource is consumed this way. *)
  let limit =
    let n = Prefs.read maxthreads in
    if n > 0 then n else
    if Prefs.read Remote.streamingActivated then 1000 else 20
  in
  Lwt_util.resize_region actionReg limit;
  Lwt_util.resize_region Files.copyReg limit;
  Lwt_util.run_in_region actionReg 1 (fun () ->
    if not !Trace.sendLogMsgsToStderr then
      Trace.statusDetail (Path.toString toPath);
    Remote.Thread.unwindProtect (fun () ->
      match fromContents, toContents with
          {typ = `ABSENT}, {ui = uiTo} ->
             logLwtNumbered
               ("Deleting " ^ Path.toString toPath ^
                "\n  from "^ root2string toRoot)
               ("Deleting " ^ Path.toString toPath)
               (fun () -> Files.delete fromRoot fromPath toRoot toPath uiTo)
        (* No need to transfer the whole directory/file if there were only
           property modifications on one side.  (And actually, it would be
           incorrect to transfer a directory in this case.) *)
        | {status= `Unchanged | `PropsChanged; desc= fromProps; ui= uiFrom},
          {status= `Unchanged | `PropsChanged; desc= toProps; ui = uiTo} ->
            logLwtNumbered
              ("Copying properties for " ^ Path.toString toPath
               ^ "\n  from " ^ root2string fromRoot ^ "\n  to " ^
               root2string toRoot)
              ("Copying properties for " ^ Path.toString toPath)
              (fun () ->
                Files.setProp
                  fromRoot fromPath toRoot toPath fromProps toProps uiFrom uiTo)
        | {typ = `FILE; ui = uiFrom}, {typ = `FILE; ui = uiTo} ->
            logLwtNumbered
              ("Updating file " ^ Path.toString toPath ^ "\n  from " ^
               root2string fromRoot ^ "\n  to " ^
               root2string toRoot)
              ("Updating file " ^ Path.toString toPath)
              (fun () ->
                Files.copy (`Update (fileSize uiFrom uiTo))
                  fromRoot fromPath uiFrom toRoot toPath uiTo id)
        | {ui = uiFrom}, {ui = uiTo} ->
            logLwtNumbered
              ("Copying " ^ Path.toString toPath ^ "\n  from " ^
               root2string fromRoot ^ "\n  to " ^
               root2string toRoot)
              ("Copying " ^ Path.toString toPath)
              (fun () ->
                 Files.copy `Copy
                   fromRoot fromPath uiFrom toRoot toPath uiTo id))
      (fun e -> Trace.log
          (Printf.sprintf
             "Failed: %s\n" (Util.printException e));
        return ()))

let propagate root1 root2 reconItem id showMergeFn =
  let path = reconItem.path1 in
  match reconItem.replicas with
    Problem p ->
      Trace.log (Printf.sprintf "[ERROR] Skipping %s\n  %s\n"
                   (Path.toString path) p);
      return ()
  | Different {rc1 = rc1; rc2 = rc2; direction = dir} ->
      match dir with
        Conflict ->
          Trace.log (Printf.sprintf "[CONFLICT] Skipping %s\n"
                       (Path.toString path));
          return ()
      | Replica1ToReplica2 ->
          doAction root1 reconItem.path1 rc1 root2 reconItem.path2 rc2 id
      | Replica2ToReplica1 ->
          doAction root2 reconItem.path2 rc2 root1 reconItem.path1 rc1 id
      | Merge ->
          if rc1.typ <> `FILE || rc2.typ <> `FILE then
            raise (Util.Transient "Can only merge two existing files");
          Files.merge
            root1 reconItem.path1 rc1.ui root2 reconItem.path2 rc2.ui id
            showMergeFn;
          return ()

let transportItem reconItem id showMergeFn =
  let (root1,root2) = Globals.roots() in
  propagate root1 root2 reconItem id showMergeFn

(* ---------------------------------------------------------------------- *)

let logStart () =
  Abort.reset ();
  let t = Unix.gettimeofday () in
  let tm = Util.localtime t in
  let m =
    Printf.sprintf
      "%s%s started propagating changes at %02d:%02d:%02d.%02d on %02d %s %04d\n"
      (if Prefs.read Trace.terse || Prefs.read Globals.batch then "" else "\n\n")
      (String.uppercase Uutil.myNameAndVersion)
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      (min 99 (truncate (mod_float t 1. *. 100.)))
      tm.Unix.tm_mday (Util.monthname tm.Unix.tm_mon)
      (tm.Unix.tm_year+1900) in
  Trace.logverbose m

let logFinish () =
  let t = Unix.gettimeofday () in
  let tm = Util.localtime t in
  let m =
    Printf.sprintf
      "%s finished propagating changes at %02d:%02d:%02d.%02d on %02d %s %04d\n%s"
      (String.uppercase Uutil.myNameAndVersion)
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      (min 99 (truncate (mod_float t 1. *. 100.)))
      tm.Unix.tm_mday (Util.monthname tm.Unix.tm_mon)
      (tm.Unix.tm_year+1900)
      (if Prefs.read Trace.terse || Prefs.read Globals.batch then "" else "\n\n") in
  Trace.logverbose m
