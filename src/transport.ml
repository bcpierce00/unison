(* Unison file synchronizer: src/transport.ml *)
(* Copyright 1999-2020, Benjamin C. Pierce

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
  | Updates (_, PrevFile (props, _, _, ress)),
    (NoUpdates | Updates (File (_, ContentsSame), _)) ->
      (Props.length props, Osx.ressLength ress)
  | _ ->
      assert false

let maxthreads =
  Prefs.createInt "maxthreads" 0
    ~category:(`Advanced `General)
    "maximum number of simultaneous file transfers"
    ("This preference controls how much concurrency is allowed during \
      the transport phase.  Normally, it should be set reasonably high \
      to maximize performance, but when Unison is used over a \
      low-bandwidth link it may be helpful to set it lower (e.g. \
      to 1) so that Unison doesn't soak up all the available bandwidth. \
      The default is the special value 0, which mean 20 threads \
      when file content streaming is deactivated and 1000 threads \
      when it is activated.")

let maxThreads () =
  let n = Prefs.read maxthreads in
  if n > 0 then n else
  if Prefs.read Remote.streamingActivated then 1000 else 20

let run dispenseTask =
  let runConcurrent limit dispenseTask =
    let avail = ref limit in
    let rec runTask thr =
      Lwt.try_bind thr
        (fun () -> nextTask ())
        (fun _ -> assert false)
        (* It is a programming error for an exception to reach this far. *)
    and nextTask () =
       match dispenseTask () with
       | None -> Lwt.return (incr avail)
       | Some thr -> runTask thr
    in
    let rec fillPool () =
      match dispenseTask () with
      | None -> ()
      | Some thr ->
          decr avail;
          let _ : unit Lwt.t = runTask thr in
          if !avail > 0 then fillPool ()
    in
    fillPool ()
  in
  (* When streaming, we can transfer many file simultaneously:
     as the contents of only one file is transferred in one direction
     at any time, little resource is consumed this way. *)
  let limit = maxThreads () in
  Lwt_util.resize_region !Files.copyReg limit;
  runConcurrent limit dispenseTask

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

let rec moveFallback
      fromRoot fromPath fromContents toRoot toPath toContents notDefault id =
  (* For cases when renaming is not possible, fall back to separate
     copy and delete actions. *)
  match fromContents, toContents with
  | {status = `MovedOut _}, {status = `MovedOut _}
  | {status = `MovedIn _},  {status = `MovedIn _} ->
      (* This is currently not possible, but it may be in future *)
      assert false
  | {status = `MovedOut (fromPath', fromContents', toContents')},
    _ ->
      doAction fromRoot fromPath' fromContents'
               toRoot fromPath' toContents' notDefault id >>= fun () ->
      doAction fromRoot fromPath (Moves.originalRc fromContents)
               toRoot toPath toContents notDefault id
  | _,
    {status = `MovedOut (toPath', toContents', fromContents')} ->
      doAction fromRoot fromPath fromContents
               toRoot toPath (Moves.originalRc toContents) notDefault id
        >>= fun () ->
      doAction fromRoot toPath' fromContents'
               toRoot toPath' toContents' notDefault id
  | {status = `MovedIn (fromPath', fromContents', toContents')},
    _ ->
      doAction fromRoot fromPath (Moves.originalRc fromContents)
               toRoot toPath toContents notDefault id >>= fun () ->
      doAction fromRoot fromPath' fromContents'
               toRoot fromPath' toContents' notDefault id
  | _,
    {status = `MovedIn (toPath', toContents', fromContents')} ->
      doAction fromRoot toPath' fromContents'
               toRoot toPath' toContents' notDefault id >>= fun () ->
      doAction fromRoot fromPath fromContents
               toRoot toPath (Moves.originalRc toContents) notDefault id
  | _, _ ->
      doAction
        fromRoot fromPath fromContents toRoot toPath toContents notDefault id

and tryMove move fallback () =
  Lwt.catch move
    (function
    | Util.Transient "EXDEV" ->
       debug (fun () ->
         Util.msg ("Attempt to rename across filesystems failed. "
           ^^ "Falling back to copy + delete.\n"));
       fallback ()
    | e -> raise e)

and doMove
      fromRoot fromPath fromContents toRoot toPath toContents notDefault id =
  (* There is some code duplication going on between cases 1 & 3 and 2 & 4
     (or even all of the cases, if you push it). It is intentionally left so
     because wrapping your head around extracting the right data from the
     `Move* replicaContents is a bit tricky, so better not make it worse. *)
  match fromContents, toContents with
  | {status = `MovedOut _}, {status = `MovedOut _}
  | {status = `MovedIn _},  {status = `MovedIn _} ->
      (* This is currently not possible, but it may be in future *)
      assert false
  | {typ = `ABSENT}, {status = `MovedOut _} ->
      (* This is currently not possible, but it may be in future *)
      assert false
  | {status = `MovedOut (fromPath', fromContents', toContents'); ui = uiFrom},
    {status = `Unchanged | `PropsChanged; desc = descTo; ui = uiTo} ->
      (* CASE 1 *)
      (* The contents of the move source in the to-replica are unchanged,
         so a move can be propagated instead of a copy.
         Whatever exists on fromPath' in toRoot, will be overwritten. *)
      logLwtNumbered
        ("Moving/renaming file " ^ Path.toString toPath ^ " --> "
         ^ Path.toString fromPath' ^ "\n  on " ^ root2string toRoot)
        ("Moving/renaming file " ^ Path.toString toPath)
        (tryMove
          (fun () ->
            Files.move
              fromRoot
                fromPath uiFrom
                fromPath' fromContents'.ui fromContents'.desc fromContents'.props
              toRoot
                toPath uiTo descTo
                fromPath' toContents'.ui toContents'.props
              notDefault (snd fromContents.size) id)
          (fun () ->
            moveFallback fromRoot fromPath fromContents
              toRoot toPath toContents notDefault id))
  | {status = `Unchanged | `PropsChanged; desc = descFrom; ui = uiFrom; props = propsFrom},
    {status = `MovedOut (toPath', toContents', {typ = `ABSENT}); ui = uiTo; props = propsTo} ->
      (* CASE 2 *)
      (* The move target in the from-replica does not exist and the
         contents of the move source are unchanged, so a revert move
         can be propagated instead of a copy. *)
      logLwtNumbered
        ("Moving/renaming file " ^ Path.toString toPath' ^ " --> "
         ^ Path.toString toPath ^ "\n  on " ^ root2string toRoot)
        ("Moving/renaming file " ^ Path.toString toPath')
        (tryMove
          (fun () ->
            Files.move
              fromRoot
                toPath' (Updates (Absent, New))
                fromPath uiFrom descFrom propsFrom
              toRoot
                toPath' toContents'.ui toContents'.desc
                toPath uiTo propsTo
              notDefault (snd toContents.size) id)
          (fun () ->
            moveFallback fromRoot fromPath fromContents
              toRoot toPath toContents notDefault id))
  | {status = `MovedIn (fromPath', fromContents',
      ({status = `Unchanged | `PropsChanged; _} as toContents'));
      ui = uiFrom; desc = descFrom; props = propsFrom},
    {ui = uiTo; props = propsTo} ->
      (* CASE 3 *)
      (* The contents of the move source in the to-replica are unchanged,
         so a move can be propagated instead of a copy.
         Whatever exists on toPath in toRoot, will be overwritten. *)
      logLwtNumbered
        ("Moving/renaming file " ^ Path.toString fromPath' ^ " --> "
         ^ Path.toString toPath ^ "\n  on " ^ root2string toRoot)
        ("Moving/renaming file " ^ Path.toString fromPath')
        (tryMove
          (fun () ->
            Files.move
              fromRoot
                fromPath' fromContents'.ui
                fromPath uiFrom descFrom propsFrom
              toRoot
                fromPath' toContents'.ui toContents'.desc
                toPath uiTo propsTo
              notDefault (snd fromContents.size) id)
          (fun () ->
            moveFallback fromRoot fromPath fromContents
              toRoot toPath toContents notDefault id))
  | {typ = `ABSENT; ui = uiFrom; props = propsFrom},
    {status = `MovedIn (toPath', toContents',
      ({status = `Unchanged | `PropsChanged; _} as fromContents'));
      ui = uiTo; desc = descTo; props = propsTo} ->
      (* CASE 4 *)
      (* The move target in the from-replica does not exist and the
         contents of the move source are unchanged, so a revert move
         can be propagated instead of a copy. *)
      logLwtNumbered
        ("Moving/renaming file " ^ Path.toString toPath ^ " --> "
         ^ Path.toString toPath' ^ "\n  on " ^ root2string toRoot)
        ("Moving/renaming file " ^ Path.toString toPath)
        (tryMove
          (fun () ->
            Files.move
              fromRoot
                fromPath (Updates (Absent, New))
                toPath' fromContents'.ui fromContents'.desc fromContents'.props
              toRoot
                toPath uiTo descTo
                toPath' toContents'.ui toContents'.props
              notDefault (snd toContents.size) id)
          (fun () ->
            moveFallback fromRoot fromPath fromContents
              toRoot toPath toContents notDefault id))
  | {status = `MovedOut _ | `MovedIn _}, _
  | _, {status = `MovedOut _ | `MovedIn _} ->
      (* Fallback to copy + delete *)
      moveFallback fromRoot fromPath fromContents
        toRoot toPath toContents notDefault id
  | _ -> assert false

and doAction
      fromRoot fromPath fromContents toRoot toPath toContents notDefault id =
  if not !Trace.sendLogMsgsToStderr then
    Trace.statusDetail (Path.toString toPath);
  Remote.Thread.unwindProtect (fun () ->
    match fromContents, toContents with
        | {status = `MovedOut _ | `MovedIn _}, _
        | _, {status = `MovedOut _ | `MovedIn _} ->
            doMove
              fromRoot fromPath fromContents
              toRoot toPath toContents notDefault id
        | {typ = `ABSENT}, {ui = uiTo} ->
           logLwtNumbered
             ("Deleting " ^ Path.toString toPath ^
              "\n  from "^ root2string toRoot)
             ("Deleting " ^ Path.toString toPath)
             (fun () ->
                Files.delete fromRoot fromPath toRoot toPath uiTo notDefault)
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
                fromRoot fromPath uiFrom [] toRoot toPath uiTo []
                notDefault id)
      | {ui = uiFrom; props = propsFrom}, {ui = uiTo; props = propsTo} ->
          logLwtNumbered
            ("Copying " ^ Path.toString toPath ^ "\n  from " ^
             root2string fromRoot ^ "\n  to " ^
             root2string toRoot)
            ("Copying " ^ Path.toString toPath)
            (fun () ->
               Files.copy `Copy
                 fromRoot fromPath uiFrom propsFrom
                 toRoot toPath uiTo propsTo
                 notDefault id))
    (fun e -> Trace.logonly
        (Printf.sprintf
           "Failed [%s]: %s\n" (Path.toString toPath) (Util.printException e));
      return ())

let propagate root1 root2 reconItem id showMergeFn =
  let path = reconItem.path1 in
  match reconItem.replicas with
    Problem p ->
      Trace.log (Printf.sprintf "[ERROR] Skipping %s\n  %s\n"
                   (Path.toString path) p);
      return ()
  | Different
        {rc1 = rc1; rc2 = rc2; direction = dir; default_direction = def} ->
      let notDefault = dir <> def in
      match dir with
        Conflict c ->
          Trace.log (Printf.sprintf "[CONFLICT] Skipping %s\n  %s\n"
                       (Path.toString path) c);
          return ()
      | Replica1ToReplica2 ->
          doAction
            root1 reconItem.path1 rc1 root2 reconItem.path2 rc2 notDefault id
      | Replica2ToReplica1 ->
          doAction
            root2 reconItem.path2 rc2 root1 reconItem.path1 rc1 notDefault id
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

let lastLogStart = ref 0.

let logStart () =
  Abort.reset ();
  let t = Unix.gettimeofday () in
  lastLogStart := t;
  let tm = Util.localtime t in
  let m =
    Printf.sprintf
      "%s%s started propagating changes at %02d:%02d:%02d.%02d on %02d %s %04d\n"
      (if Prefs.read Trace.terse || Prefs.read Globals.batch then "" else "\n\n")
      (String.capitalize_ascii Uutil.myNameAndVersion)
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
      "%s finished propagating changes at %02d:%02d:%02d.%02d on %02d %s %04d, %.3f s\n%s"
      (String.capitalize_ascii Uutil.myNameAndVersion)
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      (min 99 (truncate (mod_float t 1. *. 100.)))
      tm.Unix.tm_mday (Util.monthname tm.Unix.tm_mon)
      (tm.Unix.tm_year+1900)
      ( t -. !lastLogStart )
      (if Prefs.read Trace.terse || Prefs.read Globals.batch then "" else "\n\n") in
  Trace.logverbose m
