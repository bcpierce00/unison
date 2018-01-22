(* Unison file synchronizer: src/uitext.ml *)
(* Copyright 1999-2017, Benjamin C. Pierce

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

module Body : Uicommon.UI = struct

let debug = Trace.debug "ui"

let dumbtty =
  Prefs.createBool "dumbtty"
    (try System.getenv "EMACS" <> "" with Not_found -> false)
    "!do not change terminal settings in text UI"
    ("When set to \\verb|true|, this flag makes the text mode user "
     ^ "interface avoid trying to change any of the terminal settings.  "
     ^ "(Normally, Unison puts the terminal in `raw mode', so that it can "
     ^ "do things like overwriting the current line.) This is useful, for "
     ^ "example, when Unison runs in a shell inside of Emacs.  "
     ^ "\n\n"
     ^ "When \\verb|dumbtty| is set, commands to the user interface need to "
     ^ "be followed by a carriage return before Unison will execute them.  "
     ^ "(When it is off, Unison "
     ^ "recognizes keystrokes as soon as they are typed.)\n\n"
     ^ "This preference has no effect on the graphical user "
     ^ "interface.")

let silent =
  Prefs.createBool "silent" false "print nothing except error messages"
    ("When this preference is set to {\\tt true}, the textual user "
     ^ "interface will print nothing at all, except in the case of errors.  "
     ^ "Setting \\texttt{silent} to true automatically sets the "
     ^ "\\texttt{batch} preference to {\\tt true}.")

let cbreakMode = ref None

let supportSignals = Util.osType = `Unix || Util.isCygwin

let rawTerminal () =
  match !cbreakMode with
    None      -> ()
  | Some funs -> funs.System.rawTerminal ()

let defaultTerminal () =
  match !cbreakMode with
    None      -> ()
  | Some funs -> funs.System.defaultTerminal ()

let restoreTerminal() =
  if supportSignals && not (Prefs.read dumbtty) then
    Sys.set_signal Sys.sigcont Sys.Signal_default;
  defaultTerminal ();
  cbreakMode := None

let setupTerminal() =
  if not (Prefs.read dumbtty) then
    try
      cbreakMode := Some (System.terminalStateFunctions ());
      let suspend _ =
        defaultTerminal ();
        Sys.set_signal Sys.sigtstp Sys.Signal_default;
        Unix.kill (Unix.getpid ()) Sys.sigtstp
      in
      let resume _ =
        if supportSignals then
          Sys.set_signal Sys.sigtstp (Sys.Signal_handle suspend);
        rawTerminal ()
      in
      if supportSignals then
        Sys.set_signal Sys.sigcont (Sys.Signal_handle resume);
      resume ()
    with Unix.Unix_error _ ->
      restoreTerminal ()

let alwaysDisplay message =
  print_string message;
  flush stdout

let alwaysDisplayAndLog message =
(*  alwaysDisplay message;*)
  Trace.log (message ^ "\n")

let display message =
  if not (Prefs.read silent) then alwaysDisplay message

let displayWhenInteractive message =
  if not (Prefs.read Globals.batch) then alwaysDisplay message

let getInput () =
  match !cbreakMode with
    None ->
      let l = input_line stdin in
      if l="" then "" else String.sub l 0 1
  | Some funs ->
      let input_char () =
        (* We cannot used buffered I/Os under Windows, as character
           '\r' is not passed through (probably due to the code that
           turns \r\n into \n) *)
        let s = Bytes.create 1 in
        let n = Unix.read Unix.stdin s 0 1 in
        if n = 0 then raise End_of_file;
        if s.[0] = '\003' then raise Sys.Break;
        s.[0]
      in
      funs.System.startReading ();
      let c = input_char () in
      funs.System.stopReading ();
      let c = if c='\n' || c = '\r' then "" else String.make 1 c in
      display c;
      c

let newLine () =
  if !cbreakMode <> None then display "\n"

let overwrite () =
  if !cbreakMode <> None then display "\r"

let rec selectAction batch actions tryagain =
  let formatname = function
      "" -> "<ret>"
    | " " -> "<spc>"
    | n -> n in
  let summarizeChoices() =
    display "[";
    Safelist.iter
      (fun (names,doc,action) ->
         if (Safelist.nth names 0) = "" then
           display (formatname (Safelist.nth names 1)))
      actions;
    display "] " in
  let tryagainOrLoop() =
    tryagain ();
    selectAction batch actions tryagain in
  let rec find n = function
      [] -> raise Not_found
    | (names,doc,action)::rest ->
        if Safelist.mem n names then action else find n rest
  in
  let doAction a =
    if a="?" then
      (newLine ();
       display "Commands:\n";
       Safelist.iter (fun (names,doc,action) ->
         let n = Util.concatmap " or " formatname names in
         let space = String.make (max 2 (22 - String.length n)) ' ' in
         display ("  " ^ n ^ space ^ doc ^ "\n"))
         actions;
       tryagainOrLoop())
    else
      let action = try Some (find a actions) with Not_found -> None in
      match action with
        Some action ->
          action ()
      | None ->
          newLine ();
          if a="" then
            display ("No default command [type '?' for help]\n")
          else
            display ("Unrecognized command '" ^ String.escaped a
                     ^ "': try again  [type '?' for help]\n");
          tryagainOrLoop()
  in
  doAction (match batch with
    None   ->
      summarizeChoices();
      let handleExn s =
        (* Make sure that the error messages start on their own lines and not
         * after the prompt. *)
        alwaysDisplay "\n";
        raise (Util.Fatal ("Failure reading from the standard input ("^s^")\n"))
      in
      begin try getInput () with
        (* Simply print a slightly more informative message than the exception
         * itself (e.g. "Uncaught unix error: read failed: Resource temporarily
         * unavailable" or "Uncaught exception End_of_file"). *)
          End_of_file -> handleExn "End of file"
        | Unix.Unix_error (err, _, _) -> handleExn (Unix.error_message err)
      end
  | Some i -> i)

let alwaysDisplayErrors prefix l =
  List.iter
    (fun err -> alwaysDisplay (Format.sprintf "%s%s\n" prefix err)) l

let alwaysDisplayDetails ri =
  alwaysDisplay ((Uicommon.details2string ri "  ") ^ "\n");
  match ri.replicas with
    Problem _ ->
      ()
  | Different diff ->
      alwaysDisplayErrors "[root 1]: " diff.errors1;
      alwaysDisplayErrors "[root 2]: " diff.errors2

let displayDetails ri =
  if not (Prefs.read silent) then alwaysDisplayDetails ri

let displayri ri =
  let (r1, action, r2, path) = Uicommon.reconItem2stringList Path.empty ri in
  let forced =
    match ri.replicas with
      Different diff -> diff.direction <> diff.default_direction
    | Problem _      -> false
  in
  let (defaultAction, forcedAction) =
    match action with
      Uicommon.AError      -> ("error", "error")
    | Uicommon.ASkip _     -> ("<-?->", "<=?=>")
    | Uicommon.ALtoR false -> ("---->", "====>")
    | Uicommon.ALtoR true  -> ("--?->", "==?=>")
    | Uicommon.ARtoL false -> ("<----", "<====")
    | Uicommon.ARtoL true  -> ("<-?--", "<=?==")
    | Uicommon.AMerge      -> ("<-M->", "<=M=>")
  in
  let action = if forced then forcedAction else defaultAction in
  let s = Format.sprintf "%s %s %s   %s  " r1 action r2 path in
  match ri.replicas with
    Problem _ ->
      alwaysDisplay s
  | Different {direction = d} when isConflict d ->
      alwaysDisplay s
  | _ ->
      display s

type proceed = ConfirmBeforeProceeding | ProceedImmediately

let interact rilist =
  let (r1,r2) = Globals.roots() in
  let (host1, host2) = root2hostname r1, root2hostname r2 in
  if not (Prefs.read Globals.batch) then display ("\n" ^ Uicommon.roots2string() ^ "\n");
  let rec loop prev =
    let rec previous prev ril =
      match prev with
        ({ replicas = Problem s } as pri)::pril ->
          displayri pri; display "\n"; display s; display "\n";
          previous pril (pri::ril)
      | pri::pril -> loop pril (pri::ril)
      | [] -> loop prev ril in
    function
      [] -> (ConfirmBeforeProceeding, Safelist.rev prev)
    | ri::rest as ril ->
        let next() = loop (ri::prev) rest in
        let repeat() = loop prev ril in
        let ignore pat rest what =
          if !cbreakMode <> None then display "\n";
          display "  ";
          Uicommon.addIgnorePattern pat;
          display ("  Permanently ignoring " ^ what ^ "\n");
          begin match !Prefs.profileName with None -> assert false |
            Some(n) ->
              display ("  To un-ignore, edit "
                       ^ System.fspathToPrintString (Prefs.profilePathname n)
                       ^ " and restart " ^ Uutil.myName ^ "\n") end;
          let nukeIgnoredRis =
            Safelist.filter (fun ri -> not (Globals.shouldIgnore ri.path1)) in
          loop (nukeIgnoredRis (ri::prev)) (nukeIgnoredRis ril) in
        (* This should work on most terminals: *)
        let redisplayri() = overwrite (); displayri ri; display "\n" in
        displayri ri;
        match ri.replicas with
          Problem s -> display "\n"; display s; display "\n"; next()
        | Different ({rc1 = rc1; rc2 = rc2; direction = dir} as diff) ->
            if Prefs.read Uicommon.auto && not (isConflict dir) then begin
              display "\n"; next()
            end else
              let (descr, descl) =
                if host1 = host2 then
                  "left to right", "right to left"
                else
                  "from "^host1^" to "^host2,
                  "from "^host2^" to "^host1
              in
              if Prefs.read Globals.batch then begin
                display "\n";
                if not (Prefs.read Trace.terse) then
                  displayDetails ri
              end;
              selectAction
                (if Prefs.read Globals.batch then Some " " else None)
                [((if (isConflict dir) && not (Prefs.read Globals.batch)
                     then ["f"]  (* Offer no default behavior if we've got
                                    a conflict and we're in interactive mode *)
                     else ["";"f";" "]),
                  ("follow " ^ Uutil.myName ^ "'s recommendation (if any)"),
                  fun ()->
                    newLine ();
                    if (isConflict dir) && not (Prefs.read Globals.batch)
                    then begin
                      display "No default action [type '?' for help]\n";
                      repeat()
                    end else
                      next());
                 (["I"],
                  ("ignore this path permanently"),
                  (fun () ->
                     ignore (Uicommon.ignorePath ri.path1) rest
                       "this path"));
                 (["E"],
                  ("permanently ignore files with this extension"),
                  (fun () ->
                     ignore (Uicommon.ignoreExt ri.path1) rest
                       "files with this extension"));
                 (["N"],
                  ("permanently ignore paths ending with this name"),
                  (fun () ->
                     ignore (Uicommon.ignoreName ri.path1) rest
                       "files with this name"));
                 (["m"],
                  ("merge the versions"),
                  (fun () ->
                    diff.direction <- Merge;
                    redisplayri();
                    next()));
                 (["d"],
                  ("show differences"),
                  (fun () ->
                     newLine ();
                     Uicommon.showDiffs ri
                       (fun title text ->
                          try
                            let pager = System.getenv "PAGER" in
                            restoreTerminal ();
                            let out = System.open_process_out pager in
                            Printf.fprintf out "\n%s\n\n%s\n\n" title text;
                            let _ = System.close_process_out out in
                            setupTerminal ()
                          with Not_found ->
                            Printf.printf "\n%s\n\n%s\n\n" title text)
                       (fun s -> Printf.printf "%s\n" s)
                       Uutil.File.dummy;
                     repeat()));
                 (["x"],
                  ("show details"),
                  (fun () -> display "\n"; displayDetails ri; repeat()));
                 (["L"],
                  ("list all suggested changes tersely"),
                  (fun () -> display "\n";
                     Safelist.iter
                       (fun ri -> displayri ri; display "\n  ")
                       ril;
                     display "\n";
                     repeat()));
                 (["l"],
                  ("list all suggested changes with details"),
                  (fun () -> display "\n";
                     Safelist.iter
                       (fun ri -> displayri ri; display "\n  ";
                                  alwaysDisplayDetails ri)
                       ril;
                     display "\n";
                     repeat()));
                 (["p";"b"],
                  ("go back to previous item"),
                  (fun () ->
                     newLine();
                     previous prev ril));
                 (["g"],
                  ("proceed immediately to propagating changes"),
                  (fun() ->
                     (ProceedImmediately, Safelist.rev_append prev ril)));
                 (["q"],
                  ("exit " ^ Uutil.myName ^ " without propagating any changes"),
                  fun () -> raise Sys.Break);
                 (["/"],
                  ("skip"),
                  (fun () ->
                    if not (isConflict dir) then diff.direction <- Conflict "skip requested";
                    redisplayri();
                    next()));
                 ([">";"."],
                  ("propagate from " ^ descr),
                  (fun () ->
                    diff.direction <- Replica1ToReplica2;
                    redisplayri();
                    next()));
                 (["<";","],
                  ("propagate from " ^ descl),
                  (fun () ->
                    diff.direction <- Replica2ToReplica1;
                    redisplayri();
                    next()))
                ]
                (fun () -> displayri ri)
  in
    loop [] rilist

let verifyMerge title text =
  Printf.printf "%s\n" text;
  if Prefs.read Globals.batch then
    true
  else begin
    if Prefs.read Uicommon.confirmmerge then begin
      display "Commit results of merge? ";
      selectAction
        None   (* Maybe better: (Some "n") *)
        [(["y";"g"],
          "Yes: commit",
          (fun() -> true));
          (["n"],
           "No: leave this file unchanged",
           (fun () -> false));
        ]
        (fun () -> display "Commit results of merge? ")
    end else
      true
  end

type stateItem =
  { mutable ri : reconItem;
    mutable bytesTransferred : Uutil.Filesize.t;
    mutable bytesToTransfer : Uutil.Filesize.t }

let doTransport reconItemList =
  let items =
    Array.map
      (fun ri ->
         {ri = ri;
          bytesTransferred = Uutil.Filesize.zero;
          bytesToTransfer = Common.riLength ri})
      (Array.of_list reconItemList)
  in
  let totalBytesTransferred = ref Uutil.Filesize.zero in
  let totalBytesToTransfer =
    ref
      (Array.fold_left
         (fun s item -> Uutil.Filesize.add item.bytesToTransfer s)
         Uutil.Filesize.zero items)
  in
  let t0 = Unix.gettimeofday () in
  let showProgress i bytes dbg =
    let i = Uutil.File.toLine i in
    let item = items.(i) in
    item.bytesTransferred <- Uutil.Filesize.add item.bytesTransferred bytes;
    totalBytesTransferred := Uutil.Filesize.add !totalBytesTransferred bytes;
    let v =
      (Uutil.Filesize.percentageOfTotalSize
         !totalBytesTransferred !totalBytesToTransfer)
    in
    let t1 = Unix.gettimeofday () in
    let remTime =
      if v <= 0. then "--:--"
      else if v >= 100. then "00:00"
      else
        let t = truncate ((t1 -. t0) *. (100. -. v) /. v +. 0.5) in
        Format.sprintf "%02d:%02d" (t / 60) (t mod 60)
    in
    Util.set_infos
      (Format.sprintf "%s  %s ETA" (Util.percent2string v) remTime)
  in
  if not (Prefs.read Trace.terse) && (Prefs.read Trace.debugmods = []) then
    Uutil.setProgressPrinter showProgress;

  Transport.logStart ();
  let fFailedPaths = ref [] in
  let fPartialPaths = ref [] in
  let uiWrapper i item f =
    Lwt.try_bind f
      (fun () ->
         if partiallyProblematic item.ri && not (problematic item.ri) then
           fPartialPaths := item.ri.path1 :: !fPartialPaths;
         Lwt.return ())
      (fun e ->
        match e with
          Util.Transient s ->
            let rem =
              Uutil.Filesize.sub
                item.bytesToTransfer item.bytesTransferred
            in
            if rem <> Uutil.Filesize.zero then
              showProgress (Uutil.File.ofLine i) rem "done";
            let m = "[" ^ (Path.toString item.ri.path1)  ^ "]: " ^ s in
            alwaysDisplay ("Failed " ^ m ^ "\n");
            fFailedPaths := item.ri.path1 :: !fFailedPaths;
            return ()
        | _ ->
            fail e) in
  let im = Array.length items in
  let rec loop i actions pRiThisRound =
    if i < im then begin
      let item = items.(i) in
      let actions =
        if pRiThisRound item.ri then
          uiWrapper i item
            (fun () -> Transport.transportItem item.ri
                         (Uutil.File.ofLine i) verifyMerge)
          :: actions
        else
          actions
      in
      loop (i + 1) actions pRiThisRound
    end else
      actions
  in
  Lwt_unix.run
    (let actions = loop 0 [] (fun ri -> not (Common.isDeletion ri)) in
     Lwt_util.join actions);
  Lwt_unix.run
    (let actions = loop 0 [] Common.isDeletion in
     Lwt_util.join actions);
  Transport.logFinish ();

  Uutil.setProgressPrinter (fun _ _ _ -> ());
  Util.set_infos "";

  (Safelist.rev !fFailedPaths, Safelist.rev !fPartialPaths)

let setWarnPrinterForInitialization()=
  Util.warnPrinter :=
     Some(fun s ->
            alwaysDisplay "Error: ";
            alwaysDisplay s;
            alwaysDisplay "\n";
            exit Uicommon.fatalExit)

let setWarnPrinter() =
  Util.warnPrinter :=
     Some(fun s ->
            alwaysDisplay "Warning: ";
            alwaysDisplay s;
            if not (Prefs.read Globals.batch) then begin
              display "Press return to continue.";
              selectAction None
                [(["";" ";"y"],
                    ("Continue"),
                    fun()->());
                 (["n";"q";"x"],
                    ("Exit"),
                    fun()->
                      alwaysDisplay "\n";
                      restoreTerminal ();
                      Lwt_unix.run (Update.unlockArchives ());
                      exit Uicommon.fatalExit)]
                (fun()-> display  "Press return to continue.")
            end)

let lastMajor = ref ""

let formatStatus major minor =
  let s =
    if major = !lastMajor then "  " ^ minor
    else major ^ (if minor="" then "" else "\n  " ^ minor)
  in
    lastMajor := major;
    s

let rec interactAndPropagateChanges reconItemList
            : bool * bool * bool * (Path.t list)
              (* anySkipped?, anyPartial?, anyFailures?, failingPaths *) =
  let (proceed,newReconItemList) = interact reconItemList in
  let (updatesToDo, skipped) =
    Safelist.fold_left
      (fun (howmany, skipped) ri ->
        if problematic ri then (howmany, skipped + 1)
        else (howmany + 1, skipped))
      (0, 0) newReconItemList in
  let doit() =
    if not (Prefs.read Globals.batch || Prefs.read Trace.terse) then newLine();
    if not (Prefs.read Trace.terse) then Trace.status "Propagating updates";
    let timer = Trace.startTimer "Transmitting all files" in
    let (failedPaths, partialPaths) = doTransport newReconItemList in
    let failures = Safelist.length failedPaths in
    let partials = Safelist.length partialPaths in
    Trace.showTimer timer;
    if not (Prefs.read Trace.terse) then Trace.status "Saving synchronizer state";
    Update.commitUpdates ();
    let trans = updatesToDo - failures in
    let summary =
      Printf.sprintf
       "Synchronization %s at %s  (%d item%s transferred, %s%d skipped, %d failed)"
       (if failures=0 then "complete" else "incomplete")
       (let tm = Util.localtime (Util.time()) in
        Printf.sprintf "%02d:%02d:%02d"
          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
       trans (if trans=1 then "" else "s")
       (if partials <> 0 then
          Format.sprintf "%d partially transferred, " partials
        else
          "")
       skipped
       failures in
    Trace.log (summary ^ "\n");
    if skipped>0 then
      Safelist.iter
        (fun ri ->
         match ri.replicas with
           Problem r
         | Different {rc1 = _; rc2 = _; direction = Conflict r; default_direction = _} ->
            alwaysDisplayAndLog (Printf.sprintf "  skipped: %s (%s)"
                                                (Path.toString ri.path1) r)
         | _ -> ())
        newReconItemList;
    if partials>0 then
      Safelist.iter
        (fun p ->
           alwaysDisplayAndLog ("  partially transferred: " ^ Path.toString p))
        partialPaths;
    if failures>0 then
      Safelist.iter
        (fun p -> alwaysDisplayAndLog ("  failed: " ^ (Path.toString p)))
        failedPaths;
    (skipped > 0, partials > 0, failures > 0, failedPaths) in
  if not !Update.foundArchives then Update.commitUpdates ();
  if updatesToDo = 0 then begin
    (* BCP (3/09): We need to commit the archives even if there are
       no updates to propagate because some files (in fact, if we've
       just switched to DST on windows, a LOT of files) might have new
       modtimes in the archive. *)
    (* JV (5/09): Don't save the archive in repeat mode as it has some
       costs and its unlikely there is much change to the archives in
       this mode. *)
    if !Update.foundArchives && Prefs.read Uicommon.repeat = "" then
      Update.commitUpdates ();
    display "No updates to propagate\n";
    if skipped > 0 then begin
      let summary =
        Printf.sprintf
          "Synchronization complete at %s  (0 item transferred, %d skipped, 0 failed)"
          (let tm = Util.localtime (Util.time()) in
           Printf.sprintf "%02d:%02d:%02d"
                          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
          skipped in
      Trace.log (summary ^ "\n");
      Safelist.iter
        (fun ri ->
         match ri.replicas with
           Problem r
         | Different {rc1 = _; rc2 = _; direction = Conflict r; default_direction = _} ->
            alwaysDisplayAndLog (Printf.sprintf "  skipped: %s (%s)"
                                                (Path.toString ri.path1) r)
         | _ -> ())
        newReconItemList
      end;
    (skipped > 0, false, false, [])
  end else if proceed=ProceedImmediately then begin
    doit()
  end else begin
    displayWhenInteractive "\nProceed with propagating updates? ";
    selectAction
      (* BCP: I find it counterintuitive that every other prompt except this one
         would expect <CR> as a default.  But I got talked out of offering a
         default here, because of safety considerations (too easy to press
         <CR> one time too many). *)
      (if Prefs.read Globals.batch then Some "y" else None)
      [(["y";"g"],
        "Yes: proceed with updates as selected above",
        doit);
       (["n"],
        "No: go through selections again",
        (fun () ->
           Prefs.set Uicommon.auto false;
           newLine();
           interactAndPropagateChanges reconItemList));
       (["q"],
        ("exit " ^ Uutil.myName ^ " without propagating any changes"),
        fun () -> raise Sys.Break)
     ]
      (fun () -> display "Proceed with propagating updates? ")
  end

let checkForDangerousPath dangerousPaths =
  if Prefs.read Globals.confirmBigDeletes then begin
    if dangerousPaths <> [] then begin
      alwaysDisplayAndLog (Uicommon.dangerousPathMsg dangerousPaths);
      if Prefs.read Globals.batch then begin
          alwaysDisplay "Aborting...\n"; restoreTerminal ();
          exit Uicommon.fatalExit
      end else begin
        displayWhenInteractive "Do you really want to proceed? ";
        selectAction
          None
          [(["y"],
            "Continue",
            (fun() -> ()));
           (["n"; "q"; "x"; ""],
            "Exit",
            (fun () -> alwaysDisplay "\n"; restoreTerminal ();
                       exit Uicommon.fatalExit))]
          (fun () -> display "Do you really want to proceed? ")
      end
    end
  end

let synchronizeOnce ?wantWatcher ?skipRecentFiles pathsOpt =
  let showStatus path =
    if path = "" then Util.set_infos "" else
    let max_len = 70 in
    let mid = (max_len - 3) / 2 in
    let path =
      let l = String.length path in
      if l <= max_len then path else
      String.sub path 0 (max_len - mid - 3) ^ "..." ^
      String.sub path (l - mid) mid
    in
    let c = "-\\|/".[truncate (mod_float (4. *. Unix.gettimeofday ()) 4.)] in
    Util.set_infos (Format.sprintf "%c %s" c path)
  in
  Trace.status "Looking for changes";
  if not (Prefs.read Trace.terse) && (Prefs.read Trace.debugmods = []) then
    Uutil.setUpdateStatusPrinter (Some showStatus);

  debug (fun() -> Util.msg "temp: Globals.paths = %s\n"
           (String.concat " "
              (Safelist.map Path.toString (Prefs.read Globals.paths))));
  let updates = Update.findUpdates ?wantWatcher pathsOpt in

  Uutil.setUpdateStatusPrinter None;
  Util.set_infos "";

  let (reconItemList, anyEqualUpdates, dangerousPaths) =
    Recon.reconcileAll ~allowPartial:true updates in

  if reconItemList = [] then begin
    (if anyEqualUpdates then
      Trace.status ("Nothing to do: replicas have been changed only "
                    ^ "in identical ways since last sync.")
     else
       Trace.status "Nothing to do: replicas have not changed since last sync.");
    (Uicommon.perfectExit, [])
  end else begin
    checkForDangerousPath dangerousPaths;
    let (anySkipped, anyPartial, anyFailures, failedPaths) =
      interactAndPropagateChanges reconItemList in
    let exitStatus = Uicommon.exitCode(anySkipped || anyPartial,anyFailures) in
    (exitStatus, failedPaths)
  end

(* ----------------- Filesystem watching mode ---------------- *)

let watchinterval = 1.    (* Minimal interval between two synchronizations *)
let retrydelay = 5.       (* Minimal delay to retry failed paths *)
let maxdelay = 30. *. 60. (* Maximal delay to retry failed paths *)

module PathMap = Map.Make (Path)

let waitForChangesRoot: Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "waitForChanges"
    (fun (fspath, _) -> Fswatchold.wait (Update.archiveHash fspath))

let waitForChanges t =
  let dt = t -. Unix.gettimeofday () in
  if dt > 0. then begin
    let timeout = if dt <= maxdelay then [Lwt_unix.sleep dt] else [] in
    Lwt_unix.run
      (Globals.allRootsMap (fun r -> Lwt.return (waitForChangesRoot r ()))
         >>= fun l ->
       Lwt.choose (timeout @ l))
  end

let synchronizePathsFromFilesystemWatcher () =
  let rec loop isStart delayInfo =
    let t = Unix.gettimeofday () in
    let (delayedPaths, readyPaths) =
      PathMap.fold
        (fun p (t', _) (delayed, ready) ->
           if t' <= t then (delayed, p :: ready) else (p :: delayed, ready))
        delayInfo ([], [])
    in
    let (exitStatus, failedPaths) =
      synchronizeOnce ~wantWatcher:() ~skipRecentFiles:()
        (if isStart then None else Some (readyPaths, delayedPaths))
    in
    (* After a failure, we retry at once, then use an exponential backoff *)
    let delayInfo =
      Safelist.fold_left
        (fun newDelayInfo p ->
           PathMap.add p
             (try
                let (t', d) = PathMap.find p delayInfo in
                if t' > t then (t', d) else
                let d = max retrydelay (min maxdelay (2. *. d)) in
                (t +. d, d)
              with Not_found ->
                (t, 0.))
             newDelayInfo)
        PathMap.empty
        (Safelist.append delayedPaths failedPaths)
    in
    Lwt_unix.run (Lwt_unix.sleep watchinterval);
    let nextTime =
      PathMap.fold (fun _ (t, d) t' -> min t t') delayInfo 1e20 in
    waitForChanges nextTime;
    loop false delayInfo
  in
  loop true PathMap.empty

(* ----------------- Repetition ---------------- *)

let synchronizeUntilNoFailures repeatMode =
  let rec loop triesLeft pathsOpt =
    let (exitStatus, failedPaths) =
      synchronizeOnce
        ?wantWatcher:(if repeatMode then Some () else None) pathsOpt in
    if failedPaths <> [] && triesLeft <> 0 then begin
      loop (triesLeft - 1) (Some (failedPaths, []))
    end else begin
      exitStatus
    end in
  loop (Prefs.read Uicommon.retry) None

let rec synchronizeUntilDone () =
  let repeatinterval =
    if Prefs.read Uicommon.repeat = "" then -1 else
    try int_of_string (Prefs.read Uicommon.repeat)
    with Failure _ ->
      (* If the 'repeat' pref is not a valid number, switch modes... *)
      if Prefs.read Uicommon.repeat = "watch" then
        synchronizePathsFromFilesystemWatcher()
      else
        raise (Util.Fatal ("Value of 'repeat' preference ("
                           ^Prefs.read Uicommon.repeat
                           ^") should be either a number or 'watch'\n")) in

  let exitStatus = synchronizeUntilNoFailures(repeatinterval >= 0) in
  if repeatinterval < 0 then
    exitStatus
  else begin
    (* Do it again *)
    Trace.status (Printf.sprintf
       "\nSleeping for %d seconds...\n" repeatinterval);
    Unix.sleep repeatinterval;
    synchronizeUntilDone ()
  end

(* ----------------- Startup ---------------- *)

let handleException e =
  restoreTerminal();
  let msg = Uicommon.exn2string e in
  Trace.log (msg ^ "\n");
  if not !Trace.sendLogMsgsToStderr then alwaysDisplay ("\n" ^ msg ^ "\n")

let rec start interface =
  if interface <> Uicommon.Text then
    Util.msg "This Unison binary only provides the text GUI...\n";
  begin try
    (* Just to make sure something is there... *)
    setWarnPrinterForInitialization();
    Uicommon.uiInit
      (fun s -> Util.msg "%s\n%s\n" Uicommon.shortUsageMsg s; exit 1)
      (fun s -> Util.msg "%s" Uicommon.shortUsageMsg; exit 1)
      (fun () -> setWarnPrinter();
                 if Prefs.read silent then Prefs.set Trace.terse true;
                 if not (Prefs.read silent)
                 then Util.msg "%s\n" (Uicommon.contactingServerMsg()))
      (fun () -> Some "default")
      (fun () -> Util.msg "%s" Uicommon.shortUsageMsg; exit 1)
      (fun () -> Util.msg "%s" Uicommon.shortUsageMsg; exit 1)
      None;

    (* Some preference settings imply others... *)
    if Prefs.read silent then begin
      Prefs.set Globals.batch true;
      Prefs.set Trace.terse true;
      Prefs.set dumbtty true;
      Trace.sendLogMsgsToStderr := false;
    end;
    if Prefs.read Uicommon.repeat <> "" then begin
      Prefs.set Globals.batch true;
    end;

    (* Tell OCaml that we want to catch Control-C ourselves, so that
       we get a chance to reset the terminal before exiting *)
    Sys.catch_break true;
    (* Put the terminal in cbreak mode if possible *)
    if not (Prefs.read Globals.batch) then setupTerminal();
    setWarnPrinter();
    Trace.statusFormatter := formatStatus;

    let exitStatus = synchronizeUntilDone() in

    (* Put the terminal back in "sane" mode, if necessary, and quit. *)
    restoreTerminal();
    exit exitStatus

  with
    Sys.Break -> begin
      (* If we've been killed, then die *)
      handleException Sys.Break;
      exit Uicommon.fatalExit
    end
  | e -> begin
      (* If any other bad thing happened and the -repeat preference is
         set, then restart *)
      (* JV: it seems safer to just abort here, as we don't know in which
         state Unison is; for instance, if the connection is lost, there
         is no point in restarting as Unison will currently not attempt to
         establish a new connection. *)
      handleException e;
      if false (*Prefs.read Uicommon.repeat <> ""*) then begin
        Util.msg "Restarting in 10 seconds...\n";
        Unix.sleep 10;
        start interface
      end else
        exit Uicommon.fatalExit
    end
  end

let defaultUi = Uicommon.Text

end
