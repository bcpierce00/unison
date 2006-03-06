(* $I1: Unison file synchronizer: src/uitext.ml $ *)
(* $I2: Last modified by bcpierce on Sat, 27 Nov 2004 09:22:40 -0500 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

open Common
open Lwt

module Body : Uicommon.UI = struct
  
let debug = Trace.debug "ui"

let dumbtty =
  Prefs.createBool "dumbtty"
    (match Util.osType with
        `Unix ->
          (try (Unix.getenv "EMACS" <> "") with
           Not_found -> false)
      | _ ->
          true)
    "do not try to change terminal settings in text UI"
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
  Prefs.createBool "silent" false "print nothing (except error messages)"
    ("When this preference is set to {\\tt true}, the textual user "
     ^ "interface will print nothing at all, except in the case of errors.  "
     ^ "Setting \\texttt{silent} to true automatically sets the "
     ^ "\\texttt{batch} preference to {\\tt true}.")

let cbreakMode = ref None

let rawTerminal () =
  match !cbreakMode with
    None -> ()
  | Some state ->
      let newstate =
        { state with Unix.c_icanon = false; Unix.c_echo = false;
          Unix.c_vmin = 1 }
      in
      Unix.tcsetattr Unix.stdin Unix.TCSANOW newstate

let defaultTerminal () =
  match !cbreakMode with
    None       -> ()
  | Some state ->
      Unix.tcsetattr Unix.stdin Unix.TCSANOW state

let restoreTerminal() =
  if Util.osType = `Unix && not (Prefs.read dumbtty) then
    Sys.set_signal Sys.sigcont Sys.Signal_default;
  defaultTerminal ();
  cbreakMode := None

let setupTerminal() =
  if Util.osType = `Unix && not (Prefs.read dumbtty) then
    try
      cbreakMode := Some (Unix.tcgetattr Unix.stdin);
      let suspend _ =
        defaultTerminal ();
        Sys.set_signal Sys.sigtstp Sys.Signal_default;
        Unix.kill (Unix.getpid ()) Sys.sigtstp
      in
      let resume _ =
        Sys.set_signal Sys.sigtstp (Sys.Signal_handle suspend);
        rawTerminal ()
      in
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
  if  !cbreakMode = None then
    let l = input_line stdin in
    if l="" then "" else String.sub l 0 1
  else
    let c = input_char stdin in
    let c = if c='\n' then "" else String.make 1 c in
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
      try find a actions () with Not_found ->
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
      getInput ()
  | Some i -> i)

let alwaysDisplayDetails ri =
  alwaysDisplay ((Uicommon.details2string ri "  ") ^ "\n")

let displayDetails ri =
  if not (Prefs.read silent) then alwaysDisplayDetails ri

let displayri ri =
  let s = Uicommon.reconItem2string Path.empty ri "" ^ "  " in
  let s =
    match ri.replicas with
      Different(_,_,d,def) when !d<>def ->
        let s = Util.replacesubstring s "<-?->" "<=?=>" in
        let s = Util.replacesubstring s "---->" "====>" in
        let s = Util.replacesubstring s "<----" "<====" in
        s
    | _ -> s in
  match ri.replicas with
    Problem _ ->
      alwaysDisplay s
  | Different (_,_,d,_) when !d=Conflict ->
      alwaysDisplay s
  | _ ->
      display s

type proceed = ConfirmBeforeProceeding | ProceedImmediately

let interact rilist =
  let (r1,r2) = Globals.roots() in
  let (host1, host2) = root2hostname r1, root2hostname r2 in
  display ("\n" ^ Uicommon.roots2string() ^ "\n");
  let rec loop prev =
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
                       ^ (Prefs.profilePathname n)
                       ^ " and restart " ^ Uutil.myName ^ "\n") end;
          let nukeIgnoredRis =
            Safelist.filter (fun ri -> not (Globals.shouldIgnore ri.path)) in
          loop (nukeIgnoredRis (ri::prev)) (nukeIgnoredRis ril) in
        (* This should work on most terminals: *)
        let redisplayri() = overwrite (); displayri ri; display "\n" in
        displayri ri;
        match ri.replicas with
          Problem s -> display "\n"; display s; display "\n"; next()
        | Different(rc1,rc2,dir,_) ->
            if Prefs.read Uicommon.auto && !dir<>Conflict then begin
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
                [((if !dir=Conflict && not (Prefs.read Globals.batch)
                     then ["f"]  (* Offer no default behavior if we've got
                                    a conflict and we're in interactive mode *)
                     else ["";"f";" "]),
                  ("follow " ^ Uutil.myName ^ "'s recommendation (if any)"),
                  fun ()->
                    newLine ();
                    if !dir = Conflict && not (Prefs.read Globals.batch)
                    then begin
                      display "No default action [type '?' for help]\n";
                      repeat()
                    end else
                      next());
                 (["I"],
                  ("ignore this path permanently"),
                  (fun () ->
                     ignore (Uicommon.ignorePath ri.path) rest
                       "this path"));
                 (["E"],
                  ("permanently ignore files with this extension"),
                  (fun () ->
                     ignore (Uicommon.ignoreExt ri.path) rest
                       "files with this extension"));
                 (["N"],
                  ("permanently ignore paths ending with this name"),
                  (fun () ->
                     ignore (Uicommon.ignoreName ri.path) rest
                       "files with this name"));
                 (["m"],
                  ("merge the versions"),
                  (fun () ->
                    dir := Merge;
                    redisplayri();
                    next()));
                 (["d"],
                  ("show differences"),
                  (fun () ->
                     newLine ();
                     Uicommon.showDiffs ri
                       (fun title text ->
                          try
                            let pager = Sys.getenv "PAGER" in
                            restoreTerminal ();
                            let out = Unix.open_process_out pager in
                            Printf.fprintf out "\n%s\n\n%s\n\n" title text;
                            let _ = Unix.close_process_out out in
                            setupTerminal ()
                          with Not_found ->
                            Printf.printf "\n%s\n\n%s\n\n" title text)
                       (fun s -> Printf.printf "%s\n" s)
                       Uutil.File.dummy;
                     repeat()));
                 (["x"],
                  ("show details"),
                  (fun () -> display "\n"; displayDetails ri; repeat()));
                 (["l"],
                  ("list all suggested changes"),
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
                     match prev with
                       [] -> repeat()
                     | prevri::prevprev -> loop prevprev (prevri :: ril)));
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
                    dir := Conflict;
                    redisplayri();
                    next()));
                 ([">";"."],
                  ("propagate from " ^ descr),
                  (fun () ->
                    dir := Replica1ToReplica2;
                    redisplayri();
                    next()));
                 (["<";","],
                  ("propagate from " ^ descl),
                  (fun () ->
                    dir := Replica2ToReplica1;
                    redisplayri();
                    next()))
                ]
                (fun () -> displayri ri)
  in
    loop [] rilist
    
let verifyMerge proceed title text =
  Printf.printf "%s\n" text;
  if proceed then
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
      end
      else
	true
    end
  else
    false
      
let doTransport reconItemList =
  let totalBytesToTransfer =
    ref
      (Safelist.fold_left
         (fun l ri -> Uutil.Filesize.add l (Common.riLength ri))
         Uutil.Filesize.zero reconItemList) in
  let totalBytesTransferred = ref Uutil.Filesize.zero in
  let t0 = Unix.gettimeofday () in
  let showProgress _ b _ =
    totalBytesTransferred := Uutil.Filesize.add !totalBytesTransferred b;
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
  let uiWrapper ri f =
    catch f
      (fun e ->
        match e with
          Util.Transient s ->
            let m = "[" ^ (Path.toString ri.path)  ^ "]: " ^ s in
            alwaysDisplay ("Failed " ^ m ^ "\n");
            fFailedPaths := ri.path :: !fFailedPaths;
            return ()
        | _ ->
            fail e) in
  let counter = ref 0 in
  let rec loop ris actions pRiThisRound =
    match ris with
      [] ->
        actions
    | ri :: rest when pRiThisRound ri ->
        loop rest
          (uiWrapper ri
             (fun () -> (* We need different line numbers so that
                           transport operations are aborted independently *)
                        incr counter;
                        Transport.transportItem ri
                          (Uutil.File.ofLine !counter) verifyMerge)
           :: actions)
          pRiThisRound
    | _ :: rest ->
        loop rest actions pRiThisRound
  in
  Lwt_unix.run
    (let actions = loop reconItemList []
        (fun ri -> not (Common.isDeletion ri)) in
    Lwt_util.join actions);
  Lwt_unix.run
    (let actions = loop reconItemList [] Common.isDeletion in
    Lwt_util.join actions);
  Transport.logFinish ();

  Uutil.setProgressPrinter (fun _ _ _ -> ());
  Util.set_infos "";

  (Safelist.rev !fFailedPaths)

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
            : bool * bool * (Path.t list)
              (* anySkipped?, anyFailures?, failingPaths *) =
  let (proceed,newReconItemList) = interact reconItemList in
  let (updatesToDo, skipped) =
    Safelist.fold_left
      (fun (howmany, skipped) ri ->
        if problematic ri then (howmany, skipped + 1)
        else (howmany + 1, skipped))
      (0, 0) newReconItemList in
  let doit() =
    newLine();
    Trace.status "Propagating updates";
    let timer = Trace.startTimer "Transmitting all files" in
    let failedPaths = doTransport newReconItemList in
    let failures = Safelist.length failedPaths in
    Trace.showTimer timer;
    Trace.status "Saving synchronizer state";
    Update.commitUpdates ();
    let trans = updatesToDo - failures in
    let summary =
      Printf.sprintf
       "Synchronization %s  (%d item%s transferred, %d skipped, %d failure%s)"
       (if failures=0 then "complete" else "incomplete")
       trans (if trans=1 then "" else "s")
       skipped
       failures (if failures=1 then "" else "s") in
    Trace.log (summary ^ "\n");
    if skipped>0 then
      Safelist.iter
        (fun ri ->
        if problematic ri then
          alwaysDisplayAndLog
            ("  skipped: " ^ (Path.toString ri.path)))
        newReconItemList;
    if failures>0 then
      Safelist.iter
        (fun p -> alwaysDisplayAndLog ("  failed: " ^ (Path.toString p)))
        failedPaths;
    (skipped > 0, failures > 0, failedPaths)
  in
  if updatesToDo = 0 then
    (display "No updates to propagate\n";
     (skipped > 0, false, []))
  else if proceed=ProceedImmediately then
    doit()
  else begin
    displayWhenInteractive "\nProceed with propagating updates? ";
    selectAction
      (* BCP: I find it counterintuitive that every other prompt except this one
         would expect <CR> as a default.  But I got talked out of offering a default
         here, because of safety considerations (too easy to press <CR> one time
         too many). *)
      (if Prefs.read Globals.batch then Some "y" else None)
      [(["y";"g"],
        "Yes: proceed with updates as selected above",
        doit);
       (["n"],
        "No: go through selections again",
        (fun () -> newLine(); interactAndPropagateChanges reconItemList));
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

let synchronizeOnce() =
  Trace.status "Looking for changes";
  let (reconItemList, anyEqualUpdates, dangerousPaths) =
    Recon.reconcileAll (Update.findUpdates()) in
  if reconItemList = [] then begin
    (if anyEqualUpdates then
      Trace.status ("Nothing to do: replicas have been changed only "
                    ^ "in identical ways since last sync.")
     else
       Trace.status "Nothing to do: replicas have not changed since last sync.");
    (Uicommon.perfectExit, [])
  end else begin
    checkForDangerousPath dangerousPaths;
    let (anySkipped, anyFailures, failedPaths) =
      interactAndPropagateChanges reconItemList in
    let exitStatus = Uicommon.exitCode(anySkipped,anyFailures) in
    (exitStatus, failedPaths)
  end

let synchronizeUntilNoFailures () =
  let initValueOfPathsPreference = Prefs.read Globals.paths in
  let rec loop triesLeft =
    let (exitStatus,failedPaths) = synchronizeOnce() in
    if failedPaths <> [] && triesLeft <> 0 then begin
      loop (triesLeft - 1)
    end else begin
      Prefs.set Globals.paths initValueOfPathsPreference;
      exitStatus
    end in
  loop (Prefs.read Uicommon.retry)

let rec synchronizeUntilDone () =
  let exitStatus = synchronizeUntilNoFailures() in
  if Prefs.read Uicommon.repeat = "" then
    (* Done *)
    exitStatus
  else begin
    (* Do it again *)
    let n = try int_of_string (Prefs.read Uicommon.repeat)
            with Invalid_argument "int_of_string" ->
              assert false (* file watching not yet implemented *) in
    Trace.status (Printf.sprintf "\nSleeping for %d seconds...\n" n);
    Unix.sleep n;
    synchronizeUntilDone ()
  end

let start _ =
  begin try
    (* Just to make sure something is there... *)
    setWarnPrinterForInitialization();
    Uicommon.uiInit
      (fun s -> Util.msg "%s\n%s\n" Uicommon.shortUsageMsg s; exit 1)
      (fun s -> Util.msg "%s" Uicommon.shortUsageMsg; exit 1)
      (fun () -> if not (Prefs.read silent)
                 then Util.msg "Contacting server...\n")
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
    e ->
      restoreTerminal();
      let msg = Uicommon.exn2string e in
      Trace.log (msg ^ "\n");
      if not !Trace.sendLogMsgsToStderr then begin
        alwaysDisplay "\n";
        alwaysDisplay msg;
        alwaysDisplay "\n";
      end;
      exit Uicommon.fatalExit
  end

let defaultUi = Uicommon.Text

end
