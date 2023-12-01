(* ML side of a bridge to C for the Mac GUI *)

open Common;;
open Lwt;;

let debug = Trace.debug "startup"

let unisonNonGuiStartup() = begin
  (* If there's no GUI, don't print progress in the GUI *)
  Uutil.setProgressPrinter (fun _ _ _ -> ());
  Main.nonGuiStartup()    (* If this returns the GUI should be started *)
end;;
Callback.register "unisonNonGuiStartup" unisonNonGuiStartup;;

type stateItem = { mutable ri : reconItem;
                   mutable bytesTransferred : Uutil.Filesize.t;
                   mutable bytesToTransfer : Uutil.Filesize.t;
                   mutable whatHappened : Util.confirmation option;
                   mutable statusMessage : string option };;
let theState = ref [| |];;
let unsynchronizedPaths = ref None;;

let unisonDirectory() = Util.unisonDir
;;
Callback.register "unisonDirectory" unisonDirectory;;

(* Global progress indicator, similar to uigtk2.m; *)
external displayGlobalProgress : float -> unit = "displayGlobalProgress";;

let totalBytesToTransfer = ref Uutil.Filesize.zero;;
let totalBytesTransferred = ref Uutil.Filesize.zero;;

let lastFrac = ref 0.;;
let showGlobalProgress b =
  (* Concatenate the new message *)
  totalBytesTransferred := Uutil.Filesize.add !totalBytesTransferred b;
  let v =
    if !totalBytesToTransfer  = Uutil.Filesize.dummy then 0.
    else if !totalBytesToTransfer  = Uutil.Filesize.zero then 100.
    else (Uutil.Filesize.percentageOfTotalSize
       !totalBytesTransferred !totalBytesToTransfer)
  in
  if v = 0. || abs_float (v -. !lastFrac) > 1. then begin
    lastFrac := v;
    displayGlobalProgress v
  end;;

let initGlobalProgress b =
  totalBytesToTransfer := b;
  totalBytesTransferred := Uutil.Filesize.zero;
  displayGlobalProgress 0.;;

(* Defined in Bridge.m, used to redisplay the table
   when the status for a row changes *)
external bridgeThreadWait : int -> unit = "bridgeThreadWait";;

(* Defined in MyController.m, used to redisplay the table
   when the status for a row changes *)
external displayStatus : string -> unit = "displayStatus";;
let displayStatus s = displayStatus (Unicode.protect s);;

(*
        Called to create callback threads which wait on the C side for callbacks.
        (We create three just for good measure...)

        FIXME: the thread created by Thread.create doesn't run even if we yield --
        we have to join.  At that point we actually do get a different pthread, but
        we've caused the calling thread to block (forever).  As a result, this call
        never returns.
*)
let callbackThreadCreate() =
        let tCode () =
                bridgeThreadWait 1;
        in ignore (Thread.create tCode ()); ignore (Thread.create tCode ());
        let tid = Thread.create tCode ()
        in Thread.join tid;
;;
Callback.register "callbackThreadCreate" callbackThreadCreate;;

(* Defined in MyController.m; display the error message and exit *)
external displayFatalError : string -> unit = "fatalError";;

let fatalError message =
  let () =
    try Trace.log (message ^ "\n")
    with Util.Fatal _ -> () in (* Can't allow fatal errors in fatal error handler *)
  displayFatalError message

(* Defined in MyController.m; display the warning and ask whether to
   exit or proceed *)
external displayWarnPanel : string -> bool = "warnPanel";;

let setWarnPrinter() =
  Util.warnPrinter :=
    Some(fun s ->
      Trace.log ("Warning: " ^ s ^ "\n");
      if not (Prefs.read Globals.batch) then begin
        if (displayWarnPanel s) then begin
          Lwt_unix.run (Update.unlockArchives ());
          exit Uicommon.fatalExit
        end
      end)

let doInOtherThread f =
  Thread.create
    (fun () ->
       try
         f ()
       with
         Util.Transient s | Util.Fatal s -> fatalError s
       | exn -> fatalError (Uicommon.exn2string exn))
    ()

(* Defined in MyController.m, used to redisplay the table
   when the status for a row changes *)
external reloadTable : int -> unit = "reloadTable";;
(* from uigtk2 *)
let showProgress i bytes dbg =
(*  Trace.status "showProgress"; *)
  let i = Uutil.File.toLine i in
  let item = !theState.(i) in
  item.bytesTransferred <- Uutil.Filesize.add item.bytesTransferred bytes;
  let b = item.bytesTransferred in
  let len = item.bytesToTransfer in
  let newstatus =
    if b = Uutil.Filesize.zero || len = Uutil.Filesize.zero then "start "
    else if len = Uutil.Filesize.zero then
      Printf.sprintf "%5s " (Uutil.Filesize.toString b)
    else Util.percent2string (Uutil.Filesize.percentageOfTotalSize b len) in
  let oldstatus = item.statusMessage in
  item.statusMessage <- Some newstatus;
  showGlobalProgress bytes;
(* FIX: No status window in Mac version, see GTK version for how to do it *)
  if oldstatus <> Some newstatus then reloadTable i;;

let unisonGetVersion() = Uutil.myVersion
;;
Callback.register "unisonGetVersion" unisonGetVersion;;

(* snippets from Uicommon, duplicated for now *)
(* BCP: Duplicating this is a really bad idea!!! *)

(* First initialization sequence *)
(* Returns a string option: command line profile, if any *)
let unisonInit0() =
  ignore (Gc.set {(Gc.get ()) with Gc.max_overhead = 150});
  (* Display status in GUI instead of on stderr *)
  let formatStatus major minor = (Util.padto 30 (major ^ "  ")) ^ minor in
  Trace.messageDisplayer := displayStatus;
  Trace.statusFormatter := formatStatus;
  Trace.sendLogMsgsToStderr := false;
  (* Display progress in GUI *)
  Uutil.setProgressPrinter showProgress;
  (* Initialise global progress so progress bar is not updated *)
  initGlobalProgress Uutil.Filesize.dummy;
  (* Make sure we have a directory for archives and profiles *)
  Os.createUnisonDir();
  (* Extract any command line profile or roots *)
  let clprofile = ref None in
  begin
    try
      let args = Prefs.scanCmdLine Uicommon.usageMsg in
      match Util.StringMap.find "rest" args with
        [] -> ()
      | [profile] -> clprofile := Some profile
      | [root2;root1] -> Globals.setRawRoots [root1;root2]
      | [root2;root1;profile] ->
          Globals.setRawRoots [root1;root2];
          clprofile := Some profile
      | _ ->
          (Printf.eprintf
             "%s was invoked incorrectly (too many roots)" Uutil.myName;
           exit 1)
    with Not_found -> ()
  end;
  (* Print header for debugging output *)
  debug (fun() ->
    Printf.eprintf "%s, version %s\n\n" Uutil.myName Uutil.myVersion);
  debug (fun() -> Util.msg "initializing UI");
  debug (fun () ->
    (match !clprofile with
      None -> Util.msg "No profile given on command line"
    | Some s -> Printf.eprintf "Profile '%s' given on command line" s);
    (match Globals.rawRoots() with
      [] -> Util.msg "No roots given on command line"
    | [root1;root2] ->
        Printf.eprintf "Roots '%s' and '%s' given on command line"
          root1 root2
    | _ -> assert false));
  begin match !clprofile with
    None -> ()
  | Some n ->
      let f = Prefs.profilePathname n in
      if not(System.file_exists f)
      then (Printf.eprintf "Profile %s does not exist"
              f;
            exit 1)
  end;
  !clprofile
;;
Callback.register "unisonInit0" unisonInit0;;

(* Utility function to tell the UI whether roots were set *)

let areRootsSet () =
  match Globals.rawRoots() with
  | [] -> false
  | _ -> true
;;
Callback.register "areRootsSet" areRootsSet;;

(* Utility function to tell the UI whether -batch is set *)

let isBatchSet () =
  Prefs.read Globals.batch
;;
Callback.register "isBatchSet" isBatchSet;;

(* The first time we load preferences, we also read the command line
   arguments; if we re-load prefs (because the user selected a new profile)
   we ignore the command line *)
let firstTime = ref(true)

(* After figuring out the profile name. If the profileName is the empty
   string, it means that only the roots were specified on the command
   line *)
let do_unisonInit1 profileName =
  (* Load the profile and command-line arguments *)
  (* Restore prefs to their default values, if necessary *)
  if not !firstTime then Prefs.resetToDefaults();
  unsynchronizedPaths := None;

  if profileName <> "" then begin
    (* Tell the preferences module the name of the profile *)
    Prefs.profileName := Some(profileName);

    (* If the profile does not exist, create an empty one (this should only
       happen if the profile is 'default', since otherwise we will already
       have checked that the named one exists). *)
    if not(System.file_exists (Prefs.profilePathname profileName)) then
      Prefs.addComment "Unison preferences file";

    (* Load the profile *)
    (Trace.debug "" (fun() -> Util.msg "about to load prefs");
    Prefs.loadTheFile())
  end;

  (* Parse the command line.  This will temporarily override
     settings from the profile. *)
  if !firstTime then begin
    Trace.debug "" (fun() -> Util.msg "about to parse command line");
    Prefs.parseCmdLine Uicommon.usageMsg;
  end;

  firstTime := false;

  (* Print the preference settings *)
  Trace.debug "" (fun() -> Prefs.dumpPrefsToStderr() );

  (* FIX: if no roots, ask the user *)

  Recon.checkThatPreferredRootIsValid();

  let localRoots,remoteRoots =
    Safelist.partition
      (function Clroot.ConnectLocal _ -> true | _ -> false)
      (Globals.parsedClRawRoots ()) in

  match remoteRoots with
    [r] ->
      (* FIX: tell the user the next step (contacting server) might
         take a while *)
      Remote.openConnectionStart r
  | _::_::_ ->
    raise(Util.Fatal "cannot synchronize more than one remote root");
  | _ -> None
;;
external unisonInit1Complete : Remote.preconnection option -> unit = "unisonInit1Complete";;

(* Do this in another thread and return immedidately to free up main thread in cocoa *)
let unisonInit1 profileName =
  doInOtherThread
    (fun () ->
       let r = do_unisonInit1 profileName in
       unisonInit1Complete r)
;;
Callback.register "unisonInit1" unisonInit1;;
Callback.register "openConnectionPrompt" Remote.openConnectionPrompt;;
Callback.register "openConnectionReply" Remote.openConnectionReply;;
Callback.register "openConnectionEnd" Remote.openConnectionEnd;;
Callback.register "openConnectionCancel" Remote.openConnectionCancel;;

let commitUpdates () =
  Trace.status "Updating synchronizer state";
  let t = Trace.startTimer "Updating synchronizer state" in
  Update.commitUpdates();
  Trace.showTimer t

let do_unisonInit2 () =
  (* Canonize the names of the roots and install them in Globals. *)
  Globals.installRoots2();

  (* If both roots are local, disable the xferhint table to save time *)
  begin match Globals.roots() with
    ((Local,_),(Local,_)) -> Prefs.set Xferhint.xferbycopying false
  | _ -> ()
  end;

  (* If no paths were specified, then synchronize the whole replicas *)
  if Prefs.read Globals.paths = [] then Prefs.set Globals.paths [Path.empty];

  (* Expand any "wildcard" paths [with final component *] *)
  Globals.expandWildcardPaths();

  Update.storeRootsName ();

  Trace.debug ""
    (fun() ->
       Printf.eprintf "Roots: \n";
       Safelist.iter (fun clr -> Printf.eprintf "        %s\n" clr)
         (Globals.rawRoots ());
       Printf.eprintf "  i.e. \n";
       Safelist.iter (fun clr -> Printf.eprintf "        %s\n"
                    (Clroot.clroot2string (Clroot.parseRoot clr)))
         (Globals.rawRoots ());
       Printf.eprintf "  i.e. (in canonical order)\n";
       Safelist.iter (fun r ->
         Printf.eprintf "       %s\n" (root2string r))
         (Globals.rootsInCanonicalOrder());
       Printf.eprintf "\n"
    );

  (* Install the warning panel, hopefully it's not too late *)
  setWarnPrinter();

  Lwt_unix.run
    (Uicommon.validateAndFixupPrefs () >>=
     Globals.propagatePrefs);

  (* Initializes some backups stuff according to the preferences just loaded from the profile.
     Important to do it here, after prefs are propagated, because the function will also be
     run on the server, if any. Also, this should be done each time a profile is reloaded
     on this side, that's why it's here. *)
  Stasher.initBackups ();

  (* Turn on GC messages, if the '-debug gc' flag was provided *)
  if Trace.enabled "gc" then Gc.set {(Gc.get ()) with Gc.verbose = 0x3F};

  (* BCPFIX: Should/can this be done earlier?? *)
  Files.processCommitLogs();

  (* from Uigtk2 *)
  (* detect updates and reconcile *)
  let _ = Globals.roots () in
  let t = Trace.startTimer "Checking for updates" in
  let findUpdates () =
    Trace.status "Looking for changes";
    let updates = Update.findUpdates ~wantWatcher:true !unsynchronizedPaths in
    Trace.showTimer t;
    updates in
  let reconcile updates = Recon.reconcileAll updates in
  let (reconItemList, thereAreEqualUpdates, dangerousPaths) =
    reconcile (findUpdates ()) in
  if not !Update.foundArchives then commitUpdates ();
  if reconItemList = [] then begin
    if !Update.foundArchives then commitUpdates ();
    if thereAreEqualUpdates then
      Trace.status
        "Replicas have been changed only in identical ways since last sync"
    else
      Trace.status "Everything is up to date"
  end else
    Trace.status "Check and/or adjust selected actions; then press Go";
  Trace.status (Printf.sprintf "There are %d reconitems" (Safelist.length reconItemList));
  let stateItemList =
    Safelist.map
      (fun ri -> { ri = ri;
                   bytesTransferred = Uutil.Filesize.zero;
                   bytesToTransfer = Uutil.Filesize.zero;
                   whatHappened = None; statusMessage = None })
      reconItemList in
  theState := Array.of_list stateItemList;
  unsynchronizedPaths :=
    Some (Safelist.map (fun ri -> ri.path1) reconItemList, []);
  if dangerousPaths <> [] then begin
    Prefs.set Globals.batch false;
    Util.warn (Uicommon.dangerousPathMsg dangerousPaths)
  end;
  !theState
;;

external unisonInit2Complete : stateItem array -> unit = "unisonInit2Complete";;

(* Do this in another thread and return immedidately to free up main thread in cocoa *)
let unisonInit2 () =
  doInOtherThread
    (fun () ->
       let r = do_unisonInit2 () in
       unisonInit2Complete r)
;;
Callback.register "unisonInit2" unisonInit2;;

let unisonRiToDetails ri =
  Unicode.protect
    (match ri.whatHappened with
       Some (Util.Failed s) ->
         Path.toString ri.ri.path1 ^ "\n" ^ s
     | _ ->
         Path.toString ri.ri.path1 ^ "\n" ^
         Uicommon.details2string ri.ri "  ");;
Callback.register "unisonRiToDetails" unisonRiToDetails;;

let unisonRiToPath ri = Unicode.protect (Path.toString ri.ri.path1);;
Callback.register "unisonRiToPath" unisonRiToPath;;

let rcToString rc =
  match rc.status with
    `Deleted      -> "Deleted"
  | `Modified     -> "Modified"
  | `PropsChanged -> "PropsChanged"
  | `Created      -> "Created"
  | `Unchanged    -> "";;
let unisonRiToLeft ri =
  match ri.ri.replicas with
    Problem _ -> ""
  | Different {rc1 = rc} -> rcToString rc;;
Callback.register "unisonRiToLeft" unisonRiToLeft;;
let unisonRiToRight ri =
  match ri.ri.replicas with
    Problem _ -> ""
  | Different {rc2 = rc} -> rcToString rc;;
Callback.register "unisonRiToRight" unisonRiToRight;;

let unisonRiToFileSize ri =
  Uutil.Filesize.toFloat (riLength ri.ri);;
Callback.register "unisonRiToFileSize" unisonRiToFileSize;;

let unisonRiToFileType ri =
  riFileType ri.ri;;
Callback.register "unisonRiToFileType" unisonRiToFileType;;

let direction2niceString = function (* from Uicommon where it's not exported *)
    Conflict _         -> "<-?->"
  | Replica1ToReplica2 -> "---->"
  | Replica2ToReplica1 -> "<----"
  | Merge              -> "<-M->"
let unisonRiToDirection ri =
  match ri.ri.replicas with
    Problem _ -> "XXXXX"
  | Different diff -> direction2niceString diff.direction;;
Callback.register "unisonRiToDirection" unisonRiToDirection;;

let unisonRiSetLeft ri =
  match ri.ri.replicas with
    Problem _ -> ()
  | Different diff -> diff.direction <- Replica2ToReplica1;;
Callback.register "unisonRiSetLeft" unisonRiSetLeft;;
let unisonRiSetRight ri =
  match ri.ri.replicas with
    Problem _ -> ()
  | Different diff -> diff.direction <- Replica1ToReplica2;;
Callback.register "unisonRiSetRight" unisonRiSetRight;;
let unisonRiSetConflict ri =
  match ri.ri.replicas with
    Problem _ -> ()
  | Different diff -> diff.direction <- Conflict "skip requested";;
Callback.register "unisonRiSetConflict" unisonRiSetConflict;;
let unisonRiSetMerge ri =
  match ri.ri.replicas with
    Problem _ -> ()
  | Different diff -> diff.direction <- Merge;;
Callback.register "unisonRiSetMerge" unisonRiSetMerge;;
let unisonRiForceOlder ri =
  Recon.setDirection ri.ri `Older `Force;;
Callback.register "unisonRiForceOlder" unisonRiForceOlder;;
let unisonRiForceNewer ri =
  Recon.setDirection ri.ri `Newer `Force;;
Callback.register "unisonRiForceNewer" unisonRiForceNewer;;

let unisonRiToProgress ri =
  match (ri.statusMessage, ri.whatHappened,ri.ri.replicas) with
    (None,None,_) -> ""
  | (Some s,None,_) -> Unicode.protect s
  | (_,_,Different {direction = Conflict "files differed"}) -> ""
  | (_,_,Problem _) -> ""
  | (_,Some Util.Succeeded,_) -> "done"
  | (_,Some (Util.Failed s),_) -> "FAILED";;
Callback.register "unisonRiToProgress" unisonRiToProgress;;

let unisonRiToBytesTransferred ri =
  Uutil.Filesize.toFloat ri.bytesTransferred;;
Callback.register "unisonRiToBytesTransferred" unisonRiToBytesTransferred;;

(* --------------------------------------------------- *)

(* Defined in MyController.m, used to show diffs *)
external displayDiff : string -> string -> unit = "displayDiff";;
external displayDiffErr : string -> unit = "displayDiffErr";;
let displayDiff title text =
  displayDiff (Unicode.protect title) (Unicode.protect text);;
let displayDiffErr err = displayDiffErr (Unicode.protect err)

(* If only properties have changed, we can't diff or merge.
   'Can't diff' is produced (uicommon.ml) if diff is attempted
   when either side has PropsChanged *)
let filesAreDifferent status1 status2 =
  match status1, status2 with
   `PropsChanged, `Unchanged -> false
  | `Unchanged, `PropsChanged -> false
  | `PropsChanged, `PropsChanged -> false
  | _, _ -> true;;

(* check precondition for diff; used to disable diff button *)
let canDiff ri =
  match ri.ri.replicas with
    Problem _ -> false
  | Different {rc1 = {typ = `FILE; status = status1};
               rc2 = {typ = `FILE; status = status2}} ->
      filesAreDifferent status1 status2
  | Different _ -> false;;
Callback.register "canDiff" canDiff;;

(* from Uicommon *)
(* precondition: uc = File (Updates(_, ..) on both sides *)
let showDiffs ri printer errprinter id =
  match ri.replicas with
    Problem _ ->
      errprinter
        "Can't diff files: there was a problem during update detection"
  | Different
        {rc1 = {typ = `FILE; status = status1; ui = ui1};
         rc2 = {typ = `FILE; status = status2; ui = ui2}} ->
      if filesAreDifferent status1 status2 then
        (let (root1,root2) = Globals.roots() in
         begin
           try Files.diff root1 ri.path1 ui1 root2 ri.path2 ui2 printer id
           with Util.Transient e -> errprinter e
         end)
  | Different _ ->
      errprinter "Can't diff: path doesn't refer to a file in both replicas"

let runShowDiffs ri i =
  let file = Uutil.File.ofLine i in
    showDiffs ri.ri displayDiff displayDiffErr file;;
Callback.register "runShowDiffs" runShowDiffs;;

(* --------------------------------------------------- *)

let do_unisonSynchronize () =
  if Array.length !theState = 0 then
    Trace.status "Nothing to synchronize"
  else begin
    Trace.status "Propagating changes";
    Uicommon.transportStart ();
    let totalLength =
      Array.fold_left
        (fun l si ->
           si.bytesTransferred <- Uutil.Filesize.zero;
           let len =
             if si.whatHappened = None then Common.riLength si.ri else
             Uutil.Filesize.zero
           in
           si.bytesToTransfer <- len;
           Uutil.Filesize.add l len)
        Uutil.Filesize.zero !theState in
    initGlobalProgress totalLength;
    let t = Trace.startTimer "Propagating changes" in
    let uiWrapper i theSI =
      match theSI.whatHappened with
        None ->
          catch (fun () ->
            Transport.transportItem
              theSI.ri (Uutil.File.ofLine i)
              (fun title text ->
                 debug (fun () -> Util.msg "MERGE '%s': '%s'"
                      title text);
                 displayDiff title text; true)
                   >>= (fun () ->
                   return Util.Succeeded))
                (fun e ->
                   match e with
                     Util.Transient s ->
                       return (Util.Failed s)
                   | _ ->
                       fail e)
            >>= (fun res ->
          let rem =
            Uutil.Filesize.sub
              theSI.bytesToTransfer theSI.bytesTransferred
          in
          if rem <> Uutil.Filesize.zero then
            showProgress (Uutil.File.ofLine i) rem "done";
          theSI.whatHappened <- Some res;
          return ())
      | Some _ ->
          return () (* Already processed this one (e.g. merged it) *)
    in
    Uicommon.transportItems !theState (fun {ri; _} -> not (Common.isDeletion ri)) uiWrapper;
    Uicommon.transportItems !theState (fun {ri; _} -> Common.isDeletion ri) uiWrapper;
    Uicommon.transportFinish ();
    Trace.showTimer t;
    commitUpdates ();

    let failureList =
      Array.fold_right
        (fun si l ->
           match si.whatHappened with
             Some (Util.Failed err) ->
               (si, [err], "transport failure") :: l
           | _ ->
               l)
        !theState []
    in
    let failureCount = List.length failureList in
    let failures =
      if failureCount = 0 then [] else
      [Printf.sprintf "%d failure%s"
         failureCount (if failureCount = 1 then "" else "s")]
    in
    let partialList =
      Array.fold_right
        (fun si l ->
           match si.whatHappened with
             Some Util.Succeeded
             when partiallyProblematic si.ri &&
                  not (problematic si.ri) ->
               let errs =
                 match si.ri.replicas with
                   Different diff -> diff.errors1 @ diff.errors2
                 | _              -> assert false
               in
               (si, errs,
                "partial transfer (errors during update detection)") :: l
           | _ ->
               l)
        !theState []
    in
    let partialCount = List.length partialList in
    let partials =
      if partialCount = 0 then [] else
      [Printf.sprintf "%d partially transferred" partialCount]
    in
    let skippedList =
      Array.fold_right
        (fun si l ->
           match si.ri.replicas with
             Problem err ->
               (si, [err], "error during update detection") :: l
           | Different diff when (isConflict diff.direction) ->
               (si, [],
                if (isConflict diff.default_direction) then
                  "conflict"
                else "skipped") :: l
           | _ ->
               l)
        !theState []
    in
    let skippedCount = List.length skippedList in
    let skipped =
      if skippedCount = 0 then [] else
      [Printf.sprintf "%d skipped" skippedCount]
    in
    unsynchronizedPaths :=
      Some (Safelist.map (fun (si, _, _) -> si.ri.path1)
              (failureList @ partialList @ skippedList),
            []);
    Trace.status
      (Printf.sprintf "Synchronization complete         %s"
         (String.concat ", " (failures @ partials @ skipped)));
    initGlobalProgress Uutil.Filesize.dummy;
  end;;
external syncComplete : unit -> unit = "syncComplete";;

(* Do this in another thread and return immedidately to free up main thread in cocoa *)
let unisonSynchronize () =
  doInOtherThread
    (fun () ->
       do_unisonSynchronize ();
       syncComplete ())
;;
Callback.register "unisonSynchronize" unisonSynchronize;;

let unisonIgnorePath pathString =
  Uicommon.addIgnorePattern (Uicommon.ignorePath (Path.fromString pathString));;
let unisonIgnoreExt pathString =
  Uicommon.addIgnorePattern (Uicommon.ignoreExt (Path.fromString pathString));;
let unisonIgnoreName pathString =
  Uicommon.addIgnorePattern (Uicommon.ignoreName (Path.fromString pathString));;
Callback.register "unisonIgnorePath" unisonIgnorePath;;
Callback.register "unisonIgnoreExt"  unisonIgnoreExt;;
Callback.register "unisonIgnoreName" unisonIgnoreName;;

(* Update the state to take into account ignore patterns.
   Return the new index of the first state item that is
   not ignored starting at old index i.
*)
let unisonUpdateForIgnore i =
  let l = ref [] in
  let num = ref(-1) in
  let newI = ref None in
  (* FIX: we should actually test whether any prefix is now ignored *)
  let keep s = not (Globals.shouldIgnore s.ri.path1) in
  for j = 0 to (Array.length !theState - 1) do
    let s = !theState.(j) in
    if keep s then begin
      l := s :: !l;
      num := !num + 1;
      if (j>=i && !newI=None) then newI := Some !num
    end
  done;
  theState := Array.of_list (Safelist.rev !l);
  match !newI with None -> (Array.length !theState - 1)
  | Some i' -> i';;
Callback.register "unisonUpdateForIgnore" unisonUpdateForIgnore;;

let unisonState () = !theState;;
Callback.register "unisonState" unisonState;;

(* from Uicommon *)
let roots2niceStrings length = function
   (Local,fspath1), (Local,fspath2) ->
    let name1, name2 = Fspath.differentSuffix fspath1 fspath2 in
    (Util.truncateString name1 length, Util.truncateString name2 length)
 | (Local,fspath1), (Remote host, fspath2) ->
    (Util.truncateString "local" length, Util.truncateString host length)
 | (Remote host, fspath1), (Local,fspath2) ->
    (Util.truncateString host length, Util.truncateString "local" length)
 | _ -> assert false  (* BOGUS? *);;
let unisonFirstRootString() =
  let replica1, replica2 = roots2niceStrings 32 (Globals.roots()) in
  Unicode.protect replica1;;
let unisonSecondRootString() =
  let replica1, replica2 = roots2niceStrings 32 (Globals.roots()) in
  Unicode.protect replica2;;
Callback.register "unisonFirstRootString" unisonFirstRootString;;
Callback.register "unisonSecondRootString" unisonSecondRootString;;


(* Note, this returns whether the files conflict, NOT whether
   the current setting is Conflict *)
let unisonRiIsConflict ri =
  match ri.ri.replicas with
  | Different {default_direction = Conflict "files differ"} -> true
  | _ -> false;;
Callback.register "unisonRiIsConflict" unisonRiIsConflict;;

(* Test whether reconItem's current state is different from
   Unison's recommendation.  Used to colour arrows in
   the reconItems table *)
let changedFromDefault ri =
  match ri.ri.replicas with
    Different diff -> diff.direction <> diff.default_direction
   | _ -> false;;
Callback.register "changedFromDefault" changedFromDefault;;

let unisonRiRevert ri =
  match ri.ri.replicas with
  | Different diff -> diff.direction <- diff.default_direction
  | _ -> ();;
Callback.register "unisonRiRevert" unisonRiRevert;;

let unisonProfileInit (profileName:string) (r1:string) (r2:string) =
  Prefs.resetToDefaults();
  Prefs.profileName := Some(profileName);
  Prefs.addComment "Unison preferences file"; (* Creates the file, assumes it doesn't exist *)
  ignore (Prefs.add "root" r1);
  ignore (Prefs.add "root" r2);;
Callback.register "unisonProfileInit" unisonProfileInit;;

Callback.register "unisonPasswordMsg" Terminal.password;;
Callback.register "unisonPassphraseMsg" Terminal.passphrase;;
Callback.register "unisonAuthenticityMsg" Terminal.authenticity;;

let unisonExnInfo e =
  match e with
    Util.Fatal s -> Printf.sprintf "Fatal error: %s" s
  | Invalid_argument s -> Printf.sprintf "Invalid argument: %s" s
  | Unix.Unix_error(ue,s1,s2) ->
      Printf.sprintf "Unix error(%s,%s,%s)" (Unix.error_message ue) s1 s2
  | _ -> Printexc.to_string e;;
Callback.register "unisonExnInfo"
  (fun e -> Unicode.protect (unisonExnInfo e));;
