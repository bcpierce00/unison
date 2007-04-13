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
                   mutable whatHappened : Util.confirmation option;
                   mutable statusMessage : string option };;
let theState = ref [| |];;

let unisonDirectory() = Fspath.toString Os.unisonDir
;;
Callback.register "unisonDirectory" unisonDirectory;;
 
(* Defined in MyController.m, used to redisplay the table
   when the status for a row changes *)
external displayStatus : string -> unit = "displayStatus";;

(* Defined in MyController.m, used to redisplay the table
   when the status for a row changes *)
external reloadTable : int -> unit = "reloadTable";;
(* from uigtk2 *)
let showProgress i bytes dbg =
(*  Trace.status "showProgress"; *)
(* XXX There should be a way to reset the amount of bytes transferred... *)
  let i = Uutil.File.toLine i in
  let item = !theState.(i) in
  item.bytesTransferred <- Uutil.Filesize.add item.bytesTransferred bytes;
  let b = item.bytesTransferred in
  let len = Common.riLength item.ri in
  let newstatus =
    if b = Uutil.Filesize.zero || len = Uutil.Filesize.zero then "start "
    else if len = Uutil.Filesize.zero then
      Printf.sprintf "%5s " (Uutil.Filesize.toString b)
    else Util.percent2string (Uutil.Filesize.percentageOfTotalSize b len) in
  item.statusMessage <- Some newstatus;
(* FIX: No status window in Mac version, see GTK version for how to do it *)
  reloadTable i;;

let unisonGetVersion() = Uutil.myVersion
;;
Callback.register "unisonGetVersion" unisonGetVersion;;

(* snippets from Uicommon, duplicated for now *)
(* BCP: Duplicating this is a bad idea!!! *)
(* First initialization sequence *)
(* Returns a string option: command line profile, if any *)
let unisonInit0() =
  ignore (Gc.set {(Gc.get ()) with Gc.max_overhead = 150});
  (* Install an appropriate function for finding preference files.  (We put
     this in Util just because the Prefs module lives below the Os module in the
     dependency hierarchy, so Prefs can't call Os directly.) *)
  Util.supplyFileInUnisonDirFn 
    (fun n -> Fspath.toString (Os.fileInUnisonDir(n)));
  (* Display status in GUI instead of on stderr *)
  let formatStatus major minor = (Util.padto 30 (major ^ "  ")) ^ minor in
  Trace.messageDisplayer := displayStatus;
  Trace.statusFormatter := formatStatus;
  Trace.sendLogMsgsToStderr := false;
  (* Display progress in GUI *)
  Uutil.setProgressPrinter showProgress;
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
      | [root1;root2] -> Globals.setRawRoots [root1;root2]
      | [root1;root2;profile] ->
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
      if not(Sys.file_exists f)
      then (Printf.eprintf "Profile %s does not exist" f;
            exit 1)
  end;
  !clprofile
;;
Callback.register "unisonInit0" unisonInit0;;

(* The first time we load preferences, we also read the command line
   arguments; if we re-load prefs (because the user selected a new profile)
   we ignore the command line *)
let firstTime = ref(true)

(* After figuring out the profile name *)
let unisonInit1 profileName =
  (* Load the profile and command-line arguments *)
  (* Restore prefs to their default values, if necessary *)
  if not !firstTime then Prefs.resetToDefaults();

  (* Tell the preferences module the name of the profile *)
  Prefs.profileName := Some(profileName);
  
  (* If the profile does not exist, create an empty one (this should only
     happen if the profile is 'default', since otherwise we will already
     have checked that the named one exists). *)
   if not(Sys.file_exists (Prefs.profilePathname profileName)) then
     Prefs.addComment "Unison preferences file";

  (* Load the profile *)
  (Trace.debug "" (fun() -> Util.msg "about to load prefs");
  Prefs.loadTheFile());

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
  let localRoots,remoteRoots =
    Safelist.partition
      (function Clroot.ConnectLocal _ -> true | _ -> false)
      (Safelist.map Clroot.parseRoot (Globals.rawRoots())) in

  match remoteRoots with
    [r] ->
      (* FIX: tell the user the next step (contacting server) might
         take a while *)
      Remote.openConnectionStart r
  | _::_::_ ->
    raise(Util.Fatal "cannot synchronize more than one remote root");
  | _ -> None
;;
Callback.register "unisonInit1" unisonInit1;;
Callback.register "openConnectionPrompt" Remote.openConnectionPrompt;;
Callback.register "openConnectionReply" Remote.openConnectionReply;;
Callback.register "openConnectionEnd" Remote.openConnectionEnd;;
Callback.register "openConnectionCancel" Remote.openConnectionCancel;;

let unisonInit2 () =
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

  Recon.checkThatPreferredRootIsValid();

  Lwt_unix.run
    (Uicommon.checkCaseSensitivity () >>=
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
    let updates = Update.findUpdates () in
    Trace.showTimer t;
    updates in
  let reconcile updates = Recon.reconcileAll updates in
  let (reconItemList, thereAreEqualUpdates, dangerousPaths) =
    reconcile (findUpdates ()) in
  if reconItemList = [] then
    if thereAreEqualUpdates then
      Trace.status "Replicas have been changed only in identical ways since last sync"
    else
      Trace.status "Everything is up to date"
  else
    Trace.status "Check and/or adjust selected actions; then press Go";
  Trace.status (Printf.sprintf "There are %d reconitems" (Safelist.length reconItemList));
  let stateItemList =
    Safelist.map
      (fun ri -> { ri = ri; bytesTransferred = Uutil.Filesize.zero;
                   whatHappened = None; statusMessage = None })
      reconItemList in
  theState := Array.of_list stateItemList;
  if dangerousPaths <> [] then begin
    Prefs.set Globals.batch false;
    Util.warn (Uicommon.dangerousPathMsg dangerousPaths)
  end;
  !theState
;;
Callback.register "unisonInit2" unisonInit2;;

let unisonRiToDetails ri =
  match ri.whatHappened with
    Some (Util.Failed s) -> (Path.toString ri.ri.path) ^ "\n" ^ s
  | _ -> (Path.toString ri.ri.path) ^ "\n" ^ (Uicommon.details2string ri.ri "  ");;
Callback.register "unisonRiToDetails" unisonRiToDetails;;

let unisonRiToPath ri = Path.toString ri.ri.path;;
Callback.register "unisonRiToPath" unisonRiToPath;;

let rcToString (_,status,_,_) =
  match status with
    `Deleted      -> "Deleted"
  | `Modified     -> "Modified"
  | `PropsChanged -> "PropsChanged"
  | `Created      -> "Created"
  | `Unchanged    -> "";;
let unisonRiToLeft ri =
  match ri.ri.replicas with
    Problem _ -> ""
  | Different(rc,_,_,_) -> rcToString rc;;
Callback.register "unisonRiToLeft" unisonRiToLeft;;
let unisonRiToRight ri =
  match ri.ri.replicas with
    Problem _ -> ""
  | Different(_,rc,_,_) -> rcToString rc;;
Callback.register "unisonRiToRight" unisonRiToRight;;

let direction2niceString = function (* from Uicommon where it's not exported *)
    Conflict           -> "<-?->"
  | Replica1ToReplica2 -> "---->"
  | Replica2ToReplica1 -> "<----"
  | Merge              -> "<-M->"
let unisonRiToDirection ri =
  match ri.ri.replicas with
    Problem _ -> "XXXXX"
  | Different(_,_,d,_) -> direction2niceString !d;;
Callback.register "unisonRiToDirection" unisonRiToDirection;;

let unisonRiSetLeft ri =
  match ri.ri.replicas with
    Problem _ -> ()
  | Different(_,_,d,_) -> d := Replica2ToReplica1;;
Callback.register "unisonRiSetLeft" unisonRiSetLeft;;
let unisonRiSetRight ri =
  match ri.ri.replicas with
    Problem _ -> ()
  | Different(_,_,d,_) -> d := Replica1ToReplica2;;
Callback.register "unisonRiSetRight" unisonRiSetRight;;
let unisonRiSetConflict ri =
  match ri.ri.replicas with
    Problem _ -> ()
  | Different(_,_,d,_) -> d := Conflict;;
Callback.register "unisonRiSetConflict" unisonRiSetConflict;;
let unisonRiSetMerge ri =
  match ri.ri.replicas with
    Problem _ -> ()
  | Different(_,_,d,_) -> d := Merge;;
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
  | (Some s,None,_) -> s
  | (_,_,Different(_,_,{contents=Conflict},_)) -> ""
  | (_,_,Problem _) -> ""
  | (_,Some Util.Succeeded,_) -> "done"
  | (_,Some (Util.Failed s),_) -> "FAILED";;
Callback.register "unisonRiToProgress" unisonRiToProgress;;

let unisonSynchronize () =
  if Array.length !theState = 0 then
    Trace.status "Nothing to synchronize"
  else begin
    Trace.status "Propagating changes";
    Transport.logStart ();
    let t = Trace.startTimer "Propagating changes" in
    let im = Array.length !theState in
    let rec loop i actions pRiThisRound =
      if i < im then begin
        let theSI = !theState.(i) in
        let action =
          match theSI.whatHappened with
            None ->
              if not (pRiThisRound theSI.ri) then
                return ()
              else
                catch (fun () ->
                         Transport.transportItem
                           theSI.ri (Uutil.File.ofLine i)
                           (fun title text -> 
			     Trace.status (Printf.sprintf "MERGE %s: %s" title text); true)
                         >>= (fun () ->
                         return Util.Succeeded))
                      (fun e ->
                         match e with
                           Util.Transient s ->
                             return (Util.Failed s)
                         | _ ->
                             fail e)
                  >>= (fun res ->
                theSI.whatHappened <- Some res;
                return ())
          | Some _ ->
              return () (* Already processed this one (e.g. merged it) *)
        in
        loop (i + 1) (action :: actions) pRiThisRound
      end else
        return actions
    in
    Lwt_unix.run
      (loop 0 [] (fun ri -> not (Common.isDeletion ri)) >>= (fun actions ->
        Lwt_util.join actions));
    Lwt_unix.run
      (loop 0 [] Common.isDeletion >>= (fun actions ->
        Lwt_util.join actions));
    Transport.logFinish ();
    Trace.showTimer t;
    Trace.status "Updating synchronizer state";
    let t = Trace.startTimer "Updating synchronizer state" in
    Update.commitUpdates();
    Trace.showTimer t;

    let failures =
      let count =
        Array.fold_left
          (fun l si ->
             l + (match si.whatHappened with Some(Util.Failed(_)) -> 1 | _ -> 0))
          0 !theState in
      if count = 0 then "" else
        Printf.sprintf "%d failure%s" count (if count=1 then "" else "s") in
    let skipped =
      let count =
        Array.fold_left
          (fun l si ->
             l + (if problematic si.ri then 1 else 0))
          0 !theState in
      if count = 0 then "" else
        Printf.sprintf "%d skipped" count in
    Trace.status
      (Printf.sprintf "Synchronization complete         %s%s%s"
         failures (if failures=""||skipped="" then "" else ", ") skipped);
  end;;
Callback.register "unisonSynchronize" unisonSynchronize;;

let unisonIgnorePath si =
  Uicommon.addIgnorePattern (Uicommon.ignorePath si.ri.path);;
let unisonIgnoreExt si =
  Uicommon.addIgnorePattern (Uicommon.ignoreExt si.ri.path);;
let unisonIgnoreName si =
  Uicommon.addIgnorePattern (Uicommon.ignoreName si.ri.path);;
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
  let keep s = not (Globals.shouldIgnore s.ri.path) in
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
  replica1;;
let unisonSecondRootString() =
  let replica1, replica2 = roots2niceStrings 32 (Globals.roots()) in
  replica2;;
Callback.register "unisonFirstRootString" unisonFirstRootString;;
Callback.register "unisonSecondRootString" unisonSecondRootString;;


(* Note, this returns whether the files conflict, NOT whether
   the current setting is Conflict *)
let unisonRiIsConflict ri =
  match ri.ri.replicas with
  | Different(_,_,_,Conflict) -> true
  | _ -> false;;
Callback.register "unisonRiIsConflict" unisonRiIsConflict;;
let unisonRiRevert ri =
  match ri.ri.replicas with
  | Different(_,_,d,d0) -> d := d0
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
Callback.register "unisonAuthenticityMsg" Terminal.authenticity;;

let unisonExnInfo e =
  match e with
    Util.Fatal s -> Printf.sprintf "Fatal error: %s" s
  | Invalid_argument s -> Printf.sprintf "Invalid argument: %s" s
  | Unix.Unix_error(ue,s1,s2) ->
      Printf.sprintf "Unix error(%s,%s,%s)" (Unix.error_message ue) s1 s2
  | _ -> Printexc.to_string e;;
Callback.register "unisonExnInfo" unisonExnInfo;;
