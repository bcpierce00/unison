(* Unison file synchronizer: src/uicommon.ml *)
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

(**********************************************************************
                             UI selection
 **********************************************************************)

type interface =
   Text
 | Graphic

let minterface =
  Umarshal.(sum2 unit unit
              (function
               | Text -> I21 ()
               | Graphic -> I22 ())
              (function
               | I21 () -> Text
               | I22 () -> Graphic))

module type UI =
sig
 val start : interface -> unit
 val defaultUi : interface
end


(**********************************************************************
                             Preferences
 **********************************************************************)

let auto =
  Prefs.createBool "auto" false
    ~category:(`Basic `Syncprocess_CLI)
    "automatically accept default (nonconflicting) actions"
    ("When set to {\\tt true}, this flag causes the user "
     ^ "interface to skip asking for confirmations on "
     ^ "non-conflicting changes.  (More precisely, when the user interface "
     ^ "is done setting the propagation direction for one entry and is about "
     ^ "to move to the next, it will skip over all non-conflicting entries "
     ^ "and go directly to the next conflict.)" )

(* This has to be here rather than in uigtk.ml, because it is part of what
   gets sent to the server at startup *)
let mainWindowHeight =
  Prefs.createInt "height" 15
    ~category:(`Advanced `GUI)
    "height (in lines) of main window in graphical interface"
    ("Used to set the height (in lines) of the main window in the graphical "
     ^ "user interface.")

let expert =
  Prefs.createBool "expert" false
    ~category:(`Internal `Devel)
    "*Enable some developers-only functionality in the UI" ""

let profileLabel =
  Prefs.createString "label" ""
    ~category:(`Advanced `General)
    "provide a descriptive string label for this profile"
    ("Used in a profile to provide a descriptive string documenting its "
     ^ "settings.  (This is useful for users that switch between several "
     ^ "profiles, especially using the `fast switch' feature of the "
     ^ "graphical user interface.)")

let profileKey =
  Prefs.createString "key" ""
    ~category:(`Advanced `General)
    "define a keyboard shortcut for this profile (in some UIs)"
    ("Used in a profile to define a numeric key (0-9) that can be used in "
     ^ "the user interface to switch immediately to this profile.")
(* This preference is not actually referred to in the code anywhere, since
   the keyboard shortcuts are constructed by a separate scan of the preference
   file in uigtk.ml, but it must be present to prevent the preferences module
   from complaining about 'key = n' lines in profiles. *)

let contactquietly =
  Prefs.createBool "contactquietly" false
    ~category:(`Advanced `General)
    "suppress the 'contacting server' message during startup"
    ("If this flag is set, Unison will skip displaying the "
     ^ "`Contacting server' message (which some users find annoying) "
     ^ "during startup.")

let contactingServerMsg () =
  Printf.sprintf "Unison %s: Contacting server..." Uutil.myVersion 

let repeat =
  let parseRepeat s =
    let parseTime ts =
      try int_of_string ts with Failure _ ->
        raise (Prefs.IllegalValue ("Value of 'repeat' preference ("
          ^ s ^ ") should be either a number, 'watch' or 'watch+<number>'"))
    in
    let nonBlankLower x =
      match String.trim x with "" -> None | s -> Some (String.lowercase_ascii s)
    in
    try
      match Safelist.filterMap nonBlankLower (String.split_on_char '+' s) with
      | [] -> `NoRepeat
      | ["watch"] -> `Watch
      | ["watch"; i] | [i; "watch"] -> `WatchAndInterval (parseTime i)
      | _ -> `Interval (parseTime s)
    with
    | Prefs.IllegalValue _ as e -> `Invalid (s, e)
  in
  let externRepeat = function
    | `NoRepeat | `Invalid _ -> ""
    | `Watch -> "watch"
    | `WatchAndInterval i -> "watch+" ^ (string_of_int i)
    | `Interval i -> string_of_int i
  in
  Prefs.create "repeat" `NoRepeat
    ~category:(`Advanced `Syncprocess_CLI)
    "synchronize repeatedly (text interface only)"
    ("Setting this preference causes the text-mode interface to synchronize "
     ^ "repeatedly, rather than doing it just once and stopping.  If the "
     ^ "argument is a number, Unison will pause for that many seconds before "
     ^ "beginning again. When the argument is \\verb|watch|, Unison relies on "
     ^ "an external file monitoring process to synchronize whenever a change "
     ^ "happens.  You can combine the two with a \\verb|+| character to use "
     ^ "file monitoring and also do a full scan every specificed number of "
     ^ "seconds.  For example, \\verb|watch+3600| will react to changes "
     ^ "immediately and additionally do a full scan every hour.")
    (fun _ -> parseRepeat)
    (fun r -> [externRepeat r])
    Umarshal.(sum1 string externRepeat parseRepeat)
let repeatWatcher () =
  match Prefs.read repeat with `Watch | `WatchAndInterval _ -> true | _ -> false

let retry =
  Prefs.createInt "retry" 0
    ~category:(`Advanced `Syncprocess_CLI)
    "re-try failed synchronizations N times (text ui only)"
    ("Setting this preference causes the text-mode interface to try again "
     ^ "to synchronize "
     ^ "updated paths where synchronization fails.  Each such path will be "
     ^ "tried N times."
    )

let confirmmerge =
  Prefs.createBool "confirmmerge" false
    ~category:(`Advanced `Syncprocess)
    "ask for confirmation before committing results of a merge"
    ("Setting this preference causes both the text and graphical interfaces"
     ^ " to ask the user if the results of a merge command may be committed "
     ^ " to the replica or not. Since the merge command works on temporary files,"
     ^ " the user can then cancel all the effects of applying the merge if it"
     ^ " turns out that the result is not satisfactory.  In "
     ^ " batch-mode, this preference has no effect.  Default is false.")

let runTestsPrefName = "selftest"
let runtests =
  Prefs.createBool runTestsPrefName false
    ~category:`Expert
    ~cli_only:true
    "run internal tests and exit"
   ("Run internal tests and exit.  This option is mostly for developers and must be used "
  ^ "carefully: in particular, "
  ^ "it will delete the contents of both roots, so that it can install its own files "
  ^ "for testing.  This flag only makes sense on the command line.  When it is "
  ^ "provided, no preference file is read: all preferences must be specified on the"
  ^ "command line.  Also, since the self-test procedure involves overwriting the roots "
  ^ "and backup directory, the names of the roots and of the backupdir preference "
  ^ "must include the string "
  ^ "\"test\" or else the tests will be aborted.  (If these are not given "
  ^ "on the command line, dummy "
  ^ "subdirectories in the current directory will be created automatically.)")

(* This ref is set to Test.test during initialization, avoiding a circular
   dependency *)
let testFunction = ref (fun () -> assert false)

(**********************************************************************
                         Formatting functions
 **********************************************************************)

(* When no archives were found, we omit 'new' in status descriptions, since
   *all* files would be marked new and this won't make sense to the user. *)
let choose s1 s2 = if !Update.foundArchives then s1 else s2

let showprev =
  Prefs.createBool "showprev" false
    ~category:(`Internal `Devel)
    "*Show previous properties, if they differ from current"
    ""

(* The next function produces nothing unless the "showprev"
   preference is set.  This is because it tends to make the
   output trace too long and annoying. *)
let prevProps newprops ui =
  if not (Prefs.read showprev) then ""
  else match ui with
    NoUpdates | Error _
      -> ""
  | Updates (_, New) ->
      " (new)"
  | Updates (_, Previous(_,oldprops,_,_)) ->
      (* || Props.similar newprops oldprops *)
      " (was: "^(Props.toString oldprops)^")"

let replicaContentDesc rc =
  Props.toString (Props.setLength rc.desc (snd rc.size))

let replicaContent2string rc sep =
  let d s = s ^ sep ^ replicaContentDesc rc ^ prevProps rc.desc rc.ui in
  match rc.typ, rc.status with
    `ABSENT, `Unchanged ->
      "absent"
  | _, `Unchanged ->
      "unchanged "
     ^(Util.truncateString (Fileinfo.type2string rc.typ) 7)
     ^ sep
     ^ replicaContentDesc rc
  | `ABSENT, `Deleted -> "deleted"
  | `FILE, `Created ->
     d (choose "new file         " "file             ")
  | `FILE, `Modified ->
     d "changed file     "
  | `FILE, `PropsChanged ->
     d "changed props    "
  | `SYMLINK, `Created ->
     d (choose "new symlink      " "symlink          ")
  | `SYMLINK, `Modified ->
     d "changed symlink  "
  | `DIRECTORY, `Created ->
     d (choose "new dir          " "dir              ")
  | `DIRECTORY, `Modified ->
     d "changed dir      "
  | `DIRECTORY, `PropsChanged ->
     d "dir props changed"

  (* Some cases that can't happen... *)
  | `ABSENT, (`Created | `Modified | `PropsChanged)
  | `SYMLINK, `PropsChanged
  | (`FILE|`SYMLINK|`DIRECTORY), `Deleted ->
      assert false

let replicaContent2shortString rc =
  match rc.typ, rc.status with
    _, `Unchanged             -> "        "
  | `ABSENT, `Deleted         -> "deleted "
  | `FILE, `Created           -> choose "new file" "file    "
  | `FILE, `Modified          -> "changed "
  | `FILE, `PropsChanged      -> "props   "
  | `SYMLINK, `Created        -> choose "new link" "link    "
  | `SYMLINK, `Modified       -> "chgd lnk"
  | `DIRECTORY, `Created      -> choose "new dir " "dir     "
  | `DIRECTORY, `Modified     -> "chgd dir"
  | `DIRECTORY, `PropsChanged -> "props   "
  (* Cases that can't happen... *)
  | `ABSENT, (`Created | `Modified | `PropsChanged)
  | `SYMLINK, `PropsChanged
  | (`FILE|`SYMLINK|`DIRECTORY), `Deleted
                              -> assert false

let roots2niceStrings length = function
   (Local,fspath1), (Local,fspath2) ->
    let name1, name2 = Fspath.differentSuffix fspath1 fspath2 in
    (Util.truncateString name1 length, Util.truncateString name2 length)
 | (Local,fspath1), (Remote host, fspath2) ->
    (Util.truncateString "local" length, Util.truncateString host length)
 | (Remote host, fspath1), (Local,fspath2) ->
    (Util.truncateString host length, Util.truncateString "local" length)
 | _ -> assert false  (* BOGUS? *)

let details2string theRi sep =
  match theRi.replicas with
    Problem s ->
      Printf.sprintf "Error: %s\n" s
  | Different {rc1 = rc1; rc2 = rc2} ->
      let root1str, root2str =
        roots2niceStrings 12 (Globals.roots()) in
      Printf.sprintf "%s : %s\n%s : %s"
        root1str (replicaContent2string rc1 sep)
        root2str (replicaContent2string rc2 sep)

let displayPath previousPath path =
  let previousNames = Path.toNames previousPath in
  let names = Path.toNames path in
  if names = [] then "/" else
  (* Strip the greatest common prefix of previousNames and names
     from names.  level is the number of names in the greatest
     common prefix. *)
  let rec loop level names1 names2 =
    match (names1,names2) with
      (hd1::tl1,hd2::tl2) ->
        if Name.compare hd1 hd2 = 0
        then loop (level+1) tl1 tl2
        else (level,names2)
    | _ -> (level,names2) in
  let (level,suffixNames) = loop 0 previousNames names in
  let suffixPath =
    Safelist.fold_left Path.child Path.empty suffixNames in
  let spaces = String.make (level*3) ' ' in
  spaces ^ (Path.toString suffixPath)

let roots2string () =
  let replica1, replica2 = roots2niceStrings 12 (Globals.roots()) in
  (Printf.sprintf "%s   %s       " replica1 replica2)

type action = AError | ASkip of bool | ALtoR of bool | ARtoL of bool | AMerge

let direction2action partial dir =
  match dir with
    Conflict _         -> ASkip partial
  | Replica1ToReplica2 -> ALtoR partial
  | Replica2ToReplica1 -> ARtoL partial
  | Merge              -> AMerge

let action2niceString action =
  match action with
    AError      -> "error"
  | ASkip _     -> "<-?->"
  | ALtoR false -> "---->"
  | ALtoR true  -> "--?->"
  | ARtoL false -> "<----"
  | ARtoL true  -> "<-?--"
  | AMerge      -> "<-M->"

let reconItem2stringList oldPath theRI =
  match theRI.replicas with
    Problem s ->
      ("        ", AError, "        ", displayPath oldPath theRI.path1)
  | Different diff ->
      let partial = diff.errors1 <> [] || diff.errors2 <> [] in
      (replicaContent2shortString diff.rc1,
       direction2action partial diff.direction,
       replicaContent2shortString diff.rc2,
       Path.toString theRI.path1)

let reconItem2string oldPath theRI status =
  let (r1, action, r2, path) = reconItem2stringList oldPath theRI in
  Format.sprintf "%s %s %s %s %s" r1 (action2niceString action) r2 status path

let exn2string e =
  match e with
     Sys.Break      -> "Terminated!"
   | Util.Fatal(s)  -> Printf.sprintf "Fatal error: %s" s
   | Util.Transient(s) -> Printf.sprintf "Error: %s" s
   | Unix.Unix_error (err, fun_name, arg) ->
       Printf.sprintf "Uncaught unix error: %s failed%s: %s%s\n%s"
         fun_name
         (if String.length arg > 0 then Format.sprintf " on \"%s\"" arg else "")
         (Unix.error_message err)
         (match err with
            Unix.EUNKNOWNERR n -> Format.sprintf " (code %d)" n
          | _                  -> "")
         (Printexc.get_backtrace ())
   | Invalid_argument s ->
       Printf.sprintf "Invalid argument: %s\n%s" s (Printexc.get_backtrace ())
   | other -> Printf.sprintf "Uncaught exception %s\n%s"
       (Printexc.to_string other) (Printexc.get_backtrace ())

(* precondition: uc = File (Updates(_, ..) on both sides *)
let showDiffs ri printer errprinter id =
  match ri.replicas with
    Problem _ ->
      errprinter
        "Can't diff files: there was a problem during update detection"
  | Different {rc1 = {typ = `FILE; ui = ui1}; rc2 = {typ = `FILE; ui = ui2}} ->
      let (root1,root2) = Globals.roots() in
      begin
        try Files.diff root1 ri.path1 ui1 root2 ri.path2 ui2 printer id
        with Util.Transient e -> errprinter e
      end
  | Different _ ->
      errprinter "Can't diff: path doesn't refer to a file in both replicas"


exception Synch_props of Common.reconItem

(**********************************************************************
                  Common error messages
 **********************************************************************)

let dangerousPathMsg dangerousPaths =
  if dangerousPaths = [Path.empty] then
    "The root of one of the replicas has been completely emptied.\n\
     Unison may delete everything in the other replica.  (Set the \n\
     'confirmbigdel' preference to false to disable this check.)\n"
  else
    Printf.sprintf
      "The following paths have been completely emptied in one replica:\n  \
       %s\n\
       Unison may delete everything below these paths in the other replica.\n
       (Set the 'confirmbigdel' preference to false to disable this check.)\n"
      (String.concat "\n  "
         (Safelist.map (fun p -> "'" ^ (Path.toString p) ^ "'")
            dangerousPaths))

(**********************************************************************
                  Useful patterns for ignoring paths
 **********************************************************************)

let globx_quote s =
  let len = String.length s in
  let buf = Bytes.create (2 * len) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match s.[i] with
      '*' | '?' | '[' | '{' | '}' | ',' | '\\' as c ->
        Bytes.set buf !pos '\\'; Bytes.set buf (!pos + 1) c; pos := !pos + 2
    | c ->
        Bytes.set buf !pos c; pos := !pos + 1
  done;
  "{" ^ Bytes.sub_string buf 0 !pos ^ "}"
let quote =
  let escape_mapSeparator s =
    let sep = Util.trimWhitespace Pred.mapSeparator in
    assert ((String.length sep >= 2) &&
        (sep.[0]='-'||sep.[0]='='||sep.[0]='>'||sep.[0]='<'||sep.[0]='_'));
    let esc = "[" ^ (String.make 1 sep.[0]) ^ "]" ^
              (String.sub sep 1 ((String.length sep)-1)) in
    let rec loop s =
      let e = String.concat esc (Util.splitIntoWordsByString s sep) in
      if e = s then e else loop e in
    loop s in
  fun s -> escape_mapSeparator (globx_quote s)

let ignorePath path = "Path " ^ quote (Path.toString path)

let ignoreName path =
  match Path.finalName path with
    Some name -> "Name " ^ quote (Name.toString name)
  | None      -> assert false

let ignoreExt path =
  match Path.finalName path with
    Some name ->
      let str = Name.toString name in
      begin try
        let pos = String.rindex str '.' in
        let ext = String.sub str pos (String.length str - pos) in
        "Name {,.}*" ^ quote ext
      with Not_found -> (* str does not contain '.' *)
        "Name " ^ quote str
      end
  | None ->
      assert false

let addIgnorePattern theRegExp =
  if theRegExp = "Path " then
    raise (Util.Transient "Can't ignore the root path!");
  Globals.addRegexpToIgnore theRegExp;
  let r = Prefs.add "ignore" theRegExp in
  Trace.status r;
  (* Make sure the server has the same ignored paths (in case, for
     example, we do a "rescan") *)
  Lwt_unix.run (Globals.propagatePrefs ())

(**********************************************************************
                     Statistics for update progress
 **********************************************************************)

(* This seemingly very complex code for calculating the progress rate
   and ETA has partly to do with Unison currently not tracking progress
   very accurately. Several potentially very time-consuming operations
   are not tracked at all: hashing files before and after the copy, for
   example. The entire amount of work may not even be known in advance
   when continuing partial transfers after the previous sync has been
   interrupted. This makes it very difficult to provide meaningful rate
   and ETA information. The code below is the current best approximation.
   The way to simplify this code here is to first and foremost improve
   progress tracking and reporting. *)

module Stats = struct

let calcETA rem rate =
  if Float.is_nan rate || Float.is_nan rem || rem < 0. then "" else
  let t = truncate (rem /. rate +. 0.5) in
  (* Estimating the remaining time is not accurate. Reduce the display
     precision (and reduce more when longer time remaining). *)
  let h, (m, sec) =
    if t >= 3420 then
      let u = t + 180 in u / 3600, (((u mod 3600) / 300) * 5, 0)
    else
      0,
      if t >= 2640 then ((t + 180) / 300) * 5, 0
      else if t >= 1800 then ((t + 119) / 120) * 2, 0
      else if t >= 120 then let u = t + 15 in u / 60, ((u mod 60) / 30) * 30
      else t / 60, t mod 60
  in
  Printf.sprintf "%02d:%02d:%02d" h m sec

let movAvg curr prev ?(c = 1.) deltaTime avgPeriod =
  if Float.is_nan prev then curr else
  let a = c *. Float.min (1. -. exp (-. deltaTime /. avgPeriod)) 1. in
  (* Simplified from a *. curr +. (1. -. a) *. prev *)
  prev +. a *. (curr -. prev)

type t = (* abstract in mli *)
  { mutable t0 : float;
    mutable t : float;
    totalToComplete : int64;
    mutable completed : int64;
    mutable curRate : float;
    mutable avgRateS : float;
    mutable avgRateDoubleSGauss : float;
  }

let gaussC = 2. *. (0.025 ** 2.)
let avgPeriodS = 4.0
let avgPeriodD = 3.5
let calcPeriod = 0.25

let init totalToTransfer =
  let t0 = 0. in
  { t0; t = t0; totalToComplete = Uutil.Filesize.toInt64 totalToTransfer;
    completed = 0L;
    curRate = Float.nan; avgRateS = Float.nan; avgRateDoubleSGauss = Float.nan;
  }

let calcAvgRate' sta totTime deltaCompleted deltaTime =
  let curRate = (Int64.to_float deltaCompleted) /. deltaTime in
  (* We want to ignore small fluctuations but react faster to large
     changes (like switching from cache to disk or from disk to network
     of from receiving to sending or with wildly variable network speed). *)
  let avgRateS = movAvg curRate sta.avgRateS deltaTime
    (Float.min_num totTime avgPeriodS) in
  let cpr = (avgRateS -. sta.avgRateDoubleSGauss) /. sta.avgRateDoubleSGauss in
  let c = 1. -. exp (-.(cpr ** 2.) /. gaussC) in
  let avgRateDoubleSGauss = movAvg avgRateS sta.avgRateDoubleSGauss ~c deltaTime
    (Float.min_num totTime avgPeriodD) in
  sta.curRate <- curRate;
  sta.avgRateS <- avgRateS;
  sta.avgRateDoubleSGauss <- avgRateDoubleSGauss

let update sta t1 totalCompleted =
  let deltaTime = t1 -. sta.t in
  let totalCompleted = Uutil.Filesize.toInt64 totalCompleted in
  if sta.completed = 0L then begin
    (* Skip the very first rate calculation because it will be skewed
       due to (possibly significant) time losses during transport start. *)
    sta.completed <- totalCompleted;
    sta.t0 <- t1;
    sta.t <- t1
  end else if deltaTime >= calcPeriod then begin
    let deltaCompleted = Int64.sub totalCompleted sta.completed in
    sta.completed <- totalCompleted;
    sta.t <- t1;
    calcAvgRate' sta (t1 -. sta.t0) deltaCompleted deltaTime
  end

let curRate sta = sta.curRate
let avgRate1 sta = sta.avgRateS
let avgRate2 sta = sta.avgRateDoubleSGauss
let eta sta ?(rate = sta.avgRateDoubleSGauss) default =
  let rem = Int64.(to_float (sub sta.totalToComplete sta.completed)) in
  let eta = calcETA rem rate in
  if eta = "" then default else eta

end

(**********************************************************************
                          Update propagation
 **********************************************************************)

(* (For context: the threads in question are all cooperating Lwt threads.
   These are not OS threads or parallel running domains. There is no
   preemption and only a single thread is executing at any time.)

   Many (thousands) transport threads can run concurrently and
   independently of each other. All threads run to completion as long as
   there are no errors or only [Util.Transient] exceptions are raised.

   The threads are _not_ guaranteed to run to completion when an exception
   other than [Util.Transient] is raised in any of the threads. This means
   that the threads may not even be able to complete their own cleanup
   code and may leak resources. There must be separate resource cleanup code
   that can be run after the threads have been stopped either forcefully or
   by running to completion.

   When an uncaught exception other than [Util.Transient] is raised in any
   of the threads, the following happens:

     - the thread raising the exception is aborted (all exception handlers
       are run normally, so this thread is expected to run all its cleanup
       code);

     - any threads still waiting to be started are immediately cancelled
       (they will not have run any meaningful code and don't require any
       cleanup);

     - any threads that have already run to completion are not impacted
       in any way;

     - any (sleeping) threads running concurrently with the raising thread
       are stopped, which may have a different meaning depending on where
       in the execution each thread was when it was stopped:

         - some threads will not be able to continue at all (they are
           never woken up);
         - some threads may be woken up and may be able to continue
           running for a short while but may get an error the next time
           when accessing some resource (and may be able to run the error
           handler);
         - some threads may receive an exception and be able to continue
           running for a short while.

     - any exceptions raised in/by threads that are stopped, are ignored;

     - the original exception from the first raising thread is reraised.

   The code run in these thread must _not_ assume that:
     - it will be run to completion;
     - it can run some or all of its cleanup handlers;
     - it will have had a chance to run a certain amount of its code;
     - it will even know that it was stopped.
   Depending on where and how a thread was stopped, it may be collected by
   GC without any additional code ever being run in that thread.


   There is a limit regulating how many threads can be run concurrently in
   the [Transport] module. An attempt is made to not start threads that will
   not be able to run due to this limit. This delayed starting is done for
   two reasons. First, to make the cleanup in case of an uncaught exception
   easier: if the thread was never run then there is nothing to clean up.
   Second, even though the threads themselves are extremely lightweight,
   they still consume some resources and this will add up when the number
   of threads grows to hundreds of thousands and millions.

   Not starting up all threads at once and allowing finished threads to
   be collected by GC as soon as possible can potentially reduce the memory
   requirement by gigabytes. (This is a reference to the old implementation
   that started all threads in one go and kept them all around until all
   were completed. This approach could result in running out of memory when
   syncing large number of updates.) *)

let transportStart () = Transport.logStart ()

let transportFinish () = Transport.logFinish ()

let transportItems items pRiThisRound makeAction =
  let waiter = Lwt.wait () in
  let outstanding = ref 0 in
  let starting () = incr outstanding in
  let completed () =
    decr outstanding;
    if !outstanding = 0 then begin
      try Lwt.wakeup waiter () with Invalid_argument _ -> ()
    end
  in
  let failed e =
    try Lwt.wakeup_exn waiter e with Invalid_argument _ -> ()
  in
  let waitAllCompleted () =
    if !outstanding = 0 then Lwt.return () else waiter
  in

  let im = Array.length items in
  let idx = ref 0 in
  let stopDispense () = idx := im in
  let makeAction' i item =
    Lwt.try_bind
      (fun () -> starting (); makeAction i item)
      (fun () -> completed (); Lwt.return ())
      (fun ex -> stopDispense (); failed ex; Lwt.return ())
  in
  let rec dispenseAction () =
    let i = !idx in
    if i < im then begin
      let item = items.(i) in
      incr idx;
      if pRiThisRound item then
        Some (fun () -> makeAction' i item)
      else
        dispenseAction ()
    end else
      None
  in

  let doTransportFailCleanup () =
    (* Don't start any new threads. *)
    begin try stopDispense () with _ -> () end;
    (* Stop all transfers. *)
    begin try Abort.all () with _ -> () end;
    (* Since we don't know what state the RPC protocol is in, we need
       to close the remote connection to prevent hangs on select(2)
       the next time [Lwt_unix.run] is called.

       This will immediately trigger [on_close] handlers (which will
       do some resource cleanup). *)
    begin try
      match Globals.rootsInCanonicalOrder () with
      | [_; otherRoot] -> Remote.clientCloseRootConnection otherRoot
      | _ -> assert false
    with _ -> () end;
    (* Threads that were still in the middle of execution are just
       discarded and eventually collected by GC. Resources are cleaned
       up and reclaimed by [Remote.at_conn_close] handlers.

       Not all threads are stuck or purged and will continue the next
       time [Lwt_unix.run] is called. Try to finish these threads now.
       This must be done in a loop since each failing thread may raise
       an uncaught exception which will end [Lwt_unix.run]. We don't
       know how many threads can finish this way, so we don't know when
       to stop looping. The limit of concurrent threads is used as an
       approximation (which is probably much more than needed). *)
    let rec loop_yield n () =
      if n = 0 then Lwt.return () else Lwt_unix.yield () >>= loop_yield (n - 1)
    in
    for _ = 1 to Transport.maxThreads () do
      try
        Lwt_unix.run (loop_yield 10 ())
      with _ -> ()
    done
  in

  starting (); (* Count the dispense loop as one of the tasks to complete *)
  Transport.run dispenseAction;
  completed ();
  try
    Lwt_unix.run (waitAllCompleted ())
  with e -> begin
    (* Cleanup procedure must never raise exceptions. Just in case,
       don't shadow the original exception. *)
    let origbt = Printexc.get_raw_backtrace () in
    let () = doTransportFailCleanup () in
    Printexc.raise_with_backtrace e origbt
  end

(**********************************************************************
                   Profile and command-line parsing
 **********************************************************************)

let coreUsageMsg =
   "Usage: " ^ Uutil.myName
 ^ " [options]\n"
 ^ "    or " ^ Uutil.myName
 ^ " root1 root2 [options]\n"
 ^ "    or " ^ Uutil.myName
 ^ " profilename [options]\n"

let shortUsageMsg =
     coreUsageMsg ^ "\n"
   ^ "For a list of options, type \"" ^ Uutil.myName ^ " -help\".\n"
   ^ "For a tutorial on basic usage, type \"" ^ Uutil.myName
   ^ " -doc tutorial\".\n"
   ^ "For other documentation, type \"" ^ Uutil.myName ^ " -doc topics\".\n"

let usageMsg = coreUsageMsg

let debug = Trace.debug "startup"

(* ---- *)

(* Determine the case sensitivity of a root (does filename FOO==foo?) *)
let architecture =
  Remote.registerRootCmd
    "architecture"
    Umarshal.unit
    Umarshal.(prod3 bool bool bool id id)
    (fun (_,()) -> return (Util.osType = `Win32, Osx.isMacOSX, Util.isCygwin))

(* During startup the client determines the case sensitivity of each root.
   If any root is case insensitive, all roots must know this -- it's
   propagated in a pref.  Also, detects HFS (needed for resource forks) and
   Windows (needed for permissions) and does some sanity checking. *)
let validateAndFixupPrefs () =
  Props.validatePrefs();
  Globals.allRootsMap (fun r -> architecture r ()) >>= (fun archs ->
  let someHostIsRunningWindows =
    Safelist.exists (fun (isWin, _, _) -> isWin) archs in
  let allHostsAreRunningWindows =
    Safelist.for_all (fun (isWin, _, _) -> isWin) archs in
  let someHostIsRunningBareWindows =
    Safelist.exists (fun (isWin, _, isCyg) -> isWin && not isCyg) archs in
  let someHostRunningOsX =
    Safelist.exists (fun (_, isOSX, _) -> isOSX) archs in
  let someHostIsCaseInsensitive =
    someHostIsRunningWindows || someHostRunningOsX in
  if Prefs.read Globals.fatFilesystem then begin
    Prefs.overrideDefault Props.permMask 0;
    Prefs.overrideDefault Props.dontChmod true;
    Prefs.overrideDefault Case.caseInsensitiveMode `True;
    Prefs.overrideDefault Fileinfo.allowSymlinks `False;
    Prefs.overrideDefault Fileinfo.ignoreInodeNumbers true
  end;
  Case.init someHostIsCaseInsensitive someHostRunningOsX;
  Props.init someHostIsRunningWindows;
  Osx.init someHostRunningOsX;
  Fileinfo.init someHostIsRunningBareWindows;
  Prefs.set Globals.someHostIsRunningWindows someHostIsRunningWindows;
  Prefs.set Globals.allHostsAreRunningWindows allHostsAreRunningWindows;
  if repeatWatcher () then Prefs.set Fswatch.useWatcher true;
  Features.validateEnabled ();
  return ())

(* ---- *)

type profileInfo = {roots:string list; label:string option; key:string option}

let profileKeymap = Array.make 10 None

let provideProfileKey filename k profile info =
  try
    let i = int_of_string k in
    if 0<=i && i<=9 then
      match profileKeymap.(i) with
        None -> profileKeymap.(i) <- Some(profile,info)
      | Some(otherProfile,_) ->
          raise (Util.Fatal
            ("Error scanning profile "^
                filename ^":\n"
             ^ "shortcut key "^k^" is already bound to profile "
             ^ otherProfile))
    else
      raise (Util.Fatal
        ("Error scanning profile "^ filename ^":\n"
         ^ "Value of 'key' preference must be a single digit (0-9), "
         ^ "not " ^ k))
  with Failure _ -> raise (Util.Fatal
    ("Error scanning profile "^ filename ^":\n"
     ^ "Value of 'key' preference must be a single digit (0-9), "
     ^ "not " ^ k))

let profilesAndRoots = ref []

let scanProfiles () =
  Os.createUnisonDir ();
  Array.iteri (fun i _ -> profileKeymap.(i) <- None) profileKeymap;
  profilesAndRoots :=
    (Safelist.filterMap
       (fun f ->
          let f = Filename.chop_suffix f ".prf" in
          let filename = Prefs.profilePathname f in
          let prefs =
            try Some (Prefs.readAFile f) with
            | Util.Fatal s -> begin
                Util.warn ("Error when reading list of profiles.\n"
                         ^ "Skipping file with error: "
                         ^ filename
                         ^ "\n\n" ^ s);
                None end in
          match prefs with
          | None -> None
          | Some prefs ->
          let fileContents = Safelist.map (fun (_, n, v) -> (n, v)) prefs in
          let roots =
            Safelist.map snd
              (Safelist.filter (fun (n, _) -> n = "root") fileContents) in
          let label =
            try Some(Safelist.assoc "label" fileContents)
            with Not_found -> None in
          let key =
            try Some (Safelist.assoc "key" fileContents)
            with Not_found -> None in
          let info = {roots=roots; label=label; key=key} in
          (* If this profile has a 'key' binding, put it in the keymap *)
          (try
             let k = Safelist.assoc "key" fileContents in
             provideProfileKey filename k f info
           with Not_found -> ());
          Some (f, info))
       (Safelist.filter (fun name -> not (   Util.startswith name ".#"
                                          || Util.startswith name Os.tempFilePrefix))
          (Files.ls Util.unisonDir "*.prf")))

(* ---- *)

let initRoots displayWaitMessage termInteract =
  (* The following step contacts the server, so warn the user it could take
     some time *)
  if not (Prefs.read contactquietly || Prefs.read Trace.terse) then
    displayWaitMessage();

  (* Canonize the names of the roots, sort them (with local roots first),
     and install them in Globals. *)
  Lwt_unix.run (Globals.installRoots termInteract);

  Files.processCommitLogs ();

  Update.storeRootsName ();

  let hasRemote =
    match Globals.rootsInCanonicalOrder () with
    | _ :: (Remote _, _) :: [] -> true
    | _ -> false in
  if
    hasRemote && not (Prefs.read contactquietly || Prefs.read Trace.terse)
  then
    Trace.status (Printf.sprintf "Connected [%s]\n"
      (Util.replacesubstring (Update.getRootsName()) ", " " -> "));

  debug (fun() ->
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
       Printf.eprintf "\n");

  (* Expand any "wildcard" paths [with final component *] *)
  Globals.expandWildcardPaths ();

  Lwt_unix.run
    (validateAndFixupPrefs () >>=
     Globals.propagatePrefs);

  (* Initializes some backups stuff according to the preferences just loaded from the profile.
     Important to do it here, after prefs are propagated, because the function will also be
     run on the server, if any. Also, this should be done each time a profile is reloaded
     on this side, that's why it's here. *)
  Stasher.initBackups ()

(* ---- *)

let makeTempDir pattern =
  let path = Filename.temp_file pattern "" in
  System.unlink path; (* Remove file created by [temp_file]... *)
  System.mkdir path 0o755; (* ... and create a dir instead. *)
  path ^ Filename.dir_sep

let initComplete = ref false

(* Roots given on the command line *)
let cmdLineRawRoots = ref []

let clearClRoots () = cmdLineRawRoots := []

(* BCP: WARNING: Some of the code from here is duplicated in uimacbridge...! *)
let initPrefs ~profileName ~promptForRoots ?(prepDebug = fun () -> ()) () =
  initComplete := false;
  (* Restore prefs to their default values *)
  Prefs.resetToDefaults ();
  (* Clear out any roots left from a previous profile. They can't remain
     hanging around if [initPrefs] for the new profile receives an exception
     before fully completing. *)
  Globals.uninstallRoots ();
  Globals.setRawRoots !cmdLineRawRoots;

  (* Tell the preferences module the name of the profile *)
  Prefs.profileName := Some(profileName);

  (* Check whether the -selftest flag is present on the command line *)
  let testFlagPresent =
    Util.StringMap.mem runTestsPrefName (Prefs.scanCmdLine usageMsg) in

  (* If the -selftest flag is present, then we skip loading the preference file.
     (This is prevents possible confusions where settings from a preference
     file could cause unit tests to fail.) *)
  if not testFlagPresent then begin
    (* If the profile does not exist, create an empty one (this should only
       happen if the profile is 'default', since otherwise we will already
       have checked that the named one exists). *)
    if not(System.file_exists (Prefs.profilePathname profileName)) then
      Prefs.addComment "Unison preferences file";

    (* Load the profile *)
    (debug (fun() -> Util.msg "about to load prefs");
     Prefs.loadTheFile());

    (* Now check again that the -selftest flag has not been set, and barf otherwise *)
    if Prefs.read runtests then raise (Util.Fatal
      "The 'test' flag should only be given on the command line")
  end;

  if Prefs.read Trace.debugmods <> [] then prepDebug ();

  (* Parse the command line.  This will override settings from the profile. *)
  (* JV (6/09): always reparse the command line *)
  if true (*!firstTime*) then begin
    debug (fun() -> Util.msg "about to parse command line");
    Prefs.parseCmdLine usageMsg;
  end;

  (* Turn on GC messages, if the '-debug gc' flag was provided *)
  Gc.set {(Gc.get ()) with Gc.verbose = if Trace.enabled "gc" then 0x3F else 0};

  (* Install dummy roots and backup directory if we are running self-tests *)
  if Prefs.read runtests then begin
    let tmpdir = makeTempDir "unisontest" in
      if Globals.rawRoots() = [] then
        Prefs.loadStrings [
          "root = " ^ tmpdir ^ "a";
          "root = " ^ tmpdir ^ "b";
          "logfile = " ^ tmpdir ^ "unison.log";
        ];
      if (Prefs.read Stasher.backupdir) = "" then
        Prefs.loadStrings [ "backupdir = " ^ tmpdir ^ "backup" ]
  end;

  (* Print the preference settings *)
  debug (fun() -> Prefs.dumpPrefsToStderr() );

  Trace.logonly (Printf.sprintf "\n%s log started at %s\n\n"
    (String.capitalize_ascii Uutil.myNameAndVersion)
    (Util.time2string (Unix.gettimeofday ())));

  (* If no roots are given either on the command line or in the profile,
     ask the user *)
  if Globals.rawRoots() = [] then begin
    (* Ask the user for the roots *)
    match promptForRoots () with
    | None -> raise (Util.Fatal "no roots given on command line or in profile")
    | Some (r1, r2) ->
        begin
          (* Remember them for this run, ordering them so that the first
             will come out on the left in the UI *)
          Globals.setRawRoots [r1; r2];
          (* Save them in the current profile *)
          ignore (Prefs.add "root" r1);
          ignore (Prefs.add "root" r2)
        end
  end;

  Trace.logonly "Roots:\n";
  Globals.rawRoots () |> Safelist.iter
    (fun s -> Trace.logonly "  "; Trace.logonly s; Trace.logonly "\n");
  Trace.logonly "\n";

  (* Parse the roots to validate them *)
  let parsedRoots =
    try Globals.parsedClRawRoots () with
    | Invalid_argument s | Util.Fatal s | Prefs.IllegalValue s ->
        raise (Util.Fatal ("There's a problem with one of the roots:\n" ^ s))
  in

  (* Check to be sure that there is at most one remote root *)
  let numRemote =
    Safelist.fold_left
      (fun n (r : Clroot.clroot) -> match r with
        ConnectLocal _ -> n | ConnectByShell _ | ConnectBySocket _ -> n+1)
      0
      parsedRoots in
      if numRemote > 1 then
        raise(Util.Fatal "cannot synchronize more than one remote root");

  Recon.checkThatPreferredRootIsValid();

  (* If both roots are local, disable the xferhint table to save time *)
  if numRemote = 0 then Prefs.set Xferhint.xferbycopying false;

  (* If no paths were specified, then synchronize the whole replicas *)
  if Prefs.read Globals.paths = [] then Prefs.set Globals.paths [Path.empty];

  initComplete := true

let connectRoots ?termInteract ~displayWaitMessage () =
  let numRoots = Safelist.length (Globals.rawRoots ()) in
  if !initComplete && numRoots > 1 then
  let numConn = ref 0 in
  Lwt_unix.run (Globals.allRootsIter
    (fun r -> if Remote.isRootConnected r then incr numConn; Lwt.return ()));
  if !numConn < numRoots then initRoots displayWaitMessage termInteract

(**********************************************************************
                       Common startup sequence
 **********************************************************************)

let anonymousArgs =
  Prefs.createStringList "rest"
    ~category:(`Internal `Other)
    "*roots or profile name" ""

let testServer =
  Prefs.createBool "testserver" false
    ~category:(`Advanced `Remote)
    ~cli_only:true
    "exit immediately after the connection to the server"
    ("Setting this flag on the command line causes Unison to attempt to "
     ^ "connect to the remote server and, if successful, print a message "
     ^ "and immediately exit.  Useful for debugging installation problems. "
     ^ "Should not be set in preference files.")

(* For backward compatibility *)
let _ = Prefs.alias testServer "testServer"

(* ---- *)

let uiInitClRootsAndProfile ?(prepDebug = fun () -> ()) () =
  (* Make sure we have a directory for archives and profiles *)
  Os.createUnisonDir();

  let args = Prefs.scanCmdLine usageMsg in
  begin
    try if Util.StringMap.find "debug" args <> [] then prepDebug ()
    with Not_found -> ()
  end;

  (* Extract any command line profile or roots *)
  match begin
    try
      match Util.StringMap.find "rest" args with
      | [] -> Ok None
      | [profile] -> Ok (Some profile)
      | [root2;root1] -> Ok (cmdLineRawRoots := [root1;root2]; None)
      | [root2;root1;profile] ->
          Ok (cmdLineRawRoots := [root1;root2]; Some profile)
      | _ ->
          Error (Printf.sprintf
                   "%s was invoked incorrectly (too many roots)" Uutil.myName)
    with Not_found -> Ok None
  end with
  | Error _ as e -> e
  | Ok clprofile ->
      debug (fun () ->
        (* Print header for debugging output *)
        Printf.eprintf "%s, version %s\n\n" Uutil.myName Uutil.myVersion;
        Util.msg "initializing UI";

        (match clprofile with
         | None -> Util.msg "No profile given on command line"
         | Some s -> Printf.eprintf "Profile '%s' given on command line" s);
        (match !cmdLineRawRoots with
         | [] -> Util.msg "No roots given on command line"
         | [root1;root2] ->
             Printf.eprintf "Roots '%s' and '%s' given on command line"
               root1 root2
         | _ -> assert false));

      match clprofile with
      | None when !cmdLineRawRoots = [] ->
          (* Ask the user to choose a profile or create a new one. *)
          Ok None
      | None ->
          (* Roots given on command line. The profile should be the default. *)
          Ok (Some "default")
      | Some n ->
          let f = Prefs.profilePathname n in
          if not (System.file_exists f)
          then Error (Printf.sprintf
                        "Profile '%s' does not exist (looking for file %s)"
                        n f)
          else Ok (Some n)

(* Exit codes *)
let perfectExit = 0   (* when everything's okay *)
let skippyExit  = 1   (* when some items were skipped, but no failure occurred *)
let failedExit  = 2   (* when there's some non-fatal failure *)
let fatalExit   = 3   (* when fatal failure occurred *)
let exitCode = function
    (false, false) -> 0
  | (true, false)  -> 1
  | _              -> 2
(* (anySkipped?, anyFailure?) -> exit code *)
