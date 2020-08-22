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

module type UI =
sig
 val start : interface -> unit
 val defaultUi : interface
end


(**********************************************************************
                             Preferences
 **********************************************************************)

let auto =
  Prefs.createBool "auto" false "automatically accept default (nonconflicting) actions"
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
    "!height (in lines) of main window in graphical interface"
    ("Used to set the height (in lines) of the main window in the graphical "
     ^ "user interface.")

let expert =
  Prefs.createBool "expert" false
    "*Enable some developers-only functionality in the UI" ""

let profileLabel =
  Prefs.createString "label" ""
    "!provide a descriptive string label for this profile"
    ("Used in a profile to provide a descriptive string documenting its "
     ^ "settings.  (This is useful for users that switch between several "
     ^ "profiles, especially using the `fast switch' feature of the "
     ^ "graphical user interface.)")

let profileKey =
  Prefs.createString "key" ""
    "!define a keyboard shortcut for this profile (in some UIs)"
    ("Used in a profile to define a numeric key (0-9) that can be used in "
     ^ "the graphical user interface to switch immediately to this profile.")
(* This preference is not actually referred to in the code anywhere, since
   the keyboard shortcuts are constructed by a separate scan of the preference
   file in uigtk.ml, but it must be present to prevent the preferences module
   from complaining about 'key = n' lines in profiles. *)

let contactquietly =
  Prefs.createBool "contactquietly" false
    "!suppress the 'contacting server' message during startup"
    ("If this flag is set, Unison will skip displaying the "
     ^ "`Contacting server' message (which some users find annoying) "
     ^ "during startup.")

let contactingServerMsg () =
  Printf.sprintf "Unison %s: Contacting server..." Uutil.myVersion 

let repeat =
  Prefs.createString "repeat" ""
    "!synchronize repeatedly (text interface only)"
    ("Setting this preference causes the text-mode interface to synchronize "
     ^ "repeatedly, rather than doing it just once and stopping.  If the "
     ^ "argument is a number, Unison will pause for that many seconds before "
     ^ "beginning again. When the argument is \\verb|watch|, Unison relies on "
     ^ "an external file monitoring process to synchronize whenever a change "
     ^ "happens.")

let retry =
  Prefs.createInt "retry" 0
    "!re-try failed synchronizations N times (text ui only)"
    ("Setting this preference causes the text-mode interface to try again "
     ^ "to synchronize "
     ^ "updated paths where synchronization fails.  Each such path will be "
     ^ "tried N times."
    )

let confirmmerge =
  Prefs.createBool "confirmmerge" false
    "!ask for confirmation before committing results of a merge"
    ("Setting this preference causes both the text and graphical interfaces"
     ^ " to ask the user if the results of a merge command may be committed "
     ^ " to the replica or not. Since the merge command works on temporary files,"
     ^ " the user can then cancel all the effects of applying the merge if it"
     ^ " turns out that the result is not satisfactory.  In "
     ^ " batch-mode, this preference has no effect.  Default is false.")

let runTestsPrefName = "selftest"
let runtests =
  Prefs.createBool runTestsPrefName false
    "!run internal tests and exit"
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

(*FIX: remove when Unison version > 2.40 *)
let _ =
fun r x -> Remote.registerRootCmd "_unicodeCaseSensitive_" (fun _ -> Lwt.return ()) r x
let supportUnicodeCaseSensitive () =
  if Uutil.myMajorVersion > "2.40" (* The test is correct until 2.99... *) then
    Lwt.return true
  else begin
    Globals.allRootsMap
      (fun r -> Remote.commandAvailable r "_unicodeCaseSensitive_")
    >>= fun l ->
    Lwt.return (List.for_all (fun x -> x) l)
  end

(* Determine the case sensitivity of a root (does filename FOO==foo?) *)
let architecture =
  Remote.registerRootCmd
    "architecture"
    (fun (_,()) -> return (Util.osType = `Win32, Osx.isMacOSX, Util.isCygwin))

(* During startup the client determines the case sensitivity of each root.
   If any root is case insensitive, all roots must know this -- it's
   propagated in a pref.  Also, detects HFS (needed for resource forks) and
   Windows (needed for permissions) and does some sanity checking. *)
let validateAndFixupPrefs () =
  Props.validatePrefs();
  let supportUnicodeCaseSensitive = supportUnicodeCaseSensitive () in
  Globals.allRootsMap (fun r -> architecture r ()) >>= (fun archs ->
  supportUnicodeCaseSensitive >>= fun unicodeCS ->
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
  Case.init someHostIsCaseInsensitive (someHostRunningOsX && unicodeCS);
  Props.init someHostIsRunningWindows;
  Osx.init someHostRunningOsX;
  Fileinfo.init someHostIsRunningBareWindows;
  Prefs.set Globals.someHostIsRunningWindows someHostIsRunningWindows;
  Prefs.set Globals.allHostsAreRunningWindows allHostsAreRunningWindows;
  return ())

(* ---- *)

let promptForRoots getFirstRoot getSecondRoot =
  (* Ask the user for the roots *)
  let r1 = match getFirstRoot() with None -> exit 0 | Some r -> r in
  let r2 = match getSecondRoot() with None -> exit 0 | Some r -> r in
  (* Remember them for this run, ordering them so that the first
     will come out on the left in the UI *)
  Globals.setRawRoots [r1; r2];
  (* Save them in the current profile *)
  ignore (Prefs.add "root" r1);
  ignore (Prefs.add "root" r2)

(* ---- *)

let makeTempDir pattern =
  let ic = Unix.open_process_in (Printf.sprintf "(mktemp --tmpdir -d %s.XXXXXX || mktemp -d -t %s) 2>/dev/null" pattern pattern) in
  let path = input_line ic in
  ignore (Unix.close_process_in ic);
  path

(* The first time we load preferences, we also read the command line
   arguments; if we re-load prefs (because the user selected a new profile)
   we ignore the command line *)
let firstTime = ref(true)

(* Roots given on the command line *)
let cmdLineRawRoots = ref []

(* BCP: WARNING: Some of the code from here is duplicated in uimacbridge...! *)
let initPrefs ~profileName ~displayWaitMessage ~getFirstRoot ~getSecondRoot
              ~termInteract =
  (* Restore prefs to their default values, if necessary *)
  if not !firstTime then Prefs.resetToDefaults();
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

  (* Parse the command line.  This will override settings from the profile. *)
  (* JV (6/09): always reparse the command line *)
  if true (*!firstTime*) then begin
    debug (fun() -> Util.msg "about to parse command line");
    Prefs.parseCmdLine usageMsg;
  end;

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

  (* If no roots are given either on the command line or in the profile,
     ask the user *)
  if Globals.rawRoots() = [] then begin
    promptForRoots getFirstRoot getSecondRoot;
  end;

  Recon.checkThatPreferredRootIsValid();

  (* The following step contacts the server, so warn the user it could take
     some time *)
  if not (Prefs.read contactquietly || Prefs.read Trace.terse) then
    displayWaitMessage();

  (* Canonize the names of the roots, sort them (with local roots first),
     and install them in Globals. *)
  Lwt_unix.run (Globals.installRoots termInteract);

  (* If both roots are local, disable the xferhint table to save time *)
  begin match Globals.roots() with
    ((Local,_),(Local,_)) -> Prefs.set Xferhint.xferbycopying false
  | _ -> ()
  end;

  (* FIX: This should be before Globals.installRoots *)
  (* Check to be sure that there is at most one remote root *)
  let numRemote =
    Safelist.fold_left
      (fun n (w,_) -> match w with Local -> n | Remote _ -> n+1)
      0
      (Globals.rootsList()) in
      if numRemote > 1 then
        raise(Util.Fatal "cannot synchronize more than one remote root");

  (* If no paths were specified, then synchronize the whole replicas *)
  if Prefs.read Globals.paths = [] then Prefs.set Globals.paths [Path.empty];

  (* Expand any "wildcard" paths [with final component *] *)
  Globals.expandWildcardPaths();

  Update.storeRootsName ();

  if
    numRemote > 0 && not (Prefs.read contactquietly || Prefs.read Trace.terse)
  then
    Util.msg "Connected [%s]\n"
      (Util.replacesubstring (Update.getRootsName()) ", " " -> ");

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

  Lwt_unix.run
    (validateAndFixupPrefs () >>=
     Globals.propagatePrefs);

  (* Initializes some backups stuff according to the preferences just loaded from the profile.
     Important to do it here, after prefs are propagated, because the function will also be
     run on the server, if any. Also, this should be done each time a profile is reloaded
     on this side, that's why it's here. *)
  Stasher.initBackups ();

  firstTime := false

(**********************************************************************
                       Common startup sequence
 **********************************************************************)

let anonymousArgs =
  Prefs.createStringList "rest"
    "*roots or profile name" ""

let testServer =
  Prefs.createBool "testserver" false
    "exit immediately after the connection to the server"
    ("Setting this flag on the command line causes Unison to attempt to "
     ^ "connect to the remote server and, if successful, print a message "
     ^ "and immediately exit.  Useful for debugging installation problems. "
     ^ "Should not be set in preference files.")

(* For backward compatibility *)
let _ = Prefs.alias testServer "testServer"

(* ---- *)

let uiInit
    ~(reportError : string -> unit)
    ~(tryAgainOrQuit : string -> bool)
    ~(displayWaitMessage : unit -> unit)
    ~(getProfile : unit -> string option)
    ~(getFirstRoot : unit -> string option)
    ~(getSecondRoot : unit -> string option)
    ~(termInteract : (string -> string -> string) option) =

  (* Make sure we have a directory for archives and profiles *)
  Os.createUnisonDir();

  (* Extract any command line profile or roots *)
  let clprofile = ref None in
  begin
    try
      let args = Prefs.scanCmdLine usageMsg in
      match Util.StringMap.find "rest" args with
        [] -> ()
      | [profile] -> clprofile := Some profile
      | [root2;root1] -> cmdLineRawRoots := [root1;root2]
      | [root2;root1;profile] ->
          cmdLineRawRoots := [root1;root2];
          clprofile := Some profile
      | _ ->
          (reportError(Printf.sprintf
             "%s was invoked incorrectly (too many roots)" Uutil.myName);
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
    (match !cmdLineRawRoots with
      [] -> Util.msg "No roots given on command line"
    | [root1;root2] ->
        Printf.eprintf "Roots '%s' and '%s' given on command line"
          root1 root2
    | _ -> assert false));

  let profileName =
    begin match !clprofile with
      None ->
        let clroots_given = !cmdLineRawRoots <> [] in
        let n =
          if not(clroots_given) then begin
            (* Ask the user to choose a profile or create a new one. *)
            clprofile := getProfile();
            match !clprofile with
              None -> exit 0 (* None means the user wants to quit *)
            | Some x -> x
          end else begin
            (* Roots given on command line.
               The profile should be the default. *)
            clprofile := Some "default";
            "default"
          end in
        n
    | Some n ->
        let f = Prefs.profilePathname n in
        if not(System.file_exists f)
        then (reportError (Printf.sprintf "Profile %s does not exist"
                             (System.fspathToPrintString f));
              exit 1);
        n
    end in

  (* Load the profile and command-line arguments *)
  initPrefs
    profileName displayWaitMessage getFirstRoot getSecondRoot termInteract;

  (* Turn on GC messages, if the '-debug gc' flag was provided *)
  if Trace.enabled "gc" then Gc.set {(Gc.get ()) with Gc.verbose = 0x3F};

  if Prefs.read testServer then exit 0;

  (* BCPFIX: Should/can this be done earlier?? *)
  Files.processCommitLogs();

  (* Run unit tests if requested *)
  if Prefs.read runtests then begin
    (!testFunction)();
    exit 0
  end

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
