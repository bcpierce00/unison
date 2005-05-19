(* $I1: Unison file synchronizer: src/globals.ml $ *)
(* $I2: Last modified by bcpierce on Mon, 06 Sep 2004 14:48:05 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

open Common

let debug = Trace.debug "globals"

(*****************************************************************************)
(*                          ROOTS and PATHS                                  *)
(*****************************************************************************)

let rawroots =
  Prefs.createStringList "root"
    "root of a replica"
    ("Each use of this preference names the root of one of the replicas "
     ^ "for Unison to synchronize.  Exactly two roots are needed, so normal "
     ^ "modes of usage are either to give two values for \\verb|root| in the "
     ^ "profile, or to give no values in the profile and provide two "
     ^ "on the command line.  "
     ^ "Details of the syntax of roots can be found in "
     ^ "\\sectionref{roots}{Roots}.\n\n"
     ^ "The two roots can be given in either order; Unison will sort them "
     ^ "into a canonical order before doing anything else.  It also tries to "
     ^ "`canonize' the machine names and paths that appear in the roots, so "
     ^ "that, if Unison is invoked later with a slightly different name "
     ^ "for the same root, it will be able to locate the correct archives.")

let setRawRoots l =
  Prefs.set rawroots l

let rawRoots () = Prefs.read rawroots

let rootsInitialName () =
  match rawRoots () with
    [r2; r1] -> (r1, r2)
  | _        -> assert false

let theroots = ref []

open Lwt
let installRoots termInteract =
  let roots = rawRoots () in
  if Safelist.length roots <> 2 then
    raise (Util.Fatal (Printf.sprintf
      "Wrong number of roots: 2 expected, but %d provided (%s)\n(Maybe you gave roots both on the command line and in the profile?)"
      (Safelist.length roots)
      (String.concat ", " roots) ));
  Safelist.fold_right
    (fun r cont ->
       Remote.canonizeRoot r (Clroot.parseRoot r) termInteract
       >>= (fun r' ->
       cont >>= (fun l ->
       return (r' :: l))))
    roots (return []) >>= (fun roots' ->
  theroots := Safelist.rev roots';
  return ())

(* Alternate interface, should replace old interface eventually *)
let installRoots2 () =
  let roots = rawRoots () in
  theroots :=
    Safelist.map Remote.canonize ((Safelist.map Clroot.parseRoot) roots);
  theroots := Safelist.rev !theroots (* Not sure why this is needed... *)
  
let roots () =
  match !theroots with
    [root1;root2] -> (root1,root2)
  | _ -> assert false

let rootsList() = !theroots

let rootsInCanonicalOrder() = Common.sortRoots (!theroots)

let reorderCanonicalListToUsersOrder l =
  if rootsList() = rootsInCanonicalOrder() then l
  else Safelist.rev l

let allRootsIter f =
  Lwt_util.iter f (rootsInCanonicalOrder ())

let allRootsIter2 f l =
  let l = Safelist.combine (rootsList ()) l in
  Lwt_util.iter (fun (r, v) -> f r v)
    (Safelist.sort (fun (r, _) (r', _) -> Common.compareRoots r r') l)

let allRootsMap f =
  Lwt_util.map (fun r -> f r >>= (fun v -> return (r, v)))
    (rootsInCanonicalOrder ()) >>= (fun l ->
      return (Safelist.map snd (reorderCanonicalListToUsersOrder l)))
    
let allRootsMapWithWaitingAction f wa =
  Lwt_util.map_with_waiting_action 
    (fun r -> (f r) >>= (fun v -> return (r, v)))
    (fun r -> wa r) 
    (rootsInCanonicalOrder ()) >>= (fun l ->
      return (Safelist.map snd (reorderCanonicalListToUsersOrder l)))
    
let replicaHostnames () =
  Safelist.map 
    (function (Local, _) -> ""
            | (Remote h,_) -> h)
    (rootsList())

let allHostsIter f =
  let rec iter l =
    match l with
      [] ->
        return ()
    | root :: rem ->
        f root >>= (fun () ->
        iter rem)
  in
  iter (replicaHostnames ())

let allHostsMap f = Safelist.map f (replicaHostnames())

let paths =
  Prefs.create "path" []
    "path to synchronize"
    ("When no \\verb|path| preference is given, Unison will simply synchronize "
     ^ "the two entire replicas, beginning from the given pair of roots.  "
     ^ "If one or more \\verb|path| preferences are given, then Unison will "
     ^ "synchronize only these paths and their children.  (This is useful "
     ^ "for doing a fast sync of just one directory, for example.)  "
     ^ "Note that {\\tt path} preferences are intepreted literally---they "
     ^ "are not regular expressions.")
    (fun oldpaths string -> Safelist.append oldpaths [Path.fromString string])
    (fun l -> Safelist.map Path.toString l)

(* FIX: this does weird things in case-insensitive mode... *)
let globPath lr p =
  let p = Path.magic p in
  debug (fun() ->
    Util.msg "Checking path '%s' for expansions\n"
      (Path.toDebugString p) );
  match Path.deconstructRev p with
    Some(n,parent) when (Name.toString n = "*") -> begin
      debug (fun() -> Util.msg "Expanding path %s\n" (Path.toString p));
      match lr with
        None -> raise (Util.Fatal (Printf.sprintf
                  "Path %s ends with *, %s"
                  (Path.toString p)
                  "but first root (after canonizing) is non-local"))
      | Some lrfspath -> 
          Safelist.map (fun c -> Path.magic' (Path.child parent c))
            (Os.childrenOf lrfspath parent)
      end 
  | _ -> [Path.magic' p]

let expandWildcardPaths() =
  let lr =
    match rootsInCanonicalOrder() with
      [(Local, fspath); _] -> Some fspath
    | _ -> None in
  Prefs.set paths 
    (Safelist.flatten_map (globPath lr) (Prefs.read paths))

(*****************************************************************************)
(*                         PROPAGATION OF PREFERENCES                        *)
(*****************************************************************************)

let propagatePrefsTo =
  Remote.registerHostCmd
    "installPrefs"
    (fun prefs -> return (Prefs.load prefs))

let propagatePrefs () =
  let prefs = Prefs.dump() in
  let toHost root =
    match root with
      (Local, _) -> return ()
    | (Remote host,_) ->
        propagatePrefsTo host prefs
  in
  allRootsIter toHost

(*****************************************************************************)
(*                      PREFERENCES AND PREDICATES                           *)
(*****************************************************************************)

let batch =
  Prefs.createBool "batch" false "batch mode: ask no questions at all"
    ("When this is set to {\\tt true}, the user "
     ^ "interface will ask no questions at all.  Non-conflicting changes "
     ^ "will be propagated; conflicts will be skipped.")

let ignore =
  Pred.create "ignore"
    ("Including the preference \\texttt{-ignore \\ARG{pathspec}} causes Unison to "
     ^ "completely ignore paths that match \\ARG{pathspec} (as well as their "
     ^ "children).  This is useful for avoiding synchronizing temporary "
     ^ "files, object files, etc. The syntax of \\ARG{pathspec} is "
     ^ "described in \\sectionref{pathspec}{Path Specification}, and further "
     ^ "details on ignoring paths is found in \\sectionref{ignore}{Ignoring Paths}.")
    
let ignorenot =
  Pred.create "ignorenot"
    ("This preference overrides the preference \\texttt{ignore}. 
      It gives a list of patterns 
     (in the same format as 
     \\verb|ignore|) for paths that should definitely {\\em not} be ignored, 
     whether or not they happen to match one of the \\verb|ignore| patterns.
     \\par Note that the semantics of {\\tt ignore} and {\\tt ignorenot} is a
     little counter-intuitive.  When detecting updates, Unison examines
     paths in depth-first order, starting from the roots of the replicas
     and working downwards.  Before examining each path, it checks whether
     it matches {\\tt ignore} and does not match {\\tt ignorenot}; in this case
     it skips this path {\\em and all its descendants}.  This means that,
     if some parent of a given path matches an {\\tt ignore} pattern, then 
     it will be skipped even if the path itself matches an {\\tt ignorenot}
     pattern.  In particular, putting {\\tt ignore = Path *} in your profile
     and then using {\tt ignorenot} to select particular paths to be 
     synchronized will not work.  Instead, you should use the {\\tt path}
     preference to choose particular paths to synchronize.")
    
let shouldIgnore p =
  let p = Path.toString p in
  (Pred.test ignore p) && not (Pred.test ignorenot p) 

let merge = 
  Pred.create "merge"
    ("This preference can be used to run a merge program which will create "
     ^ "a new version for each of the files and the backup, "
     ^ "with the last backup and the both replicas.  Setting the {\\tt merge} "
     ^ "preference for a path will also cause this path to be backed up, "
     ^ "just like {\tt backup}.  "
     ^ "The syntax of \\ARG{pathspec>cmd} is "
     ^ "described in \\sectionref{pathspec}{Path Specification}, and further "
     ^ "details on Merging functions are present in "
     ^ "\\sectionref{merge}{Merging files}.")
        
let mergebatch = 
  Pred.create "mergebatch"
    ("Normally, when Unison is run with the {\\tt batch} flag set to true, it does not "
     ^ "invoke any external merge programs.  To tell it that a given file can be merged "
     ^ "even when in batch mode, use the {\\tt mergebatch} preference instead of "
     ^ "{\\tt merge}.  When running in non-batch mode, the {\\tt merge} preference is used "
     ^ "instead of {\\tt mergebatch} if both are specified for a given path.")
        
let shouldMerge p =
     Pred.test mergebatch (Path.toString p)
  || (not (Prefs.read batch) && Pred.test merge (Path.toString p))

let mergeCmdForPath p =
  if Prefs.read batch then
    Pred.assoc mergebatch (Path.toString p)
  else
    try Pred.assoc merge (Path.toString p)
    with Not_found -> Pred.assoc mergebatch (Path.toString p)

let backup =
   Pred.create "backup"
   ("Including the preference \\texttt{-backup \\ARG{pathspec}} "
   ^ "causes Unison to make back up for each path that matches "
   ^ "\\ARG{pathspec}.  More precisely, for each path that "
   ^ "matches this \\ARG{pathspec}, "
   ^ "Unison will keep several old versions of a file as a backup whenever "
   ^ "a change is propagated.  These backup files are left in the "
   ^ "directory specified by the environment variable {\\tt UNISONBACKUPDIR}, "
    ^ "if it is set; otherwise in the directory named by the {\\tt backupdir} "
    ^ "preference, if it is non-null; otherwise in "
   ^ " \\verb|.unison/backup/| by default.  The newest backed up copy will"
   ^ "have the same name as the original; older versions will be named "
   ^ "with extensions \\verb|.n.unibck|."
   ^ " The number of versions that are kept is determined by the "
   ^ "\\verb|maxbackups| preference."
   ^ "\n\n The syntax of \\ARG{pathspec} is described in \\sectionref{pathspec}{Path Specification}.")

let _ = Pred.alias backup "mirror"

let backupnot =
   Pred.create "backupnot"
   ("The values of this preference specify paths or individual files or"
    ^ " regular expressions that should {\\em not} "
    ^ "be backed up, even if the {\\tt backup} preference selects "
    ^ "them---i.e., "
    ^ "it selectively overrides {\\tt backup}.  The same caveats apply here "
    ^ "as with {\\tt ignore} and {\tt ignorenot}.")

let shouldBackup p =
  let s = (Path.toString p) in
  (Pred.test backup s && not (Pred.test backupnot s))

    
