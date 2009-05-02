(* Unison file synchronizer: src/globals.ml *)
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

let debug = Trace.debug "globals"

(*****************************************************************************)
(*                          ROOTS and PATHS                                  *)
(*****************************************************************************)

let rawroots =
  Prefs.createStringList "root"
    "root of a replica (should be used exactly twice)"
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
      "Wrong number of roots: 2 expected, but %d provided (%s)\n(Maybe you specified roots both on the command line and in the profile?)"
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
  debug (fun () -> Util.msg "Installing roots...");
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

let rec nice_rec i
  : unit Lwt.t =
  if i <= 0 then
    Lwt.return ()
  else
    Lwt_unix.yield() >>= (fun () -> nice_rec (i - 1))

(* [nice r] yields 5 times on local roots [r] to give processes
   corresponding to remote roots a chance to run *)
let nice r =
  if List.exists (fun r -> fst r <> Local) (rootsList ()) && fst r = Local then
    nice_rec 5
  else
    Lwt.return ()

let allRootsIter f =
  Lwt_util.iter
    (fun r -> nice r >>= (fun () -> f r)) (rootsInCanonicalOrder ())

let allRootsIter2 f l =
  let l = Safelist.combine (rootsList ()) l in
  Lwt_util.iter (fun (r, v) -> nice r >>= (fun () -> f r v))
    (Safelist.sort (fun (r, _) (r', _) -> Common.compareRoots r r') l)

let allRootsMap f =
  Lwt_util.map
    (fun r -> nice r >>= (fun () -> f r >>= (fun v -> return (r, v))))
    (rootsInCanonicalOrder ()) >>= (fun l ->
      return (Safelist.map snd (reorderCanonicalListToUsersOrder l)))

let allRootsMapWithWaitingAction f wa =
  Lwt_util.map_with_waiting_action
    (fun r -> nice r >>= (fun () -> f r >>= (fun v -> return (r, v))))
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

let confirmBigDeletes =
  Prefs.createBool "confirmbigdel" true
    "!ask about whole-replica (or path) deletes"
    ("!When this is set to {\\tt true}, Unison will request an extra confirmation if it appears "
     ^ "that the entire replica has been deleted, before propagating the change.  If the {\\tt batch} "
     ^ "flag is also set, synchronization will be aborted.  When the {\\tt path} preference is used, "
     ^ "the same confirmation will be requested for top-level paths.  (At the moment, this flag only "
     ^ "affects the text user interface.)  See also the {\\tt mountpoint} preference.")

let () = Prefs.alias confirmBigDeletes "confirmbigdeletes"

let ignore =
  Pred.create "ignore"
    ("Including the preference \\texttt{-ignore \\ARG{pathspec}} causes Unison to "
     ^ "completely ignore paths that match \\ARG{pathspec} (as well as their "
     ^ "children).  This is useful for avoiding synchronizing temporary "
     ^ "files, object files, etc. The syntax of \\ARG{pathspec} is "
     ^ "described in \\sectionref{pathspec}{Path Specification}, and further "
     ^ "details on ignoring paths is found in"
     ^ " \\sectionref{ignore}{Ignoring Paths}.")
    
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

let addRegexpToIgnore re =
  let oldRE = Pred.extern ignore in
  let newRE = re::oldRE in
  Pred.intern ignore newRE

let merge = 
  Pred.create "merge" ~advanced:true
    ("This preference can be used to run a merge program which will create "
     ^ "a new version for each of the files and the backup, "
     ^ "with the last backup and the both replicas.  Setting the {\\tt merge} "
     ^ "preference for a path will also cause this path to be backed up, "
     ^ "just like {\tt backup}.  "
     ^ "The syntax of \\ARG{pathspec>cmd} is "
     ^ "described in \\sectionref{pathspec}{Path Specification}, and further "
     ^ "details on Merging functions are present in "
     ^ "\\sectionref{merge}{Merging files}.")
        
let shouldMerge p = Pred.test merge (Path.toString p)

let mergeCmdForPath p = Pred.assoc merge (Path.toString p)

let someHostIsRunningWindows =
  Prefs.createBool "someHostIsRunningWindows" false "*" ""

let allHostsAreRunningWindows =
  Prefs.createBool "allHostsAreRunningWindows" false "*" ""
