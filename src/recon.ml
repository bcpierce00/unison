(* Unison file synchronizer: src/recon.ml *)
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

(* ------------------------------------------------------------------------- *)
(*                     Handling of prefer/force                              *)
(* ------------------------------------------------------------------------- *)
let debug = Trace.debug "recon"

let setDirection ri dir force =
  match ri.replicas with
    Different(rc1,rc2,d,default) when force=`Force || default=Conflict ->
      if dir=`Replica1ToReplica2 then
        d := Replica1ToReplica2
      else if dir=`Replica2ToReplica1 then
        d := Replica2ToReplica1
      else if dir=`Merge then
        if Globals.shouldMerge ri.path then d := Merge else () 
      else  (* dir = `Older or dir = `Newer *)
        let (_,s1,p1,_) = rc1 in
        let (_,s2,p2,_) = rc2 in
        if s1<>`Deleted && s2<>`Deleted then begin
          let comp = (Props.time p1) -. (Props.time p2) in
          let comp = if dir=`Newer then -. comp else comp in
          if comp = 0.0 then
            ()
          else if comp<0.0 then
            d := Replica1ToReplica2
          else
            d := Replica2ToReplica1
        end else if s1=`Deleted && dir=`Newer then begin
          d := Replica2ToReplica1
        end else if s2=`Deleted && dir=`Newer then begin
          d := Replica1ToReplica2
        end
  | _ ->
      ()

let revertToDefaultDirection ri =
  match ri.replicas with
    Different(_,_,d,default) ->
      d := default
  | _ ->
      ()

(* Find out which direction we need to propagate changes if we want to       *)
(* consider the given root to be the "truth"                                 *)
(* --                                                                        *)
(*   root := "older" | "newer" | <one of the two roots>                      *)
(*   return value := 'Older  | 'Newer  | 'Replica1ToReplica2 |               *)
(*                   'Replica2ToReplica1                                     *)
(* --                                                                        *)
let root2direction root =
  if      root="older" then `Older
  else if root="newer" then `Newer
  else
    let roots = Safelist.rev (Globals.rawRoots()) in
    let r1 = Safelist.nth roots 0 in
    let r2 = Safelist.nth roots 1 in
    debug (fun() ->
       Printf.eprintf "root2direction called to choose %s from %s and %s\n"
         root r1 r2);
    if r1 = root then `Replica1ToReplica2 else
    if r2 = root then `Replica2ToReplica1 else
    raise (Util.Fatal (Printf.sprintf
     "%s (given as argument to 'prefer' or 'force' preference)\nis not one of \
      the current roots:\n  %s\n  %s" root r1 r2))

let forceRoot: string Prefs.t =
  Prefs.createString "force" ""
    "force changes from this replica to the other"
    ("Including the preference \\texttt{-force \\ARG{root}} causes Unison to "
     ^ "resolve all differences (even non-conflicting changes) in favor of "
     ^ "\\ARG{root}.  "
     ^ "This effectively changes Unison from a synchronizer into a mirroring "
     ^ "utility.  \n\n"
     ^ "You can also specify \\verb|-force newer| (or \\verb|-force older|) "
     ^ "to force Unison to choose the file with the later (earlier) "
     ^ "modtime.  In this case, the \\verb|-times| preference must also "
     ^ "be enabled.\n\n"
     ^ "This preference is overridden by the \\verb|forcepartial| preference.\n\n"
     ^ "This preference should be used only if you are {\\em sure} you "
     ^ "know what you are doing!")

let forceRootPartial: Pred.t =
  Pred.create "forcepartial" ~advanced:true
    ("Including the preference \\texttt{forcepartial \\ARG{PATHSPEC} -> \\ARG{root}} causes Unison to "
     ^ "resolve all differences (even non-conflicting changes) in favor of "
     ^ "\\ARG{root} for the files in \\ARG{PATHSPEC} (see \\sectionref{pathspec}{Path Specification} "
     ^ "for more information).  "
     ^ "This effectively changes Unison from a synchronizer into a mirroring "
     ^ "utility.  \n\n"
     ^ "You can also specify \\verb|forcepartial PATHSPEC -> newer| "
     ^ "(or \\verb|forcepartial PATHSPEC older|) "
     ^ "to force Unison to choose the file with the later (earlier) "
     ^ "modtime.  In this case, the \\verb|-times| preference must also "
     ^ "be enabled.\n\n"
     ^ "This preference should be used only if you are {\\em sure} you "
     ^ "know what you are doing!")

let preferRoot: string Prefs.t =
  Prefs.createString "prefer" ""
    "choose this replica's version for conflicting changes"
    ("Including the preference \\texttt{-prefer \\ARG{root}} causes Unison always to "
     ^ "resolve conflicts in favor of \\ARG{root}, rather than asking for "
     ^ "guidance from the user.  (The syntax of \\ARG{root} is the same as "
     ^ "for the \\verb|root| preference, plus the special values "
     ^ "\\verb|newer| and \\verb|older|.)  \n\n"
     ^ "This preference is overridden by the \\verb|preferpartial| preference.\n\n"
     ^ "This preference should be used only if you are {\\em sure} you "
     ^ "know what you are doing!")

let preferRootPartial: Pred.t =
  Pred.create "preferpartial" ~advanced:true
    ("Including the preference \\texttt{preferpartial \\ARG{PATHSPEC} -> \\ARG{root}} "
     ^ "causes Unison always to "
     ^ "resolve conflicts in favor of \\ARG{root}, rather than asking for "
     ^ "guidance from the user, for the files in \\ARG{PATHSPEC} (see "
     ^ "\\sectionref{pathspec}{Path Specification} "
     ^ "for more information).  (The syntax of \\ARG{root} is the same as "
     ^ "for the \\verb|root| preference, plus the special values "
     ^ "\\verb|newer| and \\verb|older|.)  \n\n"
     ^ "This preference should be used only if you are {\\em sure} you "
     ^ "know what you are doing!")

(* [lookupPreferredRoot (): string * [`Force | `Prefer]] checks validity of  *)
(* preferences "force"/"preference", returns a pair (root, force)            *)
let lookupPreferredRoot () =
  if Prefs.read forceRoot <> "" then
    (Prefs.read forceRoot, `Force)
  else if Prefs.read preferRoot <> "" then
    (Prefs.read preferRoot, `Prefer)
  else
    ("",`Prefer)

(* [lookupPreferredRootPartial: Path.t -> string * [`Force | `Prefer]] checks validity of  *)
(* preferences "forcepartial", returns a pair (root, force)                                *)
let lookupPreferredRootPartial p =
  let s = Path.toString p in
  if Pred.test forceRootPartial s then
    (Pred.assoc forceRootPartial s, `Force)
  else if Pred.test preferRootPartial s then
    (Pred.assoc preferRootPartial s, `Prefer)
  else
    ("",`Prefer)

(* Use the current values of the '-prefer <ROOT>' and '-force <ROOT>'        *)
(* preferences to override the reconciler's choices                          *)
let overrideReconcilerChoices ris =
  let (root,force) = lookupPreferredRoot() in
  if root<>"" then begin
    let dir = root2direction root in
    Safelist.iter (fun ri -> setDirection ri dir force) ris
  end;
  Safelist.iter (fun ri ->
                   let (rootp,forcep) = lookupPreferredRootPartial ri.path in
                   if rootp<>"" then begin
                     let dir = root2direction rootp in
                       setDirection ri dir forcep
                   end) ris

(* Look up the preferred root and verify that it is OK (this is called at    *)
(* the beginning of the run, so that we don't have to wait to hear about     *)
(* errors                                                                    *)
(* This should also check for the partial version, but this needs a way to   *)
(* extract the associated values from a Pred.t                               *)
let checkThatPreferredRootIsValid () =
  let test_root predname = function
    | "" -> ()
    | ("newer" | "older") as r -> 
        if not (Prefs.read Props.syncModtimes) then
          raise (Util.Transient (Printf.sprintf
                                   "The '%s=%s' preference can only be used with 'times=true'"
                                   predname r))
    | r -> ignore (root2direction r) in
  let (root,pred) = lookupPreferredRoot() in
  if root<>"" then test_root (match pred with `Force -> "force" | `Prefer -> "prefer") root;
  Safelist.iter (test_root "forcepartial") (Pred.extern_associated_strings forceRootPartial);
  Safelist.iter (test_root "preferpartial") (Pred.extern_associated_strings preferRootPartial)

(* ------------------------------------------------------------------------- *)
(*                    Main Reconciliation stuff                              *)
(* ------------------------------------------------------------------------- *)

exception UpdateError of string

let rec checkForError ui =
  match ui with
    NoUpdates ->
      ()
  | Error err ->
      raise (UpdateError err)
  | Updates (uc, _) ->
      match uc with
        Dir (_, children, _, _) ->
          Safelist.iter (fun (_, uiSub) -> checkForError uiSub) children
      | Absent | File _ | Symlink _ ->
          ()

(* lifting errors in individual updates to replica problems                  *)
let propagateErrors (rplc: Common.replicas): Common.replicas =
  match rplc with
    Problem _ ->
      rplc
  | Different ((_, _, _, ui1), (_, _, _, ui2), _, _) ->
      try
        checkForError ui1;
        try
          checkForError ui2;
          rplc
        with UpdateError err ->
          Problem ("[root 2]: " ^ err)
      with UpdateError err ->
        Problem ("[root 1]: " ^ err)

type singleUpdate = Rep1Updated | Rep2Updated

let update2replicaContent (conflict: bool) ui ucNew oldType:
    Common.replicaContent =
  match ucNew with
    Absent ->
      (`ABSENT, `Deleted, Props.dummy, ui)
  | File (desc, ContentsSame) ->
      (`FILE, `PropsChanged, desc, ui)
  | File (desc, _) when oldType <> `FILE ->
      (`FILE, `Created, desc, ui)
  | File (desc, ContentsUpdated _) ->
      (`FILE, `Modified, desc, ui)
  | Symlink l when oldType <> `SYMLINK ->
      (`SYMLINK, `Created, Props.dummy, ui)
  | Symlink l ->
      (`SYMLINK, `Modified, Props.dummy, ui)
  | Dir (desc, _, _, _) when oldType <> `DIRECTORY ->
      (`DIRECTORY, `Created, desc, ui)
  | Dir (desc, _, PropsUpdated, _) ->
      (`DIRECTORY, `PropsChanged, desc, ui)
  | Dir (desc, _, PropsSame, _) when conflict ->
      (* Special case: the directory contents has been modified and the      *)
      (* directory is in conflict.  (We don't want to display a conflict     *)
      (* between an unchanged directory and a file, for instance: this would *)
      (* be rather puzzling to the user)                                     *)
      (`DIRECTORY, `Modified, desc, ui)
  | Dir (desc, _, PropsSame, _) ->
      (`DIRECTORY, `Unchanged, desc, ui)

let oldType (prev: Common.prevState): Fileinfo.typ =
  match prev with
    Previous (typ, _, _, _) -> typ
  | New                     -> `ABSENT

let oldDesc (prev: Common.prevState): Props.t =
  match prev with
    Previous (_, desc, _, _) -> desc
  | New                      -> Props.dummy

(* [describeUpdate ui] returns the replica contents for both the case of     *)
(* updating and the case of non-updatingd                                    *)
let describeUpdate ui
    : Common.replicaContent * Common.replicaContent =
  match ui with
    Updates (ucNewStatus, prev) ->
      let typ = oldType prev in
      (update2replicaContent false ui ucNewStatus typ,
       (typ, `Unchanged, oldDesc prev, NoUpdates))
  | _  -> assert false

(* Computes the reconItems when only one side has been updated.  (We split   *)
(* this out into a separate function to avoid duplicating all the symmetric  *)
(* cases.)                                                                   *)
let rec reconcileNoConflict ui whatIsUpdated
    (result: (Name.t, Common.replicas) Tree.u)
    : (Name.t, Common.replicas) Tree.u =
  let different() =
    let rcUpdated, rcNotUpdated = describeUpdate ui in
    match whatIsUpdated with
      Rep2Updated ->
        Different(rcNotUpdated, rcUpdated,
                  ref Replica2ToReplica1, Replica2ToReplica1)
    | Rep1Updated ->
        Different(rcUpdated, rcNotUpdated,
                  ref Replica1ToReplica2, Replica1ToReplica2) in
  match ui with
  | NoUpdates -> result
  | Error err ->
      Tree.add result (Problem err)
  | Updates (Dir (desc, children, permchg, _),
             Previous(`DIRECTORY, _, _, _)) ->
      let r =
        if permchg = PropsSame then result else Tree.add result (different ())
      in
      Safelist.fold_left
        (fun result (theName, uiChild) ->
           Tree.leave
             (reconcileNoConflict
                uiChild whatIsUpdated (Tree.enter result theName)))
        r children
  | Updates _ ->
      Tree.add result (propagateErrors (different ()))

(* [combineChildrn children1 children2] combines two name-sorted lists of    *)
(* type [(Name.t * Common.updateItem) list] to a single list of type         *)
(* [(Name.t * Common.updateItem * Common.updateItem]                         *)
let combineChildren children1 children2 =
  (* NOTE: This function assumes children1 and children2 are sorted.         *)
  let rec loop r children1 children2 =
    match children1,children2 with
      [],_ ->
        Safelist.rev_append r
          (Safelist.map
             (fun (name,ui) -> (name,NoUpdates,ui)) children2)
    | _,[] ->
        Safelist.rev_append r
          (Safelist.map
             (fun (name,ui) -> (name,ui,NoUpdates)) children1)
    | (name1,ui1)::rem1, (name2,ui2)::rem2 ->
        let dif = Name.compare name1 name2 in
        if dif = 0 then
          loop ((name1,ui1,ui2)::r) rem1 rem2
        else if dif < 0 then
          loop ((name1,ui1,NoUpdates)::r) rem1 children2
        else
          loop ((name2,NoUpdates,ui2)::r) children1 rem2
  in
  loop [] children1 children2

(* File are marked equal in groups of 5000 to lower memory consumption       *)
let add_equal (counter, archiveUpdated) equal v =
  let eq = Tree.add equal v in
  incr counter;
  archiveUpdated := true;
  if !counter = 5000 then begin
    counter := 0;
    let (t, eq) = Tree.slice eq in  (* take a snapshot of the tree   *)
    Update.markEqual t;             (* work on it                    *)
    eq                              (* and return the leftover spine *)
  end else
    eq

(* The main reconciliation function: takes a path and two updateItem         *)
(* structures and returns a list of reconItems containing suggestions for    *)
(* propagating changes to make the two replicas equal.                       *)
(* --                                                                        *)
(* It uses two accumulators:                                                 *)
(*   equals: (Name.t, Common.updateContent * Common.updateContent)           *)
(*           Tree.u                                                          *)
(*   unequals: (Name.t, Common.replicas) Tree.u                              *)
(* --                                                                        *)
let rec reconcile path ui1 ui2 counter equals unequals =
  let different uc1 uc2 oldType equals unequals =
    (equals,
     Tree.add unequals
       (propagateErrors
          (Different(update2replicaContent true ui1 uc1 oldType,
                     update2replicaContent true ui2 uc2 oldType,
                     ref Conflict,
                     Conflict)))) in
  let toBeMerged uc1 uc2 oldType equals unequals =
    (equals,
     Tree.add unequals
       (propagateErrors
          (Different(update2replicaContent true ui1 uc1 oldType,
                     update2replicaContent true ui2 uc2 oldType,
                     ref Merge,
                     Merge)))) in
  match (ui1, ui2) with
    (Error s, _) ->
      (equals, Tree.add unequals (Problem s))
  | (_, Error s) ->
      (equals, Tree.add unequals (Problem s))
  | (NoUpdates, _)  ->
      (equals, reconcileNoConflict ui2 Rep2Updated unequals)
  | (_, NoUpdates) ->
      (equals, reconcileNoConflict ui1 Rep1Updated unequals)
  | (Updates (Absent, _), Updates (Absent, _)) ->
      (add_equal counter equals (Absent, Absent), unequals)
  | (Updates (Dir (desc1, children1, propsChanged1, _) as uc1, prevState1),
     Updates (Dir (desc2, children2, propsChanged2, _) as uc2, prevState2)) ->
       (* See if the directory itself should have a reconItem *)
       let dirResult =
         if propsChanged1 = PropsSame && propsChanged2 = PropsSame then
           (equals, unequals)
         else if Props.similar desc1 desc2 then
           let uc1 = Dir (desc1, [], PropsSame, false) in
           let uc2 = Dir (desc2, [], PropsSame, false) in
           (add_equal counter equals (uc1, uc2), unequals)
         else
           let action =
             if propsChanged1 = PropsSame then Replica2ToReplica1
             else if propsChanged2 = PropsSame then Replica1ToReplica2
             else Conflict in
           (equals,
            Tree.add unequals
              (Different
                 (update2replicaContent false ui1 uc1 `DIRECTORY,
                  update2replicaContent false ui2 uc2 `DIRECTORY,
                  ref action, action)))
       in
       (* Apply reconcile on children. *)
       Safelist.fold_left
         (fun (equals, unequals) (name,ui1,ui2) ->
           let (eq, uneq) =
              reconcile (Path.child path name) ui1 ui2 counter
               (Tree.enter equals name) (Tree.enter unequals name)
           in
           (Tree.leave eq, Tree.leave uneq))
         dirResult
         (combineChildren children1 children2)
  | (Updates (File (desc1,contentsChanged1) as uc1, prev),
     Updates (File (desc2,contentsChanged2) as uc2, _)) ->
       begin match contentsChanged1, contentsChanged2 with
         ContentsUpdated (dig1, _, ress1), ContentsUpdated (dig2, _, ress2)
         when dig1 = dig2 ->
           if Props.similar desc1 desc2 then
             (add_equal counter equals (uc1, uc2), unequals)
           else
(* Special case: when both sides are modified files but their contents turn  *)
(* out to be the same, we want to display them as 'perms' rather than 'new'  *)
(* on both sides, to avoid confusing the user.  (The Transfer module also    *)
(* expect this.)                                                             *)
             let uc1' = File(desc1,ContentsSame) in
             let uc2' = File(desc2,ContentsSame) in
             different uc1' uc2' (oldType prev) equals unequals
       | ContentsSame, ContentsSame when Props.similar desc1 desc2 ->
           (add_equal counter equals (uc1, uc2), unequals)
       | ContentsUpdated _, ContentsUpdated _
             when Globals.shouldMerge path ->
           toBeMerged uc1 uc2 (oldType prev) equals unequals
       | _ ->
           different uc1 uc2 (oldType prev) equals unequals
       end
  | (Updates (Symlink(l1) as uc1, prev),
     Updates (Symlink(l2) as uc2, _)) ->
       if l1 = l2 then
         (add_equal counter equals (uc1, uc2), unequals)
       else
         different uc1 uc2 (oldType prev) equals unequals
  | (Updates (uc1, prev), Updates (uc2, _)) ->
      different uc1 uc2 (oldType prev) equals unequals

(* Sorts the paths so that they will be displayed in order                   *)
let sortPaths pathUpdatesList =
  Sort.list
    (fun (p1, _) (p2, _) -> Path.compare p1 p2 <= 0)
    pathUpdatesList

let rec enterPath p t =
  match Path.deconstruct p with
    None          -> t
  | Some (nm, p') -> enterPath p' (Tree.enter t nm)

let rec leavePath p t =
  match Path.deconstruct p with
    None          -> t
  | Some (nm, p') -> leavePath p' (Tree.leave t)

(* A path is dangerous if one replica has been emptied but not the other *)
let dangerousPath u1 u2 =
  let emptied u =
    match u with
      Updates (Absent, _)               -> true
    | Updates (Dir (_, _, _, empty), _) -> empty
    | _                                 -> false
  in
  emptied u1 <> emptied u2

(* The second component of the return value is true if there is at least one *)
(* file that is updated in the same way on both roots                        *)
let reconcileList (pathUpdatesList: (Path.t * Common.updateItem list) list)
    : Common.reconItem list * bool * Path.t list =
  let counter = ref 0 in
  let archiveUpdated = ref false in
  let (equals, unequals, dangerous) =
    Safelist.fold_left
      (fun (equals, unequals, dangerous) (path,updatesList) ->
        match updatesList with
          [ui1; ui2] ->
            let (equals, unequals) =
              reconcile path ui1 ui2 (counter, archiveUpdated)
                (enterPath path equals) (enterPath path unequals)
            in
            (leavePath path equals, leavePath path unequals,
             if dangerousPath ui1 ui2 then path :: dangerous else dangerous)
        | _ ->
            assert false)
      (Tree.start, Tree.start, []) pathUpdatesList in
  let unequals = Tree.finish unequals in
  debug (fun() -> Util.msg "reconcile: %d results\n" (Tree.size unequals));
  let equals = Tree.finish equals in
  Update.markEqual equals;
  (* Commit archive updates done up to now *)
  if !archiveUpdated then Update.commitUpdates ();
  let result = Tree.flatten unequals Path.empty Path.child [] in
  let unsorted =
    Safelist.map (fun (p, rplc) -> {path = p; replicas = rplc}) result in
  let sorted = Sortri.sortReconItems unsorted in
  overrideReconcilerChoices sorted;
  (sorted, not (Tree.is_empty equals), dangerous)

(* This is the main function: it takes a list of updateItem lists and,       
   according to the roots and paths of synchronization, builds the           
   corresponding reconItem list.  A second component indicates whether there 
   is any file updated in the same way on both sides. *)
let reconcileAll (ONEPERPATH(updatesListList)) =
  Trace.status "Reconciling changes";
  debug (fun() -> Util.msg "reconcileAll\n");
  let pathList = Prefs.read Globals.paths in
  let pathUpdatesList =
    sortPaths (Safelist.combine pathList updatesListList) in
  reconcileList pathUpdatesList

let reconcileTwo p ui ui' = reconcileList [(p, [ui; ui'])]
