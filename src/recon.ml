(* Unison file synchronizer: src/recon.ml *)
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

(* ------------------------------------------------------------------------- *)
(*                     Handling of prefer/force                              *)
(* ------------------------------------------------------------------------- *)
let debug = Trace.debug "recon"

let setDirection ri dir force =
  match ri.replicas with
    Different
      ({rc1 = rc1; rc2 = rc2; direction = d; default_direction = default } as diff)
          when force=`Force || isConflict default ->
      if dir=`Replica1ToReplica2 then
        diff.direction <- Replica1ToReplica2
      else if dir=`Replica2ToReplica1 then
        diff.direction <- Replica2ToReplica1
      else if dir=`Merge then begin
        if Globals.shouldMerge ri.path1 then diff.direction <- Merge
      end else begin  (* dir = `Older or dir = `Newer *)
        match rc1.status, rc2.status with
          `Deleted, _ ->
            if isConflict default then
              diff.direction <- Replica2ToReplica1
        | _, `Deleted ->
            if isConflict default then
              diff.direction <- Replica1ToReplica2
        | _ ->
            let comp = Props.time rc1.desc -. Props.time rc2.desc in
            (* If mtimes are equal then `Older and `Newer are not defined
               and will be ignored. This is safer than the previous way of
               always propagating from replica 2 to replica 1. *)
            if comp <> 0.0 then
            let comp = if dir=`Newer then -. comp else comp in
            if comp<0.0 then
              diff.direction <- Replica1ToReplica2
            else
              diff.direction <- Replica2ToReplica1
      end
  | _ ->
      ()

let revertToDefaultDirection ri =
  match ri.replicas with
    Different diff -> diff.direction <- diff.default_direction
  | _              -> ()

(* Find out which direction we need to propagate changes if we want to       *)
(* consider the given root to be the "truth"                                 *)
(* --                                                                        *)
(*   root := "older" | "newer" | <one of the two roots>                      *)
(*   return value := 'Older  | 'Newer  | 'Replica1ToReplica2 |               *)
(*                   'Replica2ToReplica1                                     *)
(* --                                                                        *)
let root2direction root =
  let partialMatch s = function
    | Clroot.ConnectLocal (None | Some "") -> false
    | Clroot.ConnectLocal (Some root) ->
        Util.startswith root s || Util.endswith root s
    | ConnectByShell (_, host, _, _, Some root)
    | ConnectBySocket (host, _, Some root) ->
        Util.startswith root s || Util.endswith root s || Util.startswith host s
    | ConnectByShell (_, host, _, _, None)
    | ConnectBySocket (host, _, None) ->
        Util.startswith host s
  in
  let partialRootMatches prefVal =
    Safelist.map (partialMatch prefVal) (Globals.parsedClRawRoots ())
  in
  if      root="older" then `Older
  else if root="newer" then `Newer
  else if root = "" then `None
  else
    let (r1, r2) = Globals.rawRootPair () in
    debug (fun() ->
       Printf.eprintf "root2direction called to choose %s from %s and %s\n"
         root r1 r2);
    if r1 = root then `Replica1ToReplica2 else
    if r2 = root then `Replica2ToReplica1 else
    match partialRootMatches root with
    | [true; false] -> `Replica1ToReplica2
    | [false; true] -> `Replica2ToReplica1
    | _ ->
        raise (Util.Fatal (Printf.sprintf "%s\nis not uniquely identifying one \
          of the current roots:\n  %s\n  %s" root r1 r2))

let rootDirCache = ref []

let clearRootDirCache () = rootDirCache := []

let prefRoot prefV =
  (* Use physical equality with cache keys. The goal is not to avoid as many
     cache misses as possible but to make cache checking much cheaper than
     calculating the value (in this case, hashing and string comparison are
     not quite cheap enough). *)
  match List.assq_opt prefV !rootDirCache with
  | Some x -> x
  | None -> let x = root2direction prefV in
            rootDirCache := (prefV, x) :: !rootDirCache; x

let forceRoot: string Prefs.t =
  Prefs.createString "force" ""
    ~category:(`Advanced `Sync)
    "force changes from this replica to the other"
    ("Including the preference \\texttt{-force \\ARG{root}} causes Unison to "
     ^ "resolve all differences (even non-conflicting changes) in favor of "
     ^ "\\ARG{root}.  "
     ^ "This effectively changes Unison from a synchronizer into a mirroring "
     ^ "utility.  \n\n"
     ^ "You can also specify a unique prefix or suffix of the path of one of "
     ^ "the roots or a unique prefix of the hostname of a remote root.\n\n"
     ^ "You can also specify \\verb|-force newer| (or \\verb|-force older|) "
     ^ "to force Unison to choose the file with the later (earlier) "
     ^ "modtime.  In this case, the \\verb|-times| preference must also "
     ^ "be enabled.  If modtimes are equal in both replicas when using "
     ^ "\\verb|newer| or \\verb|older| then this preference will have no "
     ^ "effect (changes will be synced as if without this preference or "
     ^ "remain unsynced in case of a conflict).\n\n"
     ^ "This preference is overridden by the \\verb|forcepartial| preference.\n\n"
     ^ "This preference should be used only if you are {\\em sure} you "
     ^ "know what you are doing!")

let forceRootPartial: Pred.t =
  Pred.create "forcepartial"
    ~category:(`Advanced `Sync)
    ("Including the preference \\texttt{forcepartial = \\ARG{PATHSPEC} -> \\ARG{root}} causes Unison to "
     ^ "resolve all differences (even non-conflicting changes) in favor of "
     ^ "\\ARG{root} for the files in \\ARG{PATHSPEC} (see \\sectionref{pathspec}{Path Specification} "
     ^ "for more information).  "
     ^ "This effectively changes Unison from a synchronizer into a mirroring "
     ^ "utility.  \n\n"
     ^ "You can also specify a unique prefix or suffix of the path of one of "
     ^ "the roots or a unique prefix of the hostname of a remote root.\n\n"
     ^ "You can also specify \\verb|forcepartial PATHSPEC -> newer| "
     ^ "(or \\verb|forcepartial PATHSPEC -> older|) "
     ^ "to force Unison to choose the file with the later (earlier) "
     ^ "modtime.  In this case, the \\verb|-times| preference must also "
     ^ "be enabled.  If modtimes are equal in both replicas when using "
     ^ "\\verb|newer| or \\verb|older| then this preference will have no "
     ^ "effect (changes will be synced as if without this preference or "
     ^ "remain unsynced in case of a conflict).\n\n"
     ^ "This preference should be used only if you are {\\em sure} you "
     ^ "know what you are doing!")

let preferRoot: string Prefs.t =
  Prefs.createString "prefer" ""
    ~category:(`Advanced `Sync)
    "choose this replica's version for conflicting changes"
    ("Including the preference \\texttt{-prefer \\ARG{root}} causes Unison always to "
     ^ "resolve conflicts in favor of \\ARG{root}, rather than asking for "
     ^ "guidance from the user, except for paths marked by the preference "
     ^ "\\texttt{merge}.  (The syntax of \\ARG{root} is the same as "
     ^ "for the \\verb|root| preference, plus the special values "
     ^ "\\verb|newer| and \\verb|older|.)  \n\n"
     ^ "You can also specify a unique prefix or suffix of the path of one of "
     ^ "the roots or a unique prefix of the hostname of a remote root.\n\n"
     ^ "This preference is overridden by the \\verb|preferpartial| preference.\n\n"
     ^ "This preference should be used only if you are {\\em sure} you "
     ^ "know what you are doing!")

let preferRootPartial: Pred.t =
  Pred.create "preferpartial"
    ~category:(`Advanced `Sync)
    ("Including the preference \\texttt{preferpartial = \\ARG{PATHSPEC} -> \\ARG{root}} "
     ^ "causes Unison always to "
     ^ "resolve conflicts in favor of \\ARG{root}, rather than asking for "
     ^ "guidance from the user, for the files in \\ARG{PATHSPEC} (see "
     ^ "\\sectionref{pathspec}{Path Specification} "
     ^ "for more information).  (The syntax of \\ARG{root} is the same as "
     ^ "for the \\verb|root| preference, plus the special values "
     ^ "\\verb|newer| and \\verb|older|.)  \n\n"
     ^ "You can also specify a unique prefix or suffix of the path of one of "
     ^ "the roots or a unique prefix of the hostname of a remote root.\n\n"
     ^ "This preference should be used only if you are {\\em sure} you "
     ^ "know what you are doing!")

(* [lookupPreferredRoot (): string * [`Force | `Prefer]] checks validity of  *)
(* preferences "force"/"preference", returns a pair (root, force)            *)
let lookupPreferredRoot () =
  if Prefs.read forceRoot <> "" then
    (prefRoot (Prefs.read forceRoot), `Force)
  else if Prefs.read preferRoot <> "" then
    (prefRoot (Prefs.read preferRoot), `Prefer)
  else
    (`None, `Prefer)

(* [lookupPreferredRootPartial: Path.t -> string * [`Force | `Prefer]] checks validity of  *)
(* preferences "forcepartial", returns a pair (root, force)                                *)
let lookupPreferredRootPartial p =
  let s = Path.toString p in
  if Pred.test forceRootPartial s then
    (prefRoot (Pred.assoc forceRootPartial s), `Force)
  else if Pred.test preferRootPartial s then
    (prefRoot (Pred.assoc preferRootPartial s), `Prefer)
  else
    (`None, `Prefer)

let noDeletion =
  Prefs.createStringList "nodeletion"
    ~category:(`Basic `Sync)
    "prevent file deletions on one replica"
    ("Including the preference \\texttt{-nodeletion \\ARG{root}} prevents \
      Unison from performing any file deletion on root \\ARG{root}.\n\n\
      You can also specify a unique prefix or suffix of the path of one of \
      the roots or a unique prefix of the hostname of a remote root.\n\n\
      This preference can be included twice, once for each root, if you \
      want to prevent any deletion.")

let noUpdate =
  Prefs.createStringList "noupdate"
    ~category:(`Basic `Sync)
    "prevent file updates and deletions on one replica"
    ("Including the preference \\texttt{-noupdate \\ARG{root}} prevents \
      Unison from performing any file update or deletion on root \
      \\ARG{root}.\n\n\
      You can also specify a unique prefix or suffix of the path of one of \
      the roots or a unique prefix of the hostname of a remote root.\n\n\
      This preference can be included twice, once for each root, if you \
      want to prevent any update.")

let noCreation =
  Prefs.createStringList "nocreation"
    ~category:(`Basic `Sync)
    "prevent file creations on one replica"
    ("Including the preference \\texttt{-nocreation \\ARG{root}} prevents \
      Unison from performing any file creation on root \\ARG{root}.\n\n\
      You can also specify a unique prefix or suffix of the path of one of \
      the roots or a unique prefix of the hostname of a remote root.\n\n\
      This preference can be included twice, once for each root, if you \
      want to prevent any creation.")

let noDeletionPartial =
  Pred.create "nodeletionpartial"
    ~category:(`Advanced `Sync)
    ("Including the preference \
      \\texttt{nodeletionpartial = \\ARG{PATHSPEC} -> \\ARG{root}} prevents \
      Unison from performing any file deletion in \\ARG{PATHSPEC} \
      on root \\ARG{root} (see \\sectionref{pathspec}{Path Specification} \
      for more information).  It is recommended to use {\\tt BelowPath} \
      patterns when selecting a directory and all its contents.")

let noUpdatePartial =
  Pred.create "noupdatepartial"
    ~category:(`Advanced `Sync)
    ("Including the preference \
      \\texttt{noupdatepartial = \\ARG{PATHSPEC} -> \\ARG{root}} prevents \
      Unison from performing any file update or deletion in \
      \\ARG{PATHSPEC} on root \\ARG{root} (see \
      \\sectionref{pathspec}{Path Specification} for more information). \
      It is recommended to use {\\tt BelowPath} \
      patterns when selecting a directory and all its contents.")

let noCreationPartial =
  Pred.create "nocreationpartial"
    ~category:(`Advanced `Sync)
    ("Including the preference \
      \\texttt{nocreationpartial = \\ARG{PATHSPEC} ->  \\ARG{root}} prevents \
      Unison from performing any file creation in \\ARG{PATHSPEC} \
      on root \\ARG{root} (see \\sectionref{pathspec}{Path Specification} \
      for more information). \
      It is recommended to use {\\tt BelowPath} \
      patterns when selecting a directory and all its contents.")

let maxSizeThreshold =
  Prefs.createInt "maxsizethreshold" (-1)
    ~category:(`Advanced `General)
    "prevent transfer of files bigger than this (if >=0, in Kb)"
    ("A number indicating above what filesize (in kilobytes) Unison should "
     ^ "flag a conflict instead of transferring the file. "
     ^ "This conflict remains even in the presence of force or prefer options. "
     ^ "A negative number will allow every transfer independently of the size.  "
     ^ "The default is -1. ")

let testPartialCancelPref root path actionKind =
  let partialCancelPref actionKind =
    match actionKind with
      `DELETION -> noDeletionPartial
    | `UPDATE   -> noUpdatePartial
    | `CREATION -> noCreationPartial
  in
  Pred.assoc_all (partialCancelPref actionKind) path
  |> List.exists (fun x -> root = prefRoot x)

let testCancelPref root actionKind =
  let cancelPref actionKind =
    match actionKind with
      `DELETION -> noDeletion
    | `UPDATE   -> noUpdate
    | `CREATION -> noCreation
  in
  Prefs.read (cancelPref actionKind)
  |> List.exists (fun x -> root = prefRoot x)

let actionKind fromRc toRc =
  let fromTyp = fromRc.typ in
  let toTyp = toRc.typ in
  if fromTyp = toTyp then `UPDATE else
  if toTyp = `ABSENT then `CREATION else
  `DELETION

let shouldCancel path rc1 rc2 root =
  let test kind =
    testCancelPref root kind
      ||
    testPartialCancelPref root path kind
  in
  let testSize rc =
       Prefs.read maxSizeThreshold >= 0
    && Props.length rc.desc >=
         Uutil.Filesize.ofInt64
           (Int64.mul (Int64.of_int 1000)
                      (Int64.of_int (Prefs.read maxSizeThreshold)))
  in
  match actionKind rc1 rc2 with
    `UPDATE   ->
     if test `UPDATE then true, "would update a file with noupdate or noupdatepartial set"
     else testSize rc1, "would transfer a file of size greater than maxsizethreshold"
  | `DELETION ->
     if test `UPDATE then true, "would update a file with noupdate or noupdatepartial set"
     else test `DELETION, "would delete a file with nodeletion or nodeletionpartial set"
  | `CREATION ->
     if test `CREATION then true, "would create a file with nocreation or nocreationpartial set"
     else testSize rc1, "would transfer a file of size greater than maxsizethreshold"

let filterRi ri =
  match ri.replicas with
    Problem _ ->
      ()
  | Different diff ->
     let cancel,reason =
       match diff.direction with
         Replica1ToReplica2 ->
          shouldCancel (Path.toString ri.path1) diff.rc1 diff.rc2 `Replica2ToReplica1
       | Replica2ToReplica1 ->
          shouldCancel (Path.toString ri.path1) diff.rc2 diff.rc1 `Replica1ToReplica2
       | Conflict _ | Merge ->
          false,""
     in
     if cancel
     then
       diff.direction <- Conflict reason

let filterRis ris =
  Safelist.iter filterRi ris

(* Use the current values of the '-prefer <ROOT>' and '-force <ROOT>'        *)
(* preferences to override the reconciler's choices                          *)
let overrideReconcilerChoices ris =
  clearRootDirCache ();
  let (dir, force) = lookupPreferredRoot () in
  if dir <> `None then Safelist.iter (fun ri -> setDirection ri dir force) ris;
  Safelist.iter (fun ri ->
                   let (dir, forcep) = lookupPreferredRootPartial ri.path1 in
                   if dir <> `None then setDirection ri dir forcep) ris;
  filterRis ris

(* Look up the preferred root and verify that it is OK (this is called at    *)
(* the beginning of the run, so that we don't have to wait to hear about     *)
(* errors                                                                    *)
let checkThatPreferredRootIsValid () =
  let test_root explicitRoot predname predvalue =
    match prefRoot predvalue with
    | `None | `Replica1ToReplica2 | `Replica2ToReplica1 -> ()
    | (`Newer | `Older) when explicitRoot ->
        raise (Util.Fatal ("Argument to preference '" ^ predname ^ "': "
          ^ predvalue ^ " must not be keyword 'older' or 'newer'."))
    | `Newer -> ()
    | `Older ->
        if not (Prefs.read Props.syncModtimes) then
          raise (Util.Transient (Printf.sprintf
            "The '%s=older' preference can only be used with 'times=true'"
            predname))
    | `Merge -> assert false
    | exception (Util.Fatal err) ->
        raise (Util.Fatal ("Argument to preference '" ^ predname ^ "': " ^ err))
  in
  let checkPrefs ~explicitRoot extract prefs =
    Safelist.iter (fun (pref, prefName) ->
      Safelist.iter (test_root explicitRoot prefName) (extract pref)) prefs
  in
  checkPrefs ~explicitRoot:false (fun x -> [Prefs.read x])
    [forceRoot, "force"; preferRoot, "prefer"];
  checkPrefs ~explicitRoot:false Pred.extern_associated_strings
    [forceRootPartial, "forcepartial";
     preferRootPartial, "preferpartial"];
  checkPrefs ~explicitRoot:true Prefs.read
    [noDeletion, "nodeletion"; noUpdate, "noupdate"; noCreation, "nocreation"];
  checkPrefs ~explicitRoot:true Pred.extern_associated_strings
    [noDeletionPartial, "nodeletionpartial";
     noUpdatePartial, "noupdatepartial";
     noCreationPartial, "nocreationpartial"]

(* ------------------------------------------------------------------------- *)
(*                    Main Reconciliation stuff                              *)
(* ------------------------------------------------------------------------- *)

exception UpdateError of string

let rec checkForError ui =
  match ui with
    NoUpdates ->
      ()
  | Error err ->
      if not (Fileinfo.shouldIgnore err) then raise (UpdateError err)
  | Updates (uc, _) ->
      match uc with
        Dir (_, children, _, _) ->
          Safelist.iter (fun (_, uiSub) -> checkForError uiSub) children
      | Absent | File _ | Symlink _ ->
          ()

let rec collectErrors ui rem =
  match ui with
    NoUpdates ->
      rem
  | Error err ->
      if Fileinfo.shouldIgnore err then rem else err :: rem
  | Updates (uc, _) ->
      match uc with
        Dir (_, children, _, _) ->
          Safelist.fold_right
            (fun (_, uiSub) rem -> collectErrors uiSub rem) children rem
      | Absent | File _ | Symlink _ ->
          rem

(* lifting errors in individual updates to replica problems                  *)
let propagateErrors allowPartial (rplc: Common.replicas): Common.replicas =
  match rplc with
    Problem _ ->
      rplc
  | Different diff when allowPartial ->
      Different { diff with
                  errors1 = collectErrors diff.rc1.ui [];
                  errors2 = collectErrors diff.rc2.ui [] }
  | Different diff ->
      try
        checkForError diff.rc1.ui;
        try
          checkForError diff.rc2.ui;
          rplc
        with UpdateError err ->
          Problem ("[root 2]: " ^ err)
      with UpdateError err ->
        Problem ("[root 1]: " ^ err)

(* Using the error message to ignore symlinks is a bit fragile but this is
   the easiest way to keep code changes local and avoid a huge backwards
   compatibility burden. *)

let skipIgnored result s othUi =
  match Fileinfo.shouldIgnore s, othUi with
  | false, _ -> Tree.add result (Problem s)
  | true, Error s2 ->
      if Fileinfo.shouldIgnore s2 then result else Tree.add result (Problem s2)
  | true, NoUpdates
  | true, Updates (Symlink _, _) -> result
  | true, Updates _ ->
      Tree.add result (Problem "Syncing symbolic links is disabled, but \
        this path represents a symbolic link in one of the replicas and \
        a non-link in the other replica.")

type singleUpdate = Rep1Updated | Rep2Updated

let update2replicaContent path (conflict: bool) ui props ucNew oldType:
    Common.replicaContent =
  let size = Update.updateSize path ui in
  match ucNew with
    Absent ->
      {typ = `ABSENT; status = `Deleted; desc = Props.dummy;
       ui = ui; size = size; props = props}
  | File (desc, ContentsSame) ->
      {typ = `FILE; status = `PropsChanged; desc = desc;
       ui = ui; size = size; props = props}
  | File (desc, _) when oldType <> `FILE ->
      {typ = `FILE; status = `Created; desc = desc;
       ui = ui; size = size; props = props}
  | File (desc, ContentsUpdated _) ->
      {typ = `FILE; status = `Modified; desc = desc;
       ui = ui; size = size; props = props}
  | Symlink l when oldType <> `SYMLINK ->
      {typ = `SYMLINK; status = `Created; desc = Props.dummy;
       ui = ui; size = size; props = props}
  | Symlink l ->
      {typ = `SYMLINK; status = `Modified; desc = Props.dummy;
       ui = ui; size = size; props = props}
  | Dir (desc, _, _, _) when oldType <> `DIRECTORY ->
      {typ = `DIRECTORY; status = `Created; desc = desc;
       ui = ui; size = size; props = props}
  | Dir (desc, _, PropsUpdated, _) ->
      {typ = `DIRECTORY; status = `PropsChanged; desc = desc;
       ui = ui; size = size; props = props}
  | Dir (desc, _, PropsSame, _) when conflict ->
      (* Special case: the directory contents has been modified and the      *)
      (* directory is in conflict.  (We don't want to display a conflict     *)
      (* between an unchanged directory and a file, for instance: this would *)
      (* be rather puzzling to the user)                                     *)
      {typ = `DIRECTORY; status = `Modified; desc = desc;
       ui = ui; size = size; props = props}
  | Dir (desc, _, PropsSame, _) ->
      {typ = `DIRECTORY; status = `Unchanged; desc =desc;
       ui = ui; size = size; props = props}

let oldType (prev: Common.prevState): Fileinfo.typ =
  match prev with
    Previous (typ, _, _, _) -> typ
  | New                     -> `ABSENT

let oldDesc (prev: Common.prevState): Props.t =
  match prev with
    Previous (_, desc, _, _) -> desc
  | New                      -> Props.dummy

(* [describeUpdate ui] returns the replica contents for both the case of     *)
(* updating and the case of non-updating                                     *)
let describeUpdate path props' ui props
    : Common.replicaContent * Common.replicaContent =
  match ui with
    Updates (ucNewStatus, prev) ->
      let typ = oldType prev in
      (update2replicaContent path false ui props ucNewStatus typ,
       {typ = typ; status = `Unchanged; desc = oldDesc prev;
        ui = NoUpdates; size = Update.updateSize path NoUpdates;
        props = props'})
  | _  -> assert false

(* Computes the reconItems when only one side has been updated.  (We split   *)
(* this out into a separate function to avoid duplicating all the symmetric  *)
(* cases.)                                                                   *)
let rec reconcileNoConflict allowPartial path props' ui props whatIsUpdated
    (result: (Name.t * Name.t, Common.replicas) Tree.u)
    : (Name.t * Name.t, Common.replicas) Tree.u =
  let different() =
    let rcUpdated, rcNotUpdated = describeUpdate path props' ui props in
    match whatIsUpdated with
      Rep2Updated ->
        Different {rc1 = rcNotUpdated; rc2 = rcUpdated;
                   direction = Replica2ToReplica1;
                   default_direction = Replica2ToReplica1;
                   errors1 = []; errors2 = []}
    | Rep1Updated ->
        Different {rc1 = rcUpdated; rc2 = rcNotUpdated;
                   direction = Replica1ToReplica2;
                   default_direction = Replica1ToReplica2;
                   errors1 = []; errors2 = []} in
  match ui with
  | NoUpdates -> result
  | Error err ->
      skipIgnored result err NoUpdates
  | Updates (Dir (desc, children, permchg, _),
             Previous(`DIRECTORY, _, _, _)) ->
      let r =
        if permchg = PropsSame then result else Tree.add result (different ())
      in
      Safelist.fold_left
        (fun result (theName, uiChild) ->
           Tree.leave
             (reconcileNoConflict allowPartial (Path.child path theName)
                [] uiChild [] whatIsUpdated
                (Tree.enter result (theName, theName))))
        r children
  | Updates _ ->
      Tree.add result (propagateErrors allowPartial (different ()))

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
             (fun (name,ui) -> (name,NoUpdates,name,ui)) children2)
    | _,[] ->
        Safelist.rev_append r
          (Safelist.map
             (fun (name,ui) -> (name,ui,name,NoUpdates)) children1)
    | (name1,ui1)::rem1, (name2,ui2)::rem2 ->
        let dif = Name.compare name1 name2 in
        if dif = 0 then
          loop ((name1,ui1,name2,ui2)::r) rem1 rem2
        else if dif < 0 then
          loop ((name1,ui1,name1,NoUpdates)::r) rem1 children2
        else
          loop ((name2,NoUpdates,name2,ui2)::r) children1 rem2
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
(*   equals: (Name.t * Name.t, Common.updateContent * Common.updateContent)  *)
(*           Tree.u                                                          *)
(*   unequals: (Name.t * Name.t, Common.replicas) Tree.u                     *)
(* --                                                                        *)
let rec reconcile
          allowPartial path ui1 props1 ui2 props2 counter equals unequals =
  let different uc1 uc2 reason oldType equals unequals =
    (equals,
     Tree.add unequals
       (propagateErrors allowPartial
          (Different {rc1 = update2replicaContent
                              path true ui1 props1 uc1 oldType;
                      rc2 = update2replicaContent
                              path true ui2 props2 uc2 oldType;
                      direction = Conflict reason;
                      default_direction = Conflict reason;
                      errors1 = []; errors2 = []}))) in
  let toBeMerged uc1 uc2 oldType equals unequals =
    (equals,
     Tree.add unequals
       (propagateErrors allowPartial
          (Different {rc1 = update2replicaContent
                              path true ui1 props1 uc1 oldType;
                      rc2 = update2replicaContent
                              path true ui2 props2 uc2 oldType;
                      direction = Merge; default_direction = Merge;
                      errors1 = []; errors2 = []}))) in
  match (ui1, ui2) with
    (Error s, _) ->
      (equals, skipIgnored unequals s ui2)
  | (_, Error s) ->
      (equals, skipIgnored unequals s ui1)
  | (NoUpdates, _)  ->
      (equals,
       reconcileNoConflict
         allowPartial path props1 ui2 props2 Rep2Updated unequals)
  | (_, NoUpdates) ->
      (equals,
       reconcileNoConflict
         allowPartial path props2 ui1 props1 Rep1Updated unequals)
  | (Updates (Absent, _), Updates (Absent, _)) ->
      (add_equal counter equals (Absent, Absent), unequals)
  | (Updates (Dir (desc1, children1, propsChanged1, _) as uc1, prevState1),
     Updates (Dir (desc2, children2, propsChanged2, _) as uc2, prevState2)) ->
       if Pred.test Globals.atomic (Path.toString path) then
         let action = Conflict "atomic directory" in
         (equals,
          Tree.add unequals
            (Different
                 {rc1 = update2replicaContent path true ui1 [] uc1 `DIRECTORY;
                  rc2 = update2replicaContent path true ui2 [] uc2 `DIRECTORY;
                  direction = action; default_direction = action;
                  errors1 = []; errors2 = []}))
       else
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
               else Conflict "properties changed on both sides" in
             (equals,
              Tree.add unequals
                (Different
                   {rc1 = update2replicaContent path false ui1 [] uc1 `DIRECTORY;
                    rc2 = update2replicaContent path false ui2 [] uc2 `DIRECTORY;
                    direction = action; default_direction = action;
                    errors1 = []; errors2 = []}))
         in
         (* Apply reconcile on children. *)
         Safelist.fold_left
           (fun (equals, unequals) (name1,ui1,name2,ui2) ->
              let (eq, uneq) =
                reconcile
                  allowPartial (Path.child path name1) ui1 [] ui2 [] counter
                  (Tree.enter equals (name1, name2))
                  (Tree.enter unequals (name1, name2))
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
             different uc1' uc2' "properties changed on both sides"
                       (oldType prev) equals unequals
       | ContentsSame, ContentsSame when Props.similar desc1 desc2 ->
           (add_equal counter equals (uc1, uc2), unequals)
       | ContentsSame, ContentsSame ->
           different uc1 uc2 "properties changed on both sides"
                     (oldType prev) equals unequals
       | ContentsUpdated _, ContentsUpdated _
             when Globals.shouldMerge path ->
           toBeMerged uc1 uc2 (oldType prev) equals unequals
       | _ ->
           different uc1 uc2 "contents changed on both sides"
                     (oldType prev) equals unequals
       end
  | (Updates (Symlink(l1) as uc1, prev),
     Updates (Symlink(l2) as uc2, _)) ->
       if l1 = l2 then
         (add_equal counter equals (uc1, uc2), unequals)
       else
         different uc1 uc2 "symbolic links changed on both sides"
                   (oldType prev) equals unequals
  | (Updates (uc1, prev), Updates (uc2, _)) ->
      different uc1 uc2 "conflicting updates"
                (oldType prev) equals unequals

(* Sorts the paths so that they will be displayed in order                   *)
let sortPaths pathUpdatesList =
  List.sort
    Path.compare
    pathUpdatesList

let rec enterPath p1 p2 t =
  match Path.deconstruct p1, Path.deconstruct p2 with
    None, None ->
      t
  | Some (nm1, p1'), Some (nm2, p2') ->
      enterPath p1' p2' (Tree.enter t (nm1, nm2))
  | _ ->
      assert false (* Cannot happen, as the paths are equal up to case *)

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
let reconcileList allowPartial
      (pathUpdatesList:
         ((Path.local * Common.updateItem * Props.t list) *
          (Path.local * Common.updateItem * Props.t list)) list)
      : Common.reconItem list * bool * Path.t list =
  let counter = ref 0 in
  let archiveUpdated = ref false in
  let (equals, unequals, dangerous) =
    Safelist.fold_left
      (fun (equals, unequals, dangerous)
           ((path1,ui1,props1),(path2,ui2,props2)) ->
         (* We make the paths global as we may concatenate them with
            names from the other replica *)
         let path1 = Path.makeGlobal path1 in
         let path2 = Path.makeGlobal path2 in
         let (equals, unequals) =
           reconcile allowPartial
             path1 ui1 props1 ui2 props2 (counter, archiveUpdated)
             (enterPath path1 path2 equals)
             (enterPath path1 path2 unequals)
         in
         (leavePath path1 equals, leavePath path1 unequals,
          if dangerousPath ui1 ui2 then path1 :: dangerous else dangerous))
      (Tree.start, Tree.start, []) pathUpdatesList in
  let unequals = Tree.finish unequals in
  debug (fun() -> Util.msg "reconcile: %d results\n" (Tree.size unequals));
  let equals = Tree.finish equals in
  Update.markEqual equals;
  (* Commit archive updates done up to now *)
  if !archiveUpdated then Update.commitUpdates ();
  let result =
    Tree.flatten unequals (Path.empty, Path.empty)
      (fun (p1, p2) (nm1, nm2) -> (Path.child p1 nm1, Path.child p2 nm2)) [] in
  let unsorted =
    Safelist.map
     (fun ((p1, p2), rplc) -> {path1 = p1; path2 = p2; replicas = rplc})
     result in
  let sorted = Sortri.sortReconItems unsorted in
  overrideReconcilerChoices sorted;
  (sorted, not (Tree.is_empty equals), dangerous)

(* This is the main function: it takes a list of updateItem lists and,
   according to the roots and paths of synchronization, builds the
   corresponding reconItem list.  A second component indicates whether there
   is any file updated in the same way on both sides. *)
let reconcileAll ?(allowPartial = false) updatesList =
  Trace.status "Reconciling changes";
  debug (fun() -> Util.msg "reconcileAll\n");
  reconcileList allowPartial updatesList
