(* Unison file synchronizer: src/moves.ml *)
(* Copyright 2021-2023, Tõivo Leedjärv

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

let debug = Trace.debug "moves"
let debugverbose = Trace.debug "moves+"

let featMovesValid = ref (fun _ _ -> None)

let featMoves =
  Features.register "Sync: Moves"
  (Some (fun a b -> !featMovesValid a b))

let enabled () = Features.enabled featMoves

let detectmoves =
  Prefs.createBool "moves" false
    ~category:(`Advanced `Sync)
    ~local:true
    ~send:enabled
    "optimize transfers by detecting renames and moves"
    "When this preference is set, Unison will try to avoid transferring \
     file contents across the network, or making a local copy, by \
     recognizing when a file or a directory has been renamed or moved to \
     a new location.  This usually allows to propagate only the rename, \
     without transferring or copying any data.  The default value is \
     \\texttt{false}."

let () = featMovesValid :=
  fun feats enabledThis ->
    if Prefs.read detectmoves
        && not (enabledThis && Features.mem "prevState2" feats) then
      Some ("You have requested detection and propagation of moves/renames \
        (the \"moves\" preference) but the server does not support this.")
    else None

(*------------*)

let moves = Hashtbl.create 100
let hides = Hashtbl.create 100

let hide path = Hashtbl.add hides path true
let isHidden path = Hashtbl.mem hides path

let set path differ = Hashtbl.add moves path differ
let getMove path = Hashtbl.find_opt moves path

let makeReconItem path ri =
  match getMove path with
  | Some replicas -> Some {ri with replicas}
  | None -> if isHidden path then None else Some ri

let r = function
  | {replicas = Different _; path1; _} as ri when Prefs.read detectmoves ->
      makeReconItem path1 ri
  | {replicas = Different _; _}
  | {replicas = Problem _ ; _} as ri -> Some ri

(*------------*)

type 'a moveCand =
  { path : Path.t;
    dir : Path.t;
    name : Name.t;
    differ : Common.difference }

type file
type dir

(* The two lists of ['a moveCands] are lists of candidates for detecting moves.
   The first list is a list of creates, the second is a list of deletes. The
   lengths of these lists don't have to match.
   Move candidates of a ['a moveCands] all have the same contents (actual
   contents for files, names and contents of children recursively for
   directories). *)
type 'a moveCands = ('a moveCand list * 'a moveCand list) ref

module FPMap =
  Hashtbl.Make
    (struct
       type t = Os.fullfingerprint
       let hash = Os.fullfingerprintHash
       let equal = Os.fullfingerprintEqual
     end)

(* Since moves are detected on the client then must keep candidates for both
   replicas separately. *)
let files1 : file moveCands FPMap.t = FPMap.create 1000
let files2 : file moveCands FPMap.t = FPMap.create 1000

let dirs1 : (int, dir moveCands) Hashtbl.t = Hashtbl.create 100
let dirs2 : (int, dir moveCands) Hashtbl.t = Hashtbl.create 100

let reset () =
  FPMap.reset files1;
  FPMap.reset files2;
  Hashtbl.reset dirs1;
  Hashtbl.reset dirs2;
  Hashtbl.reset hides;
  Hashtbl.reset moves

let filesOfRep = function
  | `REP1 -> files1
  | `REP2 -> files2

let dirsOfRep = function
  | `REP1 -> dirs1
  | `REP2 -> dirs2

let repToString = function
  | `REP1 -> "replica1"
  | `REP2 -> "replica2"

let getRc x = function
  | `REP1 -> x.differ.rc1
  | `REP2 -> x.differ.rc2

let getOtherRc x = function
  | `REP1 -> x.differ.rc2
  | `REP2 -> x.differ.rc1

let withRc x rc = function
  | `REP1 -> {x.differ with rc1 = rc}
  | `REP2 -> {x.differ with rc2 = rc}

let originalRc rc =
  match rc.status with
  | `MovedOut _ -> {rc with typ = `ABSENT; status = `Deleted}
  | `MovedIn _  -> {rc with status = `Created}
  | _ -> rc

let moveRc rc path' rc' otherRc' =
  match rc.status with
  | `Deleted ->
      {rc with typ = rc'.typ; status = `MovedOut (path', rc', otherRc');
        size = Update.updateSize path' rc'.ui}
  | `Created ->
      {rc with status = `MovedIn (path', rc', otherRc')}
  | _ -> assert false

let moveDiffer rep a b =
  debugverbose (fun () ->
    Util.msg "Creating a move instruction for paths '%s' and '%s' in %s\n"
      (Path.toString a.path) (Path.toString b.path) (repToString rep));
  let rc' = moveRc (getRc a rep) b.path (getRc b rep) (getOtherRc b rep) in
  let diff = withRc a rc' rep
  and errors1 = Safelist.append a.differ.errors1 b.differ.errors1
  and errors2 = Safelist.append a.differ.errors2 b.differ.errors2 in
  Different {diff with errors1; errors2}

(* A pair of a delete and a create items identified as a potential move is
   replaced by a single "move" item only when at least one of the delete or
   create is not conflicting and neither is marked for merging.

   If there are no conflicts then the move item is created on the old path,
   unless the new path is overwriting something (so `Created really means
   change of type), in which case the move item is created on the new path.
   Othewise, the move item is created on the path that has the conflict. This
   is done to make it easier for the user to understand what is going on and
   allow resolving the conflict atomically (by propagating or reverting the
   "move" as a single action).

   The item on the other path will be embedded inside the move item and then
   eventually removed from list of reconItems. *)
let prepMove rep a b =
  debug (fun () ->
    Util.msg "Found a possible move: %s patha=%s pathb=%s\n"
      (repToString rep) (Path.toString a.path) (Path.toString b.path));
  let merge = a.differ.direction = Merge || b.differ.direction = Merge in
  let conflictA = Common.isConflict a.differ.direction || merge
  and conflictB = Common.isConflict b.differ.direction || merge in
  let deletedA = (getRc a rep).status = `Deleted
  and deletedB = (getRc b rep).status = `Deleted in
  (* A `Created can shadow a `Deleted of old entity on the same path *)
  let shadowedA = (getOtherRc a rep).typ <> `ABSENT
  and shadowedB = (getOtherRc b rep).typ <> `ABSENT in
  (* TODO Perhaps should not create a "move" if a deletion of another
     entity has been shadowed on the new path? *)
  match conflictA, conflictB with
  | false, false when deletedA && not shadowedB ->
                    set a.path (moveDiffer rep a b); hide b.path
  | false, false when deletedA ->
                    hide a.path; set b.path (moveDiffer rep b a)
  | false, false when deletedB && not shadowedA ->
                    hide a.path; set b.path (moveDiffer rep b a)
  | false, false -> set a.path (moveDiffer rep a b); hide b.path
  | true, false  -> set a.path (moveDiffer rep a b); hide b.path
  | false, true  -> hide a.path; set b.path (moveDiffer rep b a)
  | true, true   ->
      debug (fun () -> Util.msg "Move source and target both have conflicts\n")

(*------------*)

let extractStamp rc =
  match rc.ui with
  | NoUpdates | Error _ -> assert false
  | Updates (uc, prev) ->
      match uc with
      | Absent | File (_, ContentsSame) ->
          begin match prev with
          | PrevFile (_, _, stamp, _) -> stamp
          | PrevDir _ | PrevSymlink | New -> assert false
          end
      | File (_, ContentsUpdated (_, stamp, _)) -> stamp
      | Dir _ | Symlink _ -> assert false

let extractProps rc =
  match rc.ui with
  | NoUpdates | Error _ -> assert false
  | Updates (uc, prev) ->
      match uc with
      | Absent ->
          begin match prev with
          | PrevFile (props, _, _, _)
          | PrevDir props -> props
          | PrevSymlink | New -> assert false
          end
      | File (props, _)
      | Dir (props, _, _, _) -> props
      | Symlink _ -> assert false

exception Best of (int * int)
exception Time

(* Heuristics for detecting moves:
   1. Moved files and directories are detected by matching deletes and
      creations with same contents. A move/rename together with content
      change is not detected.
      For files, contents means file contents. For directories, contents
      means the names and contents of all children recursively.
   2. A delete and a create are matched as a possible move/rename with
      following preference (with best matches first):
      - (for files only) inodes match
      - mtimes and parents are same, names not (a renamed file/dir)
      - mtimes and names are the same, parents not (a moved file/dir)
      - parents are the same, names not (a renamed file/dir)
      - names are the same, parents not (a moved file/dir)
      - mtime are the same, parents and names not (renamed and moved file/dir)
      - nothing is the same (except contents) (renamed and moved file/dir)
      - parents and names together can't be the same, that indicates an error *)
let commonPref rep best i a b =
  let rc = getRc a rep and rc' = getRc b rep in
  let time = Props.time (extractProps rc)
  and time' = Props.time (extractProps rc') in
  (* Can't use Props.same_time because it does not always
     actually compare synced times. *)
  let timeSame = time -. time' = 0.
  and dirSame = Path.compare a.dir b.dir = 0
  and nameSame = Name.eq a.name b.name in
  let s = (* Lower score means better match *)
    match timeSame, dirSame, nameSame with
    | true,  true,  false -> 1
    | true,  false, true  -> 2
    | false, true,  false -> 3
    | false, false, true  -> 4
    | true,  false, false -> 5
    | false, false, false -> 6
    | _, true, true -> assert false
  in
  if s < snd best then (i, s) else best

let filePref rep best i (a : file moveCand) (b : file moveCand) =
  match extractStamp (getRc a rep), extractStamp (getRc b rep) with
  | InodeStamp inode, InodeStamp inode' when inode = inode' ->
      raise_notrace (Best (i, -1)) (* Contents and inode match exactly *)
  | _ ->
      commonPref rep best i a b

(* A helper function to get the nth element from list and at the same time
   remove it from the list. *)
let takeNth l n =
  if n < 0 then invalid_arg "takeNth" else
  let rec nth_aux n accu = function
    | [] -> failwith "takeNth"
    | a :: l -> if n = 0 then (a, Safelist.rev_append accu l)
                else nth_aux (n - 1) (a :: accu) l
  in nth_aux n [] l

(* A helper function to fold over list with element index and accumulator. *)
let rec foldi_left f i accu = function
  | [] -> accu
  | a :: l -> foldi_left f (succ i) (f i accu a) l

(* Given two lists, finds unique pairs of best matching elements from each list
   using the provided match grading function. The "best" match is considered
   only locally, not globally. That is, the result is not a set of absolute
   best possible matches for all elements as a whole; instead, every element
   will get their best possible match at that time, reducing the search pool
   for the following elements.

   Inputs must be a list of created files/directories and a list of deleted
   files/directories (in any order: al, dl or dl, al; no difference). A pair
   of matching elements from these lists is taken as a moved file/directory.

   [al] stands for adds list (created files/dirs) and [dl] for deletes list.

   [matches] runs in quadratic time (length of al (≈ length of dl)), worst case.
   The lists are expected to be very small (only a couple of elements), thus
   the expected run time is constant. *)
let matches t heurf rep (al, dl) =
  let matches_aux (adds : 'a moveCand list) (del : 'a moveCand) =
    if Unix.gettimeofday () -. t > 15. then
      raise_notrace Time; (* A safety valve, just in case *)
    let best =
      try
        foldi_left (fun i best add ->
          heurf rep best i del add) 0 (-1, 128 (* just a big number *)) adds
      with Best b -> b
    in
    match best with
    | (-1, _) -> adds
    | (bi, _) ->
        let (add, adds') = takeNth adds bi in
        prepMove rep del add;
        adds'
  in
  ignore (Safelist.fold_left matches_aux al dl)

let detect () =
  let t = Unix.gettimeofday () in
  try
    [`REP1; `REP2] |> Safelist.iter (fun rep ->
      FPMap.iter (fun _ l -> matches t filePref rep !l) (filesOfRep rep);
      Hashtbl.iter (fun _ l -> matches t commonPref rep !l) (dirsOfRep rep))
  with Time ->
    (* This is unlikely to ever happen, except with synthetic test cases. *)
    debug (fun () ->
      Util.msg "Detecting moves is taking too long... Skipping the rest.\n")

(*------------*)

let addCandidate added path key differ mfind madd =
    let add el (al, dl) = if added then el :: al, dl else al, el :: dl in
    match Path.deconstructRev (Path.forceLocal path) with
    | None -> ()
    | Some (name, dir) ->
        let dir = Path.makeGlobal dir in
        let el = { path; dir; name; differ } in
        match mfind key with
        | None -> madd key (ref (add el ([], [])))
        | Some l -> l := add el !l

let addFileCandidate added rep path fp differ =
  if not (Os.isPseudoFingerprint fp) then begin
    debug (fun () ->
      Util.msg "addFileCandidate: %s %s path=%s, fp=%s\n"
        (repToString rep) (if added then "NEW" else "DEL")
        (Path.toString path) (Os.fullfingerprint_to_string fp));
    let map = filesOfRep rep in
    addCandidate added path fp differ (FPMap.find_opt map) (FPMap.add map)
  end

let addDirCandidate added rep path len differ =
  debug (fun () ->
    Util.msg "addDirCandidate: %s %s path=%s, contentHash=%d\n"
      (repToString rep) (if added then "NEW" else "DEL")
      (Path.toString path) len);
  let map = dirsOfRep rep in
  addCandidate added path len differ (Hashtbl.find_opt map) (Hashtbl.add map)

let rec hashArchive arch h =
  match arch with
  | Update.ArchiveDir (_, children) ->
      Update.NameMap.fold
        (fun n a h ->
           Uutil.hash2 (Name.hash n)
                       (hashArchive a h))
        children h
  | ArchiveFile (_, dig, _, _) ->
      Uutil.hash2 (Uutil.hash dig) h
  | ArchiveSymlink content ->
      Uutil.hash2 (Uutil.hash content) h
  | NoArchive ->
      135

(* Note that for `Created items, [prevState] does not need to be NoArchive.
   When the type of entity at the path has changed, the deletion of old entity
   has been shadowed by the creation of new entity.

   We could extract that implict deletion and use it to detect a move of the
   old entity. We don't do that because we have no control over propagation
   direction and order. For example, if 'b' was renamed to 'c' and 'a' was
   renamed to 'b', we could detect both renames but the only way to propagate
   them correctly is to do it atomically (both in same direction and in same
   exact sequence). Since this is not guaranteed, we are not even going to
   detect such shadowed moves. *)
let processHint rep path differ = function
  | {status = `Created; ui = Updates (File (_, ContentsUpdated (fp, _, _)), _);
        size; _} ->
      (* New file -- potential move target *)
      if snd size <> Uutil.Filesize.zero then
        addFileCandidate true rep path fp differ
  | {status = `Deleted; ui = Updates (Absent, PrevFile (_, fp, _, _)); _} ->
      (* Deleted file -- potential move source *)
      addFileCandidate false rep path fp differ
  | {status = `Created; ui = Updates (Dir (_, chldrn, _, _), _) as ui; _} ->
      (* New directory -- potential move target *)
      if chldrn <> [] then
        let dirHash = hashArchive (Update.makeArchive ui) 0 in
        addDirCandidate true rep path dirHash differ
  | {status = `Deleted; ui = Updates (Absent, PrevDir _); _} ->
      (* Deleted directory -- potential move source *)
      let dirHash = hashArchive (Update.getSubArchiveLocal path) 0 in
      (* The archive hash algorithm results in the same value on both the
         client and server. That's why we don't need to transfer the PrevDir
         contents from the server and can use the local archive to get the
         previous dir hash. *)
      addDirCandidate false rep path dirHash differ
  (* Not interested in the following *)
  | {status = `Created; ui = Updates (Symlink _, _); _}
  | {status = `Deleted; ui = Updates (_, PrevSymlink); _}
  | {status = `Modified | `PropsChanged | `Unchanged | `MovedOut _ | `MovedIn _; _}
  | {ui = NoUpdates | Error _; _}
  (* Impossible combinations  *)
  | {status = `Created; ui = Updates (Absent, _); _}
  | {status = `Created; ui = Updates (File (_, ContentsSame), _); _}
  | {status = `Deleted; ui = Updates ((File _ | Dir _ | Symlink _), _); _}
  | {status = `Deleted; ui = Updates (_, New); _} ->
      ()

let addHint path = function
  | Different differ as rplc when Prefs.read detectmoves ->
      processHint `REP1 path differ differ.rc1;
      processHint `REP2 path differ differ.rc2;
      rplc
  | Different _
  | Problem _ as rplc -> rplc
