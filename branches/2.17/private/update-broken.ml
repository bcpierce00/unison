(* $I1: Unison file synchronizer: private/update-broken.ml $ *)
(* $I2: Last modified by bcpierce on Mon, 19 Jul 1999 18:14:04 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

open Util
open Os
open Common

(* type partial memory is for strategies using abstract representation of 
the filesystems *)
type partialMemory =
   InoModTime
 | Digest

(* The archive type is a property of each archive. Once you use one strategy
you should go on using this same strategy for every synchronisation, unless
you delete the archive. All archives are identified by the root list of
the filesystems whose synchronisation they represent *)
type archiveType =
   Mirror
 | Partial of partialMemory

let moduleName = "update"
let dangerPath = [string2name "danger"]
let infoPath = [string2name "info"]
let arcContentPath = [string2name "archive"]


(*****************************************************************************)
(*                        OPTIONS                                            *)
(*****************************************************************************)

let allTree =
 Prefs.createBoolPref "all" false "display all files (not just changed ones)"

let flagF =
 Prefs.createBoolPref "f" false "create automatically new archives"

let archiveType =
 Prefs.createPref "archive" Mirror "type of archive"
  (fun _ -> function 
     "mirror" -> Mirror
   | "inoModTime" -> Partial InoModTime
   | "digest" -> Partial Digest
   | other -> raise (Prefs.IllegalValue (
                  "option archive :\n"
                ^ "mirror -> archive is a complete mirror\n"
                ^ "inoModTime -> compact archive using unix informations\n"
                ^ other ^ " is not a legal value")))


(*****************************************************************************)
(*                           COMMON DEFINITIONS                              *)
(*****************************************************************************)

let arcType2string = function
  Mirror -> "mirror" 
| Partial InoModTime -> "inoModTime"
| Partial Digest -> "digest"

let archiveFspath rootList =
  let string = String.concat "" (List.map root2string rootList) in
  Archive.string2archiveFspath string

let giveFspathList rootList =
  Util.filterMap
  (function
     Local fspath -> Some fspath
   | Remote _ -> None)
  rootList

(*************************************************************************)
(*                       FUNCTION GIVE ARCHIVE TYPE                      *)
(*************************************************************************)
(* This function gives the strategy used for the archive identified by   *)
(* the root list                                                         *)
(*************************************************************************)

let givArcType rootList =
  let archive = archiveFspath rootList in
  let infoFileName = file2string archive infoPath in
  if Sys.file_exists infoFileName then
    begin 
      let infoFile  = open_in infoFileName in
      let result =
        (match input_line infoFile with
          "inoModTime" -> Partial InoModTime
        | "mirror" -> Mirror
        | "digest" -> Partial Digest
        | _ -> raise (OsError "Invalide state")) in
        close_in infoFile;result
    end
  else !archiveType


(*************************************************************************)
(*                       FUNCTION CREATE ARCHIVE IF NEEDED               *)
(*************************************************************************)
(* It will use the strategy given in the preferences if no archive is    *)
(* found                                                                 *)
(*************************************************************************)

let createIfNeeded rootList =
  let archiveFspath = archiveFspath rootList in
  if not (Ask.exists archiveFspath emptypath) then
    (Action.createDir archiveFspath emptypath;
     let infoFile = open_out (file2string archiveFspath infoPath) in
     output_string infoFile (arcType2string !archiveType);
     output_char infoFile '\n';
     output_string infoFile
       (String.concat "\n" (List.map root2string rootList));
     close_out infoFile;
     !archiveType)
  else givArcType rootList

(*****************************************************************************)
(*                               MODULE DANGER                               *)
(*****************************************************************************)
(* Provides the functions used to treat the dangerous paths :                *)
(* A path is dangerous if the last markUpdated action on this path failed    *)
(*****************************************************************************)

module Danger = struct

  module PathSet =
    Set.Make (struct
		type t = path
		let compare = comparePath
	      end)


  let storeDangerousPaths, loadDangerousPaths =
   (Remote.giveMarshalToFileFunctions "dangerousPaths" dangerPath 
    : PathSet.t Remote.marshalingToFileFunctions)

  let addPathName set path =
   PathSet.add path set

  let addPathList set pathList =
   List.fold_left addPathName set pathList

  let mark rootList pathList =
   ignore (createIfNeeded rootList);
   let arcFspath = archiveFspath rootList in
   let oldSet = loadDangerousPaths arcFspath PathSet.empty in
   let newSet = addPathList oldSet pathList in
   storeDangerousPaths newSet arcFspath

  let ifDangerous dangerSet path =
   PathSet.mem path dangerSet

  let delete dangerSet path =
   PathSet.remove path dangerSet

  let deleteFamily dangerSet path =
   PathSet.fold
    (fun element dangerSet ->
     if (ifChildPath element path) 
     then dangerSet
     else PathSet.add element dangerSet)
      (PathSet.remove path dangerSet)
      PathSet.empty

  let childNames dangerSet path =
   let length = List.length path in
   PathSet.fold
    (fun element names ->
      if (ifChildPath element path) 
      then StringSet.add
       (name2string (List.nth element length)) names
      else names)
    dangerSet
    StringSet.empty

end (* module Danger *)

(*****************************************************************************)
(*                          MODULE TYPE UPDATES                              *)
(*****************************************************************************)
(* Signature of the update detector.  Below, we provide three                *)
(* implementations of this signature: a local implementation, which does     *)
(* the real work, a proxy implementation, which invokes an update detection  *)
(* process on a remote host and parses its reply, and a stub implementation  *)
(* (the one actually called by the main program) that switches between       *)
(* the other two appropriately.                                              *)
(*****************************************************************************)

module type UPDATES = sig

  val find :
      root list
   -> Danger.PathSet.t       (* list of paths to mark dangerous *)
   -> path list              (* Subtrees we intend to reconcile *)
   -> bool * updateItem list list (* Structures describing dirty files and
                                directories (1 for each given path) *)
  val mark :
      root list
   -> (path * bool) list
   -> confirmation list

end (* module type UPDATES *)

(*****************************************************************************)
(*                            MODULE MIRRORUPDATES                           *)
(*****************************************************************************)

module MirrorUpdates : UPDATES = struct

(* create the update tree for a regular file referred by currfspath, path *)
let buildUpdateReg currfspath arcFspath path =
 try
  let desc = Info.describe currfspath path [Info.Mod;Info.Size] in
  if (Ask.sameFiles currfspath arcFspath path) then
   if !allTree 
   then NoUpdates (Some (File desc))
   else NoUpdates None
  else
   let oldDesc = Info.describe arcFspath path [Info.Mod;Info.Size] in
   Updates (File desc, Previous (Info.SREG, oldDesc))
 with OsError s -> Error ("Os error : " ^s)

let buildNewFile currfspath path prevState =
 let newDesc = Info.describe currfspath path [Info.Mod;Info.Size] in
 Updates (File newDesc, prevState)

(* creates the updateItem corresponding to the content of a created directory*)
let rec buildNewDir dangerous currfspath path prevState =
 let desc = Info.describe currfspath path [Info.Mod;Info.Size] in
 let dirChildren = StringSet.elements
  (StringSet.union
   (Info.childrenOf currfspath path)
   (Danger.childNames dangerous path)) in
 let dirList = updateAllDir dangerous currfspath path dirChildren in
  Updates (Dir (desc, dirList), prevState)

and updateAllDir dangerous currfspath path dirChildren =
 Util.filterMap
  (fun fileName ->
   let childName = string2name fileName in
   let newPath = childPath path childName in
   if Ignore.test newPath
   then None
   else
    let prevState = Previous (Info.NOTHING, Info.emptydesc) in
    let update =
    (match (Info.typeOf currfspath newPath) with
       Info.SREG -> buildNewFile currfspath newPath prevState
     | Info.SDIR -> buildNewDir dangerous currfspath newPath prevState
     | other -> Error "Non implemented type") in
    Some(childName, update))
  dirChildren

(* create the update tree for a regular file or a dir referred by pat *)
let rec buildUpdate dangerous currfspath arcFspath path =
 let newType = Info.typeOf currfspath path in
 let oldType = Info.typeOf arcFspath path in
 let danger = Danger.ifDangerous dangerous path in
 match (newType, danger, oldType) with
   (Info.NOTHING, true, oldType) ->
    Updates (Absent, Previous (oldType, Info.emptydesc))
 | (Info.NOTHING, false, Info.NOTHING) ->
    if !allTree then
     NoUpdates (Some Absent)
    else NoUpdates None
 | (Info.NOTHING, false, oldType) ->
    let oldDesc = Info.describe arcFspath path [Info.Mod;Info.Size] in
    Updates (Absent, Previous (oldType, oldDesc))
 | (Info.SREG, false, Info.SREG) ->
    buildUpdateReg currfspath arcFspath path
 | (Info.SREG, _, _) ->
    let oldDesc = Info.describe currfspath path [Info.Mod;Info.Size] in
    let prevState = Previous (oldType, oldDesc) in
    buildNewFile currfspath path prevState
 | (Info.SDIR, _, Info.SDIR) ->
    let oldDesc = Info.describe currfspath path [Info.Mod;Info.Size] in
    let prevState = Previous (oldType, oldDesc) in
    if danger then
     buildNewDir dangerous currfspath path prevState
    else
     buildUpdateDir dangerous currfspath arcFspath path prevState
 | (Info.SDIR, _, _) ->
    let oldDesc = Info.describe currfspath path [Info.Mod;Info.Size] in
    let prevState = Previous (oldType, oldDesc) in
    buildNewDir dangerous currfspath path prevState
 | (type1, _, _) -> Error "NonImplemented type"

(* create the update tree for a directory referred by pat *)
and buildUpdateDir dangerous currfspath arcFspath path prevState =
 try
  let curChi = Info.childrenOf currfspath path in
  let arcChi = Info.childrenOf arcFspath path in   
  let unionDir = StringSet.elements 
   (StringSet.union
    (StringSet.union curChi arcChi)
    (Danger.childNames dangerous path)) in
  let updateContentDir =
   exploreDir dangerous currfspath arcFspath unionDir path in
  let desc = Info.describe currfspath path [Info.Mod;Info.Size] in
  let realUpdated = Util.filter
   (fun (name, update) ->
     match update with
	 Updates _ -> true | Error _ -> true | _ -> false)
   updateContentDir in
  let updatesNumber = List.length realUpdated in
  let updateContentAll = Dir (desc, updateContentDir) in
  let updateContent = Dir (desc, realUpdated) in
  match (updatesNumber, !allTree) with
    (0, true) -> NoUpdates (Some updateContentAll)
  | (0, false) -> NoUpdates None
  | (_, true) -> Updates (updateContentAll, prevState)
  | (_, false) -> Updates (updateContent, prevState)
 with OsError err -> Error ("OsError : " ^err)

(* exploreDir gives the (name, updateItem) list of the content of dir pat *)
and exploreDir dangerous currfspath arcFspath unionDir path =
 Util.filterMap
  (fun fileName ->
    let childName = string2name fileName in
    let newPath = childPath path childName in
    if Ignore.test newPath then None
    else
     let update =
      buildUpdate dangerous currfspath arcFspath newPath in
     match update with
       NoUpdates None -> None
     | other -> Some (childName, other))
  unionDir

let find rootList dangerSet pathList =
 let arcFspath = archiveFspath rootList in
 let fspathList = giveFspathList rootList in
 let arcContentFspath = file2fspath arcFspath arcContentPath in
 let oldDangerous = Danger.loadDangerousPaths arcFspath Danger.PathSet.empty in
 let dangerous = Danger.PathSet.union oldDangerous dangerSet in
 let bool = Ask.exists arcFspath emptypath in
 let updateItemList = 
  List.map 
   (fun currfspath ->
    List.map
    (buildUpdate dangerous currfspath arcContentFspath)
    pathList) 
   fspathList in
 (bool, updateItemList)

(*****************************************************************************)
(*                         FUNCTION LOCALMARKUPDATED                         *)
(*****************************************************************************)

let updateArchive  dangerous currfspath arcFspath goInDirectory path =
  let oldType = Info.typeOf arcFspath path in
  let newType = Info.typeOf currfspath path in
  let notDangerous = Danger.deleteFamily dangerous path in
  let fileName = file2string currfspath path in
  match (newType, oldType) with
    (Info.NOTHING, Info.NOTHING) ->
     (notDangerous, Succeeded)
  | (Info.NOTHING, _) ->
     (try Action.delete arcFspath path;
          (notDangerous, Succeeded)
      with OsError err ->  (dangerous, Failed err))
  | (Info.SREG, _) ->
      let conf = Files.copy false (Local currfspath, Local arcFspath) path in
      if (confirmation2bool conf) then (notDangerous, Succeeded)
      else (dangerous, conf)
  | (Info.SDIR, _) ->
     if not goInDirectory 
     then
      (let conf = Files.mkdir (Local arcFspath) path in
       if (confirmation2bool conf) 
       then (Danger.delete dangerous path, Succeeded)
       else (dangerous, conf))
     else
      (let conf = Files.copy false (Local currfspath, Local arcFspath) path in
       if (confirmation2bool conf) 
       then (notDangerous, Succeeded)
       else (dangerous, conf))
    | _ -> raise (Can'tHappen ("Update", "updateArchive"))

let mark rootList pathBoolList =
 let fspathList = giveFspathList rootList in
 let currfspath = List.hd fspathList in
 let arcFspath = archiveFspath rootList in
 let arcContentFspath = file2fspath arcFspath arcContentPath in
 let dangerous = Danger.loadDangerousPaths arcFspath 
  Danger.PathSet.empty in
 let markSingle (confList, dangerousSet) = 
 (function (path, goInDirectory) -> 
   let newDangerousSet, newConf =
    updateArchive dangerousSet currfspath
    arcContentFspath goInDirectory path in
  (confList@[newConf], newDangerousSet)) in
 let confList, newDangerous = List.fold_left
  markSingle ([], dangerous) pathBoolList in
 Danger.storeDangerousPaths newDangerous arcFspath;
 confList

end (* module MirrorUpdates *)

(*****************************************************************************)
(*                 INODE NUMBER / DIGEST UPDATE DETECTORS                    *)
(*****************************************************************************)

module InexactUpdates : UPDATES = struct

type content =
    Ino of float                (* last synchronization time *)
         * int list             (* inode number list *)
 | Dig of string                (* one digest for all replicas *)

(* We have an archive per host, so we need to have a list of information
in each path : one information per local replica *)
type archive =
   ArchiveDir of Info.desc list * (name * archive) list
 | ArchiveLeaf of Info.filetype * content * Info.desc list
 | NoArchive

let oldInfoOf rank currfspath = function
  ArchiveDir (oldDescList, _) ->
    let oldDesc = List.nth oldDescList rank in
    Previous (Info.SDIR, oldDesc)
| ArchiveLeaf (oldType, _, oldInfoList) ->
    let oldDesc = List.nth oldInfoList rank in
    Previous (oldType, oldDesc)
| NoArchive -> Previous (Info.NOTHING, Info.emptydesc)

(* create the update tree for a regular file referred by path *)
let buildUpdateReg rank currfspath path content descList =
  let oldDesc = List.nth descList rank in
  let newDesc = Info.describe currfspath path [Info.Mod;Info.Size] in
  match content with
    Ino (syncTime, inodeList) ->
      (try
         let oldInodeNumber = List.nth inodeList rank in
         let newInodeNumber = Info.inodeNumberOf currfspath path in
         let newModTime = Info.modTimeOf newDesc in
         if ((newInodeNumber = oldInodeNumber)
             &&(newModTime <= syncTime))
         then (if !allTree then
           NoUpdates (Some (File newDesc)) else NoUpdates None)
         else
           Updates (File newDesc, Previous (Info.SREG, oldDesc))
       with OsError err -> Error ("Os error : " ^err))
  | Dig oldDigest ->
      (try
         let newDigest = Info.md5Digest currfspath path in
         if oldDigest = newDigest
	 then (if !allTree then NoUpdates (Some (File newDesc)) 
                           else NoUpdates None)
         else
           Updates (File newDesc, Previous (Info.SREG, oldDesc))
       with OsError err -> Error ("Os error : " ^err))

let buildNewFile currfspath path prevState =
  let newDesc = Info.describe currfspath path [Info.Mod;Info.Size] in
  Updates (File newDesc, prevState)

(* creates the updateItem corresponding to the content of a created directory*)
let rec buildNewDir dangerous currfspath path prevState =
  (* Trace.message ("buildNewDir " ^ (file2string currfspath path) ^ "\n"); *)
  Trace.statusDetail (path2string path);
  let desc = Info.describe currfspath path [Info.Mod;Info.Size] in
  let dirChildren = StringSet.elements 
                      (StringSet.union
                         (Info.childrenOf currfspath path)
                         (Danger.childNames dangerous path)) in
  let dirList = updateAllDir dangerous currfspath path dirChildren in
  Updates (Dir (desc, dirList), prevState)

and updateAllDir dangerous  currfspath path dirChildren =
  Util.filterMap
    (fun fileName ->
       let childName = string2name fileName in
       let newPath = childPath path childName in
       if Ignore.test newPath
       then None
       else
         let prevState = Previous (Info.NOTHING, Info.emptydesc) in
         let update =
           (match Info.typeOf currfspath newPath with
             Info.SREG -> buildNewFile currfspath newPath prevState
           | Info.SDIR -> buildNewDir dangerous currfspath newPath prevState
           | other -> Error "Non implemented type") in 
           Some(childName, update))
    dirChildren

(* create the update tree for a regular file or a dir referred by its path *)
let rec buildUpdate rank archive dangerous currfspath path =
  Trace.statusDetail (path2string path);
  let newType = Info.typeOf currfspath path in
  (match newType with
      Info.SDIR -> Trace.statusDetail (path2string path)
    | _ -> ());
  let danger = Danger.ifDangerous dangerous path in
  match (newType, danger, archive) with
    (Info.NOTHING, true, _) -> Updates(Absent, oldInfoOf rank currfspath archive)
  | (Info.NOTHING, false, NoArchive) -> if !allTree
    then NoUpdates (Some Absent)
    else NoUpdates None
  | (Info.NOTHING, false, _) -> 
     Updates (Absent, oldInfoOf rank currfspath archive)
  | (Info.SREG, false, ArchiveLeaf (Info.SREG, content, theList)) ->
      buildUpdateReg rank currfspath path content theList
  | (Info.SREG, _, _) ->
      buildNewFile currfspath path (oldInfoOf rank currfspath archive)
  | (Info.SDIR, _, ArchiveDir (_, children)) ->
      let prevState = oldInfoOf rank currfspath archive in
      if danger
      then buildNewDir dangerous currfspath path prevState
      else buildUpdateDir rank children dangerous currfspath path prevState
  | (Info.SDIR, _, _) ->
      buildNewDir dangerous  currfspath path (oldInfoOf rank currfspath archive)
  | (type1, _, _) -> Error ("NonImplemented types")

(* create the update tree for a directory referred by pat *)
and buildUpdateDir rank arcChi dangerous currfspath path prevState =
  try
    let curChi = Info.childrenOf currfspath path in
    let unionDir =
      StringSet.elements
        (List.fold_right 
           (fun name y -> StringSet.add (name2string name) y)
           (fst (List.split arcChi))
           (StringSet.union curChi (Danger.childNames dangerous path))) in
    let updateContentDir =
      exploreDir rank arcChi dangerous currfspath path unionDir in
    let realUpdated = Util.filter
                      (fun (name, update) ->
                         match update with
                           Updates _ -> true | Error _ -> true | _ -> false)
                      updateContentDir in
    let desc = Info.describe currfspath path [Info.Mod;Info.Size] in
    let updatesNumber = List.length realUpdated in
    let updateContentAll = Dir (desc, updateContentDir) in
    let updateContent = Dir (desc, realUpdated) in
    match (updatesNumber, !allTree) with
      (0, true) -> NoUpdates (Some updateContentAll)
    | (0, false) -> NoUpdates None
    | (_, true) -> Updates (updateContentAll, prevState)
    | (_, false) -> Updates (updateContent, prevState)
  with OsError err -> Error ("OsError : " ^err)

(* exploreDir gives the (name, updateItem) list of the content of dir pat *)
and exploreDir rank arcChi dangerous currfspath path unionDir =
  Util.filterMap
    (fun fileName ->
       let childName = string2name fileName in
       let newPath = childPath path childName in
       if Ignore.test newPath
       then None
       else
         let update =
           buildUpdate rank
             (try List.assoc childName arcChi
              with Not_found -> NoArchive)
             dangerous  currfspath newPath in
             match update with
               NoUpdates None -> None
             | theUpdate -> Some(childName, theUpdate))
    unionDir

let rec findPath archive = function
  [] -> archive
| name::remainder ->
    match archive with
      ArchiveDir (oldDesc, children) ->
        (try let newArchive = List.assoc name children in
        findPath newArchive remainder
	 with Not_found -> NoArchive)
    | _ -> NoArchive

let storeArchive, loadArchive =
  (Remote.giveMarshalToFileFunctions "archive" arcContentPath
   : archive Remote.marshalingToFileFunctions)

let find rootList dangerSet pathList =
  let arcFspath = archiveFspath rootList in
  let oldDangerous = Danger.loadDangerousPaths arcFspath
                       Danger.PathSet.empty in
  let dangerous = Danger.PathSet.union oldDangerous dangerSet in
  let archive = loadArchive arcFspath NoArchive in
  let fspathList = giveFspathList rootList in
  let arcType = givArcType rootList in
  let bool = (archive<>NoArchive) in
  let updateItemList, _ = List.fold_left 
   (fun (updateItemList, i) currfspath ->
     let result =
      List.map (fun path ->
                 buildUpdate i (findPath archive path) dangerous currfspath path)
      pathList in 
     (updateItemList@[result], i +1))
   ([], 0) fspathList in
  (bool, updateItemList)

(*****************************************************************************)
(*                         FUNCTION LOCALMARKUPDATED                         *)
(*****************************************************************************)

let rec updateLeaf arcType markChildren archive fspathList path =
  let newType = Info.typeOf (List.hd fspathList) path in
  match newType with
    Info.SREG ->
      let content =
        (match arcType with
          Partial InoModTime ->
            let inodeList =
              List.map (fun fspath -> Info.inodeNumberOf fspath path)
                fspathList in
            Ino (getTime(), inodeList)
        | Partial Digest -> Dig (Info.md5Digest (List.hd fspathList) path)
        | _ -> raise (Can'tHappen ("Update", "updateLeaf"))) in
      let descList =
        List.map (fun fspath ->
	 	     Info.describe fspath path [Info.Mod;Info.Size])
          fspathList in
      ArchiveLeaf (Info.SREG, content, descList)
  | Info.SDIR ->
      let descList =
        List.map (fun fspath ->
                     Info.describe fspath path [Info.Mod;Info.Size])
          fspathList in
      let newBornList =
        (if not markChildren then
          (match archive with
             ArchiveDir (oldDesc, children) -> children
           | _ -> [])
         else let children =
           StringSet.elements (Info.childrenOf (List.hd fspathList) path) in
           List.fold_left
             (fun list childString ->
	        let childName = string2name childString in
	        let theChildPath = childPath path childName in
	        (if (Ignore.test theChildPath)
	         then list
	         else (childName, updateLeaf arcType markChildren
                       NoArchive fspathList theChildPath)
                   ::list))
             [] children)
      in ArchiveDir (descList, newBornList)
  | _ -> raise (Can'tHappen ("Update", "updateArchive"))

let rec remove_assoc name = function
  [] -> []
| pair::remainder ->
    if compareName (fst pair) name = 0 then remainder
    else pair::(remove_assoc name remainder)

let rec updateArchive arcType markChildren archive fspathList path =
  function
    [] -> updateLeaf arcType markChildren archive fspathList path
  | name::remainder ->
      let child, children =
        (match archive with
	  ArchiveDir (_, children) ->
	    ((try List.assoc name children
              with Not_found -> NoArchive), children)
        | _ -> (NoArchive, [])) in
      let newArchive = updateArchive arcType markChildren child
                         fspathList path remainder in
      let withoutChild = remove_assoc name children in
      let withNewChild =
        (match newArchive with
	  NoArchive -> withoutChild
	| _ -> (name, newArchive)::withoutChild) in
      let descList =
        List.map (fun fspath ->
		     Info.describe fspath path [Info.Mod;Info.Size])
          fspathList in
      ArchiveDir (descList, withNewChild)

let rec killArchive archive = function
  [] -> NoArchive
| name::remainder ->
    match archive with
      ArchiveDir (oldDescList, children) ->
        (try
	   let child = List.assoc name children in
	   let newArchive = killArchive child remainder in
	   let withoutChild = remove_assoc name children in
	   let withNewChild =
	     (match newArchive with
	       NoArchive -> withoutChild
	     | _ -> (name, newArchive)::withoutChild) in
	     ArchiveDir (oldDescList, withNewChild)
	 with Not_found ->
	   ArchiveDir (oldDescList, (remove_assoc name children)))
    | _ -> archive

let markRec arcType fspathList (archive, dangerous) (path, markChildren) =
  match Info.typeOf (List.hd fspathList) path with
    Info.NOTHING ->
      (killArchive archive path, Danger.deleteFamily dangerous path)
  | Info.SDIR ->
      (updateArchive arcType markChildren archive fspathList path path,
       Danger.deleteFamily dangerous path)
  | other ->
      (updateArchive arcType false archive fspathList path path,
       Danger.deleteFamily dangerous path)

let mark rootList pathBoolList =
  let arcFspath = archiveFspath rootList in
  let archive = loadArchive arcFspath NoArchive in
  let arcType = givArcType rootList in
  let fspathList = giveFspathList rootList in
  let pathList, boolList = List.split pathBoolList in
  let dangerous = Danger.loadDangerousPaths arcFspath Danger.PathSet.empty in
  let newArchive, newDangerous = List.fold_left
                                   (markRec arcType fspathList)
                                   (archive, dangerous) pathBoolList in
  storeArchive newArchive arcFspath;
  Danger.storeDangerousPaths newDangerous arcFspath;
  List.map (fun path -> Succeeded) pathList

end (* module InoModUpdates *)

(*****************************************************************************)
(*                          MAIN LOCAL FUNCTIONS                             *)
(*****************************************************************************)

let askDangerousLocal rootList =
  let arcFspath = archiveFspath rootList in
  Danger.loadDangerousPaths arcFspath Danger.PathSet.empty
    
let markDangerousLocal rootList pathList =
  try 
    Danger.mark rootList pathList;true
  with _ -> false

let findLocal rootList dangerSet pathList =
  (match givArcType rootList with
    Mirror -> MirrorUpdates.find
  | Partial _ -> InexactUpdates.find)
  rootList dangerSet pathList

let markUpdatedLocal rootList pathBoolList =
  (match createIfNeeded rootList with
     Mirror -> MirrorUpdates.mark
   | Partial _ -> InexactUpdates.mark)
  rootList pathBoolList

(*****************************************************************************)
(*                       XML PRINTERS FOR ASK DANGEROUS                      *)
(*****************************************************************************)

let printDangerousRequest outChan (host, rootList) =
  output_string outChan "ask for dangerous paths :\n";
  Printf.fprintf outChan "host:%s\n" host;
  printRootList outChan "  " rootList

let printDangerousList outChan pathSet =
  let pathList = Danger.PathSet.elements pathSet in
  output_string outChan "result\n";
  Printer.printPathList outChan "  " pathList

(*****************************************************************************)
(*                       XML PRINTERS FOR MARK DANGEROUS                     *)
(*****************************************************************************)

let printMarkDangerousRequest outChan (host, rootList, pathList) =
  Printf.fprintf outChan "ask for marking paths dangerous:\nhost : %s\n" host;
  printRootList outChan "  " rootList;
  output_char outChan '\n';
  Printer.printPathList outChan "  " pathList

let printMarkDangerousResult outChan bool =
  output_string outChan "result\n";
  printBool outChan "" bool

(*****************************************************************************)
(*                    XML PRINTERS FOR FIND UPDATES                          *)
(*****************************************************************************)

let printUpdateRequest outChan
  (host, rootList, dangerList, pathList) =
  Printf.fprintf outChan "ask for updateItems :\nhost : %s\n" host;
  printRootList outChan "  " rootList;
  Printer.printPathList outChan "  " dangerList;
  Printer.printPathList outChan "  " pathList

let printDesc indent desc =
  let result = Info.desc2xml (indent^ "  ") desc in
  Printf.sprintf "description : %s%s" indent result

let printType indent filetype =
  Printf.sprintf "type : %s%s" indent (Info.filetype2string filetype)

let printPrevState indent = function
  Previous (oldType, desc) ->
    Printf.sprintf "%sprevious state:\n%s%s\n%s%s"
      indent indent (printType (indent^ "  ") oldType)
      indent (printDesc (indent^ "  ") desc)

let rec printUpdateItem outChan indent updateItem =
  match updateItem with
    Error str ->
      Printf.fprintf outChan "%ssuspect file : %s" indent str
  | NoUpdates updateContentOption ->
      Printf.fprintf outChan "%sthis file has not changed\n" indent;
      printUpdateContentOption outChan indent updateContentOption;
  | Updates (updateContent, prevState) ->
      Printf.fprintf outChan "%sthere is some change\n" indent;
      printUpdateContent outChan indent updateContent;
      Printf.fprintf outChan "\n%s"
        (printPrevState indent prevState)

and printUpdateContentOption outChan indent = function
  None ->  Printf.fprintf outChan "%sno updates\n" indent
| Some updateContent ->
    printUpdateContent outChan indent updateContent

and printUpdateContent outChan indent = function
  File theDesc ->
    Printf.fprintf outChan "%sregular file:\n%s\n%s\n"
      indent (printDesc (indent^ "  ") theDesc) indent
| Absent ->
    Printf.fprintf outChan  "%sthis file has been deleted\n" indent
| Dir (theDesc, dirItemList) ->
    Printf.fprintf outChan "%sthis is a directory\n%s\n"
      indent (printDesc (indent^ "  ") theDesc);
    printDirItemList outChan (indent^ "  ") dirItemList

and printDirItemList outChan indent =
  List.iter (fun (name, updateItem) ->
               Printf.fprintf outChan "\n%s%s" indent (name2string name);
               printUpdateItem outChan (indent^ "  ") updateItem)

let printUpdateItemList outChan (bool, updateItemLists) =
  output_string outChan "result\n";
  Util.printBool outChan "  " bool;
  List.iter 
   (fun updateItemList -> 
     List.iter (printUpdateItem outChan "    ") updateItemList)
   updateItemLists

(*****************************************************************************)
(*                       XML PRINTERS FOR MARKUPDATE                         *)
(*****************************************************************************)

let printPathBoolList outChan indent =
  List.iter (fun (path, bool) ->
               output_string outChan (Printer.printPath indent path);
               output_char outChan '\n';
               Util.printBool outChan indent bool;
               output_char outChan '\n')

let printMarkUpdatedRequest outChan (host, rootList, pathBoolList) =
  Printf.fprintf
    outChan "ask for paths to be mark updated :\nhost : %s\n" host;
  printRootList outChan "  " rootList;
  printPathBoolList outChan "    " pathBoolList

let printConfirmationList outChan =
  List.iter
    (fun confirmation ->
       printConfirmation outChan confirmation;
       output_char outChan '\n')

let printMarkUpdatedResult outChan confirmationList =
  Printf.fprintf outChan "result\n";
  printConfirmationList outChan confirmationList

(*****************************************************************************)
(*                      ASK DANGEROUS FUNCTIONS                              *)
(*****************************************************************************)

let askDangerousRequestMarshal,
askDangerousRequestUnmarshal =
  (Remote.giveMarshalFunctions "askDangerousRequest"
   : (root list) Remote.marshalingFunctions)

let askDangerousResultMarshal,
askDangerousResultUnmarshal =
  (Remote.giveMarshalFunctions "askDangerousResult"
   : Danger.PathSet.t Remote.marshalingFunctions)

let askDangerousProxy host rootList =
  let newRootList = rootsSeenBy host rootList in
  Remote.print printDangerousRequest (host, rootList);
  Remote.invoke
    askDangerousRequestMarshal askDangerousResultUnmarshal
    host moduleName "askDangerous" newRootList printDangerousList

(*****************************************************************************)
(*                      MARK DANGEROUS FUNCTIONS                             *)
(*****************************************************************************)

let markDangerousRequestMarshal,
markDangerousRequestUnmarshal =
  (Remote.giveMarshalFunctions "markDangerousRequest"
   : (root list * path list) Remote.marshalingFunctions)

let markDangerousResultMarshal,
markDangerousResultUnmarshal =
  (Remote.giveMarshalFunctions "markDangerousResult"
   : bool Remote.marshalingFunctions)

let markDangerousProxy host rootList pathList =
  let newRootList = rootsSeenBy host rootList in
  Remote.print printMarkDangerousRequest (host, rootList, pathList);
  Remote.invoke
    markDangerousRequestMarshal markDangerousResultUnmarshal
    host moduleName "markDangerous"
    (newRootList, pathList) printMarkDangerousResult

(****************************************************************************)
(*                    FUNCTION GIVE HOST LIST                               *)
(****************************************************************************)
(* Be carefull if you change this function :
   - For efficiency, we want each host appears only once.
   - The order is important : local always first *) 
let giveHosts rootList =
  let bool = ref false in
  let hostSet = List.fold_left 
                  (fun hostSet root ->
                     match root with 
                       Local _ -> bool:= true;hostSet
                  | Remote (host, _) -> StringSet.add host hostSet)
                  StringSet.empty rootList in
  let result = StringSet.elements hostSet in
  if !bool then (""::result) else result

(*****************************************************************************)
(*                           FIND UPDATES FUNCTIONS                          *)
(*****************************************************************************)

let findUpdatesRequestMarshal,
findUpdatesRequestUnmarshal =
  (Remote.giveMarshalFunctions "findUpdatesRequest"
   : (root list * Danger.PathSet.t * path list)
     Remote.marshalingFunctions)

let findUpdatesResultMarshal,
findUpdatesResultUnmarshal =
  (Remote.giveMarshalFunctions "findUpdatesResult"
   : (bool * updateItem list list) Remote.marshalingFunctions)

let findProxy host rootList dangerSet pathList =
  let newRootList = rootsSeenBy host rootList in
  Remote.print printUpdateRequest
    (host, newRootList,
     (Danger.PathSet.elements dangerSet), pathList);
  Remote.invoke
    findUpdatesRequestMarshal findUpdatesResultUnmarshal host
    moduleName "findUpdates" (newRootList, dangerSet, pathList)
    printUpdateItemList

(* SafeFind computes the updateItems of the (root, path) of synchronization.
   It uses suspensions to emulate paralellisations. First we ask for the 
   dangerous paths on each side and then we give it to the update functions,
   so that they return the updateItems. *)
let safeFind () =
  Trace.status "Checking for updates";
  let rootList = !Globals.replicaRoots in
  let pathList = !Globals.synchroPaths in
  let hostList = giveHosts rootList in
  let t = Trace.startTimer " Collecting dangerous paths" in
  let freezes =
    List.map (function
                "" -> (fun () -> askDangerousLocal rootList)
              | host -> askDangerousProxy host rootList)
      hostList in 
  let dangerousPathsList =
    List.map (fun freeze -> freeze ()) freezes in
  let dangerousPaths =
    List.fold_left Danger.PathSet.union
      Danger.PathSet.empty dangerousPathsList in
      Trace.showTimer t;
      let t = Trace.startTimer " Collecting updates" in
      let freezes2 =
        List.map
          (function
             "" -> (fun () -> findLocal rootList dangerousPaths pathList)
           | host ->
               findProxy host rootList dangerousPaths pathList)
          hostList in
      let bools, updateItemLists =
        List.split
          (List.map (fun freeze ->
		       Trace.statusDetail
                        "Waiting for updates from remote host";
		       freeze())
             freezes2) in
        Trace.showTimer t;
        let result = splitN (List.flatten updateItemLists) 
         (List.length pathList) in
        Trace.status "";
        (List.for_all (fun x -> x) bools, result)

(*****************************************************************************)
(*                           MARK UPDATED FUNCTIONS                          *)
(*****************************************************************************)

let markUpdatedRequestMarshal,
markUpdatedRequestUnmarshal =
  (Remote.giveMarshalFunctions "markUpdatedRequest"
   : (root list * (path * bool) list) Remote.marshalingFunctions)

let markUpdatedResultMarshal,
markUpdatedResultUnmarshal =
  (Remote.giveMarshalFunctions "markUpdatedResult"
   : (confirmation list) Remote.marshalingFunctions)

let markUpdatedProxy host rootList pathBoolList =
  let newRootList = rootsSeenBy host rootList in
  Remote.print printMarkUpdatedRequest
    (host, rootList, pathBoolList);
  Remote.invoke
    markUpdatedRequestMarshal markUpdatedResultUnmarshal
    host moduleName "markUpdated" (newRootList, pathBoolList)
    printMarkUpdatedResult

(* We proceed in two steps : first we mark dangerous all the paths we want to 
   mark updated on each host(and not each replica). Then we try to 
   mark updated and delete dangerous paths in those hosts *)
let markUpdated pathBoolList =
  let rootList = !Globals.replicaRoots in
  let hostList = giveHosts rootList in
  let pathList = fst (List.split pathBoolList) in
  let freezes = List.map 
                  (function 
                     "" -> (fun () -> markDangerousLocal rootList pathList)
                   | host -> markDangerousProxy host rootList pathList) 
                  hostList in
  let result = List.for_all (fun freeze -> freeze ()) freezes in
  if result then 
    begin
      let freezes2 =
        List.map (function 
                    "" -> (fun () -> markUpdatedLocal rootList pathBoolList)
                  | host -> markUpdatedProxy host rootList pathBoolList) 
          hostList in
      let confListList = 
        splitN (List.map (fun freeze -> freeze ()) freezes2) 
          (List.length pathList) in
          List.map 
            (List.fold_left 
               (fun prevConf newConf -> match prevConf with 
                 Failed _ -> prevConf
               | _ -> newConf)
               Succeeded) 
            confListList
    end
  else List.map 
    (fun path -> Failed ("Internet failure on path" ^(path2string path))) 
    pathList

(*****************************************************************************)
(*                              SERVER FUNCTION                              *)
(*****************************************************************************)

let server command arguments inFd =
  match command with
    "askDangerous" ->
      let rootList =
        askDangerousRequestUnmarshal arguments inFd in
      let answer = askDangerousLocal rootList in
      askDangerousResultMarshal answer
  | "markDangerous" ->
      let (rootList, pathList) =
        markDangerousRequestUnmarshal arguments inFd in
      let answer = markDangerousLocal rootList pathList in
      markDangerousResultMarshal answer
  | "findUpdates" ->
      let findUpdatesRequest =
        findUpdatesRequestUnmarshal arguments inFd in
      let (rootList, dangerSet, pathList) =
        findUpdatesRequest in
      let answer =
        findLocal rootList dangerSet pathList in
        findUpdatesResultMarshal answer
  | "markUpdated" ->
      let (rootList, pathBoolList) =
        markUpdatedRequestUnmarshal arguments inFd in
      let answer =
        markUpdatedLocal rootList pathBoolList in
        markUpdatedResultMarshal answer
  | _ -> raise (Can'tHappen ("Update", "server"))

