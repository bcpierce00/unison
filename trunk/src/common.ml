(* Unison file synchronizer: src/common.ml *)
(* Copyright 1999-2010, Benjamin C. Pierce 

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


type hostname = string

(* Canonized roots                                                           *)
type host =
    Local
  | Remote of hostname

type root = host * Fspath.t

type 'a oneperpath = ONEPERPATH of 'a list

(* ------------------------------------------------------------------------- *)
(*                       Printing                                            *)
(* ------------------------------------------------------------------------- *)

let root2hostname root =
  match root with
    (Local, _) -> "local"
  | (Remote host, _) -> host

let root2string root =
  match root with
    (Local, fspath) -> Fspath.toPrintString fspath
  | (Remote host, fspath) -> "//"^host^"/"^(Fspath.toPrintString fspath)

(* ------------------------------------------------------------------------- *)
(*                      Root comparison                                      *)
(* ------------------------------------------------------------------------- *)

let compareRoots x y =
  match x,y with
    (Local,fspath1), (Local,fspath2) ->
      (* FIX: This is a path comparison, should it take case
         sensitivity into account ? *)
      Fspath.compare fspath1 fspath2
  | (Local,_), (Remote _,_) -> -1
  | (Remote _,_), (Local,_) -> 1
  | (Remote host1, fspath1), (Remote host2, fspath2) ->
      let result =
        (* FIX: Should this ALWAYS be a case insensitive compare? *)
        compare host1 host2 in
      if result = 0 then
        (* FIX: This is a path comparison, should it take case
           sensitivity into account ? *)
        Fspath.compare fspath1 fspath2
      else
        result

let sortRoots rootList = Safelist.sort compareRoots rootList

(* ---------------------------------------------------------------------- *)

type prevState =
    Previous of Fileinfo.typ * Props.t * Os.fullfingerprint * Osx.ressStamp
  | New

type contentschange =
    ContentsSame
  | ContentsUpdated of Os.fullfingerprint * Fileinfo.stamp * Osx.ressStamp

type permchange     = PropsSame    | PropsUpdated

type updateItem =
    NoUpdates                         (* Path not changed *)
  | Updates                           (* Path changed in this replica *)
      of updateContent                (*   - new state *)
       * prevState                    (*   - summary of old state *)
  | Error                             (* Error while detecting updates *)
      of string                       (*   - description of error *)

and updateContent =
    Absent                            (* Path refers to nothing *)
  | File                              (* Path refers to an ordinary file *)
      of Props.t                      (*   - summary of current state *)
       * contentschange               (*   - hint to transport agent *)
  | Dir                               (* Path refers to a directory *)
      of Props.t                      (*   - summary of current state *)
       * (Name.t * updateItem) list   (*   - children;
                                             MUST KEEP SORTED for recon *)
       * permchange                   (*   - did permissions change? *)
       * bool                         (*   - is the directory now empty? *)
  | Symlink                           (* Path refers to a symbolic link *)
      of string                       (*   - link text *)

(* ------------------------------------------------------------------------- *)

type status =
  [ `Deleted
  | `Modified
  | `PropsChanged
  | `Created
  | `Unchanged ]

type replicaContent =
  { typ : Fileinfo.typ;
    status : status;
    desc : Props.t;                (* Properties (for the UI) *)
    ui : updateItem;
    size : int * Uutil.Filesize.t; (* Number of items and size *)
    props : Props.t list }         (* Parent properties *)

type direction =
    Conflict
  | Merge
  | Replica1ToReplica2
  | Replica2ToReplica1

let direction2string = function
    Conflict -> "conflict"
  | Merge -> "merge"
  | Replica1ToReplica2 -> "replica1 to replica2"
  | Replica2ToReplica1 -> "replica2 to replica1"

type difference =
  { rc1 : replicaContent;
    rc2 : replicaContent;
    errors1 : string list;
    errors2 : string list;
    mutable direction : direction;
    default_direction : direction }

type replicas =
    Problem of string       (* There was a problem during update detection *)
  | Different of difference (* Replicas differ *)

type reconItem = {path1 : Path.t; path2 : Path.t; replicas : replicas}

let ucLength = function
    File(desc,_)    -> Props.length desc
  | Dir(desc,_,_,_) -> Props.length desc
  | _               -> Uutil.Filesize.zero

let uiLength = function
    Updates(uc,_) -> ucLength uc
  | _             -> Uutil.Filesize.zero

let riAction rc rc' =
  match rc.status, rc'.status with
    `Deleted, _ ->
      `Delete
  | (`Unchanged | `PropsChanged), (`Unchanged | `PropsChanged) ->
      `SetProps
  | _ ->
      `Copy

let rcLength rc rc' =
  if riAction rc rc' = `SetProps then
    Uutil.Filesize.zero
  else
    snd rc.size

let riLength ri =
  match ri.replicas with
    Different {rc1 = {status= `Unchanged | `PropsChanged};
               rc2 = {status= `Unchanged | `PropsChanged}} ->
      Uutil.Filesize.zero (* No contents propagated *)
  | Different {rc1 = rc1; rc2 = rc2; direction = dir} ->
      begin match dir with
        Replica1ToReplica2 -> rcLength rc1 rc2
      | Replica2ToReplica1 -> rcLength rc2 rc1
      | Conflict           -> Uutil.Filesize.zero
      | Merge              -> Uutil.Filesize.zero (* underestimate :-*)
      end
  | _ ->
      Uutil.Filesize.zero

let fileInfos ui1 ui2 =
  match ui1, ui2 with
    (Updates (File (desc1, ContentsUpdated (fp1, _, ress1)),
              Previous (`FILE, desc2, fp2, ress2)),
     NoUpdates)
  | (Updates (File (desc1, ContentsUpdated (fp1, _, ress1)),
              Previous (`FILE, desc2, fp2, ress2)),
     Updates (File (_, ContentsSame), _))
  | (NoUpdates,
     Updates (File (desc2, ContentsUpdated (fp2, _, ress2)),
              Previous (`FILE, desc1, fp1, ress1)))
  | (Updates (File (_, ContentsSame), _),
     Updates (File (desc2, ContentsUpdated (fp2, _, ress2)),
              Previous (`FILE, desc1, fp1, ress1)))
  | (Updates (File (desc1, ContentsUpdated (fp1, _, ress1)), _),
     Updates (File (desc2, ContentsUpdated (fp2, _, ress2)), _)) ->
       (desc1, fp1, ress1, desc2, fp2, ress2)
  | _ ->
      raise (Util.Transient "Can't diff")

let problematic ri =
  match ri.replicas with
    Problem _      -> true
  | Different diff -> diff.direction = Conflict

let partiallyProblematic ri =
  match ri.replicas with
    Problem _      ->
      true
  | Different diff ->
      diff.direction = Conflict || diff.errors1 <> [] || diff.errors2 <> []

let isDeletion ri =
  match ri.replicas with
    Different {rc1 = rc1; rc2 = rc2; direction = rDir} ->
      (match rDir, rc1.typ, rc2.typ with
        Replica1ToReplica2, `ABSENT, _ -> true
      | Replica2ToReplica1, _, `ABSENT -> true
      | _ -> false)
  | _ -> false

let rcType rc = Fileinfo.type2string rc.typ

let riFileType ri =
  match ri.replicas with
    Different {rc1 = rc1; rc2 = rc2; default_direction = dir} ->
      begin match dir with
        Replica2ToReplica1 -> rcType rc2
      | _		   -> rcType rc1
      end
  | _ -> "nonexistent"
