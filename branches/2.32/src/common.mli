(* Unison file synchronizer: src/common.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(***************************************************************************)
(*               COMMON TYPES USED BY ALL MODULES                          *)
(***************************************************************************)

type hostname = string

(* "Canonized" names of hosts *)
type host =
    Local
  | Remote of string

(* Roots for replicas (this is the type that is used by most of the code) *)
type root = host * Fspath.t

val root2string : root -> string

(* Give a printable hostname from a root (local prints as "local") *)
val root2hostname : root -> hostname

val compareRoots : root -> root -> int
val sortRoots : root list -> root list
(* Note, local roots come before remote roots *)

(* There are a number of functions in several modules that accept or return
   lists containing one element for each path-to-be-synchronized specified
   by the user using the -path option.  This type constructor is used
   instead of list, to help document their behavior -- in particular,
   allowing us to write 'blah list list' as 'blah list oneperpath' in a few
   places. *)
type 'a oneperpath = ONEPERPATH of 'a list


(*****************************************************************************)
(*            COMMON TYPES USED BY UPDATE MODULE AND RECONCILER              *)
(*****************************************************************************)

(* An updateItem describes the difference between the current state of the
   filesystem below a given path and the state recorded in the archive below
   that path.  The other types are helpers. *)

type prevState =
    Previous of Fileinfo.typ * Props.t * Os.fullfingerprint * Osx.ressStamp
  | New

type contentschange =
    ContentsSame
  | ContentsUpdated of Os.fullfingerprint * Fileinfo.stamp * Osx.ressStamp
type permchange = PropsSame | PropsUpdated

(* Variable name prefix: "ui" *)
type updateItem =
    NoUpdates                         (* Path not changed *)
  | Updates                           (* Path changed in this replica *)
      of updateContent                (*   - new state *)
       * prevState                    (*   - summary of old state *)
  | Error                             (* Error while detecting updates *)
      of string                       (*   - description of error *)

(* Variable name prefix: "uc" *)
and updateContent =
    Absent                            (* Path refers to nothing *)
  | File                              (* Path refers to an ordinary file *)
      of Props.t                      (*   - summary of current state *)
       * contentschange               (*   - hint to transport agent *)
  | Dir                               (* Path refers to a directory *)
      of Props.t                      (*   - summary of current state *)
       * (Name.t * updateItem) list   (*   - children
                                             MUST KEEP SORTED for recon *)
       * permchange                   (*   - did permissions change? *)
       * bool                         (*   - is the directory now empty? *)
  | Symlink                           (* Path refers to a symbolic link *)
      of string                       (*   - link text *)


(*****************************************************************************)
(*            COMMON TYPES SHARED BY RECONCILER AND TRANSPORT AGENT          *)
(*****************************************************************************)

type status =
  [ `Deleted
  | `Modified
  | `PropsChanged
  | `Created
  | `Unchanged ]

(* Variable name prefix: "rc" *)
type replicaContent = Fileinfo.typ * status * Props.t * updateItem

type direction =
    Conflict
  | Merge
  | Replica1ToReplica2
  | Replica2ToReplica1

val direction2string : direction -> string

(* Variable name prefix: "rplc" *)
type replicas =
    Problem of string    (* There was a problem during update detection *)
  | Different            (* Replicas differ *)
    of replicaContent    (*   - content of first replica *)
     * replicaContent    (*   - content of second replica *)
     * direction ref     (*   - action to take (it's a ref so that the
                                user interface can change it) *)
     * direction         (*   - default action to take *)

(* Variable name prefix: "ri" *)
type reconItem =
    {path : Path.t;
     replicas : replicas}

val ucLength : updateContent -> Uutil.Filesize.t
val uiLength : updateItem -> Uutil.Filesize.t
val riLength : reconItem -> Uutil.Filesize.t
val riFileType : reconItem -> string
val fileInfos :
  updateItem -> updateItem ->
  Props.t * Os.fullfingerprint * Osx.ressStamp *
  Props.t * Os.fullfingerprint * Osx.ressStamp

(* True if the ri's type is Problem or if it is Different and the direction
   is Conflict *)
val problematic : reconItem -> bool
val isDeletion  : reconItem -> bool
