(* Unison file synchronizer: src/common.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

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

type prevState251 =
    Previous of Fileinfo.typ * Props.t251 * Os.fullfingerprint * Osx.ressStamp
  | New

type contentschange251 =
    ContentsSame
  | ContentsUpdated of Os.fullfingerprint * Fileinfo.stamp251 * Osx.ressStamp
type permchange = PropsSame | PropsUpdated

(* Variable name prefix: "ui" *)
type updateItem251 =
    NoUpdates                         (* Path not changed *)
  | Updates                           (* Path changed in this replica *)
      of updateContent251             (*   - new state *)
       * prevState251                 (*   - summary of old state *)
  | Error                             (* Error while detecting updates *)
      of string                       (*   - description of error *)

(* Variable name prefix: "uc" *)
and updateContent251 =
    Absent                            (* Path refers to nothing *)
  | File                              (* Path refers to an ordinary file *)
      of Props.t251                   (*   - summary of current state *)
       * contentschange251            (*   - hint to transport agent *)
  | Dir                               (* Path refers to a directory *)
      of Props.t251                   (*   - summary of current state *)
       * (Name.t * updateItem251) list(*   - children
                                             MUST KEEP SORTED for recon *)
       * permchange                   (*   - did permissions change? *)
       * bool                         (*   - is the directory now empty? *)
  | Symlink                           (* Path refers to a symbolic link *)
      of string                       (*   - link text *)

type prevState =
  | PrevDir of Props.t
  | PrevFile of Props.t * Os.fullfingerprint * Fileinfo.stamp * Osx.ressStamp
  | PrevSymlink
  | New

type contentschange =
    ContentsSame
  | ContentsUpdated of Os.fullfingerprint * Fileinfo.stamp * Osx.ressStamp

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

val mupdateItem : updateItem Umarshal.t
val mupdateContent : updateContent Umarshal.t

val ui_to_compat251 : updateItem -> updateItem251
val ui_of_compat251 : updateItem251 -> updateItem
val uc_to_compat251 : updateContent -> updateContent251
val uc_of_compat251 : updateContent251 -> updateContent

(*****************************************************************************)
(*            COMMON TYPES SHARED BY RECONCILER AND TRANSPORT AGENT          *)
(*****************************************************************************)

(* `MovedOut is set as the status on the old path. In this case, the new path
   will not have a separate difference record at all (the corresponding
   replicaContent records for both replicas are embedded in the `MovedOut
   status). Status of the new path is guaranteed to be not conflicting.
   `MovedOut is a combination of `Deleted on the old path and `Created on the
   new path. The virtual status equivalent for both paths combined is
   `Unchanged or `PropsChanged, meaning that except for the path change (and
   potentially the props), the file/dir contents have not changed.

       (in the illustrations below, the boxes with double lines represent
          the two replicaContents of the one difference record that is
                  visible to the user and will be propagated)

                         REPLICA A                          REPLICA B

                                                on path n'
                              /   +--------------+      +-----------------+
         on path n           |    | p = `Created |      | q = `Unchanged  |
  +======================+   |    +--------------+      +-----------------+
  |                      |   /
  | `MovedOut (n', p, q) |  <                   on path n
  |                      |   \    +--------------+      +=================+
  +======================+   |    |   `Deleted   |      | anything except |
                             |    +--------------+      |    `Deleted     |
                              \                         +=================+


   If `MovedOut can not be set (for example, there is a conflict on the new
   path) then `MovedIn may be set instead. `MovedOut and `MovedIn are never
   used together on a pair of paths.
   `MovedIn is set as the status of the new path. In this case, the old path
   will not have a separate difference record at all (the corresponding
   replicaContent records for both replicas are embedded in the `MovedIn
   status). Status of the old path is guaranteed to be not conflicting.
   `MovedIn is a combination of `Created on the new path and `Deleted on the
   old path. The virtual status equivalent for both paths combined is
   `Unchanged or `PropsChanged, meaning tat except for the path change (and
   potentially the props), the file/dir contents have not changed.

                         REPLICA A                          REPLICA B

                                                on path n
                              /   +--------------+      +================+
         on path n           |    |   `Created   |      |    anything    |
  +======================+   |    +--------------+      +================+
  |                      |   /
  | `MovedIn  (n', p, q) |  <
  |                      |   \                  on path n'
  +======================+   |    +--------------+      +----------------+
                             |    | p = `Deleted |      | q = `Unchanged |
                              \   +--------------+      +----------------+

  (Usually the status of replica b on path n will not be `Unchanged, because
   then `MovedOut will be used instead. It can be `Unchanged if typ is not
   `ABSENT. In other words, `Create in replica b is overwriting something.)

   Note that even though path for only one replica is recorded in `MovedOut/
   `MovedIn, it will not cause trouble when in case insensitive mode on a
   case sensitive filesystem (commit 005a53075b998dba27eeff74a1fc8f9d73558fb8
   for details). Correct path translation is done by [Update.translatePath]. *)
type status =
  [ `Deleted
  | `Modified
  | `PropsChanged
  | `Created
  | `MovedOut of Path.t          (* new path *)
              * replicaContent   (* rc of new path, this replica *)
              * replicaContent   (* rc of new path, other replica *)
  | `MovedIn of Path.t           (* old path *)
              * replicaContent   (* rc of old path, this replica *)
              * replicaContent   (* rc of old path, other replica *)
  | `Unchanged ]

(* Variable name prefix: "rc" *)
and replicaContent =
  { typ : Fileinfo.typ;
    status : status;
    desc : Props.t;                (* Properties (for the UI) *)
    ui : updateItem;
    size : int * Uutil.Filesize.t; (* Number of items and size *)
    props : Props.t list }         (* Parent properties *)

type direction =
    Conflict of string (* The string is the reason of the conflict *)
  | Merge
  | Replica1ToReplica2
  | Replica2ToReplica1

val direction2string : direction -> string

val isConflict : direction -> bool

type difference =
  { rc1 : replicaContent;           (* - content of first replica *)
    rc2 : replicaContent;           (* - content of second replica *)
    errors1 : string list;          (* - deep errors in first replica *)
    errors2 : string list;          (* - deep errors in second replica *)
    mutable direction : direction;  (* - action to take (it's mutable so that
                                         the user interface can change it) *)
    default_direction : direction } (* - default action to take *)

(* Variable name prefix: "rplc" *)
type replicas =
    Problem of string       (* There was a problem during update detection *)
  | Different of difference (* Replicas differ *)

(* Variable name prefix: "ri" *)
type reconItem = {path1 : Path.t; path2 : Path.t; replicas : replicas}

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
(* True if the ri is problematic or if it has some deep errors in a
   directory *)
val partiallyProblematic : reconItem -> bool
val isDeletion  : reconItem -> bool
