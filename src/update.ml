(* Unison file synchronizer: src/update.ml *)
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
let (>>=)  = Lwt.(>>=)

let debug = Trace.debug "update"
let debugverbose = Trace.debug "update+"
let debugalias = Trace.debug "rootalias"
let debugignore = Trace.debug "ignore"

(*****************************************************************************)
(*                             ARCHIVE DATATYPE                              *)
(*****************************************************************************)

(* Remember to increment archiveFormat each time the representation of the
   archive changes: old archives will then automatically be discarded.  (We
   do not use the unison version number for this because usually the archive
   representation does not change between unison versions.) *)
(*FIX: Use similar_correct in props.ml next time the
  format is modified (see file props.ml for the new function) *)
(*FIX: use Case.normalize next time the format is modified *)
(*FIX: also change Fileinfo.stamp to drop the info.ctime component, next time the
  format is modified *)
(*FIX: also make Jerome's suggested change about file times (see his mesg in
       unison-pending email folder). *)
let archiveFormat = 22

module NameMap = MyMap.Make (Name)

type archive =
    ArchiveDir of Props.t * archive NameMap.t
  | ArchiveFile of Props.t * Os.fullfingerprint * Fileinfo.stamp * Osx.ressStamp
  | ArchiveSymlink of string
  | NoArchive

(* For directories, only the permissions part of the file description (desc)
   is used for synchronization at the moment. *)

let archive2string = function
    ArchiveDir(_) -> "ArchiveDir"
  | ArchiveFile(_) -> "ArchiveFile"
  | ArchiveSymlink(_) -> "ArchiveSymlink"
  | NoArchive -> "NoArchive"

(*****************************************************************************)
(*                             ARCHIVE NAMING                                *)
(*****************************************************************************)

(* DETERMINING THE ARCHIVE NAME                                              *)

(* The canonical name of a root consists of its canonical host name and
   canonical fspath.

   The canonical name of a set of roots consists of the canonical names of
   the roots in sorted order.

   There is one archive for each root to be synchronized.  The canonical
   name of the archive is the canonical name of the root plus the canonical
   name of the set of all roots to be synchronized.  Because this is a long
   string we store the archive in a file whose name is the hash of the
   canonical archive name.

   For example, suppose we are synchronizing roots A and B, with canonical
   names A' and B', where A' < B'.  Then the canonical archive name for root
   A is A' + A' + B', and the canonical archive name for root B is B' + A' +
   B'.

   Currently, we determine A' + B' during startup and store this in the
   ref cell rootsName, below.  This rootsName is passed as an argument to
   functions that need to determine a canonical archive name.  Note, since
   we have a client/server architecture, there are TWO rootsName ref cells
   (one in the client's address space, one in the server's).  It is vital
   therefore that the rootsName be determined on the client and passed to
   the server.  This is not good and we should get rid of the ref cell in
   the future; we have implemented it this way at first for historical
   reasons. *)

let rootsName : string Prefs.t =
  Prefs.createString "rootsName" "" "*Canonical root names" ""

let getRootsName () = Prefs.read rootsName

let foundArchives = ref true

(*****************************************************************************)
(*                           COMMON DEFINITIONS                              *)
(*****************************************************************************)

let rootAliases : string list Prefs.t =
  Prefs.createStringList "rootalias"
   "!register alias for canonical root names"
   ("When calculating the name of the archive files for a given pair of roots,"
   ^ " Unison replaces any roots matching the left-hand side of any rootalias"
   ^ " rule by the corresponding right-hand side.")

(* [root2stringOrAlias root] returns the string form of [root], taking into
   account the preference [rootAliases], whose items are of the form `<a> ->
   <b>' *)
let root2stringOrAlias (root: Common.root): string =
  let r = Common.root2string root in
  let aliases : (string * string) list =
    Safelist.map
      (fun s -> match Util.splitIntoWordsByString s " -> " with
        [n;n'] -> (Util.trimWhitespace n, Util.trimWhitespace n')
      | _ -> raise (Util.Fatal (Printf.sprintf
                                  "rootalias %s should be two strings separated by ' -> '" s)))
      (Prefs.read rootAliases) in
  let r' = try Safelist.assoc r aliases with Not_found -> r in
  if r<>r' then debugalias (fun()->
    Util.msg "Canonical root name %s is aliased to %s\n" r r');
  r'

(* (Called from the UI startup sequence...) `normalize' root names,
   sort them, get their string form, and put into the preference [rootsname]
   as a comma-separated string *)
let storeRootsName () =
  let n =
    String.concat ", "
      (Safelist.sort compare
         (Safelist.map root2stringOrAlias
            (Safelist.map
               (function
                   (Common.Local,f) ->
                     (Common.Remote Os.myCanonicalHostName,f)
                | r ->
                   r)
               (Globals.rootsInCanonicalOrder())))) in
  Prefs.set rootsName n

(* How many characters of the filename should be used for the unique id of
   the archive?  On Unix systems, we use the full fingerprint (32 bytes).
   On windows systems, filenames longer than 8 bytes can cause problems, so
   we chop off all but the first 6 from the fingerprint. *)
let significantDigits =
  match Util.osType with
    `Win32 -> 6
  | `Unix -> 32

let thisRootsGlobalName (fspath: Fspath.t): string =
  root2stringOrAlias (Common.Remote Os.myCanonicalHostName, fspath)

(* ----- *)

(* The status of an archive *)
type archiveVersion = MainArch | NewArch | ScratchArch | Lock

let showArchiveName =
  Prefs.createBool "showarchive" false
    "!show 'true names' (for rootalias) of roots and archive"
    ("When this preference is set, Unison will print out the 'true names'"
     ^ "of the roots, in the same form as is expected by the {\\tt rootalias}"
     ^ "preference.")

let _ = Prefs.alias showArchiveName "showArchiveName"

let archiveHash fspath =
  (* Conjoin the canonical name of the current host and the canonical
     presentation of the current fspath with the list of names/fspaths of
     all the roots and the current archive format *)
  let thisRoot = thisRootsGlobalName fspath in
  let r = Prefs.read rootsName in
  let n = Printf.sprintf "%s;%s;%d" thisRoot r archiveFormat in
  let d = Fingerprint.toString (Fingerprint.string n) in
  debugverbose (fun()-> Util.msg "Archive name is %s; hashcode is %s\n" n d);
  if Prefs.read showArchiveName then
    Util.msg "Archive name is %s; hashcode is %s\n" n d;
  (String.sub d 0 significantDigits)

(* We include the hash part of the archive name in the names of temp files
   created by this run of Unison.  The reason for this is that, during
   update detection, we are going to silently delete any old temp files that
   we find along the way, and we want to prevent ourselves from deleting
   temp files belonging to other instances of Unison that may be running
   in parallel, e.g. synchronizing with a different host. *)
let addHashToTempNames fspath = Os.includeInTempNames (archiveHash fspath)

(* [archiveName fspath] returns a pair (arcName, thisRootsGlobalName) *)
let archiveName fspath (v: archiveVersion): string * string =
  let n = archiveHash fspath in
  let temp = match v with
    MainArch -> "ar" | NewArch -> "tm" | ScratchArch -> "sc" | Lock -> "lk"
  in
  (Printf.sprintf "%s%s" temp n,
   thisRootsGlobalName fspath)


(*****************************************************************************)
(*                             SANITY CHECKS                                 *)
(*****************************************************************************)

(* [checkArchive] checks the sanity of an archive, and returns its
   hash-value. 'Sanity' means (1) no repeated name under any path, and (2)
   NoArchive appears only at root-level (indicated by [top]).  Property: Two
   archives of the same labeled-tree structure have the same hash-value.
   NB: [h] is the hash accumulator *)
let rec checkArchive (top: bool) (path: Path.t) (arch: archive) (h: int): int =
  match arch with
    ArchiveDir (desc, children) ->
      begin match NameMap.validate children with
        `Ok ->
          ()
      | `Duplicate nm ->
          raise
            (Util.Fatal (Printf.sprintf
                           "Corrupted archive: \
                            the file %s occurs twice in path %s"
                           (Name.toString nm) (Path.toString path)));
      | `Invalid ->
          raise
            (Util.Fatal (Printf.sprintf
                           "Corrupted archive: the files are not \
                            correctely ordered in directory %s"
                           (Path.toString path)));
      end;
      NameMap.fold
        (fun n a h ->
           Uutil.hash2 (Name.hash n)
                       (checkArchive false (Path.child path n) a h))
        children (Props.hash desc h)
  | ArchiveFile (desc, dig, _, ress) ->
      Uutil.hash2 (Hashtbl.hash dig) (Props.hash desc h)
  | ArchiveSymlink content ->
      Uutil.hash2 (Hashtbl.hash content) h
  | NoArchive ->
      135

(* [archivesIdentical l] returns true if all elements in [l] are the
   same and distinct from None *)
let archivesIdentical l =
  match l with
    h::r -> h <> None && Safelist.for_all (fun h' -> h = h') r
  | _    -> true

(*****************************************************************************)
(*                      LOADING AND SAVING ARCHIVES                          *)
(*****************************************************************************)

(* [formatString] and [verboseArchiveName thisRoot] are the verbose forms of
   archiveFormat and root names.  They appear in the header of the archive
   files *)
let formatString = Printf.sprintf "Unison archive format %d" archiveFormat

let verboseArchiveName thisRoot =
  Printf.sprintf "Archive for root %s synchronizing roots %s"
    thisRoot (Prefs.read rootsName)

(* Load in the archive in [fspath]; check that archiveFormat (first line)
   and roots (second line) match skip the third line (time stamp), and read
   in the archive *)
let loadArchiveLocal (fspath: Fspath.t) (thisRoot: string) :
    (archive * int * string) option =
  let f = Fspath.toString fspath in
  debug (fun() -> Util.msg "Loading archive from %s\n" f);
  Util.convertUnixErrorsToFatal "loading archive" (fun () ->
    if Sys.file_exists f then
      let c = open_in_bin f in
      let header = input_line c in
      (* Sanity check on archive format *)
      if header<>formatString then begin
        Util.warn
          (Printf.sprintf
             "Archive format mismatch: found\n '%s'\n\
              but expected\n '%s'.\n\
              I will delete the old archive and start from scratch.\n"
             header formatString);
        None
      end else
      let roots = input_line c in
      (* Sanity check on roots. *)
      if roots <> verboseArchiveName thisRoot then begin
        Util.warn
          (Printf.sprintf
             "Archive mismatch: found\n '%s'\n\
              but expected\n '%s'.\n\
              I will delete the old archive and start from scratch.\n"
             roots (verboseArchiveName thisRoot));
        None
      end else
        (* Throw away the timestamp line *)
        let _ = input_line c in
        (* Load the datastructure *)
        try
          let ((archive, hash, magic) : archive * int * string) =
            Marshal.from_channel c in
          close_in c;
          Some (archive, hash, magic)
        with Failure s -> raise (Util.Fatal (Printf.sprintf
           "Archive file seems damaged (%s): \
            throw away archives on both machines and try again" s))
    else
      (debug (fun() -> Util.msg "Archive %s not found\n" f);
      None))

(* Inverse to loadArchiveLocal *)
let storeArchiveLocal fspath thisRoot archive hash magic =
 let f = Fspath.toString fspath in
 debug (fun() -> Util.msg "Saving archive in %s\n" f);
 Util.convertUnixErrorsToFatal "saving archive" (fun () ->
   let c =
     open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o600 f
   in
   output_string c formatString;
   output_string c "\n";
   output_string c (verboseArchiveName thisRoot);
   output_string c "\n";
   output_string c (Printf.sprintf "Written at %s\n"
                      (Util.time2string (Util.time())));
   Marshal.to_channel c (archive, hash, magic) [Marshal.No_sharing];
   close_out c)

(* Remove the archieve under the root path [fspath] with archiveVersion [v] *)
let removeArchiveLocal ((fspath: Fspath.t), (v: archiveVersion)): unit Lwt.t =
  Lwt.return
    (let (name,_) = archiveName fspath v in
     let f = Fspath.toString (Os.fileInUnisonDir name) in
     debug (fun() -> Util.msg "Removing archive %s\n" f);
     Util.convertUnixErrorsToFatal "removing archive" (fun () ->
       if Sys.file_exists f then Sys.remove f))

(* [removeArchiveOnRoot root v] invokes [removeArchive fspath v] on the
   server, where [fspath] is the path to root on the server *)
let removeArchiveOnRoot: Common.root -> archiveVersion -> unit Lwt.t =
  Remote.registerRootCmd "removeArchive" removeArchiveLocal

(* [commitArchive (fspath, ())] commits the archive for [fspath] by changing
   the filenames from ScratchArch-ones to a NewArch-ones *)
let commitArchiveLocal ((fspath: Fspath.t), ())
    : unit Lwt.t =
  Lwt.return
    (let (fromname,_) = archiveName fspath ScratchArch in
     let (toname,_) = archiveName fspath NewArch in
     let ffrom = Fspath.toString (Os.fileInUnisonDir fromname) in
     let fto = Fspath.toString (Os.fileInUnisonDir toname) in
     Util.convertUnixErrorsToFatal
       "committing"
         (fun () -> Unix.rename ffrom fto))

(* [commitArchiveOnRoot root v] invokes [commitArchive fspath v] on the
   server, where [fspath] is the path to root on the server *)
let commitArchiveOnRoot: Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd "commitArchive" commitArchiveLocal

let archiveInfoCache = Hashtbl.create 7
(* [postCommitArchive (fspath, v)] finishes the committing protocol by
   copying files from NewArch-files to MainArch-files *)
let postCommitArchiveLocal (fspath,())
    : unit Lwt.t =
  Lwt.return
    (let (fromname,_) = archiveName fspath NewArch in
     let (toname, thisRoot) = archiveName fspath MainArch in
     let ffrom = Fspath.toString (Os.fileInUnisonDir fromname) in
     let fto = Fspath.toString (Os.fileInUnisonDir toname) in
     debug (fun() -> Util.msg "Copying archive %s to %s\n" ffrom fto);
     Util.convertUnixErrorsToFatal "copying archive" (fun () ->
       let outFd =
         open_out_gen
           [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o600 fto in
       Unix.chmod fto 0o600; (* In case the file already existed *)
       let inFd = open_in_gen [Open_rdonly; Open_binary] 0o444 ffrom in
       Uutil.readWrite inFd outFd (fun _ -> ());
       close_in inFd;
       close_out outFd;
       let arcFspath = Os.fileInUnisonDir toname in
       let info = Fileinfo.get false arcFspath Path.empty in
       Hashtbl.replace archiveInfoCache thisRoot info))

(* [postCommitArchiveOnRoot root v] invokes [postCommitArchive fspath v] on
   the server, where [fspath] is the path to root on the server *)
let postCommitArchiveOnRoot: Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd "postCommitArchive" postCommitArchiveLocal


(*************************************************************************)
(*                           Archive cache                               *)
(*************************************************************************)

(* archiveCache: map(rootGlobalName, archive) *)
let archiveCache = Hashtbl.create 7

(*  commitAction: map(rootGlobalName * transactionId, action: unit -> unit) *)
let commitActions = Hashtbl.create 7

(* Retrieve an archive from the cache *)
let getArchive (thisRoot: string): archive =
  Hashtbl.find archiveCache thisRoot

(* Update the cache. *)
let setArchiveLocal (thisRoot: string) (archive: archive) =
  (* Also this: *)
  debug (fun () -> Printf.eprintf "Setting archive for %s\n" thisRoot);
  Hashtbl.replace archiveCache thisRoot archive

let fileUnchanged oldInfo newInfo =
  oldInfo.Fileinfo.typ = `FILE && newInfo.Fileinfo.typ = `FILE
    &&
  Props.same_time oldInfo.Fileinfo.desc newInfo.Fileinfo.desc
    &&
  match Fileinfo.stamp oldInfo, Fileinfo.stamp newInfo with
    Fileinfo.InodeStamp in1, Fileinfo.InodeStamp in2 -> in1 = in2
  | Fileinfo.CtimeStamp t1,  Fileinfo.CtimeStamp t2  -> t1  = t2
  | _                                                -> false

let archiveUnchanged fspath newInfo =
  let (arcName, thisRoot) = archiveName fspath MainArch in
  try
    fileUnchanged (Hashtbl.find archiveInfoCache thisRoot) newInfo
  with Not_found ->
    false

(*************************************************************************
                           DUMPING ARCHIVES
 *************************************************************************)

let rec showArchive = function
    ArchiveDir (props, children) ->
      Format.printf "Directory, %s@\n @[" (Props.syncedPartsToString props);
      NameMap.iter (fun n c ->
        Format.printf "%s -> @\n " (Name.toString n);
        showArchive c)
        children;
      Format.printf "@]"
  | ArchiveFile (props, fingerprint, _, _) ->
      Format.printf "File, %s   %s@\n"
        (Props.syncedPartsToString props)
        (Os.fullfingerprint_to_string fingerprint)
  | ArchiveSymlink(s) ->
      Format.printf "Symbolic link: %s@\n" s
  | NoArchive ->
      Format.printf "No archive@\n"

let dumpArchiveLocal (fspath,()) =
  let (name, root) = archiveName fspath MainArch in
  let archive = getArchive root in
  let f = Util.fileInHomeDir "unison.dump" in
  debug (fun () -> Printf.eprintf "Dumping archive into `%s'\n" f);
  let ch = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o600 f in
  let (outfn,flushfn) = Format.get_formatter_output_functions () in
  Format.set_formatter_out_channel ch;
  Format.printf "Contents of archive for %s\n" root;
  Format.printf "Written at %s\n\n" (Util.time2string (Util.time()));
  showArchive archive;
  Format.print_flush();
  Format.set_formatter_output_functions outfn flushfn;
  flush ch;
  close_out ch;
  Lwt.return ()

let dumpArchiveOnRoot : Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd "dumpArchive" dumpArchiveLocal

(*************************************************************************)
(*                         Loading archives                              *)
(*************************************************************************)

(* Load (main) root archive and cache it on the given server *)
let loadArchiveOnRoot: Common.root -> bool -> (int * string) option Lwt.t =
  Remote.registerRootCmd
    "loadArchive"
    (fun (fspath, optimistic) ->
       let (arcName,thisRoot) = archiveName fspath MainArch in
       let arcFspath = Os.fileInUnisonDir arcName in
       if optimistic then begin
         let (newArcName, _) = archiveName fspath NewArch in
         if
           (* If the archive is not in a stable state, we need to
              perform archive recovery.  So, the optimistic loading
              fails. *)
           Sys.file_exists (Fspath.toString (Os.fileInUnisonDir newArcName))
             ||
           let (lockFilename, _) = archiveName fspath Lock in
           let lockFile = Fspath.toString (Os.fileInUnisonDir lockFilename) in
           Lock.is_locked lockFile
         then
           Lwt.return None
         else
           let (arcName,thisRoot) = archiveName fspath MainArch in
           let arcFspath = Os.fileInUnisonDir arcName in
           let info = Fileinfo.get false arcFspath Path.empty in
           if archiveUnchanged fspath info then
             (* The archive is unchanged.  So, we don't need to do
                anything. *)
             Lwt.return (Some (0, ""))
           else begin
             match loadArchiveLocal arcFspath thisRoot with
               Some (arch, hash, magic) ->
                 let info' = Fileinfo.get false arcFspath Path.empty in
                 if fileUnchanged info info' then begin
                   setArchiveLocal thisRoot arch;
                   Hashtbl.replace archiveInfoCache thisRoot info;
                   Lwt.return (Some (hash, magic))
                 end else
                   (* The archive was modified during loading.  We fail. *)
                   Lwt.return None
             | None ->
                   (* No archive found *)
                   Lwt.return None
           end
       end else begin
         match loadArchiveLocal arcFspath thisRoot with
           Some (arch, hash, magic) ->
             setArchiveLocal thisRoot arch;
             let info = Fileinfo.get false arcFspath Path.empty in
             Hashtbl.replace archiveInfoCache thisRoot info;
             Lwt.return (Some (hash, magic))
         | None ->
             (* No archive found *)
             setArchiveLocal thisRoot NoArchive;
             Hashtbl.remove archiveInfoCache thisRoot;
             Lwt.return (Some (0, ""))
       end)

let dumpArchives =
  Prefs.createBool "dumparchives" false
    "*dump contents of archives just after loading"
    ("When this preference is set, Unison will create a file unison.dump "
     ^ "on each host, containing a text summary of the archive, immediately "
     ^ "after loading it.")

(* For all roots (local or remote), load the archive and cache *)
let loadArchives (optimistic: bool) : bool Lwt.t =
  Globals.allRootsMap (fun r -> loadArchiveOnRoot r optimistic)
     >>= (fun checksums ->
  let identicals = archivesIdentical checksums in
  if not (optimistic || identicals) then
    raise (Util.Fatal(
	"Internal error: On-disk archives are not identical.\n"
      ^ "\n"
      ^ "This can happen when both machines have the same hostname.\n"
      ^ "\n"
      ^ "If this is not the case and you get this message repeatedly, please:\n"
      ^ "  a) Send a bug report to unison-users@yahoogroups.com (you may need"
      ^ "     to join the group before you will be allowed to post).\n"
      ^ "  b) Move the archive files on each machine to some other directory\n"
      ^ "     (in case they may be useful for debugging).\n"
      ^ "     The archive files on this machine are in the directory\n"
      ^ (Printf.sprintf "       %s\n" (Fspath.toString Os.unisonDir))
      ^ "     and have names of the form\n"
      ^ "       arXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
      ^ "     where the X's are a hexidecimal number .\n"
      ^ "  c) Run unison again to synchronize from scratch.\n"));
  if Prefs.read dumpArchives then 
    Globals.allRootsMap (fun r -> dumpArchiveOnRoot r ())
     >>= (fun _ -> Lwt.return identicals)
  else Lwt.return identicals)

(* commitActions(thisRoot, id) <- action *)
let setCommitAction (thisRoot: string) (id: int) (action: unit -> unit): unit =
  let key = (thisRoot, id) in
  Hashtbl.add commitActions key action

(* perform and remove the action associated with (thisRoot, id) *)
let softCommitLocal (thisRoot: string) (id: int) =
  debug (fun () ->
    Util.msg "Committing %d\n" id);
  let key = (thisRoot, id) in
  Hashtbl.find commitActions key ();
  Hashtbl.remove commitActions key

(* invoke softCommitLocal on a given root (which is possibly remote) *)
let softCommitOnRoot: Common.root -> int -> unit Lwt.t =
  Remote.registerRootCmd
    "softCommit"
    (fun (fspath, id) ->
       Lwt.return (softCommitLocal (thisRootsGlobalName fspath) id))

(* Commit the archive on all roots. The archive must have been updated on
   all roots before that.  I.e., carry out the action corresponding to [id]
   on all the roots *)
let softCommit (id: int): unit Lwt.t =
  Util.convertUnixErrorsToFatal "softCommit" (*XXX*)
    (fun () ->
       Globals.allRootsIter
         (fun r -> softCommitOnRoot r id))

(* [rollBackLocal thisRoot id] removes the action associated with (thisRoot,
   id) *)
let rollBackLocal thisRoot id =
  let key = (thisRoot, id) in
  try Hashtbl.remove commitActions key with Not_found -> ()

let rollBackOnRoot: Common.root -> int -> unit Lwt.t =
  Remote.registerRootCmd
    "rollBack"
    (fun (fspath, id) ->
       Lwt.return (rollBackLocal (thisRootsGlobalName fspath) id))

(* Rollback the archive on all roots. *)
(* I.e., remove the action associated with [id] on all roots *)
let rollBack id =
  Util.convertUnixErrorsToFatal "rollBack" (*XXX*)
    (fun () ->
       Globals.allRootsIter
         (fun r -> rollBackOnRoot r id))

let ids = ref 0
let new_id () = incr ids; !ids

type transaction = int

(* [transaction f]: transactional execution
 * [f] should take in a unique id, which it can use to `setCommitAction',
 * and returns a thread.
 * When the thread finishes execution, the committing action associated with
 * [id] is invoked.
 *)
let transaction (f: int -> unit Lwt.t): unit Lwt.t =
  let id = new_id () in
  Lwt.catch
    (fun () ->
       f id >>= (fun () ->
       softCommit id))
    (fun exn ->
       match exn with
         Util.Transient _ ->
           rollBack id >>= (fun () ->
           Lwt.fail exn)
       | _ ->
           Lwt.fail exn)

(*****************************************************************************)
(*                               Archive locking                             *)
(*****************************************************************************)

let lockArchiveLocal fspath =
  let (lockFilename, _) = archiveName fspath Lock in
  let lockFile = Fspath.toString (Os.fileInUnisonDir lockFilename) in
  if Lock.acquire lockFile then
    None
  else
    Some (Printf.sprintf "The file %s on host %s should be deleted"
            lockFile Os.myCanonicalHostName)

let lockArchiveOnRoot: Common.root -> unit -> string option Lwt.t =
  Remote.registerRootCmd
    "lockArchive" (fun (fspath, ()) -> Lwt.return (lockArchiveLocal fspath))

let unlockArchiveLocal fspath =
  Lock.release
    (Fspath.toString (Os.fileInUnisonDir (fst (archiveName fspath Lock))))

let unlockArchiveOnRoot: Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "unlockArchive"
    (fun (fspath, ()) -> Lwt.return (unlockArchiveLocal fspath))

let ignorelocks =
  Prefs.createBool "ignorelocks" false
    "!ignore locks left over from previous run (dangerous!)"
    ("When this preference is set, Unison will ignore any lock files "
     ^ "that may have been left over from a previous run of Unison that "
     ^ "was interrupted while reading or writing archive files; by default, "
     ^ "when Unison sees these lock files it will stop and request manual "
     ^ "intervention.  This "
     ^ "option should be set only if you are {\\em positive} that no other "
     ^ "instance of Unison might be concurrently accessing the same archive "
     ^ "files (e.g., because there was only one instance of unison running "
     ^ "and it has just crashed or you have just killed it).  It is probably "
     ^ "not a good idea to set this option in a profile: it is intended for "
     ^ "command-line use.")

let locked = ref false

let lockArchives () =
  assert (!locked = false);
  Globals.allRootsMap
    (fun r -> lockArchiveOnRoot r ()) >>= (fun result ->
  if Safelist.exists (fun x -> x <> None) result
  && not (Prefs.read ignorelocks) then begin
    Globals.allRootsIter2
      (fun r st ->
         match st with
           None   -> unlockArchiveOnRoot r ()
         | Some _ -> Lwt.return ())
      result >>= (fun () ->
    let whatToDo = Safelist.filterMap (fun st -> st) result in
    raise
      (Util.Fatal
         (String.concat "\n"
            (["Warning: the archives are locked.  ";
              "If no other instance of " ^ Uutil.myName ^ " is running, \
               the locks should be removed."]
             @ whatToDo @
              ["Please delete lock files as appropriate and try again."]))))
    end else begin
      locked := true;
      Lwt.return ()
    end)

let unlockArchives () =
  if !locked then begin
    Globals.allRootsIter (fun r -> unlockArchiveOnRoot r ()) >>= (fun () ->
    locked := false;
    Lwt.return ())
  end else
    Lwt.return ()

(*************************************************************************)
(*                          CRASH RECOVERY                               *)
(*************************************************************************)

(* We avoid getting into an unsafe situation if the synchronizer is
   interrupted during the writing of the archive files by adopting a
   simple joint commit protocol.

   The invariant that we maintain at all times is:
      if all hosts have a temp archive,
        then these temp archives contain coherent information
      if NOT all hosts have a temp archive,
        then the regular archives contain coherent information

   When we WRITE archives (markUpdated), we maintain this invariant
   as follows:
     - first, write all archives to a temporary filename
     - then copy all the temp files to the corresponding regular archive
       files
     - finally, delete all the temp files

   Before we LOAD archives (findUpdates), we perform a crash recovery
   procedure, in case there was a crash during any of the above operations.
     - if all hosts have a temporary archive, we copy these to the
       regular archive names
     - otherwise, if some hosts have temporary archives, we delete them
*)

let archivesExistOnRoot: Common.root -> unit -> (bool * bool) Lwt.t =
  Remote.registerRootCmd
    "archivesExist"
    (fun (fspath,rootsName) ->
       let (oldname,_) = archiveName fspath MainArch in
       let oldexists =
         Sys.file_exists (Fspath.toString (Os.fileInUnisonDir oldname)) in
       let (newname,_) = archiveName fspath NewArch in
       let newexists =
         Sys.file_exists (Fspath.toString (Os.fileInUnisonDir newname)) in
       Lwt.return (oldexists, newexists))

let (archiveNameOnRoot
       : Common.root ->  archiveVersion -> (string * string * bool) Lwt.t)
    =
  Remote.registerRootCmd
    "archiveName"
      (fun (fspath, v) ->
       let (name,_) = archiveName fspath v in
       Lwt.return
         (name,
          Os.myCanonicalHostName,
          Sys.file_exists (Fspath.toString (Os.fileInUnisonDir name))))

let forall = Safelist.for_all (fun x -> x)
let exists = Safelist.exists (fun x -> x)

let doArchiveCrashRecovery () =
  (* Check which hosts have copies of the old/new archive *)
  Globals.allRootsMap (fun r -> archivesExistOnRoot r ()) >>= (fun exl ->
  let oldnamesExist,newnamesExist =
    Safelist.split exl
  in

  (* Do something with the new archives, if there are any *)
  begin if forall newnamesExist then begin
    (* All new versions were written: use them *)
    Util.warn
      (Printf.sprintf
         "Warning: %s may have terminated abnormally last time.\n\
          A new archive exists on all hosts: I'll use them.\n"
         Uutil.myName);
    Globals.allRootsIter (fun r -> postCommitArchiveOnRoot r ()) >>= (fun () ->
    Globals.allRootsIter (fun r -> removeArchiveOnRoot r NewArch))
  end else if exists newnamesExist then begin
    Util.warn
      (Printf.sprintf
         "Warning: %s may have terminated abnormally last time.\n\
          A new archive exists on some hosts only; it will be ignored.\n"
         Uutil.myName);
    Globals.allRootsIter (fun r -> removeArchiveOnRoot r NewArch)
  end else
    Lwt.return ()
  end >>= (fun () ->

  (* Now verify that there are old archives on all hosts *)
  if forall oldnamesExist then begin
    (* We're happy *)
    foundArchives := true;
    Lwt.return ()
  end else if exists oldnamesExist then
    Globals.allRootsMap
      (fun r -> archiveNameOnRoot r MainArch) >>= (fun names ->
    let whatToDo =
      Safelist.map
        (fun (name,host,exists) ->
          Printf.sprintf "  Archive %s on host %s %s"
            name
            host
            (if exists then "should be DELETED" else "is MISSING"))
        names in
    raise
      (Util.Fatal
         (String.concat "\n"
            (["Warning: inconsistent state.  ";
              "The archive file is missing on some hosts.";
              "For safety, the remaining copies should be deleted."]
             @ whatToDo @
             ["Please delete archive files as appropriate and try again."]))))
  else begin
    foundArchives := false;
    let expectedRoots =
      String.concat "\n\t" (Safelist.map root2string (Globals.rootsList ())) in
     Util.warn
     ("No archive files were found for these roots, whose canonical names are:\n\t"
     ^ expectedRoots ^ "\nThis can happen either\n"
     ^ "because this is the first time you have synchronized these roots, \n"
     ^ "or because you have upgraded Unison to a new version with a different\n"
     ^ "archive format.  \n\n"
     ^ "Update detection may take a while on this run if the replicas are \n"
     ^ "large.\n\n"
     ^ "Unison will assume that the 'last synchronized state' of both replicas\n"
     ^ "was completely empty.  This means that any files that are different\n"
     ^ "will be reported as conflicts, and any files that exist only on one\n"
     ^ "replica will be judged as new and propagated to the other replica.\n"
     ^ "If the two replicas are identical, then no changes will be reported.\n\n"
     ^ "If you see this message repeatedly, it may be because one of your machines\n"
     ^ "is getting its address from DHCP, which is causing its host name to change\n"
     ^ "between synchronizations.  See the documentation for the UNISONLOCALHOSTNAME\n"
     ^ "environment variable for advice on how to correct this.\n"
     ^ "\n"
     ^ "Donations to the Unison project are gratefully accepted: \n"
     ^ "http://www.cis.upenn.edu/~bcpierce/unison\n"
     ^ "\n"
     (* ^ "\nThe expected archive names were:\n" ^ expectedNames *) );
    Lwt.return ()
  end))

(*************************************************************************
                       Update a part of an archive
 *************************************************************************)

(* perform [action] on the relative path [rest] in the archive.  If it
   returns [(ar, result)], then update archive with [ar] at [rest] and
   return [result]. *)
let rec updatePathInArchive archive fspath
    (here: Path.local) (rest: Path.t)
    (action: archive -> Fspath.t -> Path.local -> archive * 'c):
    archive * 'c
    =
  debugverbose
    (fun() ->
      Printf.eprintf "updatePathInArchive %s %s [%s] [%s]\n"
        (archive2string archive) (Fspath.toString fspath)
        (Path.toString here) (Path.toString rest));
  match Path.deconstruct rest with
    None ->
      action archive fspath here
  | Some(name, rest') ->
      let (desc, name', child, otherChildren) =
        match archive with
          ArchiveDir (desc, children) ->
            begin try
              let (name', child) = NameMap.findi name children in
              (desc, name', child, NameMap.remove name children)
            with Not_found ->
              (desc, name, NoArchive, children)
            end
        | _ ->
            (Props.dummy, name, NoArchive, NameMap.empty) in
      match
        updatePathInArchive child fspath (Path.child here name') rest' action
      with
        NoArchive, res ->
          if otherChildren = NameMap.empty && desc == Props.dummy then
            NoArchive, res
          else
            ArchiveDir (desc, otherChildren), res
      | child, res ->
          ArchiveDir (desc, NameMap.add name' child otherChildren), res

(*************************************************************************)
(*                  Extract of a part of a archive                       *)
(*************************************************************************)

(* Get the archive found at [rest] of [archive] *)
let rec getPathInArchive archive here rest =
  match Path.deconstruct rest with
    None ->
      (here, archive)
  | Some (name, rest') ->
      let (name', child) =
        match archive with
          ArchiveDir (desc, children) ->
            begin try
              NameMap.findi name children
            with Not_found ->
              (name, NoArchive)
            end
        | _ ->
            (name, NoArchive)
      in
      getPathInArchive child (Path.child here name') rest'

let translatePathLocal fspath path =
  let root = thisRootsGlobalName fspath in
  let (localPath, _) = getPathInArchive (getArchive root) Path.empty path in
  localPath

let translatePath =
  Remote.registerRootCmd "translatePath"
    (fun (fspath, path) -> Lwt.return (translatePathLocal fspath path))

let isDir fspath path =
  let fullFspath = Fspath.concat fspath path in
  try
    (Fspath.stat fullFspath).Unix.LargeFile.st_kind = Unix.S_DIR
  with Unix.Unix_error _ -> false

(***********************************************************************
                             MOUNT POINTS
************************************************************************)

let mountpoints = 
  Prefs.createStringList "mountpoint"
    "!abort if this path does not exist"
    ("Including the preference \\texttt{-mountpoint PATH} causes Unison to "
     ^ "double-check, at the end of update detection, that \\texttt{PATH} exists "
     ^ "and abort if it does not.  This is useful when Unison is used to synchronize "
     ^ "removable media.  This preference can be given more than once.  "
     ^ "See \\sectionref{mountpoints}{Mount Points}.")

let abortIfAnyMountpointsAreMissing fspath =
  Safelist.iter
    (fun s ->
       let path = Path.fromString s in
       if not (Os.exists fspath path) then
         raise (Util.Fatal
           (Printf.sprintf "Path %s / %s is designated as a mountpoint, but points to nothing on host %s\n"
             (Fspath.toString fspath) (Path.toString path) Os.myCanonicalHostName)))
    (Prefs.read mountpoints)


(***********************************************************************
                           UPDATE DETECTION
************************************************************************)

(* Generate a tree of changes. Also, update the archive in case some
   timestamps have been changed without the files being actually updated. *)

let fastcheck =
  Prefs.createString "fastcheck" "default"
    "!do fast update detection (true/false/default)"
    ( "When this preference is set to \\verb|true|, \
       Unison will use the modification time and length of a file as a
       `pseudo inode number' \
       when scanning replicas for updates, \
       instead of reading the full contents of every file.  Under \
       Windows, this may cause Unison to miss propagating an update \
       if the modification time and length of the \
       file are both unchanged by the update.  However, Unison will never \
       {\\em overwrite} such an update with a change from the other \
       replica, since it always does a safe check for updates just \
       before propagating a change.  Thus, it is reasonable to use \
       this switch under Windows most of the time and occasionally \
       run Unison once with {\\tt fastcheck} set to \
       \\verb|false|, if you are \
       worried that Unison may have overlooked an update.  The default \
       value of the preference is \\verb|auto|, which causes Unison to \
       use fast checking on Unix replicas (where it is safe) and slow \
       checking on  Windows replicas.  For backward compatibility, \
       \\verb|yes|, \\verb|no|, and \\verb|default| can be used in place \
       of \\verb|true|, \\verb|false|, and \\verb|auto|.  See \
       \\sectionref{fastcheck}{Fast Checking} for more information.")

let useFastChecking () =
      (Prefs.read fastcheck = "yes")
   || (Prefs.read fastcheck = "true")
   || (Prefs.read fastcheck = "default" && Util.osType = `Unix)
   || (Prefs.read fastcheck = "auto" && Util.osType = `Unix)

let immutable = Pred.create "immutable" ~advanced:true
   ("This preference specifies paths for directories whose \
     immediate children are all immutable files --- i.e., once a file has been \
     created, its contents never changes.  When scanning for updates, \
     Unison does not check whether these files have been modified; \
     this can speed update detection significantly (in particular, for mail \
     directories).")

let immutablenot = Pred.create "immutablenot" ~advanced:true
   ("This preference overrides {\\tt immutable}.")

(** Status display **)

(* BCP (3/09) We used to try to be smart about showing status messages
   at regular intervals, but people seem to find this confusing.
   Let's replace all this with something simpler -- just show directories as
   they are scanned...  (but I'll leave the code in for now, in case we find
   we want to restore the old behavior). *)
(*
  let bigFileLength = 10 * 1024
  let bigFileLengthFS = Uutil.Filesize.ofInt bigFileLength
  let smallFileLength = 1024
  let fileLength = ref 0
  let t0 = ref 0.

  (* Note that we do *not* want to do any status displays from the server
     side, since this will cause the server to block until the client has
     finished its own update detection and can receive and acknowledge
     the status display message -- thus effectively serializing the client 
     and server! *)
  let showStatusAddLength info =
    if not !Trace.runningasserver then begin
      let len1 = Props.length info.Fileinfo.desc in
      let len2 = Osx.ressLength info.Fileinfo.osX.Osx.ressInfo in
      if len1 >= bigFileLengthFS || len2 >= bigFileLengthFS then
        fileLength := bigFileLength
      else
        fileLength :=
          min bigFileLength
           (!fileLength + Uutil.Filesize.toInt len1 + Uutil.Filesize.toInt len2)
    end

  let showStatus path =
    if not !Trace.runningasserver then begin
      fileLength := !fileLength + smallFileLength;
      if !fileLength >= bigFileLength then begin
        fileLength := 0;
        let t = Unix.gettimeofday () in
        if t -. !t0 > 0.05 then begin
          Trace.statusDetail ("scanning... got to " ^ Path.toString path);
          t0 := t
        end
      end
    end
*)

let showStatus path = ()
let showStatusAddLength info = ()

let showStatusDir path =
  if not !Trace.runningasserver then begin
        Trace.statusDetail ("scanning... " ^ Path.toString path);
  end

(* ------- *)

let symlinkInfo =
  Common.Previous (`SYMLINK, Props.dummy, Os.fullfingerprint_dummy, Osx.ressDummy)

let absentInfo = Common.New

let oldInfoOf archive =
  match archive with
    ArchiveDir  (oldDesc, _) ->
      Common.Previous (`DIRECTORY, oldDesc, Os.fullfingerprint_dummy, Osx.ressDummy)
  | ArchiveFile (oldDesc, dig, _, ress) ->
      Common.Previous (`FILE, oldDesc, dig, ress)
  | ArchiveSymlink _ ->
      symlinkInfo
  | NoArchive ->
      absentInfo

(* Check whether a file's permissions have not changed *)
let isPropUnchanged info archiveDesc =
  Props.similar info.Fileinfo.desc archiveDesc

(* Handle file permission change *)
let checkPropChange info archive archDesc =
  if isPropUnchanged info archDesc then begin
    debugverbose (fun() -> Util.msg "  Unchanged file\n");
    NoUpdates
  end else begin
    debug (fun() -> Util.msg "  File permissions updated\n");
    Updates (File (info.Fileinfo.desc, ContentsSame),
             oldInfoOf archive)
  end

(* HACK: we disable fastcheck for Excel (and MPP) files on Windows, as Excel
   sometimes modifies a file without updating the time stamp. *)
let excelFile path =
  let s = Path.toString path in
     Util.endswith s ".xls"
  || Util.endswith s ".mpp"

(* Check whether a file has changed has changed, by comparing its digest and
   properties against [archDesc], [archDig], and [archStamp].
   Returns a pair (optArch, ui) where [optArch] is *not* None when the file remains
   unchanged but time might be changed.  [optArch] is used by [buildUpdate]
   series functions to compute the _old_ archive with updated time stamp
   (thus, there will no false update the next time) *)
let checkContentsChange
      currfspath path info archive archDesc archDig archStamp archRess fastCheck
   : archive option * Common.updateItem
   =
  debug (fun () ->
           Util.msg "checkContentsChange: ";
           begin
             match archStamp with
               Fileinfo.InodeStamp inode ->
                 (Util.msg "archStamp is inode (%d)" inode;
                  Util.msg " / info.inode (%d)" info.Fileinfo.inode)
             | Fileinfo.CtimeStamp stamp ->
                 (Util.msg "archStamp is ctime (%f)" stamp;
                  Util.msg " / info.ctime (%f)" info.Fileinfo.ctime)
           end;
           Util.msg " / times: %f = %f... %b"
             (Props.time archDesc) (Props.time info.Fileinfo.desc)
             (Props.same_time info.Fileinfo.desc archDesc);
           Util.msg " / lengths: %s - %s"
             (Uutil.Filesize.toString (Props.length archDesc))
             (Uutil.Filesize.toString  (Props.length info.Fileinfo.desc));
           Util.msg "\n");
  let dataClearlyUnchanged =
    fastCheck
      &&
    Props.same_time info.Fileinfo.desc archDesc
      &&
    Props.length info.Fileinfo.desc = Props.length archDesc
      &&
    not (excelFile path)
      &&
    match archStamp with
      Fileinfo.InodeStamp inode ->
        info.Fileinfo.inode = inode
    | Fileinfo.CtimeStamp ctime ->
        (* BCP [Apr 07]: This doesn't work -- ctimes are unreliable
                         under windows.  :-(  
           info.Fileinfo.ctime = ctime *)
        true in
  let ressClearlyUnchanged =
    fastCheck
      &&
    Osx.ressUnchanged archRess info.Fileinfo.osX.Osx.ressInfo
      None dataClearlyUnchanged in
  if dataClearlyUnchanged && ressClearlyUnchanged then begin
    Xferhint.insertEntry (currfspath, path) archDig;
    None, checkPropChange info archive archDesc
  end else begin
    debugverbose (fun() -> Util.msg "  Double-check possibly updated file\n");
    showStatusAddLength info;
    let (info, newDigest) =
      Os.safeFingerprint currfspath path info
        (if dataClearlyUnchanged then Some archDig else None) in
    Xferhint.insertEntry (currfspath, path) newDigest;
    debug (fun() -> Util.msg "  archive digest = %s   current digest = %s\n"
             (Os.fullfingerprint_to_string archDig)
             (Os.fullfingerprint_to_string newDigest));
    if archDig = newDigest then begin
      let newprops = Props.setTime archDesc (Props.time info.Fileinfo.desc) in
      let newarch =
        ArchiveFile

          (newprops, archDig, Fileinfo.stamp info, Fileinfo.ressStamp info) in
      debugverbose (fun() ->
        Util.msg "  Contents match: update archive with new time...%f\n" 
                   (Props.time newprops));      
      Some newarch, checkPropChange info archive archDesc
    end else begin
      debug (fun() -> Util.msg "  Updated file\n");
      None,
      Updates (File (info.Fileinfo.desc,
                     ContentsUpdated (newDigest, Fileinfo.stamp info,
                                      Fileinfo.ressStamp info)),
               oldInfoOf archive)
    end
  end


(* getChildren = childrenOf + repetition check

   Find the children of fspath+path, and return them, sorted, and
   partitioned into those with case conflicts, those with illegal
   cross platform filenames, and those without problems.

   Note that case conflicts and illegal filenames can only occur under Unix,
   when syncing with a Windows file system. *)
let badWindowsFilenameRx =
  (* FIX: This should catch all device names (like aux, con, ...).  I don't
     know what all the possible device names are. *)
  Rx.case_insensitive
    (Rx.rx "\\.*|aux|con|lpt1|prn|(.*[\000-\031\\/<>:\"|].*)")

let isBadWindowsFilename s =
  (* FIX: should also check for a max filename length, not sure how much *)
  Rx.match_string badWindowsFilenameRx (Name.toString s)
let badFilename s =
  (* Don't check unless we are syncing with Windows *)
  Prefs.read Globals.someHostIsRunningWindows &&
  isBadWindowsFilename s

let getChildren fspath path =
  let children =
    (* We sort them in reverse order, as findDuplicate will reverse
       the list again *)
    Safelist.sort (fun nm1 nm2 -> - (Name.compare nm1 nm2))
      (Os.childrenOf fspath path) in
  (* If Unison overall is running in case-insensitive mode but the
     local filesystem is case sensitive, then we need to check that
     two local files do not have the same name modulo case... *)
  (* We do it all the time, as this may happen anyway due to race
     conditions... *)
  let childStatus nm count =
    if count > 1 then
      `Dup
    else if badFilename nm then
      `Bad
    else
      `Ok
  in
  let rec findDuplicates' res nm count l =
    match l with
      [] ->
        (nm, childStatus nm count) :: res
    | nm' :: rem ->
        if Name.eq nm nm' then
          findDuplicates' res nm (count + 1) rem
        else
          findDuplicates' ((nm, childStatus nm count) :: res) nm' 1 rem
  and findDuplicates l =
    match l with
      []        -> []
    | nm :: rem -> findDuplicates' [] nm 1 rem
  in
  findDuplicates children

(* from a list of (name, archive) pairs {usually the items in the same
   directory}, build two lists: the first a named list of the _old_
   archives, with their timestamps updated for the files whose contents
   remain unchanged, the second a named list of updates; also returns
   whether the directory is now empty *)
let rec buildUpdateChildren
    fspath path (archChi: archive NameMap.t) fastCheck
    : archive NameMap.t option * (Name.t * Common.updateItem) list * bool
    =
  showStatusDir path;
  let t = Trace.startTimerQuietly
            (Printf.sprintf "checking %s" (Path.toString path)) in
  let skip =
    Pred.test immutable (Path.toString path) &&
    not (Pred.test immutablenot (Path.toString path))
  in
  let curChildren = ref (getChildren fspath path) in
  let emptied = not (NameMap.is_empty archChi) && !curChildren = [] in
  let updates = ref [] in
  let archUpdated = ref false in
  let handleChild nm archive status =
    let path' = Path.child path nm in
    if Globals.shouldIgnore path' then begin
      debugignore (fun()->Util.msg "buildUpdateChildren: ignoring path %s\n"
                            (Path.toString path'));
      archive
    end else begin
      showStatus path';
      match status with
        `Ok | `Abs ->
          if skip && archive <> NoArchive && status <> `Abs then begin
            begin match archive with
              ArchiveFile (archDesc, archDig, archStamp, archRess) ->
                Xferhint.insertEntry (fspath, path') archDig 
            | _ ->
                ()
            end;
            archive
          end else begin
            let (arch,uiChild) =
              buildUpdateRec archive fspath path' fastCheck in
            if uiChild <> NoUpdates then
              updates := (nm, uiChild) :: !updates;
            match arch with
              None      -> archive
            | Some arch -> archUpdated := true; arch
          end
      | `Dup ->
          let uiChild =
            Error
              ("Two or more files on a case-sensitive system have names \
                identical except for case.  They cannot be synchronized to a \
                case-insensitive file system.  (" ^
               Path.toString path' ^ ")")
          in
          updates := (nm, uiChild) :: !updates;
          archive
      | `Bad ->
          let uiChild =
            Error ("The name of this Unix file is not allowed in Windows ("
                   ^ Path.toString path' ^ ")")
          in
          updates := (nm, uiChild) :: !updates;
          archive
    end
  in
  let rec matchChild nm archive =
    match !curChildren with
      [] ->
        (nm, handleChild nm archive `Abs)
    | (nm', st) :: rem ->
        let c = Name.compare nm nm' in
        if c < 0 then
          (nm, handleChild nm archive `Abs)
        else begin
          curChildren := rem;
          if c = 0 then begin
            if nm <> nm' then archUpdated := true;
            (nm', handleChild nm' archive st)
          end else begin
            let arch = handleChild nm' NoArchive st in
            assert (arch = NoArchive);
            matchChild nm archive
          end
        end
  in
  let newChi = NameMap.mapii matchChild archChi in
  Safelist.iter
    (fun (nm, st) ->
       let arch = handleChild nm NoArchive st in
       assert (arch = NoArchive))
    !curChildren;
  Trace.showTimer t;
  (* The Recon module relies on the updates to be sorted *)
  ((if !archUpdated then Some newChi else None),
   Safelist.rev !updates, emptied)

and buildUpdateRec archive currfspath path fastCheck =
  try
    debug (fun() ->
      Util.msg "buildUpdate: %s\n"
        (Fspath.concatToString currfspath path));
    let info = Fileinfo.get true currfspath path in
    match (info.Fileinfo.typ, archive) with
      (`ABSENT, NoArchive) ->
        debug (fun() -> Util.msg "  buildUpdate -> Absent and no archive\n");
        None, NoUpdates
    | (`ABSENT, _) ->
        debug (fun() -> Util.msg "  buildUpdate -> Deleted\n");
        None, Updates (Absent, oldInfoOf archive)
    (* --- *)
    | (`FILE, ArchiveFile (archDesc, archDig, archStamp, archRess)) ->
        checkContentsChange
          currfspath path info archive
          archDesc archDig archStamp archRess fastCheck
    | (`FILE, _) ->
        debug (fun() -> Util.msg "  buildUpdate -> Updated file\n");
        None,
        begin
          showStatusAddLength info;
          let (info, dig) = Os.safeFingerprint currfspath path info None in
          Xferhint.insertEntry (currfspath, path) dig;
          Updates (File (info.Fileinfo.desc,
                         ContentsUpdated (dig, Fileinfo.stamp info,
                                          Fileinfo.ressStamp info)),
                   oldInfoOf archive)
        end
    (* --- *)
    | (`SYMLINK, ArchiveSymlink prevl) ->
        let l = Os.readLink currfspath path in
        debug (fun() ->
          if l = prevl then
            Util.msg "  buildUpdate -> Symlink %s (unchanged)\n" l
          else
            Util.msg "  buildUpdate -> Symlink %s (previously: %s)\n" l prevl);
        (None,
         if l = prevl then NoUpdates else
         Updates (Symlink l, oldInfoOf archive))
    | (`SYMLINK, _) ->
        let l = Os.readLink currfspath path in
        debug (fun() -> Util.msg "  buildUpdate -> New symlink %s\n" l);
        None, Updates (Symlink l, oldInfoOf archive)
    (* --- *)
    | (`DIRECTORY, ArchiveDir (archDesc, prevChildren)) ->
        debugverbose (fun() -> Util.msg "  buildUpdate -> Directory\n");
        let (permchange, desc) =
          if isPropUnchanged info archDesc then
            (PropsSame, archDesc)
          else
            (PropsUpdated, info.Fileinfo.desc) in
        let (newChildren, childUpdates, emptied) =
          buildUpdateChildren currfspath path prevChildren fastCheck in
        (begin match newChildren with
           Some ch -> Some (ArchiveDir (archDesc, ch))
         | None    -> None
         end,
         if childUpdates <> [] || permchange = PropsUpdated then
           Updates (Dir (desc, childUpdates, permchange, emptied),
                    oldInfoOf archive)
         else
           NoUpdates)
    | (`DIRECTORY, _) ->
        debug (fun() -> Util.msg "  buildUpdate -> New directory\n");
        let (newChildren, childUpdates, _) =
          buildUpdateChildren currfspath path NameMap.empty fastCheck in
        (* BCPFIX: This is a bit of a hack and does not really work, since
           it means that we calculate the size of a directory just once and
           then never update our idea of how big it is.  The size should
           really be recalculated when things change. *)
        let newdesc =
           Props.setLength info.Fileinfo.desc
             (Safelist.fold_left
               (fun s (_,ui) -> Uutil.Filesize.add s (uiLength ui))
               Uutil.Filesize.zero childUpdates) in
        (None,
         Updates (Dir (newdesc, childUpdates, PropsUpdated, false),
                  oldInfoOf archive))
  with
    Util.Transient(s) -> None, Error(s)

(* Compute the updates for [path] against archive.  Also returns an
   archive, which is the old archive with time stamps updated
   appropriately (i.e., for those files whose contents remain
   unchanged). *)
let rec buildUpdate archive fspath fullpath here path =
  match Path.deconstruct path with
    None ->
      showStatus path;
      let (arch, ui) =
        buildUpdateRec archive fspath here (useFastChecking()) in
      (begin match arch with
         None      -> archive
       | Some arch -> arch
       end,
       ui)
  | Some(name, path') ->
      if not (isDir fspath here) then
        let error =
          if Path.isEmpty here then
            Printf.sprintf
              "path %s is not valid because the root of one of the replicas \
               is not a directory"
              (Path.toString fullpath)
          else
            Printf.sprintf
              "path %s is not valid because %s is not a directory in one of \
               the replicas"
              (Path.toString fullpath) (Path.toString here)
        in
        (* FIX: We have to fail here (and in other error cases below)
           rather than report an error for this path, which would be
           more user friendly.  Indeed, the archive is otherwise
           modified in inconsistent way when the failure occurs only
           on one replica (see at the end of this function).
           A better solution should be not to put the archives in a
           different state, but this is a lot more work. *)
        raise (Util.Transient error)
(*      (archive, Error error) *)
      else
      let children = getChildren fspath here in
      let (name', status) =
        try
          Safelist.find (fun (name', _) -> Name.eq name name') children
        with Not_found ->
          (name, if badFilename name then `Bad else `Ok)
      in
      match status with
        `Bad ->
          raise (Util.Transient
                   ("The path " ^ Path.toString fullpath ^
                    " is not allowed in Windows"))
      | `Dup ->
          raise (Util.Transient
            ("The path " ^ Path.toString fullpath ^
             " is ambiguous (i.e., the name of this path or one of its "
             ^ "ancestors is the same, modulo capitalization, as another "
             ^ "path in a case-sensitive filesystem, and you are "
             ^ "synchronizing this filesystem with a case-insensitive "
             ^ "filesystem.  ")) 
      | `Ok ->
          let (desc, child, otherChildren) =
            match archive with
              ArchiveDir (desc, children) ->
                begin try
                  let child = NameMap.find name children in
                  (desc, child, NameMap.remove name children)
                with Not_found ->
                  (desc, NoArchive, children)
                end
            | _ ->
                (Props.dummy, NoArchive, NameMap.empty)
          in
          let (arch, updates) =
            buildUpdate child fspath fullpath (Path.child here name') path'
          in
          (* We need to put a directory in the archive here for path
             translation.  This is fine because we check that there
             really is a directory on both replica.
             Note that we may also put NoArchive deep inside an
             archive...
          *)
          (ArchiveDir (desc, NameMap.add name' arch otherChildren),
           updates)

(* for the given path, find the archive and compute the list of update
   items; as a side effect, update the local archive w.r.t. time-stamps for
   unchanged files *)
let findLocal fspath pathList: Common.updateItem list =
  debug (fun() -> Util.msg "findLocal %s\n" (Fspath.toString fspath));
  addHashToTempNames fspath;
  (* Maybe we should remember the device number where the root lives at 
     the beginning of update detection, so that we can check, below, that 
     the device has not changed.  This check allows us to abort in case 
     the root is on a removable device and this device gets removed during
     update detection, causing all the files to appear to have been
     deleted.  --BCP 2006 *)
  let (arcName,thisRoot) = archiveName fspath MainArch in
  let archive = getArchive thisRoot in
  let (archive, updates) =
    Safelist.fold_right
      (fun path (arch, upd) ->
         if Globals.shouldIgnore path then
           (arch, NoUpdates :: upd)
         else
           let (arch', ui) =
             buildUpdate arch fspath path Path.empty path
           in
           arch', ui :: upd)
      pathList (archive, [])
  in
  setArchiveLocal thisRoot archive;
  abortIfAnyMountpointsAreMissing fspath;
  updates

let findOnRoot =
  Remote.registerRootCmd
    "find"
    (fun (fspath, pathList) ->
       Lwt.return (findLocal fspath pathList))

let findUpdatesOnPaths pathList : Common.updateItem list Common.oneperpath =
  Lwt_unix.run
    (loadArchives true >>= (fun ok ->
     begin if ok then Lwt.return () else begin
       lockArchives () >>= (fun () ->
       Remote.Thread.unwindProtect
         (fun () ->
            doArchiveCrashRecovery () >>= (fun () ->
            loadArchives false))
         (fun _ ->
            unlockArchives ()) >>= (fun _ ->
       unlockArchives ()))
     end end >>= (fun () ->
     let t = Trace.startTimer "Collecting changes" in
     Globals.allRootsMapWithWaitingAction (fun r ->
       debug (fun() -> Util.msg "findOnRoot %s\n" (root2string r));
       findOnRoot r pathList)
       (fun (host, _) ->
         begin match host with
           Remote(_) -> Trace.statusDetail "Waiting for changes from server"
         | _ -> ()
         end)
       >>= (fun updates ->
     Trace.showTimer t;
     let result = Safelist.transpose updates in
     Trace.status "";
     Lwt.return (ONEPERPATH(result))))))

let findUpdates () : Common.updateItem list Common.oneperpath =
  (* TODO: We should filter the paths to remove duplicates (including prefixes)
     and ignored paths *)
(* FIX: The following line can be deleted -- it's just for debugging *)
debug (fun() -> Util.msg "Running bogus external program\n");
let _ = External.runExternalProgram "dir" in
debug (fun() -> Util.msg "Finished running bogus external program\n");
  findUpdatesOnPaths (Prefs.read Globals.paths)


(*****************************************************************************)
(*                          Committing updates to disk                       *)
(*****************************************************************************)

(* To prepare for committing, write to Scratch Archive *)
let prepareCommitLocal (fspath, magic) =
  let (newName, root) = archiveName fspath ScratchArch in
  let archive = getArchive root in
  (**
     :ZheDebug:
     Format.set_formatter_out_channel stdout;
     Format.printf "prepareCommitLocal: %s\n" (thisRootsGlobalName fspath);
     showArchive archive;
     Format.print_flush();
   **)
  let archiveHash = checkArchive true Path.empty archive 0 in
  storeArchiveLocal
    (Os.fileInUnisonDir newName) root archive archiveHash magic;
  Lwt.return (Some archiveHash)

let prepareCommitOnRoot
   = Remote.registerRootCmd "prepareCommit" prepareCommitLocal

(* To really commit, first prepare (write to scratch arch.), then make sure
   the checksum on all archives are equal, finally flip scratch to main.  In
   the event of checksum mismatch, dump archives on all roots and fail *)
let commitUpdates () =
  Lwt_unix.run
    (debug (fun() -> Util.msg "Updating archives\n");
     lockArchives () >>= (fun () ->
     Remote.Thread.unwindProtect
       (fun () ->
          let magic =
            Format.sprintf "%.f.%d" (Unix.gettimeofday ()) (Unix.getpid ())
          in
          Globals.allRootsMap (fun r -> prepareCommitOnRoot r magic)
            >>= (fun checksums ->
          if archivesIdentical checksums then begin
            (* Move scratch archives to new *)
            Globals.allRootsIter (fun r -> commitArchiveOnRoot r ())
              >>= (fun () ->
            (* Copy new to main *)
            Globals.allRootsIter (fun r -> postCommitArchiveOnRoot r ())
              >>= (fun () ->
            (* Clean up *)
            Globals.allRootsIter
              (fun r -> removeArchiveOnRoot r NewArch)))
          end else begin
            unlockArchives () >>= (fun () ->
            Util.msg "Dumping archives to ~/unison.dump on both hosts\n";
            Globals.allRootsIter (fun r -> dumpArchiveOnRoot r ())
              >>= (fun () ->
            Util.msg "Finished dumping archives\n";
            raise (Util.Fatal (
                 "Internal error: New archives are not identical.\n"
               ^ "Retaining original archives.  "
               ^    "Please run Unison again to bring them up to date.\n"
               (*
               ^ "If you get this message, please \n "
               ^ "  a) notify unison-help@cis.upenn.edu\n"
               ^ "  b) send us the contents of the file unison.dump \n"
               ^ "     from both hosts (or just do a 'diff'\n"
               ^ "     on these files and tell us what the differences\n"
               ^ "     look like)\n" *)
                   ))))
          end))
       (fun _ -> unlockArchives ()) >>= (fun () ->
     unlockArchives ())))

(*****************************************************************************)
(*                            MARKING UPDATES                                *)
(*****************************************************************************)

(* the result of patching [archive] using [ui] *)
let rec updateArchiveRec ui archive =
  match ui with
    NoUpdates | Error _ ->
      archive
  | Updates (uc, _) ->
      match uc with
        Absent ->
          NoArchive
      | File (desc, ContentsSame) ->
          begin match archive with
            ArchiveFile (_, dig, stamp, ress) ->
              ArchiveFile (desc, dig, stamp, ress)
          | _ ->
              assert false
          end
      | File (desc, ContentsUpdated (dig, stamp, ress)) ->
          ArchiveFile (desc, dig, stamp, ress)
      | Symlink l ->
          ArchiveSymlink l
      | Dir (desc, children, _, _) ->
          begin match archive with
            ArchiveDir (_, arcCh) ->
              let ch =
                Safelist.fold_right
                  (fun (nm, uiChild) ch ->
                    let ch' = NameMap.remove nm ch in
                    let child =
                      try NameMap.find nm ch with Not_found -> NoArchive in
                    match updateArchiveRec uiChild child with
                      NoArchive -> ch'
                    | arch      -> NameMap.add nm arch ch')
                  children arcCh in
              ArchiveDir (desc, ch)
          | _ ->
              ArchiveDir
                (desc,
                 Safelist.fold_right
                   (fun (nm, uiChild) ch ->
                      match updateArchiveRec uiChild NoArchive with
                        NoArchive -> ch
                      | arch      -> NameMap.add nm arch ch)
                   children NameMap.empty)
          end

(* Remove ignored files and properties that are not synchronized *)
let rec stripArchive path arch =
  if Globals.shouldIgnore path then NoArchive else
  match arch with
    ArchiveDir (desc, children) ->
      ArchiveDir
        (Props.strip desc,
         NameMap.fold
           (fun nm ar ch ->
              match stripArchive (Path.child path nm) ar with
                NoArchive -> ch
              | ar'       -> NameMap.add nm ar' ch)
           children NameMap.empty)
  | ArchiveFile (desc, dig, stamp, ress) ->
      ArchiveFile (Props.strip desc, dig, stamp, ress)
  | ArchiveSymlink _ | NoArchive ->
      arch

let updateArchiveLocal fspath path ui id =
  debug (fun() ->
    Util.msg "updateArchiveLocal %s %s\n"
      (Fspath.toString fspath) (Path.toString path));
  let root = thisRootsGlobalName fspath in
  let archive = getArchive root in
  let (localPath, subArch) = getPathInArchive archive Path.empty path in
  let newArch = updateArchiveRec ui (stripArchive path subArch) in
  let commit () =
    let _ = Stasher.stashCurrentVersion fspath localPath None in
    let archive = getArchive root in
    let archive, () =
      updatePathInArchive archive fspath Path.empty path
        (fun _ _ _ -> newArch, ()) in
    setArchiveLocal root archive in
  setCommitAction root id commit;
  debug (fun() ->
    Util.msg "updateArchiveLocal --> %s\n" (Path.toString localPath));
  (localPath, newArch)

let updateArchiveOnRoot =
  Remote.registerRootCmd
    "updateArchive"
    (fun (fspath, (path, ui, id)) ->
       Lwt.return (updateArchiveLocal fspath path ui id))

let updateArchive root path ui id =
  updateArchiveOnRoot root (path, ui, id)

(* This function is called for files changed only in identical ways.
   It only updates the archives and perhaps makes backups. *)
let markEqualLocal fspath paths =
  let root = thisRootsGlobalName fspath in
  let archive = ref (getArchive root) in
  Tree.iteri paths Path.empty Path.child
    (fun path uc ->
       debug (fun() ->
         Util.msg "markEqualLocal %s %s\n"
           (Fspath.toString fspath) (Path.toString path));
       let arch, (subArch, localPath) =
         updatePathInArchive !archive fspath Path.empty path
           (fun archive _ localPath ->
              let arch = updateArchiveRec (Updates (uc, New)) archive in
              arch, (arch, localPath))
       in
       Stasher.stashCurrentVersion fspath localPath None;
       archive := arch);
  setArchiveLocal root !archive

let markEqualOnRoot =
  Remote.registerRootCmd
    "markEqual"
    (fun (fspath, paths) -> markEqualLocal fspath paths; Lwt.return ())

let markEqual equals =
  debug (fun()-> Util.msg "Marking %d paths equal\n" (Tree.size equals));
  if not (Tree.is_empty equals) then begin
    Lwt_unix.run
      (Globals.allRootsIter2
         markEqualOnRoot
         [Tree.map (fun n -> n) (fun (uc1,uc2) -> uc1) equals;
          Tree.map (fun n -> n) (fun (uc1,uc2) -> uc2) equals])
  end

let rec replaceArchiveRec fspath path arch paranoid deleteBadTempFiles =
  match arch with
    ArchiveDir (desc, children) ->
      ArchiveDir (desc,
                  NameMap.mapi
                    (fun nm a ->
                       replaceArchiveRec
                         fspath (Path.child path nm) a paranoid deleteBadTempFiles)
                    children)
  | ArchiveFile (desc, dig, stamp, ress) ->
      if paranoid then begin
        (* Paranoid check: recompute the file's digest to match it with
           the archive's *)
        let info = Fileinfo.get false fspath path in
        let dig' = Os.fingerprint fspath path info in
        let ress' = Osx.stamp info.Fileinfo.osX in
        if dig' <> dig then begin
          let savepath = Path.addSuffixToFinalName path "-bad" in
          (* if deleteBadTempFiles then Os.delete fspath path; *)
          if deleteBadTempFiles then
            Os.rename "save temp" fspath path fspath savepath; 
          raise (Util.Transient (Printf.sprintf
            "The file %s was incorrectly transferred  (fingerprint mismatch in %s)%s"
            (Path.toString path)
            (Os.reasonForFingerprintMismatch dig dig')
            (if deleteBadTempFiles
               then " -- temp file saved as" ^ Path.toString savepath
               else "")));
        end;
        ArchiveFile (Props.override info.Fileinfo.desc desc,
                     dig, Fileinfo.stamp info, ress')
      end else begin
        ArchiveFile (desc, dig, stamp, ress)
      end
  | ArchiveSymlink l ->
      ArchiveSymlink l
  | NoArchive ->
      arch

let replaceArchiveLocal fspath pathTo location arch id paranoid deleteBadTempFiles =
  debug (fun() -> Util.msg
             "replaceArchiveLocal %s %s\n"
             (Fspath.toString fspath)
             (Path.toString pathTo)
        );
  let root = thisRootsGlobalName fspath in
  let localPath = translatePathLocal fspath pathTo in
  let (workingDir, tempPathTo) =
    match location with
      None     -> (fspath, localPath)
    | Some loc -> loc
  in
  let newArch =
    replaceArchiveRec workingDir tempPathTo arch paranoid deleteBadTempFiles in
  let commit () =
    debug (fun() -> Util.msg "replaceArchiveLocal: committing\n");
    let _ = Stasher.stashCurrentVersion fspath localPath (Some tempPathTo) in
    let archive = getArchive root in
    let archive, () =
      updatePathInArchive archive fspath Path.empty pathTo
        (fun _ _ _ -> newArch, ())
    in
    setArchiveLocal root archive
  in
  setCommitAction root id commit;
  localPath

let replaceArchiveOnRoot =
  Remote.registerRootCmd
    "replaceArchive"
    (fun (fspath, (pathTo, location, arch, id, paranoid, deleteBadTempFiles)) ->
       Lwt.return (replaceArchiveLocal fspath pathTo location arch
                                       id paranoid deleteBadTempFiles))

let replaceArchive root pathTo location archive id paranoid deleteBadTempFiles =
  replaceArchiveOnRoot root
    (pathTo, location, archive, id, paranoid, deleteBadTempFiles)

(* Update the archive to reflect
      - the last observed state of the file on disk (ui)
      - the permission bits that have been propagated from the other
        replica, if any (permOpt) *)
let doUpdateProps arch propOpt ui =
  let newArch =
    match ui with
      Updates (File (desc, ContentsSame), _) ->
        begin match arch with
          ArchiveFile (_, dig, stamp, ress) ->
            ArchiveFile (desc, dig, stamp, ress)
        | _ ->
            assert false
        end
    | Updates (File (desc, ContentsUpdated (dig, stamp, ress)), _) ->
        ArchiveFile(desc, dig, stamp, ress)
    | Updates (Dir (desc, _, _, _), _) ->
        begin match arch with
          ArchiveDir (_, children) -> ArchiveDir (desc, children)
        | _                        -> ArchiveDir (desc, NameMap.empty)
        end
    | NoUpdates ->
        arch
    | Updates _ | Error _ ->
        assert false
  in
  match propOpt with
    Some desc' ->
      begin match newArch with
        ArchiveFile (desc, dig, stamp, ress) ->
          ArchiveFile (Props.override desc desc', dig, stamp, ress)
      | ArchiveDir (desc, children) ->
          ArchiveDir (Props.override desc desc', children)
      | _ ->
          assert false
      end
  | None -> newArch

let updatePropsLocal fspath path propOpt ui id =
  debug (fun() ->
    Util.msg "updatePropsLocal %s %s\n"
      (Fspath.toString fspath) (Path.toString path));
  let root = thisRootsGlobalName fspath in
  let commit () =
    let archive = getArchive root in
    let archive, () =
      updatePathInArchive archive fspath Path.empty path
        (fun arch _ _ -> doUpdateProps arch propOpt ui, ()) in
    setArchiveLocal root archive in
  setCommitAction root id commit;
  let localPath = translatePathLocal fspath path in
  localPath

let updatePropsOnRoot =
  Remote.registerRootCmd
   "updateProps"
     (fun (fspath, (path, propOpt, ui, id)) ->
        Lwt.return (updatePropsLocal fspath path propOpt ui id))

let updateProps root path propOpt ui id =
   updatePropsOnRoot root (path, propOpt, ui, id)

(*************************************************************************)
(*                  Make sure no change has happened                     *)
(*************************************************************************)

let checkNoUpdatesLocal fspath pathInArchive ui =
  debug (fun() ->
    Util.msg "checkNoUpdatesLocal %s %s\n"
      (Fspath.toString fspath) (Path.toString pathInArchive));
  let archive = getArchive (thisRootsGlobalName fspath) in
  let (localPath, archive) =
    getPathInArchive archive Path.empty pathInArchive in
  (* Update the original archive to reflect what we believe is the current
     state of the replica... *)
  let archive = updateArchiveRec ui archive in
  (* ...and check that this is a good description of what's out in the world *)
  let (_, uiNew) = buildUpdateRec archive fspath localPath false in
  if uiNew <> NoUpdates then
    raise (Util.Transient (
             "Destination updated during synchronization\n"
           ^ (if useFastChecking() then
                "  (if this happens repeatedly on a file that has not been changed, \n"
              ^ "  try running once with 'fastcheck' set to false)"
              else "")))

let checkNoUpdatesOnRoot =
  Remote.registerRootCmd
    "checkNoUpdates"
    (fun (fspath, (pathInArchive, ui)) ->
       Lwt.return (checkNoUpdatesLocal fspath pathInArchive ui))

let checkNoUpdates root pathInArchive ui =
  checkNoUpdatesOnRoot root (pathInArchive, ui)
