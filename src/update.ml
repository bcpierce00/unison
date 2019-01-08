(* Unison file synchronizer: src/update.ml *)
(* Copyright 1999-2018, Benjamin C. Pierce

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

let ignoreArchives =
  Prefs.createBool "ignorearchives" false
    "!ignore existing archive files"
    ("When this preference is set, Unison will ignore any existing "
     ^ "archive files and behave as though it were being run for the first "
     ^ "time on these replicas.  It is "
     ^ "not a good idea to set this option in a profile: it is intended for "
     ^ "command-line use.")

(*****************************************************************************)
(*                             ARCHIVE DATATYPE                              *)
(*****************************************************************************)

(* Remember to increment archiveFormat each time the representation of the
   archive changes: old archives will then automatically be discarded.  (We
   do not use the unison version number for this because usually the archive
   representation does not change between unison versions.) *)
(*FIX: also change Fileinfo.stamp to drop the info.ctime component, next
  time the format is modified *)
(*FIX: also make Jerome's suggested change about file times (see his mesg in
       unison-pending email folder). *)
(*FIX: we could also drop the use of 8.3-style filenames on Windows, next
  time the format is changed *)
(* FIX: use a special stamp rather than the current hack to leave a flag
   in the archive when a file transfer fails so as to turn off fastcheck
   for this file on the next sync. *)
(*FIX: consider changing the way case-sensitivity mode is stored in
  the archive *)
(*FIX: we should use only one Marshal.from_channel *)
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
                     (Common.Remote (Os.myCanonicalHostName ()),f)
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
  root2stringOrAlias (Common.Remote (Os.myCanonicalHostName ()), fspath)

(* ----- *)

(* The status of an archive *)
type archiveVersion = MainArch | NewArch | ScratchArch | Lock | FPCache

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
    MainArch -> "ar" | NewArch -> "tm" | ScratchArch -> "sc"
  | Lock     -> "lk" | FPCache   -> "fp"
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
(* Note that we build the current path as a list of names, as this is
   much cheaper than using values of type [Path.t] *)
let rec checkArchive
      (top: bool) (path: Name.t list) (arch: archive) (h: int): int =
  match arch with
    ArchiveDir (desc, children) ->
      begin match NameMap.validate children with
        `Ok ->
          ()
      | `Duplicate nm ->
          let path =
            List.fold_right (fun n p -> Path.child p n) path Path.empty in
          raise
            (Util.Fatal (Printf.sprintf
                           "Corrupted archive: \
                            the file %s occurs twice in path %s"
                           (Name.toString nm) (Path.toString path)));
      | `Invalid (nm, nm') ->
          let path =
            List.fold_right (fun n p -> Path.child p n) path Path.empty in
          raise
            (Util.Fatal (Printf.sprintf
                           "Corrupted archive: the files %s and %s are not \
                            correctly ordered in directory %s"
                           (Name.toString nm) (Name.toString nm')
                           (Path.toString path)));
      end;
      NameMap.fold
        (fun n a h ->
           Uutil.hash2 (Name.hash n)
                       (checkArchive false (n :: path) a h))
        children (Props.hash desc h)
  | ArchiveFile (desc, dig, _, ress) ->
      Uutil.hash2 (Uutil.hash dig) (Props.hash desc h)
  | ArchiveSymlink content ->
      Uutil.hash2 (Uutil.hash content) h
  | NoArchive ->
      135

(* [archivesIdentical l] returns true if all elements in [l] are the
   same and distinct from None *)
let archivesIdentical l =
  match l with
    h::r -> h <> None && Safelist.for_all (fun h' -> h = h') r
  | _    -> true

let (archiveNameOnRoot
       : Common.root ->  archiveVersion -> (string * string * bool) Lwt.t)
    =
  Remote.registerRootCmd
    "archiveName"
      (fun (fspath, v) ->
       let (name,_) = archiveName fspath v in
       Lwt.return
         (name,
          Os.myCanonicalHostName (),
          System.file_exists (Os.fileInUnisonDir name)))


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
let loadArchiveLocal fspath (thisRoot: string) :
    (archive * int * string * Proplist.t) option =
  debug (fun() ->
    Util.msg "Loading archive from %s\n" (System.fspathToDebugString fspath));
  Util.convertUnixErrorsToFatal "loading archive" (fun () ->
    if System.file_exists fspath then
      let c = System.open_in_bin fspath in
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
          let properties =
            try
              ignore (input_char c); (* Marker *)
              Marshal.from_channel c
            with End_of_file ->
              Proplist.empty
          in
          close_in c;
          Some (archive, hash, magic, properties)
        with Failure s -> raise (Util.Fatal (Printf.sprintf
           "Archive file seems damaged (%s): \
            throw away archives on both machines and try again" s))
    else
      (debug (fun() ->
         Util.msg "Archive %s not found\n"
           (System.fspathToDebugString fspath));
      None))

(* Inverse to loadArchiveLocal *)
let storeArchiveLocal fspath thisRoot archive hash magic properties =
 debug (fun() ->
    Util.msg "Saving archive in %s\n" (System.fspathToDebugString fspath));
 Util.convertUnixErrorsToFatal "saving archive" (fun () ->
   let c =
     System.open_out_gen
       [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o600 fspath
   in
   output_string c formatString;
   output_string c "\n";
   output_string c (verboseArchiveName thisRoot);
   output_string c "\n";
   (* This third line is purely informative *)
   output_string c (Printf.sprintf "Written at %s - %s mode\n"
                      (Util.time2string (Util.time()))
                      ((Case.ops())#modeDesc));
   Marshal.to_channel c (archive, hash, magic) [Marshal.No_sharing];
   output_char c '\000'; (* Marker that indicates that the archive
                            is followed by a property list *)
   Marshal.to_channel c properties [Marshal.No_sharing];
   close_out c)

(* Remove the archieve under the root path [fspath] with archiveVersion [v] *)
let removeArchiveLocal ((fspath: Fspath.t), (v: archiveVersion)): unit Lwt.t =
  Lwt.return
    (let (name,_) = archiveName fspath v in
     let fspath = Os.fileInUnisonDir name in
     debug (fun() ->
       Util.msg "Removing archive %s\n" (System.fspathToDebugString fspath));
     Util.convertUnixErrorsToFatal "removing archive" (fun () ->
       try System.unlink fspath
       with Unix.Unix_error (Unix.ENOENT, _, _) -> ()))

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
     let ffrom = Os.fileInUnisonDir fromname in
     let fto = Os.fileInUnisonDir toname in
     Util.convertUnixErrorsToFatal
       "committing"
         (fun () -> System.rename ffrom fto))

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
     let ffrom = Os.fileInUnisonDir fromname in
     let fto = Os.fileInUnisonDir toname in
     debug (fun() ->
       Util.msg "Copying archive %s to %s\n"
         (System.fspathToDebugString ffrom)
         (System.fspathToDebugString fto));
     Util.convertUnixErrorsToFatal "copying archive" (fun () ->
       begin try
         System.unlink fto
       with Unix.Unix_error (Unix.ENOENT, _, _) -> () end;
       begin try
         System.link ffrom fto
       with Unix.Unix_error _ ->
         let outFd =
           System.open_out_gen
             [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o600 fto in
         System.chmod fto 0o600; (* In case the file already existed *)
         let inFd = System.open_in_bin ffrom in
         Uutil.readWrite inFd outFd (fun _ -> ());
         close_in inFd;
         close_out outFd
       end;
       let arcFspath = Os.fileInUnisonDir toname in
       let info = Fileinfo.get' arcFspath in
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

(* Retrieve an archive from the cache *)
let getArchive (thisRoot: string): archive =
  Hashtbl.find archiveCache thisRoot

(* Update the cache. *)
let setArchiveLocal (thisRoot: string) (archive: archive) =
  (* Also this: *)
  debug (fun () -> Printf.eprintf "Setting archive for %s\n" thisRoot);
  Hashtbl.replace archiveCache thisRoot archive

(* archiveCache: map(rootGlobalName, property list) *)
let archivePropCache = Hashtbl.create 7

(* Retrieve an archive property list from the cache *)
let getArchiveProps (thisRoot: string): Proplist.t =
  Hashtbl.find archivePropCache thisRoot

(* Update the property list cache. *)
let setArchivePropsLocal (thisRoot: string) (props: Proplist.t) =
  Hashtbl.replace archivePropCache thisRoot props

let fileUnchanged oldInfo newInfo =
  oldInfo.Fileinfo.typ = `FILE && newInfo.Fileinfo.typ = `FILE
    &&
  Props.same_time oldInfo.Fileinfo.desc newInfo.Fileinfo.desc
    &&
  Props.length oldInfo.Fileinfo.desc = Props.length newInfo.Fileinfo.desc
    &&
  match Fileinfo.stamp oldInfo, Fileinfo.stamp newInfo with
    Fileinfo.InodeStamp in1, Fileinfo.InodeStamp in2 -> in1 = in2
  | Fileinfo.CtimeStamp _,   Fileinfo.CtimeStamp _   -> true
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
  debug (fun () -> Printf.eprintf "Dumping archive into `%s'\n"
                     (System.fspathToDebugString f));
  let ch = System.open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o600 f in
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

(*****************************************************************************)
(*                          ARCHIVE CASE CONVERSION                          *)
(*****************************************************************************)

(* Stamp for marking unchange directories *)
let dirStampKey : Props.dirChangedStamp Proplist.key =
  Proplist.register "unchanged directory stamp"

(* Property containing a description of the archive case sensitivity mode *)
let caseKey : string Proplist.key = Proplist.register "case mode"

(* Turn a case sensitive archive into a case insensitive archive.
   Directory children are resorted and duplicates are removed.
*)
let rec makeCaseSensitiveRec arch =
  match arch with
    ArchiveDir (desc, children) ->
      let dups = ref [] in
      let children =
        NameMap.fold
          (fun nm ch chs ->
             if Name.badEncoding nm then chs else begin
               if NameMap.mem nm chs then dups := nm :: !dups;
               NameMap.add nm (makeCaseSensitiveRec ch) chs
             end)
          children NameMap.empty
      in
      let children =
        List.fold_left (fun chs nm -> NameMap.remove nm chs) children !dups in
      ArchiveDir (desc, children)
  | ArchiveFile _ | ArchiveSymlink _ | NoArchive ->
      arch

let makeCaseSensitive thisRoot =
  setArchiveLocal thisRoot (makeCaseSensitiveRec (getArchive thisRoot));
  (* We need to recheck all directories, so we mark them possibly changed *)
  setArchivePropsLocal thisRoot
    (Proplist.add dirStampKey (Props.freshDirStamp ())
       (Proplist.add caseKey (Case.ops ())#modeDesc
          (getArchiveProps thisRoot)))

let makeCaseSensitiveOnRoot =
  Remote.registerRootCmd "makeCaseSensitive"
    (fun (fspath, ()) ->
       makeCaseSensitive (thisRootsGlobalName fspath);
       Lwt.return ())

(*FIX: remove when Unison version > 2.40 *)
let canMakeCaseSensitive () =
  Globals.allRootsMap (fun r -> Remote.commandAvailable r "makeCaseSensitive")
    >>= fun l ->
  Lwt.return (List.for_all (fun x -> x) l)

(****)

(* Get the archive case sensitivity mode from the archive magic. *)
let archiveMode magic =
  let currentMode = (Case.ops ())#modeDesc in
  if magic = "" then currentMode (* Newly created archive *) else
  try
    String.sub magic 0 (String.index magic '\000')
  with Not_found ->
    (* Legacy format.  Cannot be Unicode case insensitive. *)
    if (Case.ops ())#mode = Case.UnicodeInsensitive then
      "some non-Unicode"
    else
      currentMode

let checkArchiveCaseSensitivity l =
  let root = thisRootsGlobalName (snd (Globals.localRoot ())) in
  let curMode = (Case.ops ())#modeDesc in
  let archMode = Proplist.find caseKey (getArchiveProps root) in
  if curMode = archMode then
    Lwt.return ()
  else begin
    begin if archMode = Case.caseSensitiveModeDesc then
      canMakeCaseSensitive ()
    else
      Lwt.return false
    end >>= fun convert ->
    if convert then
      Globals.allRootsIter (fun r -> makeCaseSensitiveOnRoot r ())
    else begin
      (* We cannot compute the archive name locally as it
         currently depends on the os type *)
      Globals.allRootsMap
        (fun r -> archiveNameOnRoot r MainArch) >>= fun names ->
      let l =
        List.map
          (fun (name, host, _) ->
             Format.sprintf "    archive %s on host %s" name host)
          names
      in
      Lwt.fail
        (Util.Fatal
           (String.concat "\n"
              ("Warning: incompatible case sensitivity settings." ::
                Format.sprintf "Unison is currently in %s mode," curMode ::
                Format.sprintf
                  "while the archives were created in %s mode." archMode ::
                "You should either change Unison's setup or delete" ::
                "the following archives from the .unison directories:" ::
                l @
                ["(or invoke Unison once with -ignorearchives flag).";
                 "Then, try again."])))
    end
  end

(****)

let rec populateCacheFromArchiveRec path arch =
  match arch with
    ArchiveDir (_, children) ->
      NameMap.iter
        (fun nm ch -> populateCacheFromArchiveRec (Path.child path nm) ch)
        children
  | ArchiveFile (desc, dig, stamp, ress) ->
      Fpcache.save path (desc, dig, stamp, ress)
  | ArchiveSymlink _ | NoArchive ->
      ()

let populateCacheFromArchive fspath arch =
  let (cacheFilename, _) = archiveName fspath FPCache in
  let cacheFile = Os.fileInUnisonDir cacheFilename in
  Fpcache.init true (Prefs.read ignoreArchives) cacheFile;
  populateCacheFromArchiveRec Path.empty arch;
  Fpcache.finish ()

(*************************************************************************)
(*                         Loading archives                              *)
(*************************************************************************)

let setArchiveData thisRoot fspath (arch, hash, magic, properties) info =
  let archMode = archiveMode magic in
  let curMode = (Case.ops ())#modeDesc in
  let properties = Proplist.add caseKey archMode properties in
  setArchiveLocal thisRoot arch;
  setArchivePropsLocal thisRoot properties;
  Hashtbl.replace archiveInfoCache thisRoot info;
  if archMode <> curMode then populateCacheFromArchive fspath arch;
  Lwt.return (Some (hash, magic))

let clearArchiveData thisRoot =
  setArchiveLocal thisRoot NoArchive;
  setArchivePropsLocal thisRoot
    (Proplist.add caseKey (Case.ops ())#modeDesc Proplist.empty);
  Hashtbl.remove archiveInfoCache thisRoot;
  Lwt.return (Some (0, ""))

(* Load (main) root archive and cache it on the given server *)
let loadArchiveOnRoot: Common.root -> bool -> (int * string) option Lwt.t =
  Remote.registerRootCmd
    "loadArchive"
    (fun (fspath, optimistic) ->
       let (arcName,thisRoot) = archiveName fspath MainArch in
       let arcFspath = Os.fileInUnisonDir arcName in

       if Prefs.read ignoreArchives then begin
         foundArchives := false;
         clearArchiveData thisRoot
       end else if optimistic then begin
         let (newArcName, _) = archiveName fspath NewArch in
         if
           (* If the archive is not in a stable state, we need to
              perform archive recovery.  So, the optimistic loading
              fails. *)
           System.file_exists (Os.fileInUnisonDir newArcName)
             ||
           let (lockFilename, _) = archiveName fspath Lock in
           let lockFile = Os.fileInUnisonDir lockFilename in
           Lock.is_locked lockFile
         then
           Lwt.return None
         else
           let (arcName,thisRoot) = archiveName fspath MainArch in
           let arcFspath = Os.fileInUnisonDir arcName in
           let info = Fileinfo.get' arcFspath in
           if archiveUnchanged fspath info then
             (* The archive is unchanged.  So, we don't need to do
                anything. *)
             Lwt.return (Some (0, ""))
           else begin
             match loadArchiveLocal arcFspath thisRoot with
               Some archData ->
                 let info' = Fileinfo.get' arcFspath in
                 if fileUnchanged info info' then
                   setArchiveData thisRoot fspath archData info
                 else
                   (* The archive was modified during loading.  We fail. *)
                   Lwt.return None
             | None ->
                   (* No archive found *)
                   Lwt.return None
           end
       end else begin
         match loadArchiveLocal arcFspath thisRoot with
           Some archData ->
             setArchiveData thisRoot fspath archData (Fileinfo.get' arcFspath)
         | None ->
             (* No archive found *)
             clearArchiveData thisRoot
       end)

let dumpArchives =
  Prefs.createBool "dumparchives" false
    "*dump contents of archives just after loading"
    ("When this preference is set, Unison will create a file unison.dump "
     ^ "on each host, containing a text summary of the archive, immediately "
     ^ "after loading it.")

(* For all roots (local or remote), load the archive and cache *)
let loadArchives (optimistic: bool) =
  Globals.allRootsMap (fun r -> loadArchiveOnRoot r optimistic)
     >>= (fun checksums ->
  let identicals = archivesIdentical checksums in
  if not (optimistic || identicals) then
    raise (Util.Fatal(
        "Internal error: On-disk archives are not identical.\n"
      ^ "\n"
      ^ "This can happen when both machines have the same hostname.\n"
      ^ "It can also happen when one copy of Unison has been compiled with\n"
      ^ "OCaml version 3 and one with OCaml version 4.\n"
      ^ "\n"
      ^ "If this is not the case and you get this message repeatedly, please:\n"
      ^ "  a) Send a bug report to unison-users@yahoogroups.com (you may need\n"
      ^ "     to join the group before you will be allowed to post).\n"
      ^ "  b) Move the archive files on each machine to some other directory\n"
      ^ "     (in case they may be useful for debugging).\n"
      ^ "     The archive files on this machine are in the directory\n"
      ^ (Printf.sprintf "       %s\n"
           (System.fspathToPrintString Os.unisonDir))
      ^ "     and have names of the form\n"
      ^ "       arXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
      ^ "     where the X's are a hexidecimal number .\n"
      ^ "  c) Run unison again to synchronize from scratch.\n"));
  Lwt.return (identicals, checksums))


(*****************************************************************************)
(*                               Archive locking                             *)
(*****************************************************************************)

let lockArchiveLocal fspath =
  let (lockFilename, _) = archiveName fspath Lock in
  let lockFile = Os.fileInUnisonDir lockFilename in
  if Lock.acquire lockFile then
    None
  else
    Some (Printf.sprintf "The file %s on host %s should be deleted"
            (System.fspathToPrintString lockFile) (Os.myCanonicalHostName ()))

let lockArchiveOnRoot: Common.root -> unit -> string option Lwt.t =
  Remote.registerRootCmd
    "lockArchive" (fun (fspath, ()) -> Lwt.return (lockArchiveLocal fspath))

let unlockArchiveLocal fspath =
  Lock.release
    (Os.fileInUnisonDir (fst (archiveName fspath Lock)))

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
         System.file_exists (Os.fileInUnisonDir oldname) in
       let (newname,_) = archiveName fspath NewArch in
       let newexists =
         System.file_exists (Os.fileInUnisonDir newname) in
       Lwt.return (oldexists, newexists))

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
             ["Please delete archive files as appropriate and try again";
             "or invoke Unison with -ignorearchives flag."]))))
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
    (here: Path.local) (rest: 'a Path.path)
    (action: archive -> Path.local -> archive):
    archive
    =
  debugverbose
    (fun() ->
      Printf.eprintf "updatePathInArchive %s %s [%s] [%s]\n"
        (archive2string archive) (Fspath.toDebugString fspath)
        (Path.toString here) (Path.toString rest));
  match Path.deconstruct rest with
    None ->
      action archive here
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
        NoArchive ->
          if NameMap.is_empty otherChildren && desc == Props.dummy then
            NoArchive
          else
            ArchiveDir (desc, otherChildren)
      | child ->
          ArchiveDir (desc, NameMap.add name' child otherChildren)

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
     ^ "See \\sectionref{mountpoints}{Mount Points and Removable Media}.")

let abortIfAnyMountpointsAreMissing fspath =
  Safelist.iter
    (fun s ->
       let path = Path.fromString s in
       if not (Os.exists fspath path) then
         raise (Util.Fatal
           (Printf.sprintf "Path %s / %s is designated as a mountpoint, but points to nothing on host %s\n"
             (Fspath.toPrintString fspath) (Path.toString path)
             (Os.myCanonicalHostName ()))))
    (Prefs.read mountpoints)

(***********************************************************************
                           Set of paths
************************************************************************)

type pathTree = PathTreeLeaf
              | PathTreeNode of pathTree NameMap.t

let rec addPathToTree path tree =
  match Path.deconstruct path, tree with
    None, _ | _, Some PathTreeLeaf ->
      PathTreeLeaf
  | Some (nm, p), None ->
      PathTreeNode (NameMap.add nm (addPathToTree p None) NameMap.empty)
  | Some (nm, p), Some (PathTreeNode children) ->
      let t = try Some (NameMap.find nm children) with Not_found -> None in
      PathTreeNode (NameMap.add nm (addPathToTree p t) children)

let rec removePathFromTree path tree =
  match Path.deconstruct path, tree with
    None, _ ->
      None
  | Some (nm, p), PathTreeLeaf ->
      Some tree
  | Some (nm, p), PathTreeNode children ->
      try
        let t = NameMap.find nm children in
        match removePathFromTree p t with
          None ->
            let newChildren = NameMap.remove nm children in
            if NameMap.is_empty children then None else
            Some (PathTreeNode newChildren)
        | Some t ->
            Some (PathTreeNode (NameMap.add nm t children))
      with Not_found ->
        Some tree

let pathTreeOfList l =
  Safelist.fold_left (fun t p -> Some (addPathToTree p t)) None l

let removePathsFromTree l treeOpt =
  Safelist.fold_left
    (fun t p ->
       match t with
         None   -> None
       | Some t -> removePathFromTree p t)
    treeOpt l

let rec getSubTree path tree =
  match Path.deconstruct path, tree with
    None, _ ->
      Some tree
  | Some (nm, p), PathTreeLeaf ->
      Some PathTreeLeaf
  | Some (nm, p), PathTreeNode children ->
      try
        let t = NameMap.find nm children in
        getSubTree p t
      with Not_found ->
        None

(***********************************************************************
                           UPDATE DETECTION
************************************************************************)

(* Generate a tree of changes. Also, update the archive in case some
   timestamps have been changed without the files being actually updated. *)

let fastcheck =
  Prefs.createBoolWithDefault "fastcheck"
    "!do fast update detection (true/false/default)"
    ( "When this preference is set to \\verb|true|, \
       Unison will use the modification time and length of a file as a
       `pseudo inode number' \
       when scanning replicas for updates, \
       instead of reading the full contents of every file.  (This does not \
       apply to the very first run, when Unison will always scan \
       all files regarless of this switch).  Under \
       Windows, this may cause Unison to miss propagating an update \
       if the modification time and length of the \
       file are both unchanged by the update.  However, Unison will never \
       {\\em overwrite} such an update with a change from the other \
       replica, since it always does a safe check for updates just \
       before propagating a change.  Thus, it is reasonable to use \
       this switch under Windows most of the time and occasionally \
       run Unison once with {\\tt fastcheck} set to \
       \\verb|false|, if you are \
       worried that Unison may have overlooked an update. \
       For backward compatibility, \
       \\verb|yes|, \\verb|no|, and \\verb|default| can be used in place \
       of \\verb|true|, \\verb|false|, and \\verb|auto|.  See \
       \\sectionref{fastcheck}{Fast Update Detection} for more information.")

let useFastChecking () =
      Prefs.read fastcheck = `True
   || (Prefs.read fastcheck = `Default (*&& Util.osType = `Unix*))

let immutable = Pred.create "immutable" ~advanced:true
  ("This preference specifies paths for directories whose \
     immediate children are all immutable files --- i.e., once a file has been \
     created, its contents never changes.  When scanning for updates, \
     Unison does not check whether these files have been modified; \
     this can speed update detection significantly (in particular, for mail \
     directories).")

let immutablenot = Pred.create "immutablenot" ~advanced:true
  ("This preference overrides {\\tt immutable}.")

type scanInfo =
    { fastCheck : bool;
      dirFastCheck : bool;
      dirStamp : Props.dirChangedStamp;
      archHash : string;
      showStatus : bool }

(** Status display **)

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
let showStatusAddLength scanInfo info =
  let len1 = Props.length info.Fileinfo.desc in
  let len2 = Osx.ressLength info.Fileinfo.osX.Osx.ressInfo in
    if len1 >= bigFileLengthFS || len2 >= bigFileLengthFS then
      fileLength := bigFileLength
    else
      fileLength :=
        min bigFileLength
          (!fileLength + Uutil.Filesize.toInt len1 + Uutil.Filesize.toInt len2)

let showStatus scanInfo path =
  fileLength := !fileLength + smallFileLength;
  if !fileLength >= bigFileLength then begin
    fileLength := 0;
    let t = Unix.gettimeofday () in
      if t -. !t0 > 0.05 then begin
        if scanInfo.showStatus then
          Uutil.showUpdateStatus (Path.toString path);
        t0 := t
      end
  end

let showStatusDir path = ()

(* BCP (4/09) The code above tries to be smart about showing status messages
   at regular intervals, but people seem to find this confusing.
   I tried replace all this with something simpler -- just show directories as
   they are scanned -- but this seems worse: it prints far too much stuff.
   So I'm going to revert to the old version. *)
(*
  let showStatus path = ()
  let showStatusAddLength info = ()
  let showStatusDir path =
  if not !Trace.runningasserver then begin
  Trace.statusDetail ("scanning... " ^ Path.toString path);
  end
*)

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

(* Check whether the directory immediate children may have changed *)
let rec noChildChange childUpdates =
  match childUpdates with
    [] ->
      true
  | (_, Updates (File _, Previous (`FILE, _, _, _))) :: rem
  | (_, Updates (Dir _, Previous (`DIRECTORY, _, _, _))) :: rem
  | (_, Updates (Symlink _, Previous (`SYMLINK, _, _, _))) :: rem ->
      noChildChange rem
  | _ ->
      false

(* Check whether the directory contents is different from what is in
   the archive *)
let directoryCheckContentUnchanged
      currfspath path info archDesc childUpdates scanInfo =
  if
    noChildChange childUpdates
      &&
    let (info', dataUnchanged, ressUnchanged) =
      Fileinfo.unchanged currfspath path info in
    dataUnchanged
  then begin
    let (archDesc, updated) =
      let inode =
        match Fileinfo.stamp info with Fileinfo.InodeStamp i -> i | _ -> 0 in
      Props.setDirChangeFlag archDesc scanInfo.dirStamp inode in
    let updated =
      updated || not (Props.same_time info.Fileinfo.desc archDesc) in
    if updated then
      debugverbose (fun()->
        Util.msg "Contents of directory %s marked unchanged\n"
          (Fspath.toDebugString (Fspath.concat currfspath path)));
    (Props.setTime archDesc (Props.time info.Fileinfo.desc), updated)
  end else begin
    let (archDesc, updated) =
      Props.setDirChangeFlag archDesc Props.changedDirStamp 0 in
    if updated then
      debugverbose (fun()->
        Util.msg "Contents of directory %s marked changed\n"
          (Fspath.toDebugString (Fspath.concat currfspath path)));
    (archDesc, updated)
  end

(* Check whether the list of children of a directory is clearly unchanged *)
let dirContentsClearlyUnchanged info archDesc scanInfo =
  scanInfo.dirFastCheck
    &&
  let inode =
   match Fileinfo.stamp info with Fileinfo.InodeStamp i -> i | _ -> 0 in
  Props.dirMarkedUnchanged archDesc scanInfo.dirStamp inode
    &&
  Props.same_time info.Fileinfo.desc archDesc
    &&
  (* Check the date is meaningful: the root directory of a FAT
     filesystem does not have modification time, so the time returned
     by [stat] is usually way in the past. *)
  Props.time archDesc >= 631152000. (* Jan 1, 1990 *)

(* Check whether a file's permissions have not changed *)
let isPropUnchanged desc archiveDesc = Props.similar desc archiveDesc

(* Handle file permission change *)
let checkPropChange desc archive archDesc =
  if isPropUnchanged desc archDesc then begin
    debugverbose (fun() -> Util.msg "  Unchanged file\n");
    NoUpdates
  end else begin
    debug (fun() -> Util.msg "  File permissions updated\n");
    Updates (File (desc, ContentsSame), oldInfoOf archive)
  end

(* Check whether a file has changed has changed, by comparing its digest and
   properties against [archDesc], [archFp], and [archStamp].
   Returns a pair (optArch, ui) where [optArch] is *not* None when the file remains
   unchanged but time might be changed.  [optArch] is used by [buildUpdate]
   series functions to compute the _old_ archive with updated time stamp
   (thus, there will no false update the next time) *)
let checkContentsChange
      currfspath path info archive archDesc archFp archStamp archRess scanInfo
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
                 (Util.msg "archStamp is ctime (%f)" stamp)
           end;
           Util.msg " / times: %f = %f... %b"
             (Props.time archDesc) (Props.time info.Fileinfo.desc)
             (Props.same_time info.Fileinfo.desc archDesc);
           Util.msg " / lengths: %s - %s"
             (Uutil.Filesize.toString (Props.length archDesc))
             (Uutil.Filesize.toString  (Props.length info.Fileinfo.desc));
           Util.msg "\n");
  let fastCheck = scanInfo.fastCheck in
  let dataClearlyUnchanged =
    Fpcache.dataClearlyUnchanged fastCheck path info archDesc archStamp in
  let ressClearlyUnchanged =
    Fpcache.ressClearlyUnchanged fastCheck info archRess dataClearlyUnchanged
  in
  if dataClearlyUnchanged && ressClearlyUnchanged then begin
    Xferhint.insertEntry currfspath path archFp;
    None, checkPropChange info.Fileinfo.desc archive archDesc
  end else begin
    debugverbose (fun() -> Util.msg "  Double-check possibly updated file\n");
    showStatusAddLength scanInfo info;
    let (newDesc, newFp, newStamp, newRess) =
      Fpcache.fingerprint fastCheck currfspath path info
        (if dataClearlyUnchanged then Some archFp else None) in
    Xferhint.insertEntry currfspath path newFp;
    debug (fun() -> Util.msg "  archive digest = %s   current digest = %s\n"
             (Os.fullfingerprint_to_string archFp)
             (Os.fullfingerprint_to_string newFp));
    if archFp = newFp then begin
      let newprops = Props.setTime archDesc (Props.time newDesc) in
      let newarch = ArchiveFile (newprops, archFp, newStamp, newRess) in
      debugverbose (fun() ->
        Util.msg "  Contents match: update archive with new time...%f\n"
                   (Props.time newprops));
      Some newarch, checkPropChange newDesc archive archDesc
    end else begin
      debug (fun() -> Util.msg "  Updated file\n");
      (* [BCP 5/2011] We might add a sanity check here: if the file contents
         have changed but the modtime has not, signal an error.  I.e., abort if
           Props.same_time info.Fileinfo.desc archDesc
         is true at this point.
      *)
      None,
      Updates (File (newDesc, ContentsUpdated (newFp, newStamp, newRess)),
               oldInfoOf archive)
    end
  end


(* getChildren = childrenOf + repetition check

   Find the children of fspath+path, and return them, sorted, and
   partitioned into those with case conflicts, those with illegal
   cross platform filenames, and those without problems.

   Note that case conflicts and illegal filenames can only occur under Unix,
   when syncing with a Windows file system. *)
let checkFilename s =
  if Name.badEncoding s then
    `BadEnc
  else if
    (* Don't check unless we are syncing with Windows *)
    Prefs.read Globals.someHostIsRunningWindows &&
    Name.badFile s
  then
    `BadName
  else
    `Ok

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
    else
      checkFilename nm
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
    fspath path (archChi: archive NameMap.t) unchangedChildren scanInfo
    : archive NameMap.t option * (Name.t * Common.updateItem) list *
      bool * bool
    =
  showStatusDir path;
  Fswatch.scanDirectory path;
  let skip =
    Pred.test immutable (Path.toString path) &&
    not (Pred.test immutablenot (Path.toString path)) in

  if unchangedChildren then begin
    if skip then begin
      if Prefs.read Xferhint.xferbycopying then
        NameMap.iter
          (fun nm archive ->
             match archive with
               ArchiveFile (_, archFp, _, _) ->
                 Xferhint.insertEntry fspath (Path.child path nm) archFp
             | _ ->
                 ())
          archChi;
      (None, [], false, false)
    end else begin
      let updates = ref [] in
      let archUpdated = ref false in
      let handleChild nm archive =
        let path' = Path.child path nm in
        debugverbose (fun () -> Util.msg
          "buildUpdateChildren(handleChild): %s\n" (Path.toString path'));
        if Globals.shouldIgnore path' then begin
          (* We have to ignore paths which are in the archive but no
             longer exists in the filesystem. Note that we cannot
             reach this point for files that exists on the filesystem
             ([hasIgnoredChildren] below would have been true). *)
          debugignore (fun()->Util.msg "buildUpdateChildren: ignoring path %s\n"
                                (Path.toString path'));
          archive
        end else begin
          showStatus scanInfo path';
          let (arch,uiChild) =
            buildUpdateRec archive fspath path' scanInfo in
          if uiChild <> NoUpdates then
            updates := (nm, uiChild) :: !updates;
          match arch with
            None      -> archive
          | Some arch -> archUpdated := true; arch
        end in
      let newChi = NameMap.mapi handleChild archChi in
      (* The Recon module relies on the updates to be sorted *)
      ((if !archUpdated then Some newChi else None),
       Safelist.rev !updates, false, false)
    end
  end else
  let curChildren = ref (getChildren fspath path) in
  let emptied = not (NameMap.is_empty archChi) && !curChildren = [] in
  let hasIgnoredChildren = ref false in
  let updates = ref [] in
  let archUpdated = ref false in
  let handleChild nm archive status =
    let path' = Path.child path nm in
    if Globals.shouldIgnore path' then begin
      hasIgnoredChildren := !hasIgnoredChildren || (archive <> NoArchive);
      debugignore (fun()->Util.msg "buildUpdateChildren: ignoring path %s\n"
                            (Path.toString path'));
      archive
    end else begin
      showStatus scanInfo path';
      match status with
        `Ok | `Abs ->
          if skip && archive <> NoArchive && status <> `Abs then begin
            begin match archive with
              ArchiveFile (_, archFp, _, _) ->
                Xferhint.insertEntry fspath path' archFp
            | _ ->
                ()
            end;
            archive
          end else begin
            let (arch,uiChild) =
              buildUpdateRec archive fspath path' scanInfo in
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
                case-insensitive file system.  (File '" ^
               Path.toString path' ^ "')")
          in
          updates := (nm, uiChild) :: !updates;
          archive
      | `BadEnc ->
          let uiChild =
            Error ("The file name is not encoded in Unicode.  (File '"
                   ^ Path.toString path' ^ "')")
          in
          updates := (nm, uiChild) :: !updates;
          archive
      | `BadName ->
          let uiChild =
            Error ("The name of this Unix file is not allowed under Windows.  \
                    (File '" ^ Path.toString path' ^ "')")
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
  (* The Recon module relies on the updates to be sorted *)
  ((if !archUpdated then Some newChi else None),
   Safelist.rev !updates, emptied, !hasIgnoredChildren)

and buildUpdateRec archive currfspath path scanInfo =
  try
    debug (fun() ->
      Util.msg "buildUpdateRec: %s\n"
        (Fspath.toDebugString (Fspath.concat currfspath path)));
    let info = Fileinfo.get true currfspath path in
    match (info.Fileinfo.typ, archive) with
      (`ABSENT, NoArchive) ->
        debug (fun() -> Util.msg "  buildUpdate -> Absent and no archive\n");
        None, NoUpdates
    | (`ABSENT, _) ->
        debug (fun() -> Util.msg "  buildUpdate -> Deleted\n");
        None, Updates (Absent, oldInfoOf archive)
    (* --- *)
    | (`FILE, ArchiveFile (archDesc, archFp, archStamp, archRess)) ->
        checkContentsChange
          currfspath path info archive
          archDesc archFp archStamp archRess scanInfo
    | (`FILE, _) ->
        debug (fun() -> Util.msg "  buildUpdate -> New file\n");
        None,
        begin
          showStatusAddLength scanInfo info;
          let (desc, fp, stamp, ress) =
            Fpcache.fingerprint ~newfile:true
              scanInfo.fastCheck currfspath path info None in
          Xferhint.insertEntry currfspath path fp;
          Updates (File (desc, ContentsUpdated (fp, stamp, ress)),
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
          (* BCP 10/17: If this directory is being treated atomically,
             then we want to use its real modtime; otherwise, we don't
             want to consider it as modified unless its own properties
             have changed (i.e., we don't want touching a file inside
             the directory to count as a modification to the
             directory). *)
          if isPropUnchanged info.Fileinfo.desc archDesc then
            if Pred.test Globals.atomic (Path.toString path) then
              (PropsSame, info.Fileinfo.desc)
            else
              (PropsSame, archDesc)
          else
            (PropsUpdated, info.Fileinfo.desc) in
        let unchanged =
          dirContentsClearlyUnchanged info archDesc scanInfo in
        let (newChildren, childUpdates, emptied, hasIgnoredChildren) =
          buildUpdateChildren
            currfspath path prevChildren unchanged scanInfo in
        let (archDesc, updated) =
          (* If the archive contain ignored children, we cannot use it to
             skip reading the directory contents from the filesystem.
             Actually, we could check for ignored children in the archive,
             but this has a significant cost.  We could mark directories
             with ignored children, and only perform the checks for them,
             but that does not seem worthwhile, as directories with
             ignored children are expected to be rare in the archive.
             (These are files or directories which used not to be
             ignored and are now ignored.) *)
          if hasIgnoredChildren then (archDesc, true) else
          directoryCheckContentUnchanged
            currfspath path info archDesc childUpdates scanInfo in
        (begin match newChildren with
           Some ch ->
             Some (ArchiveDir (archDesc, ch))
         | None ->
             if updated then Some (ArchiveDir (archDesc, prevChildren))
             else None
         end,
         if childUpdates <> [] || permchange = PropsUpdated then
           Updates (Dir (desc, childUpdates, permchange, emptied),
                    oldInfoOf archive)
         else
           NoUpdates)
    | (`DIRECTORY, _) ->
        debug (fun() -> Util.msg "  buildUpdate -> New directory\n");
        let (newChildren, childUpdates, _, _) =
          buildUpdateChildren
            currfspath path NameMap.empty false scanInfo in
        (None,
         Updates (Dir (info.Fileinfo.desc, childUpdates, PropsUpdated, false),
                  oldInfoOf archive))
  with
    Util.Transient(s) -> None, Error(s)

(* Compute the updates for the tree of paths [tree] against archive. *)
let rec buildUpdatePathTree archive fspath here tree scanInfo =
  match tree, archive with
    PathTreeNode children, ArchiveDir (archDesc, archChildren) ->
      let curChildren =
        lazy (List.fold_left (fun m (nm, st) -> NameMap.add nm st m)
                NameMap.empty (getChildren fspath here))
      in
      let updates = ref [] in
      let archUpdated = ref false in
      let newChi = ref archChildren in
      let handleChild nm archive status tree' =
        let path' = Path.child here nm in
        if Os.isTempFile (Name.toString nm) || Globals.shouldIgnore path' then
          archive
        else begin
          match status with
            `Ok | `Abs ->
              let (arch,uiChild) =
                buildUpdatePathTree archive fspath path' tree' scanInfo in
              if uiChild <> NoUpdates then
                updates := (nm, uiChild) :: !updates;
              begin match arch with
                None      -> archive
              | Some arch -> archUpdated := true; arch
              end
          | `Dup ->
              let uiChild =
                Error
                  ("Two or more files on a case-sensitive system have names \
                    identical except for case.  They cannot be synchronized \
                    to a case-insensitive file system.  (File '" ^
                   Path.toString path' ^ "')")
              in
              updates := (nm, uiChild) :: !updates;
              archive
          | `BadEnc ->
              let uiChild =
                Error ("The file name is not encoded in Unicode.  (File '"
                       ^ Path.toString path' ^ "')")
              in
              updates := (nm, uiChild) :: !updates;
              archive
          | `BadName ->
              let uiChild =
                Error
                  ("The name of this Unix file is not allowed under Windows.  \
                    (File '" ^ Path.toString path' ^ "')")
              in
              updates := (nm, uiChild) :: !updates;
              archive
        end
      in
      NameMap.iter
        (fun nm tree' ->
           let inArchive = NameMap.mem nm archChildren in
           let arch =
             if tree' = PathTreeLeaf || not inArchive then begin
               let (nm', st) =
                 try
                   NameMap.findi nm (Lazy.force curChildren)
                 with Not_found -> try
                   (fst (NameMap.findi nm archChildren), `Abs)
                 with Not_found ->
                   (nm, `Abs)
               in
               let arch =
                 try NameMap.find nm archChildren with Not_found -> NoArchive
               in
               handleChild nm' arch st tree'
             end else begin
               let (nm', arch) = NameMap.findi nm archChildren in
               handleChild nm' arch `Ok tree'
             end
           in
           if inArchive then newChi := NameMap.add nm arch !newChi)
        children;
      (begin if !archUpdated then
          Some (ArchiveDir (archDesc, !newChi))
        else
          None
       end,
       if !updates <> [] then
         (* The Recon module relies on the updates to be sorted *)
         Updates (Dir (archDesc, Safelist.rev !updates, PropsSame, false),
                  oldInfoOf archive)
       else
         NoUpdates)
  | _ ->
      showStatus scanInfo here;
      Fswatch.startScanning scanInfo.archHash fspath here;
      let res = buildUpdateRec archive fspath here scanInfo in
      Fswatch.stopScanning ();
      res

(* Compute the updates for [path] against archive.  Also returns an
   archive, which is the old archive with time stamps updated
   appropriately (i.e., for those files whose contents remain
   unchanged).  The filenames are also updated to match the filesystem
   contents.  The directory permissions along the path are also
   collected, in case we need to build the directory hierarchy
   on one side. *)
let rec buildUpdate archive fspath fullpath here path pathTree scanInfo =
  match Path.deconstruct path with
    None ->
      let (arch, ui) =
        buildUpdatePathTree archive fspath here pathTree scanInfo in
      (begin match arch with
         None      -> archive
       | Some arch -> arch
       end,
       ui, here, [])
  | Some(name, path') ->
      let info = Fileinfo.get true fspath here in
      if info.Fileinfo.typ <> `DIRECTORY && info.Fileinfo.typ <> `ABSENT then
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
        (archive, Error error, translatePathLocal fspath fullpath, [])
      else
      let (name', status) =
        if info.Fileinfo.typ = `ABSENT then
          (name, checkFilename name)
        else
          let children = getChildren fspath here in
          try
            Safelist.find (fun (name', _) -> Name.eq name name') children
          with Not_found ->
            (name, checkFilename name)
      in
      match status with
      | `BadEnc ->
          let error =
            Format.sprintf
              "The filename %s in path %s is not encoded in Unicode"
              (Name.toString name) (Path.toString fullpath)
          in
          (archive, Error error, translatePathLocal fspath fullpath, [])
      | `BadName ->
          let error =
            Format.sprintf
              "The filename %s in path %s is not allowed under Windows"
              (Name.toString name) (Path.toString fullpath)
          in
          (archive, Error error, translatePathLocal fspath fullpath, [])
      | `Dup ->
          let error =
            Format.sprintf
              "The path %s is ambiguous at filename %s (i.e., the name \
               of this path is the same, modulo capitalization, as \
               another path in a case-sensitive filesystem, and you are \
               synchronizing this filesystem with a case-insensitive \
               filesystem."
              (Path.toString fullpath) (Name.toString name)
          in
          (archive, Error error, translatePathLocal fspath fullpath, [])
      | `Ok ->
          match archive with
            ArchiveDir (desc, children) ->
              let archChild =
                try NameMap.find name children with Not_found -> NoArchive in
              let otherChildren = NameMap.remove name children in
              let (arch, updates, localPath, props) =
                buildUpdate
                  archChild fspath fullpath (Path.child here name')
                  path' pathTree scanInfo
              in
              let children =
                if arch = NoArchive then otherChildren else
                NameMap.add name' arch otherChildren
              in
              (ArchiveDir (desc, children), updates, localPath,
               if info.Fileinfo.typ = `ABSENT then [] else
               info.Fileinfo.desc :: props)
          | _ ->
              let (arch, updates, localPath, props) =
                buildUpdate
                  NoArchive fspath fullpath (Path.child here name')
                  path' pathTree scanInfo
              in
              assert (arch = NoArchive);
              (archive, updates, localPath,
               if info.Fileinfo.typ = `ABSENT then [] else
               info.Fileinfo.desc :: props)

(* All the predicates that may change the set of files scanned during
   update detection *)
let updatePredicates =
  [("immutable", immutable); ("immutablenot", immutablenot);
   ("ignore", Globals.ignorePred); ("ignorenot", Globals.ignorenotPred);
   ("follow", Path.followPred)]

let predKey : (string * string list) list Proplist.key =
  Proplist.register "update predicates"
let rsrcKey : bool Proplist.key = Proplist.register "rsrc pref"

let checkNoUpdatePredicateChange thisRoot =
  let props = getArchiveProps thisRoot in
  let oldPreds = try Proplist.find predKey props with Not_found -> [] in
  let newPreds =
    List.map (fun (nm, p) -> (nm, Pred.extern p)) updatePredicates in
(*
List.iter
  (fun (nm, l) ->
     Format.eprintf "%s@." nm;
     List.iter (fun s -> Format.eprintf "  %s@." s) l)
newPreds;
Format.eprintf "==> %b@." (oldPreds = newPreds);
*)
  let oldRsrc =
    try Some (Proplist.find rsrcKey props) with Not_found -> None in
  let newRsrc = Prefs.read Osx.rsrc in
  try
    if oldPreds <> newPreds || oldRsrc <> Some newRsrc then raise Not_found;
    Proplist.find dirStampKey props
  with Not_found ->
    let stamp = Props.freshDirStamp () in
    setArchivePropsLocal thisRoot
      (Proplist.add dirStampKey stamp
         (Proplist.add predKey newPreds
            (Proplist.add rsrcKey newRsrc props)));
    stamp

(* This contains the list of synchronized paths and the directory stamps
   used by the previous update detection, when a watcher process is used.
   This make it possible to know when the state of the watcher process
   needs to be reset. *)
let previousFindOptions = Hashtbl.create 7

(* for the given path, find the archive and compute the list of update
   items; as a side effect, update the local archive w.r.t. time-stamps for
   unchanged files *)
let findLocal wantWatcher fspath pathList subpaths :
      (Path.local * Common.updateItem * Props.t list) list =
  debug (fun() -> Util.msg
    "findLocal %s (%s)\n" (Fspath.toDebugString fspath)
    (String.concat " " (Safelist.map Path.toString pathList)));
  addHashToTempNames fspath;
  (* Maybe we should remember the device number where the root lives at
     the beginning of update detection, so that we can check, below, that
     the device has not changed.  This check would allow us to abort in case
     the root is on a removable device and this device gets removed during
     update detection, causing all the files to appear to have been
     deleted.  --BCP 2006 *)
  let (arcName,thisRoot) = archiveName fspath MainArch in
  let archive = getArchive thisRoot in
  let dirStamp = checkNoUpdatePredicateChange thisRoot in
(*
let t1 = Unix.gettimeofday () in
*)
  let scanInfo =
    { fastCheck = useFastChecking ();
      (* Directory optimization is disabled under Windows,
         as Windows does not update directory modification times
         on FAT filesystems. *)
      dirFastCheck = useFastChecking () && Util.osType = `Unix;
      dirStamp = dirStamp; archHash = archiveHash fspath;
      showStatus = not !Trace.runningasserver }
  in
  let (cacheFilename, _) = archiveName fspath FPCache in
  let cacheFile = Os.fileInUnisonDir cacheFilename in
  Fpcache.init scanInfo.fastCheck (Prefs.read ignoreArchives) cacheFile;
  let unchangedOptions =
    try
      Hashtbl.find previousFindOptions scanInfo.archHash
      = (scanInfo.dirStamp, pathList)
    with Not_found ->
      false
  in
  let paths =
    match subpaths with
      Some (unsynchronizedPaths, blacklistedPaths) when unchangedOptions ->
        let (>>) x f = f x in
        let paths =
          Fswatchold.getChanges scanInfo.archHash
          (* We do not really need to filter here (they are filtered also
             by [buildUpdatePathTree], but that might reduce greatly and
             cheaply number of paths to consider... *)
          >> List.filter (fun path -> not (Globals.shouldIgnore path))
        in
        let filterPaths paths subpaths =
          let number_list l =
            let i = ref (-1) in
            Safelist.map (fun x -> incr i; (!i, x)) l
          in
          paths >> (* We number paths, to be able to recover their
                      initial order. *)
                   number_list
                >> (* We put longest paths first, in order to deal
                      correctly with nested paths (tough that might be
                      overkill...) *)
                   List.sort (fun (_, p1) (_, p2) -> Path.compare p2 p1)
                >> (* We extract the set of changed paths included in
                      each synchronized path *)
                   List.fold_left
                     (fun (l, tree) (i, p) ->
                        match tree with
                          None ->
                            ((i, (p, None)) :: l, None)
                        | Some tree ->
                            ((i, (p, getSubTree p tree)) :: l,
                             removePathFromTree p tree))
                     ([], pathTreeOfList subpaths)
                >> fst
                >> (* Finally, we restaure the initial order *)
                   List.sort (fun (i1, _) (i2, _) -> compare i1 i2)
                >> List.map snd
        in
        filterPaths pathList (Safelist.append unsynchronizedPaths paths)
    | _ ->
        if wantWatcher && Fswatchold.start scanInfo.archHash fspath then
          Hashtbl.replace previousFindOptions
            scanInfo.archHash (scanInfo.dirStamp, pathList)
        else
          Hashtbl.remove previousFindOptions scanInfo.archHash;
        Safelist.map (fun p -> (p, Some PathTreeLeaf)) pathList
  in
  let (archive, updates) =
    Safelist.fold_right
      (fun (path, pathTreeOpt) (arch, upd) ->
         match pathTreeOpt with
           Some pathTree when not (Globals.shouldIgnore path) ->
             let (arch', ui, localPath, props) =
               buildUpdate arch fspath path Path.empty path pathTree scanInfo
             in
             (arch', (localPath, ui, props) :: upd)
         | _ ->
             (arch, (translatePathLocal fspath path, NoUpdates, []) :: upd))
      paths (archive, [])
  in
  Fpcache.finish ();
(*
let t2 = Unix.gettimeofday () in
Format.eprintf "Update detection: %f@." (t2 -. t1);
*)
  setArchiveLocal thisRoot archive;
  abortIfAnyMountpointsAreMissing fspath;
  updates

let findOnRoot =
  Remote.registerRootCmd
    "find"
    (fun (fspath, (wantWatcher, pathList, subpaths)) ->
       Lwt.return (findLocal wantWatcher fspath pathList subpaths))

let findUpdatesOnPaths ?wantWatcher pathList subpaths =
  Lwt_unix.run
    (loadArchives true >>= (fun (ok, checksums) ->
     begin if ok then Lwt.return checksums else begin
       lockArchives () >>= (fun () ->
       Remote.Thread.unwindProtect
         (fun () ->
            doArchiveCrashRecovery () >>= (fun () ->
            loadArchives false))
         (fun _ ->
            unlockArchives ()) >>= (fun (_, checksums) ->
       unlockArchives () >>= fun () ->
       Lwt.return checksums))
     end end >>= (fun checksums ->
     checkArchiveCaseSensitivity checksums >>= fun () ->
     begin if Prefs.read dumpArchives then
       Globals.allRootsIter (fun r -> dumpArchiveOnRoot r ())
     else
       Lwt.return ()
     end >>= fun () ->
     let t = Trace.startTimer "Collecting changes" in
     Globals.allRootsMapWithWaitingAction (fun r ->
       debug (fun() -> Util.msg "findOnRoot %s\n" (root2string r));
       findOnRoot r (wantWatcher <> None, pathList, subpaths))
       (fun (host, _) ->
         begin match host with
           Remote _ -> Uutil.showUpdateStatus "";
                       Trace.statusDetail "Waiting for changes from server"
         | _        -> ()
         end)
       >>= (fun updates ->
     Trace.showTimer t;
     let result =
       Safelist.map
         (fun r ->
            match r with
              [i1; i2] -> (i1, i2)
            | _        -> assert false)
         (Safelist.transpose updates)
     in
     Trace.status "";
     Lwt.return result))))

let findUpdates ?wantWatcher subpaths =
  try
    (* TODO: We should filter the paths to remove duplicates (including
       prefixes) and ignored paths *)
    findUpdatesOnPaths ?wantWatcher (Prefs.read Globals.paths) subpaths
  (* Append a message (so that it can be seen in the graphical UI) to point out
     that a server failure can be caused by a discrepancy in features' support
     between a client and a server having compatible version numbers. *)
  with Util.Fatal ("Lost connection with the server" as msg) ->
    if Uutil.myMajorVersion = "2.51"
    then raise (Util.Fatal (msg^"\nMaybe the server does not support "
                               ^"negative pathspec patterns"
                               ^"\n(see the standard error of the server)"))
    else raise (Util.Fatal msg)


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
  let archiveHash = checkArchive true [] archive 0 in
  let props = getArchiveProps root in
  storeArchiveLocal
    (Os.fileInUnisonDir newName) root archive archiveHash magic props;
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
            Format.sprintf "%s\000%.0f.%d"
              ((Case.ops ())#modeDesc) (Unix.gettimeofday ()) (Unix.getpid ())
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
    NoUpdates ->
      archive
  | Error _ ->
      NoArchive
  | Updates (uc, _) ->
      match uc with
        Absent ->
          NoArchive
      | File (desc, ContentsSame) ->
          begin match archive with
            ArchiveFile (_, fp, stamp, ress) ->
              ArchiveFile (desc, fp, stamp, ress)
          | _ ->
              assert false
          end
      | File (desc, ContentsUpdated (fp, stamp, ress)) ->
          ArchiveFile (desc, fp, stamp, ress)
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
  | ArchiveFile (desc, fp, stamp, ress) ->
      ArchiveFile (Props.strip desc, fp, stamp, ress)
  | ArchiveSymlink _ | NoArchive ->
      arch

let updateArchive fspath path ui =
  debug (fun() ->
    Util.msg "updateArchive %s %s\n"
      (Fspath.toDebugString fspath) (Path.toString path));
  let root = thisRootsGlobalName fspath in
  let archive = getArchive root in
  let (_, subArch) = getPathInArchive archive Path.empty path in
  updateArchiveRec ui (stripArchive path subArch)

(* (For breaking the dependency loop between update.ml and stasher.ml...) *)
let stashCurrentVersion = ref (fun _ _ -> ())
let setStasherFun f = stashCurrentVersion := f

(* This function is called for files changed only in identical ways.
   It only updates the archives and perhaps makes backups. *)
let markEqualLocal fspath paths =
  let root = thisRootsGlobalName fspath in
  let archive = ref (getArchive root) in
  Tree.iteri paths Path.empty Path.child
    (fun path uc ->
       debug (fun() ->
         Util.msg "markEqualLocal %s %s\n"
           (Fspath.toDebugString fspath) (Path.toString path));
       let arch =
         updatePathInArchive !archive fspath Path.empty path
           (fun archive localPath ->
              !stashCurrentVersion fspath localPath;
              updateArchiveRec (Updates (uc, New)) archive)
       in
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
         [Tree.map (fun (nm1, nm2) -> nm1) (fun (uc1,uc2) -> uc1) equals;
          Tree.map (fun (nm1, nm2) -> nm2) (fun (uc1,uc2) -> uc2) equals])
  end

let replaceArchiveLocal fspath path newArch =
  debug (fun() -> Util.msg
             "replaceArchiveLocal %s %s\n"
             (Fspath.toDebugString fspath)
             (Path.toString path)
        );
  let root = thisRootsGlobalName fspath in
  let archive = getArchive root in
  let archive =
    updatePathInArchive archive fspath Path.empty path (fun _ _ -> newArch) in
  setArchiveLocal root archive

let replaceArchiveOnRoot =
  Remote.registerRootCmd
    "replaceArchive"
    (fun (fspath, (pathTo, arch)) ->
       replaceArchiveLocal fspath pathTo arch;
       Lwt.return ())

let replaceArchive root pathTo archive =
  replaceArchiveOnRoot root (pathTo, archive)

(* Update the archive to reflect
      - the last observed state of the file on disk (ui)
      - the permission bits that have been propagated from the other
        replica, if any (permOpt) *)
let doUpdateProps arch propOpt ui =
  let newArch =
    match ui with
      Updates (File (desc, ContentsSame), _) ->
        begin match arch with
          ArchiveFile (_, fp, stamp, ress) ->
            ArchiveFile (desc, fp, stamp, ress)
        | _ ->
            assert false
        end
    | Updates (File (desc, ContentsUpdated (fp, stamp, ress)), _) ->
        ArchiveFile(desc, fp, stamp, ress)
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
        ArchiveFile (desc, fp, stamp, ress) ->
          ArchiveFile (Props.override desc desc', fp, stamp, ress)
      | ArchiveDir (desc, children) ->
          ArchiveDir (Props.override desc desc', children)
      | _ ->
          assert false
      end
  | None -> newArch

let updateProps fspath path propOpt ui =
  debug (fun() ->
    Util.msg "updateProps %s %s\n"
      (Fspath.toDebugString fspath) (Path.toString path));
  let root = thisRootsGlobalName fspath in
  let archive = getArchive root in
  let archive =
    updatePathInArchive archive fspath Path.empty path
      (fun arch _ -> doUpdateProps arch propOpt ui) in
  setArchiveLocal root archive

(*************************************************************************)
(*                  Make sure no change has happened                     *)
(*************************************************************************)

let fastCheckMiss path desc ress oldDesc oldRess =
  useFastChecking()
    &&
  Props.same_time desc oldDesc
    &&
  Props.length desc = Props.length oldDesc
    &&
  not (Fpcache.excelFile path)
    &&
  Osx.ressUnchanged oldRess ress None true

let doMarkPossiblyUpdated arch =
  match arch with
    ArchiveFile (desc, fp, stamp, ress) ->
      (* It would be cleaner to have a special stamp for this *)
      ArchiveFile (desc, fp, Fileinfo.InodeStamp (-1), ress)
  | _ ->
      (* Should not happen, actually.  But this is hard to test... *)
      arch

let markPossiblyUpdated fspath path =
  debug (fun() ->
    Util.msg "markPossiblyUpdated %s %s\n"
      (Fspath.toDebugString fspath) (Path.toString path));
  let root = thisRootsGlobalName fspath in
  let archive = getArchive root in
  let archive =
    updatePathInArchive archive fspath Path.empty path
      (fun arch _ -> doMarkPossiblyUpdated arch) in
  setArchiveLocal root archive

let rec markPossiblyUpdatedRec fspath path ui =
  match ui with
    Updates (File (desc, ContentsUpdated (_, _, ress)),
             Previous (`FILE, oldDesc, _, oldRess)) ->
      if fastCheckMiss path desc ress oldDesc oldRess then
        markPossiblyUpdated fspath path
  | Updates (Dir (_, uiChildren, _, _), _) ->
      List.iter
        (fun (nm, uiChild) ->
           markPossiblyUpdatedRec fspath (Path.child path nm) uiChild)
        uiChildren
  | _ ->
      ()

let reportUpdate warnFastCheck explanation =
  let msg =
    "Destination updated during synchronization\n" ^ explanation ^
   if warnFastCheck then
     "  (if this happens repeatedly on a file that has not been changed, \n\
     \  try running once with 'fastcheck' set to false)"
   else
     ""
  in
  raise (Util.Transient msg)

let rec explainUpdate path ui =
  match ui with
    NoUpdates ->
      ()
  | Error err ->
      raise (Util.Transient ("Could not check destination:\n" ^ err))
  | Updates (Absent, _) ->
      reportUpdate false
        (Format.sprintf "The file %s has been deleted\n"
           (Path.toString path))
  | Updates (File (_, ContentsSame), _) ->
      reportUpdate false
        (Format.sprintf "The properties of file %s have been modified\n"
           (Path.toString path))
  | Updates (File (desc, ContentsUpdated (_, _, ress)),
             Previous (`FILE, oldDesc, oldFp, oldRess)) ->
      if not (Os.isPseudoFingerprint oldFp) then
        reportUpdate (fastCheckMiss path desc ress oldDesc oldRess)
          (Format.sprintf "The contents of file %s have been modified\n"
             (Path.toString path))
  | Updates (File (_, ContentsUpdated _), _) ->
      reportUpdate false
        (Format.sprintf "The file %s has been created\n"
           (Path.toString path))
  | Updates (Symlink _, Previous (`SYMLINK, _, _, _)) ->
      reportUpdate false
        (Format.sprintf "The symlink %s has been modified\n"
           (Path.toString path))
  | Updates (Symlink _, _) ->
      reportUpdate false
        (Format.sprintf "The symlink %s has been created\n"
           (Path.toString path))
  | Updates (Dir (_, _, PropsUpdated, _), Previous (`DIRECTORY, _, _, _)) ->
      reportUpdate false
        (Format.sprintf
           "The properties of directory %s have been modified\n"
           (Path.toString path))
  | Updates (Dir (_, _, PropsUpdated, _), _) ->
      reportUpdate false
        (Format.sprintf "The directory %s has been created\n"
           (Path.toString path))
  | Updates (Dir (_, uiChildren, PropsSame, _), _) ->
      List.iter
        (fun (nm, uiChild) -> explainUpdate (Path.child path nm) uiChild)
        uiChildren

let checkNoUpdates fspath pathInArchive ui =
  debug (fun() ->
    Util.msg "checkNoUpdates %s %s\n"
      (Fspath.toDebugString fspath) (Path.toString pathInArchive));
  let archive = getArchive (thisRootsGlobalName fspath) in
  let (localPath, archive) =
    getPathInArchive archive Path.empty pathInArchive in
  (* Update the original archive to reflect what we believe is the current
     state of the replica... *)
  let archive = updateArchiveRec ui archive in
  (* ...and check that this is a good description of what's out in the world *)
  let scanInfo =
    { fastCheck = false; dirFastCheck = false;
      dirStamp = Props.changedDirStamp; archHash = "" (* Not used *);
      showStatus = false } in
  let (_, uiNew) = buildUpdateRec archive fspath localPath scanInfo in
  markPossiblyUpdatedRec fspath pathInArchive uiNew;
  explainUpdate pathInArchive uiNew;
  archive

(*****************************************************************************)
(*                                UPDATE SIZE                                *)
(*****************************************************************************)

let sizeZero = (0, Uutil.Filesize.zero)
let sizeOne = (1, Uutil.Filesize.zero)
let sizeAdd (items, bytes) (items', bytes') =
  (items + items', Uutil.Filesize.add bytes bytes')

let fileSize desc ress =
  (1, Uutil.Filesize.add (Props.length desc) (Osx.ressLength ress))

let rec archiveSize arch =
  match arch with
    NoArchive ->
      sizeZero
  | ArchiveDir (_, arcCh) ->
      NameMap.fold
        (fun _ ar size -> sizeAdd size (archiveSize ar))
        arcCh sizeOne
  | ArchiveFile (desc, _, _, ress) ->
      fileSize desc ress
  | ArchiveSymlink _ ->
      sizeOne

let rec updateSizeRec archive ui =
  match ui with
    NoUpdates ->
      archiveSize archive
  | Error _ ->
      sizeZero
  | Updates (uc, _) ->
      match uc with
        Absent ->
          sizeZero
      | File (desc, ContentsSame) ->
          begin match archive with
            ArchiveFile (_, _, _, ress) -> fileSize desc ress
          | _                           -> assert false
          end
      | File (desc, ContentsUpdated (_, _, ress)) ->
          fileSize desc ress
      | Symlink l ->
          sizeOne
      | Dir (_, children, _, _) ->
          match archive with
            ArchiveDir (_, arcCh) ->
              let ch = NameMap.map (fun ch -> (ch, NoUpdates)) arcCh in
              let ch =
                List.fold_left
                  (fun ch (nm, uiChild) ->
                     let arcChild =
                       try fst (NameMap.find nm ch)
                       with Not_found -> NoArchive
                     in
                     NameMap.add nm (arcChild, uiChild) ch)
                  ch children
              in
              NameMap.fold
                (fun _ (ar, ui) size -> sizeAdd size (updateSizeRec ar ui))
                ch sizeOne
          | _ ->
              List.fold_left
                (fun size (_, uiChild) ->
                   sizeAdd size (updateSizeRec NoArchive uiChild))
                sizeOne children

let updateSize path ui =
  let rootLocal = Globals.localRoot () in
  let fspathLocal = snd rootLocal in
  let root = thisRootsGlobalName fspathLocal in
  let archive = getArchive root in
  let (_, subArch) = getPathInArchive archive Path.empty path in
  updateSizeRec subArch ui

(*****************************************************************************)
(*                                MISC                                       *)
(*****************************************************************************)

let rec iterFiles fspath path arch f =
  match arch with
    ArchiveDir (_, children) ->
      NameMap.iter
        (fun nm arch -> iterFiles fspath (Path.child path nm) arch f) children
  | ArchiveFile (desc, fp, stamp, ress) ->
      f fspath path fp
  | _ ->
      ()

(* Hook for filesystem auto-detection (not implemented yet) *)
let inspectFilesystem =
  Remote.registerRootCmd
    "inspectFilesystem"
    (fun _ -> Lwt.return Proplist.empty)
