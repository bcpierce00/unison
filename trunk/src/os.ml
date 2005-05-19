(* $I1: Unison file synchronizer: src/os.ml $ *)
(* $I2: Last modified by vouillon on Fri, 05 Nov 2004 10:12:27 -0500 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

(* This file attempts to isolate operating system specific details from the  *)
(* rest of the program.                                                      *)

let debug = Util.debug "os"

let myCanonicalHostName =
  try Unix.getenv "UNISONLOCALHOSTNAME"
  with Not_found -> Unix.gethostname()

let tempFilePrefix = ".#"
let backupFileSuffix = ".unison.bak"
let tempFileSuffixFixed = ".unison.tmp"
let tempFileSuffix = ref tempFileSuffixFixed
let includeInTempNames s =
  tempFileSuffix :=
    if s = "" then tempFileSuffixFixed
    else "." ^ s ^ tempFileSuffixFixed


(*****************************************************************************)
(*                             OPTIONS                                       *)
(*****************************************************************************)
(* It seems to me that all the options should be put into a single options   *)
(* file.  I put these here for lack of a better palce.                       *)
(*****************************************************************************)

let backups =
  Prefs.createBool "backups" false
    "keep backup copies of files (see also 'backup')"
    ("When this flag is {\\tt true}, "
     ^ "Unison will keep the old version of a file as a backup whenever "
     ^ "a change is propagated.  These backup files are left in the same "
     ^ "directory, with extension \\verb|.bak|.  ")

let maxbackups =
  Prefs.createInt "maxbackups" 2
    "number of backed up versions of a file"
    ("This preference specifies the number of backup versions that will "
     ^ "be kept by unison, for each path that matches the predicate "
     ^ "\\verb|backup|.  The default is 2.")

let _ = Prefs.alias maxbackups "mirrorversions"

let minbackups =
  Prefs.createInt "minbackups" 1
    "number of backup version that will be kept regardless of age"
    ("When a backup exceeds \\verb|maxbackupage| days old, it will be "
     ^ "deleted during the next sync.  However \\verb|minbackups| versions "
     ^ "will be kept regardless of age.")

let maxbackupage =
  Prefs.createInt "maxbackupage" 100
    "number of days after which backup versions get deleted"
    ("When a backup exceeds \\verb|maxbackupage| days old, it will be "
     ^ "deleted during the next sync.  However \\verb|minbackups| versions "
     ^ "will be kept regardless of age.  The check is made during file "
     ^ "synchronization.  A value of 0 will keep files regardless of age.")


(*****************************************************************************)
(*                      QUERYING THE FILESYSTEM                              *)
(*****************************************************************************)

let exists fspath path =
  (Fileinfo.get false fspath path).Fileinfo.typ <> `ABSENT

let readLink fspath path =
  Util.convertUnixErrorsToTransient
  "reading symbolic link"
    (fun () ->
       let abspath = Fspath.concatToString fspath path in
       Unix.readlink abspath)

let isAppleDoubleFile file =
  Prefs.read Osx.rsrc &&
  String.length file > 2 && file.[0] = '.' && file.[1] = '_'

(*****************************************************************************)
(* When we see a backup file, check to see if it should be deleted because   *)
(* it is too old or because there are too many backup version for a given    *)
(* file.   <<>>                                                              *)

let backupRe =
  Str.regexp ( "^.*\\.\\.\\([0-9]+\\)\\." ^ backupFileSuffix ^ "$" )

let rec removeBackupIfUnwanted fspath path = 
  if Prefs.read backups then begin
    let absolutePath = Fspath.concatToString fspath path in
    let stat = Unix.lstat absolutePath in
    let age =
      int_of_float ((Unix.time () -. stat.Unix.st_mtime) /. (60.0 *. 60.0)) in
    let version = 
      let pathstring = Path.toString path in
      if Str.string_match backupRe pathstring 0 then
        int_of_string (Str.matched_group 1 pathstring)
      else
        0 in
    if version > Prefs.read maxbackups && version > Prefs.read minbackups
    then begin
      debug (fun ()->
        Util.msg "File %s ver %d is too high\n" absolutePath version);
      delete fspath path
    end else if
      Prefs.read maxbackupage <> 0 && age > Prefs.read maxbackupage && 
      version > Prefs.read minbackups
    then begin
      debug (fun ()-> Util.msg "File %s age %d is too old\n" absolutePath age);
      delete fspath path
    end else begin
      debug (fun()-> Util.msg "File %s just right\n" absolutePath);
    end 
  end

(* Assumes that (fspath, path) is a directory, and returns the list of       *)
(* children, except for '.' and '..'.  Note that childrenOf and delete are   *)
(* mutually recursive: this is because one of the side-effects of childrenOf *)
(* is to delete old files left around by Unison.                             *)
(* And to get rid of too old or too many backup files.           <<>>        *)
and childrenOf fspath path =
  Util.convertUnixErrorsToTransient
  "scanning directory"
    (fun () ->
      let rec loop children directory =
        let newFile = try Unix.readdir directory with End_of_file -> "" in
        if newFile = "" then children else
        let newChildren =
          if newFile = "." || newFile = ".." || isAppleDoubleFile newFile then
            children
          else if Util.endswith newFile backupFileSuffix then begin
            let newPath = Path.child path (Name.fromString newFile) in
            removeBackupIfUnwanted fspath newPath;
            children
          end else if
            Util.endswith newFile tempFileSuffixFixed &&
            Util.startswith newFile tempFilePrefix
          then begin
            if Util.endswith newFile !tempFileSuffix then begin
              let newPath = Path.child path (Name.fromString newFile) in
              debug (fun()-> Util.msg "deleting old temp file %s\n"
                               (Fspath.concatToString fspath newPath));
              delete fspath newPath
            end;
            children
          end else
            Name.fromString newFile :: children in
        loop newChildren directory
      in
      let absolutePath = Fspath.concat fspath path in
      let directory =
        try
          Some (Fspath.opendir absolutePath)
        with Unix.Unix_error (Unix.ENOENT, _, _) ->
          (* FIX (in Ocaml): under Windows, when a directory is empty
             (not even "." and ".."), FindFirstFile fails with
             ERROR_FILE_NOT_FOUND while ocaml expects the error
             ERROR_NO_MORE_FILES *)
          None
      in
      match directory with
        Some directory ->
          let result = loop [] directory in
          Unix.closedir directory;
          result
      | None ->
          [])

(*****************************************************************************)
(*                        ACTIONS ON FILESYSTEM                              *)
(*****************************************************************************)

(* Deletes a file or a directory, but checks before if there is something    *)
and delete fspath path =
  Util.convertUnixErrorsToTransient
  "deleting"
    (fun () ->
       let absolutePath = Fspath.concatToString fspath path in
       match (Fileinfo.get false fspath path).Fileinfo.typ with
         `DIRECTORY ->
           begin try
             Unix.chmod absolutePath 0o700
           with Unix.Unix_error _ -> () end;
           Safelist.iter
             (fun child -> delete fspath (Path.child path child))
             (childrenOf fspath path);
           Unix.rmdir absolutePath
       | `FILE ->
           if Util.osType <> `Unix then begin
             try
               Unix.chmod absolutePath 0o600;
             with Unix.Unix_error _ -> ()
           end;
           Unix.unlink absolutePath;
           if Prefs.read Osx.rsrc then begin
             let pathDouble = Osx.appleDoubleFile fspath path in
             if Sys.file_exists pathDouble then
               Unix.unlink pathDouble
           end
       | `SYMLINK ->
           (* Note that chmod would not do the right thing on links *)
           Unix.unlink absolutePath
       | `ABSENT ->
           ())

let rename sourcefspath sourcepath targetfspath targetpath =
  let source = Fspath.concat sourcefspath sourcepath in
  let source' = Fspath.toString source in
  let target = Fspath.concat targetfspath targetpath in
  let target' = Fspath.toString target in
  Util.convertUnixErrorsToTransient
  "renaming"
    (fun () ->
       debug (fun() -> Util.msg "rename %s to %s\n" source' target');
       Unix.rename source' target';
       if Prefs.read Osx.rsrc then begin
         let sourceDouble = Osx.appleDoubleFile sourcefspath sourcepath in
         let targetDouble = Osx.appleDoubleFile targetfspath targetpath in
         if Sys.file_exists sourceDouble then
           Unix.rename sourceDouble targetDouble
         else if Sys.file_exists targetDouble then
           Unix.unlink targetDouble
       end)

let renameIfAllowed sourcefspath sourcepath targetfspath targetpath =
  let source = Fspath.concat sourcefspath sourcepath in
  let source' = Fspath.toString source in
  let target = Fspath.concat targetfspath targetpath in
  let target' = Fspath.toString target in
  Util.convertUnixErrorsToTransient
  "renaming"
    (fun () ->
       debug (fun() -> Util.msg "rename %s to %s\n" source' target');
       let allowed =
         try Unix.rename source' target'; None with
           Unix.Unix_error (Unix.EPERM, _, _) as e -> Some e
       in
       if allowed = None && Prefs.read Osx.rsrc then begin
         let sourceDouble = Osx.appleDoubleFile sourcefspath sourcepath in
         let targetDouble = Osx.appleDoubleFile targetfspath targetpath in
         if Sys.file_exists sourceDouble then
           Unix.rename sourceDouble targetDouble
         else if Sys.file_exists targetDouble then
           Unix.unlink targetDouble
       end;
       allowed)

let symlink =
  if Util.isCygwin || (Util.osType != `Win32) then
    fun fspath path l ->
      Util.convertUnixErrorsToTransient
      "writing symbolic link"
      (fun () ->
         let abspath = Fspath.concatToString fspath path in
         Unix.symlink l abspath)
  else
    fun fspath path l ->
      raise (Util.Transient "symlink not supported under Win32")

(* Create a new directory, using the permissions from the given props        *)
let createDir fspath path props =
  Util.convertUnixErrorsToTransient
  "creating directory"
    (fun () ->
       let absolutePath = Fspath.concatToString fspath path in
       Unix.mkdir absolutePath (Props.perms props))

(*****************************************************************************)
(*                              FINGERPRINTS                                 *)
(*****************************************************************************)

type fullfingerprint = Fingerprint.t * Fingerprint.t

let fingerprint fspath path info =
  (Fingerprint.file fspath path,
   Osx.ressFingerprint fspath path info.Fileinfo.osX)

(* FIX: not completely safe under Unix                                       *)
(* (with networked file system such as NFS)                                  *)
let safeFingerprint fspath path info optDig =
  let rec retryLoop count info optDig optRessDig =
    if count = 0 then
      raise (Util.Transient
               (Printf.sprintf
                  "Failed to fingerprint file \"%s\": \
                   the file keeps on changing"
                  (Fspath.concatToString fspath path)))
    else
      let dig =
        match optDig with
          None     -> Fingerprint.file fspath path
        | Some dig -> dig
      in
      let ressDig =
        match optRessDig with
          None      -> Osx.ressFingerprint fspath path info.Fileinfo.osX
        | Some ress -> ress
      in
      let (info', dataUnchanged, ressUnchanged) =
        Fileinfo.unchanged fspath path info in
      if dataUnchanged && ressUnchanged then
        (info', (dig, ressDig))
      else
        retryLoop (count - 1) info'
          (if dataUnchanged then Some dig else None)
          (if ressUnchanged then Some ressDig else None)
  in
  retryLoop 10 info (* Maximum retries: 10 times *)
    (match optDig with None -> None | Some (d, _) -> Some d)
    (match optDig with None -> None | Some (_, d) -> Some d)

let fullfingerprint_to_string (fp,rfp) =
  Printf.sprintf "(%s,%s)" (Fingerprint.toString fp) (Fingerprint.toString rfp)

let fullfingerprint_dummy = (Fingerprint.dummy,Fingerprint.dummy)

(*****************************************************************************)
(*                           UNISON DIRECTORY                                *)
(*****************************************************************************)

(* Gives the fspath of the archive directory on the machine, depending on    *)
(* which OS we use                                                           *)
let unisonDir =
  try Fspath.canonize (Some (Unix.getenv "UNISON"))
  with Not_found ->
    let genericName = Util.fileInHomeDir (Printf.sprintf ".%s" Uutil.myName) in
    if Osx.isMacOSX then
      let osxName = Util.fileInHomeDir "Library/Application Support/Unison" in
      if Sys.file_exists genericName then Fspath.canonize (Some genericName)
      else Fspath.canonize (Some osxName)
    else
      Fspath.canonize (Some genericName)

(* build a fspath representing an archive child path whose name is given     *)
let fileInUnisonDir str =
  let n =
    try Name.fromString str
    with Invalid_argument _ ->
      raise (Util.Transient
               ("Ill-formed name of file in UNISON directory: "^str))
  in
    Fspath.child unisonDir n

(* Make sure archive directory exists                                        *)
let createUnisonDir() =
  try ignore (Fspath.stat unisonDir)
  with Unix.Unix_error(_) ->
    Util.convertUnixErrorsToFatal
      (Printf.sprintf "creating unison directory %s"
         (Fspath.toString unisonDir))
      (fun () ->
         ignore (Unix.mkdir (Fspath.toString unisonDir) 0o700))

(*****************************************************************************)
(*                           TEMPORARY FILES                                 *)
(*****************************************************************************)

(* Generates an unused fspath for a temporary file.                          *)
let freshPath fspath path prefix suffix =
  let rec f i =
    let s =
      if i=0 then suffix
      else Printf.sprintf "..%03d.%s" i suffix in
    let tempPath =
      Path.addPrefixToFinalName
        (Path.addSuffixToFinalName path s)
        prefix
    in
    if exists fspath tempPath then f (i + 1) else tempPath
  in f 0

let tempPath fspath path =
  freshPath fspath path tempFilePrefix !tempFileSuffix

(* Generates a file name for a backup file.  If backup file already exists,  *)
(* the old file will be renamed with the count incremented.  The newest      *)
(* backup file is always path..001..unison.bak and larger numbers mean older *)
(* files.                                                                    *)
let backupPath fspath path =
  let rec f i =
    let s = Printf.sprintf "..%03d.%s" i backupFileSuffix in
    let tempPath = Path.addSuffixToFinalName path s in
    if exists fspath tempPath then rename fspath tempPath fspath (f (i + 1));
    tempPath
  in
  f 1

(*****************************************************************************)
(*                     INTERRUPTED SYSTEM CALLS                              *)
(*****************************************************************************)
(* Needed because in lwt/lwt_unix.ml we set a signal handler for SIG_CHLD,
   which means that slow system calls can be interrupted to handle
   SIG_CHLD.  We want to restart these system calls.  It would be much
   better to do this using SA_RESTART, however, ocaml's Unix module does
   not support this, probably because it isn't nicely portable. *)
let accept fd =
   let rec loop () =
     try Unix.accept fd
     with Unix.Unix_error(Unix.EINTR,_,_) -> loop() in
   loop()
