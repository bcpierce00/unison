(* Unison file synchronizer: src/os.ml *)
(* Copyright 1999-2007 (see COPYING for details) *)

(* This file attempts to isolate operating system specific details from the  *)
(* rest of the program.                                                      *)

let debug = Util.debug "os"

let myCanonicalHostName =
  try Unix.getenv "UNISONLOCALHOSTNAME"
  with Not_found -> Unix.gethostname()

let tempFilePrefix = ".unison."
let tempFileSuffixFixed = ".unison.tmp"
let tempFileSuffix = ref tempFileSuffixFixed
let includeInTempNames s =
  tempFileSuffix :=
    if s = "" then tempFileSuffixFixed
    else "." ^ s ^ tempFileSuffixFixed

let xferDelete = ref (fun (fp,p) -> ())
let xferRename = ref (fun (fp,p) (ftp,tp) -> ())

let initializeXferFunctions del ren =
  xferDelete := del;
  xferRename := ren
      

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

let rec isAppleDoubleFile file =
  Prefs.read Osx.rsrc &&
  String.length file > 2 && file.[0] = '.' && file.[1] = '_'

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
(* does it belong to here ? *)
(*          else if Util.endswith newFile backupFileSuffix then begin *)
(*             let newPath = Path.child path (Name.fromString newFile) in *)
(*             removeBackupIfUnwanted fspath newPath; *)
(*             children *)
(*           end  *)
	  else if
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
          begin try
            let result = loop [] directory in
            Unix.closedir directory;
            result
          with Unix.Unix_error _ as e ->
            begin try
              Unix.closedir directory
            with Unix.Unix_error _ -> () end;
            raise e
          end
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
	  (!xferDelete) (fspath, path);
          Unix.rmdir absolutePath
      | `FILE ->
          if Util.osType <> `Unix then begin
            try
              Unix.chmod absolutePath 0o600;
            with Unix.Unix_error _ -> ()
          end;
	  (!xferDelete) (fspath, path);
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
    
let rename fname sourcefspath sourcepath targetfspath targetpath =
  let source = Fspath.concat sourcefspath sourcepath in
  let source' = Fspath.toString source in
  let target = Fspath.concat targetfspath targetpath in
  let target' = Fspath.toString target in
  if source' = target' then
    raise (Util.Transient ("Rename ("^fname^"): identical source and target " ^ source'));
  Util.convertUnixErrorsToTransient ("renaming " ^ source' ^ " to " ^ target')
    (fun () ->
      debug (fun() -> Util.msg "rename %s to %s\n" source' target');
      (!xferRename) (sourcefspath, sourcepath) (targetfspath, targetpath);
      Unix.rename source' target';
      if Prefs.read Osx.rsrc then begin
        let sourceDouble = Osx.appleDoubleFile sourcefspath sourcepath in
        let targetDouble = Osx.appleDoubleFile targetfspath targetpath in
        if Sys.file_exists sourceDouble then
          Unix.rename sourceDouble targetDouble
        else if Sys.file_exists targetDouble then
          Unix.unlink targetDouble
      end)
    
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
