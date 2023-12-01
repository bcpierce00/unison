(* Unison file synchronizer: src/os.ml *)
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


(* This file attempts to isolate operating system specific details from the  *)
(* rest of the program.                                                      *)

let debug = Util.debug "os"

(* Assumption: Prefs are not loaded on server, so clientHostName is always *)
(* set to myCanonicalHostName. *)

let localCanonicalHostName =
  try System.getenv "UNISONLOCALHOSTNAME"
  with Not_found -> Unix.gethostname()

let clientHostName : string Prefs.t =
  Prefs.createString "clientHostName" localCanonicalHostName
    ~category:(`Advanced `Remote)
    "set host name of client"
    ("When specified, the host name of the client will not be guessed " ^
     "and the provided host name will be used to find the archive.")

let serverHostName = localCanonicalHostName

let myCanonicalHostName () =
  if !Trace.runningasserver then serverHostName else Prefs.read clientHostName

let tempFilePrefix = ".unison."
let tempFileSuffixFixed = ".unison.tmp"
let tempFileSuffix = ref tempFileSuffixFixed
let includeInTempNames s =
  (* BCP: Added this in Jan 08.  If (as I believe) it never fails, then this tricky
     stuff can be deleted. *)
  assert (s<>"");
  tempFileSuffix :=
    if s = "" then tempFileSuffixFixed
    else "." ^ s ^ tempFileSuffixFixed

let isTempFile file =
  Util.endswith file tempFileSuffixFixed &&
  Util.startswith file tempFilePrefix

(*****************************************************************************)
(*                      QUERYING THE FILESYSTEM                              *)
(*****************************************************************************)

let exists fspath path =
  Fileinfo.getType false fspath path <> `ABSENT

let readLink fspath path =
  Util.convertUnixErrorsToTransient
  "reading symbolic link"
    (fun () ->
       let abspath = Fspath.concat fspath path in
       let l = Fs.readlink abspath in
       if Sys.win32 || Sys.cygwin then
         Fileutil.backslashes2forwardslashes l
       else
         l
    )

let rec isAppleDoubleFile file =
  Prefs.read Osx.rsrc &&
  String.length file > 2 && file.[0] = '.' && file.[1] = '_'

(* Assumes that (fspath, path) is a directory, and returns the list of       *)
(* children, except for '.' and '..'.                                        *)
let allChildrenOf fspath path =
  Util.convertUnixErrorsToTransient
  "scanning directory"
    (fun () ->
      let rec loop children directory =
        let newFile = try directory.Fs.readdir () with End_of_file -> "" in
        if newFile = "" then children else
        let newChildren =
          if newFile = "." || newFile = ".." then
            children
          else
            Name.fromString newFile :: children in
        loop newChildren directory
      in
      let absolutePath = Fspath.concat fspath path in
      let directory =
        try
          Some (Fs.opendir absolutePath)
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
            directory.Fs.closedir ();
            result
          with Unix.Unix_error _ as e ->
            begin try
              directory.Fs.closedir ()
            with Unix.Unix_error _ -> () end;
            raise e
          end
      | None ->
          [])

(* Assumes that (fspath, path) is a directory, and returns the list of       *)
(* children, except for temporary files and AppleDouble files.               *)
let rec childrenOf fspath path =
  List.filter
    (fun filename ->
       let file = Name.toString filename in
       if isAppleDoubleFile file then
         false
(* does it belong to here ? *)
(*          else if Util.endswith file backupFileSuffix then begin *)
(*             let newPath = Path.child path filename in *)
(*             removeBackupIfUnwanted fspath newPath; *)
(*             false *)
(*           end  *)
       else if isTempFile file then begin
         if Util.endswith file !tempFileSuffix then begin
           let p = Path.child path filename in
           let i = Fileinfo.getBasic false fspath p in
           let secondsinthirtydays = 2592000.0 in
           if Props.time i.Fileinfo.desc +. secondsinthirtydays < Util.time()
           then begin
             debug (fun()-> Util.msg "deleting old temp file %s\n"
                      (Fspath.toDebugString (Fspath.concat fspath p)));
             delete fspath p
           end else
             debug (fun()-> Util.msg
                      "keeping temp file %s since it is less than 30 days old\n"
                      (Fspath.toDebugString (Fspath.concat fspath p)));
         end;
         false
       end else
         true)
    (allChildrenOf fspath path)

(*****************************************************************************)
(*                        ACTIONS ON FILESYSTEM                              *)
(*****************************************************************************)

(* Deletes a file or a directory, but checks before if there is something    *)
and delete fspath path =
  Util.convertUnixErrorsToTransient
    "deleting"
    (fun () ->
      let absolutePath = Fspath.concat fspath path in
      match Fileinfo.getType false fspath path with
        `DIRECTORY ->
          begin try
            Fs.chmod absolutePath 0o700
          with Unix.Unix_error _ -> () end;
          Safelist.iter
            (fun child -> delete fspath (Path.child path child))
            (allChildrenOf fspath path);
          Fs.rmdir absolutePath
      | `FILE ->
          if not Sys.unix then begin
            try
              Fs.chmod absolutePath 0o600;
            with Unix.Unix_error _ -> ()
          end;
          Fs.unlink absolutePath;
          if Prefs.read Osx.rsrc then begin
            let pathDouble = Fspath.appleDouble absolutePath in
            if Fs.file_exists pathDouble then
              Fs.unlink pathDouble
          end
      | `SYMLINK ->
           (* Note that chmod would not do the right thing on links *)
          Fs.unlink absolutePath
      | `ABSENT ->
          ())

let rename fname sourcefspath sourcepath targetfspath targetpath =
  let source = Fspath.concat sourcefspath sourcepath in
  let source' = Fspath.toPrintString source in
  let target = Fspath.concat targetfspath targetpath in
  let target' = Fspath.toPrintString target in
  if source = target then
    raise (Util.Transient ("Rename ("^fname^"): identical source and target " ^ source'));
  Util.convertUnixErrorsToTransient ("renaming " ^ source' ^ " to " ^ target')
    (fun () ->
      debug (fun() -> Util.msg "rename %s to %s\n" source' target');
      Fs.rename source target;
      if Prefs.read Osx.rsrc then begin
        let sourceDouble = Fspath.appleDouble source in
        let targetDouble = Fspath.appleDouble target in
        if Fs.file_exists sourceDouble then
          Fs.rename sourceDouble targetDouble
        else if Fs.file_exists targetDouble then
          Fs.unlink targetDouble
      end)

let symlink =
  if Fs.hasSymlink () then
    fun fspath path l ->
      Util.convertUnixErrorsToTransient
      "writing symbolic link"
      (fun () ->
         let abspath = Fspath.concat fspath path in
         Fs.symlink l abspath)
  else
    fun fspath path l ->
      raise (Util.Transient
               (Format.sprintf
                  "Cannot create symlink \"%s\": \
                   symlinks are not supported on this system%s"
                  (Fspath.toPrintString (Fspath.concat fspath path))
                  (if Sys.win32 || Sys.cygwin then
                     " or elevated privileges may be required"
                  else "")
               ))

(* Create a new directory, using the permissions from the given props        *)
let createDir fspath path perms =
  Util.convertUnixErrorsToTransient
  "creating directory"
    (fun () ->
       let absolutePath = Fspath.concat fspath path in
       Fs.mkdir absolutePath perms)

(*****************************************************************************)
(*                              FINGERPRINTS                                 *)
(*****************************************************************************)

type fullfingerprint = Fingerprint.t * Fingerprint.t

let mfullfingerprint = Umarshal.(prod2 Fingerprint.m Fingerprint.m id id)

let fingerprint fspath path typ =
  (Fingerprint.file fspath path,
   Osx.ressFingerprint fspath path typ)

let pseudoFingerprint path size =
  (Fingerprint.pseudo path size, Fingerprint.dummy)

let isPseudoFingerprint (fp,rfp) =
  Fingerprint.ispseudo fp

(* FIX: not completely safe under Unix                                       *)
(* (with networked file system such as NFS)                                  *)
let safeFingerprint fspath path info optFp =
    let rec retryLoop count info optFp optRessFp =
      if count = 0 then
        raise (Util.Transient
                 (Printf.sprintf
                    "Failed to fingerprint file \"%s\": \
                     the file keeps on changing"
                    (Fspath.toPrintString (Fspath.concat fspath path))))
      else
        let fp =
          match optFp with
            None     -> Fingerprint.file fspath path
          | Some fp -> fp
        in
        let ressFp =
          match optRessFp with
            None      -> Osx.ressFingerprint fspath path info.Fileinfo.typ
          | Some ress -> ress
        in
        let (info', dataUnchanged, ressUnchanged) =
          Fileinfo.unchanged fspath path info in
        if dataUnchanged && ressUnchanged then
          (info', (fp, ressFp))
        else
          retryLoop (count - 1) info'
            (if dataUnchanged then Some fp else None)
            (if ressUnchanged then Some ressFp else None)
    in
    retryLoop 10 info (* Maximum retries: 10 times *)
      (match optFp with None -> None | Some (d, _) -> Some d)
      None

let fullfingerprint_to_string (fp,rfp) =
  Printf.sprintf "(%s,%s)" (Fingerprint.toString fp) (Fingerprint.toString rfp)

let reasonForFingerprintMismatch (fpdata,fpress) (fpdata',fpress') =
  if fpdata = fpdata' then "resource fork"
  else if fpress = fpress' then "file contents"
  else "both file contents and resource fork"

let fullfingerprint_dummy = (Fingerprint.dummy,Fingerprint.dummy)

let fullfingerprintHash (fp, rfp) =
  Fingerprint.hash fp + 31 * Fingerprint.hash rfp

let fullfingerprintEqual (fp, rfp) (fp', rfp') =
  Fingerprint.equal fp fp' && Fingerprint.equal rfp rfp'


(*****************************************************************************)
(*                           UNISON DIRECTORY                                *)
(*****************************************************************************)

(* Make sure archive directory exists                                        *)
let createUnisonDir() =
  try ignore (System.stat Util.unisonDir)
  with Unix.Unix_error(_) ->
    Util.convertUnixErrorsToFatal
      (Printf.sprintf "creating unison directory %s"
         Util.unisonDir)
      (fun () ->
         ignore (System.mkdir Util.unisonDir 0o700))

(*****************************************************************************)
(*                           TEMPORARY FILES                                 *)
(*****************************************************************************)

(* Truncate a filename to at most [l] bytes, making sure of not
   truncating an UTF-8 character.  Assumption: [String.length s > l] *)
let rec truncate_filename s l =
  if l > 0 && Char.code s.[l] land 0xC0 = 0x80 then
    truncate_filename s (l - 1)
  else
    String.sub s 0 l

(* We need to be careful not to use longer temp-file names than the
   file system permits.  eCryptfs has the lowest file name length
   limit we know of, at 143 bytes. *)
let maxFileNameLength = 143

(* Generates an unused fspath for a temporary file.                          *)
let genTempPath fresh fspath path prefix suffix =
  let rec f i =
    let s =
      if i=0 then suffix
      else Printf.sprintf "..%03d.%s" i suffix in
    let tempPath =
      match Path.deconstructRev path with
        None ->
          assert false
      | Some (name, parentPath) ->
          let name = Name.toString name in
          let nameLen = String.length name in
          let prefixLen = String.length prefix in
          let suffixLen = String.length s in
          let maxLen = maxFileNameLength - prefixLen - suffixLen in
          let name =
            if nameLen <= maxLen then name else
              let nameDigest = Digest.to_hex (Digest.string name) in
              let nameDigestLen = String.length nameDigest in
              let maxLen = maxLen - nameDigestLen in
              assert (maxLen>0);
              (truncate_filename name maxLen ^ nameDigest)
          in
          Path.child parentPath (Name.fromString (prefix ^ name ^ s))
    in
    if fresh && exists fspath tempPath then f (i + 1) else tempPath
  in f 0

let tempPath ?(fresh=true) fspath path =
  genTempPath fresh fspath path tempFilePrefix !tempFileSuffix
