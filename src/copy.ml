(* Unison file synchronizer: src/copy.ml *)
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

let (>>=) = Lwt.bind

let debug = Trace.debug "copy"

(****)

let protect f g =
  try
    f ()
  with e ->
    begin try g () with Sys_error _  | Unix.Unix_error _ -> () end;
    raise e

let lwt_protect f g =
  Lwt.catch f
    (fun e ->
       begin try g () with Sys_error _  | Unix.Unix_error _ -> () end;
       Lwt.fail e)

(****)

(* If newFpOpt = Some newfp, check that the current source contents
   matches newfp.  Otherwise, check whether the source file has been
   modified during synchronization. *)
let checkForChangesToSourceLocal
      fspathFrom pathFrom archDesc archFp archStamp archRess newFpOpt paranoid =
  (* Retrieve attributes of current source file *)
  let sourceInfo = Fileinfo.getBasicWithRess true fspathFrom pathFrom in
  let sourceType = sourceInfo.Fileinfo.typ in
  match newFpOpt with
    None ->
      (* no newfp provided: so we need to compare the archive with the
         current source *)
      let clearlyChanged =
           sourceType <> `FILE
        || Props.length sourceInfo.Fileinfo.desc <> Props.length archDesc
        || Osx.ressLength sourceInfo.Fileinfo.osX.Osx.ressInfo <>
           Osx.ressLength archRess    in
      let dataClearlyUnchanged =
           not clearlyChanged
        && Props.same_time sourceInfo.Fileinfo.desc archDesc
        && not (Fpcache.excelFile pathFrom)
        && match archStamp with
             Some (Fileinfo.InodeStamp inode) -> sourceInfo.Fileinfo.inode = inode
           | Some (Fileinfo.NoStamp)          -> true
           | Some (Fileinfo.RescanStamp)      -> false
           | None                             -> false   in
      let ressClearlyUnchanged =
           not clearlyChanged
        && Osx.ressUnchanged archRess sourceInfo.Fileinfo.osX.Osx.ressInfo
                             None dataClearlyUnchanged   in
      if dataClearlyUnchanged && ressClearlyUnchanged then begin
        if paranoid && not (Os.isPseudoFingerprint archFp) then begin
          let newFp = Os.fingerprint fspathFrom pathFrom sourceType in
          if archFp <> newFp then begin
            Update.markPossiblyUpdated fspathFrom pathFrom;
            raise (Util.Transient (Printf.sprintf
              "The source file %s\n\
               has been modified but the fast update detection mechanism\n\
               failed to detect it.  Try running once with the fastcheck\n\
               option set to 'no'."
              (Fspath.toPrintString (Fspath.concat fspathFrom pathFrom))))
          end
        end
      end else if
           clearlyChanged
        || archFp <> Os.fingerprint fspathFrom pathFrom sourceType
      then
        raise (Util.Transient (Printf.sprintf
          "The source file %s\nhas been modified during synchronization.  \
           Transfer aborted."
          (Fspath.toPrintString (Fspath.concat fspathFrom pathFrom))))
  | Some newfp ->
      (* newfp provided means that the archive contains a pseudo-fingerprint... *)
      assert (Os.isPseudoFingerprint archFp);
      (* ... so we can't compare the archive with the source; instead we
         need to compare the current source to the new fingerprint: *)
      if newfp <> Os.fingerprint fspathFrom pathFrom sourceType then
        raise (Util.Transient (Printf.sprintf
          "Current source file %s\n not same as transferred file.  \
           Transfer aborted."
          (Fspath.toPrintString (Fspath.concat fspathFrom pathFrom))))

let mcheckForChangesToSource =
  Umarshal.(prod2
              (prod4 Path.mlocal Props.m Os.mfullfingerprint (option Fileinfo.mstamp) id id)
              (prod3 Osx.mressStamp (option Os.mfullfingerprint) bool id id)
              id id)

let archStamp_to_compat251 = function
  | Some stamp -> Some (Fileinfo.stamp_to_compat251 stamp)
  | None -> None

let archStamp_of_compat251 = function
  | Some stamp -> Some (Fileinfo.stamp_of_compat251 stamp)
  | None -> None

let convV0 = Remote.makeConvV0FunArg
  (fun (fspathFrom,
          ((pathFrom, archDesc, archFp, archStamp), (archRess, newFpOpt, paranoid))) ->
       (fspathFrom,
          (pathFrom, Props.to_compat251 archDesc, archFp,
            archStamp_to_compat251 archStamp, archRess, newFpOpt, paranoid)))
  (fun (fspathFrom,
          (pathFrom, archDesc, archFp, archStamp, archRess, newFpOpt, paranoid)) ->
       (fspathFrom,
          ((pathFrom, Props.of_compat251 archDesc, archFp,
            archStamp_of_compat251 archStamp), (archRess, newFpOpt, paranoid))))

let checkForChangesToSourceOnRoot =
  Remote.registerRootCmd
    "checkForChangesToSource" ~convV0
    mcheckForChangesToSource Umarshal.unit
    (fun (fspathFrom,
          ((pathFrom, archDesc, archFp, archStamp), (archRess, newFpOpt, paranoid))) ->
      checkForChangesToSourceLocal
        fspathFrom pathFrom archDesc archFp archStamp archRess newFpOpt paranoid;
      Lwt.return ())

let checkForChangesToSource
      root pathFrom archDesc archFp archStamp archRess newFpOpt paranoid =
  checkForChangesToSourceOnRoot
    root ((pathFrom, archDesc, archFp, archStamp), (archRess, newFpOpt, paranoid))

(****)

let fileIsTransferred fspathTo pathTo desc fp ress =
  let info = Fileinfo.getBasicWithRess false fspathTo pathTo in
  (Fileinfo.basic info,
   info.Fileinfo.typ = `FILE
     &&
   Props.length info.Fileinfo.desc = Props.length desc
     &&
   Osx.ressLength info.Fileinfo.osX.Osx.ressInfo =
   Osx.ressLength ress
     &&
   let fp' = Os.fingerprint fspathTo pathTo info.Fileinfo.typ in
   fp' = fp)

(* We slice the files in 1GB chunks because that's the limit for
   Fingerprint.subfile on 32 bit architectures *)
let fingerprintLimit = Uutil.Filesize.ofInt64 1072693248L

let rec fingerprintPrefix fspath path offset len accu =
  if len = Uutil.Filesize.zero then accu else begin
    let l = min len fingerprintLimit in
    let fp = Fingerprint.subfile (Fspath.concat fspath path) offset l in
    fingerprintPrefix fspath path
      (Int64.add offset (Uutil.Filesize.toInt64 l)) (Uutil.Filesize.sub len l)
      (fp :: accu)
  end

let fingerprintPrefixRemotely =
  Remote.registerServerCmd
    "fingerprintSubfile"
    Umarshal.(prod3 Fspath.m Path.mlocal Uutil.Filesize.m id id)
    Umarshal.(list Fingerprint.m)
    (fun _ (fspath, path, len) ->
       Lwt.return (fingerprintPrefix fspath path 0L len []))

let appendThreshold = Uutil.Filesize.ofInt (1024 * 1024)

let validFilePrefix connFrom fspathFrom pathFrom fspathTo pathTo info desc =
  let len = Props.length info.Fileinfo.desc in
  if
    info.Fileinfo.typ = `FILE &&
    len >= appendThreshold && len < Props.length desc
  then begin
    Lwt.try_bind
      (fun () ->
         fingerprintPrefixRemotely connFrom (fspathFrom, pathFrom, len))
      (fun fpFrom ->
         let fpTo = fingerprintPrefix fspathTo pathTo 0L len [] in
         Lwt.return (if fpFrom = fpTo then Some len else None))
      (fun _ ->
         Lwt.return None)
  end else
    Lwt.return None

(* IMPORTANT!
   This is the 2.51-compatible version of type [transferStatus]. It must always
   remain exactly the same as the type [transferStatus] in version 2.51.5. This
   means that if any of the types it is composed of changes then for each
   changed type also a 2.51-compatible version must be created. *)
type transferStatus251 =
    TransferSucceeded of Fileinfo.t251
  | TransferNeedsDoubleCheckAgainstCurrentSource of Fileinfo.t251 * Os.fullfingerprint
  | TransferFailed of string

type transferStatus =
    TransferSucceeded of Fileinfo.basic
  | TransferNeedsDoubleCheckAgainstCurrentSource of Fileinfo.basic * Os.fullfingerprint
  | TransferFailed of string

let mtransferStatus = Umarshal.(sum3
                                  Fileinfo.mbasic
                                  (prod2 Fileinfo.mbasic Os.mfullfingerprint id id)
                                  string
                                  (function
                                   | TransferSucceeded a -> I31 a
                                   | TransferNeedsDoubleCheckAgainstCurrentSource (a, b) -> I32 (a, b)
                                   | TransferFailed a -> I33 a)
                                  (function
                                   | I31 a -> TransferSucceeded a
                                   | I32 (a, b) -> TransferNeedsDoubleCheckAgainstCurrentSource (a, b)
                                   | I33 a -> TransferFailed a))

let transferStatus_to_compat251 (st : transferStatus) : transferStatus251 =
  match st with
  | TransferSucceeded info -> TransferSucceeded (Fileinfo.to_compat251 info)
  | TransferNeedsDoubleCheckAgainstCurrentSource (info, fp) ->
      TransferNeedsDoubleCheckAgainstCurrentSource (Fileinfo.to_compat251 info, fp)
  | TransferFailed s -> TransferFailed s

let transferStatus_of_compat251 (st : transferStatus251) : transferStatus =
  match st with
  | TransferSucceeded info -> TransferSucceeded (Fileinfo.of_compat251 info)
  | TransferNeedsDoubleCheckAgainstCurrentSource (info, fp) ->
      TransferNeedsDoubleCheckAgainstCurrentSource (Fileinfo.of_compat251 info, fp)
  | TransferFailed s -> TransferFailed s

(* Paranoid check: recompute the transferred file's fingerprint to match it
   with the archive's.  If the old
   fingerprint was a pseudo-fingerprint, we can't tell just from looking at the
   new file and the archive information, so we return
   TransferProbablySucceeded in this case, along with the new fingerprint
   that we can check in checkForChangesToSource when we've
   calculated the current source fingerprint.
 *)
let paranoidCheck fspathTo pathTo realPathTo desc fp ress =
  let info = Fileinfo.getBasic false fspathTo pathTo in
  let fp' = Os.fingerprint fspathTo pathTo info.Fileinfo.typ in
  if Os.isPseudoFingerprint fp then begin
    Lwt.return (TransferNeedsDoubleCheckAgainstCurrentSource (info,fp'))
  end else if fp' <> fp then begin
    debug (fun() -> Util.msg "Fingerprints differ: %s vs %s\n"
      (Os.fullfingerprint_to_string fp)
      (Os.fullfingerprint_to_string fp'));
    Lwt.return (TransferFailed (Os.reasonForFingerprintMismatch fp fp'))
  end else
    Lwt.return (TransferSucceeded info)

let saveTempFileLocal (fspathTo, (pathTo, realPathTo, reason)) =
  debug (fun() -> Util.msg "Failed (%s): Saving old temp file %s\n"
         reason (Path.toString pathTo));
  let savepath =
    Os.tempPath ~fresh:true fspathTo
      (match Path.deconstructRev realPathTo with
         Some (nm, _) -> Path.addSuffixToFinalName
                           (Path.child Path.empty nm) "-bad"
       | None         -> Path.fromString "bad")
  in
  (* BCP: 12/17: Added a try around this call so that, if we're in the middle of failing
     when we do this, we don't fail again and confuse the user about the reason for the
     failure! *)
  begin try Os.rename "save temp" fspathTo pathTo fspathTo savepath with Util.Transient _ -> () end;
  Lwt.fail
    (Util.Transient
       (Printf.sprintf
        "The file %s was incorrectly transferred  (fingerprint mismatch in %s) \
         -- temp file saved as %s"
        (Path.toString pathTo)
        reason
        (Fspath.toDebugString (Fspath.concat fspathTo savepath))))

let saveTempFileOnRoot =
  Remote.registerRootCmd "saveTempFile"
    Umarshal.(prod3 Path.mlocal Path.mlocal string id id) Umarshal.unit
    saveTempFileLocal

(****)

let removeOldTempFile fspathTo pathTo =
  if Os.exists fspathTo pathTo then begin
    debug (fun() -> Util.msg "Removing old %s / %s\n"
           (Fspath.toDebugString fspathTo) (Path.toString pathTo));
    Os.delete fspathTo pathTo
  end

let openFileIn fspath path kind =
  match kind with
    `DATA ->
      Fs.open_in_bin (Fspath.concat fspath path)
  | `DATA_APPEND len ->
      let ch = Fs.open_in_bin (Fspath.concat fspath path) in
      LargeFile.seek_in ch (Uutil.Filesize.toInt64 len);
      ch
  | `RESS ->
      Osx.openRessIn fspath path

let openFileOut fspath path kind len =
  match kind with
    `DATA ->
      let fullpath = Fspath.concat fspath path in
      let flags = [Unix.O_WRONLY;Unix.O_CREAT] in
      let perm = 0o600 in
      begin match Util.osType with
        `Win32 ->
          Fs.open_out_gen
            [Open_wronly; Open_creat; Open_excl; Open_binary] perm fullpath
      | `Unix ->
          let fd =
            try
              Fs.openfile fullpath (Unix.O_EXCL :: flags) perm
            with
              Unix.Unix_error
                ((Unix.EOPNOTSUPP | Unix.EUNKNOWNERR 524), _, _) ->
              (* O_EXCL not supported under a Netware NFS-mounted filesystem.
                 Solaris and Linux report different errors. *)
                Fs.openfile fullpath (Unix.O_TRUNC :: flags) perm
          in
          Unix.out_channel_of_descr fd
      end
  | `DATA_APPEND len ->
      let fullpath = Fspath.concat fspath path in
      let perm = 0o600 in
      let ch = Fs.open_out_gen [Open_wronly; Open_binary] perm fullpath in
      if not (Prefs.read Props.dontChmod) then Fs.chmod fullpath perm;
      LargeFile.seek_out ch (Uutil.Filesize.toInt64 len);
      ch
  | `RESS ->
      Osx.openRessOut fspath path len

let setFileinfo fspathTo pathTo realPathTo update desc =
  match update with
    `Update _ -> Fileinfo.set fspathTo pathTo (`Copy realPathTo) desc
  | `Copy     -> Fileinfo.set fspathTo pathTo (`Set Props.fileDefault) desc

(****)

let copyContents fspathFrom pathFrom fspathTo pathTo fileKind fileLength ido =
  let use_id f = match ido with Some id -> f id | None -> () in
  let inFd = openFileIn fspathFrom pathFrom fileKind in
  protect
    (fun () ->
       let outFd = openFileOut fspathTo pathTo fileKind fileLength in
       protect
         (fun () ->
            Uutil.readWriteBounded inFd outFd fileLength
              (fun l ->
                 use_id (fun id ->
(* (Util.msg "Copied file %s (%d bytes)\n" (Path.toString pathFrom) l); *)
                   Uutil.showProgress id (Uutil.Filesize.ofInt l) "l"));
            close_in inFd;
            close_out outFd;
(* ignore (Sys.command ("ls -l " ^ (Fspath.toString (Fspath.concat fspathTo pathTo)))) *)
         )
         (fun () -> close_out_noerr outFd))
    (fun () -> close_in_noerr inFd)

let localFile
     fspathFrom pathFrom fspathTo pathTo realPathTo update desc ressLength ido =
  Util.convertUnixErrorsToTransient
    "copying locally"
    (fun () ->
      debug (fun () ->
        Util.msg "Copy.localFile %s / %s to %s / %s\n"
          (Fspath.toDebugString fspathFrom) (Path.toString pathFrom)
          (Fspath.toDebugString fspathTo) (Path.toString pathTo));
      removeOldTempFile fspathTo pathTo;
      copyContents
        fspathFrom pathFrom fspathTo pathTo `DATA (Props.length desc) ido;
      if ressLength > Uutil.Filesize.zero then
        copyContents
          fspathFrom pathFrom fspathTo pathTo `RESS ressLength ido;
      setFileinfo fspathTo pathTo realPathTo update desc)

(****)

let tryCopyMovedFile fspathTo pathTo realPathTo update desc fp ress id =
  if not (Prefs.read Xferhint.xferbycopying) then None else
  Util.convertUnixErrorsToTransient "tryCopyMovedFile" (fun() ->
    debug (fun () -> Util.msg "tryCopyMovedFile: -> %s /%s/\n"
      (Path.toString pathTo) (Os.fullfingerprint_to_string fp));
    match Xferhint.lookup fp with
      None ->
        None
    | Some (candidateFspath, candidatePath, hintHandle) ->
        debug (fun () ->
          Util.msg
            "tryCopyMovedFile: found match at %s,%s. Try local copying\n"
            (Fspath.toDebugString candidateFspath)
            (Path.toString candidatePath));
        try
          (* If candidateFspath is the replica root, the argument
             [true] is correct.  Otherwise, we don't expect to point
             to a symlink, and therefore we still get the correct
             result. *)
          let info = Fileinfo.getBasic true candidateFspath candidatePath in
          if
            info.Fileinfo.typ <> `ABSENT &&
            Props.length info.Fileinfo.desc = Props.length desc
          then begin
            localFile
              candidateFspath candidatePath fspathTo pathTo realPathTo
              update desc (Osx.ressLength ress) (Some id);
            let (info, isTransferred) =
              fileIsTransferred fspathTo pathTo desc fp ress in
            if isTransferred then begin
              debug (fun () -> Util.msg "tryCopyMoveFile: success.\n");
              let msg =
                Printf.sprintf
                 "Shortcut: copied %s/%s from local file %s/%s\n"
                 (Fspath.toPrintString fspathTo)
                 (Path.toString realPathTo)
                 (Fspath.toPrintString candidateFspath)
                 (Path.toString candidatePath)
              in
              Some (info, msg)
            end else begin
              debug (fun () ->
                Util.msg "tryCopyMoveFile: candidate file %s modified!\n"
                  (Path.toString candidatePath));
              Xferhint.deleteEntry hintHandle;
              None
            end
          end else begin
            debug (fun () ->
              Util.msg "tryCopyMoveFile: candidate file %s disappeared!\n"
                (Path.toString candidatePath));
            Xferhint.deleteEntry hintHandle;
            None
          end
        with
          Util.Transient s ->
            debug (fun () ->
              Util.msg
                "tryCopyMovedFile: local copy from %s didn't work [%s]"
                (Path.toString candidatePath) s);
            Xferhint.deleteEntry hintHandle;
            None)

(****)

(* The file transfer functions here depend on an external module
   'transfer' that implements a generic transmission and the rsync
   algorithm for optimizing the file transfer in the case where a
   similar file already exists on the target. *)

let rsyncActivated =
  Prefs.createBool "rsync" true
    ~category:(`Advanced `Remote)
    "activate the rsync transfer mode"
    ("Unison uses the 'rsync algorithm' for 'diffs-only' transfer "
     ^ "of updates to large files.  Setting this flag to false makes Unison "
     ^ "use whole-file transfers instead.  Under normal circumstances, "
     ^ "there is no reason to do this, but if you are having trouble with "
     ^ "repeated 'rsync failure' errors, setting it to "
     ^ "false should permit you to synchronize the offending files.")

let decompressor = ref Remote.MsgIdMap.empty

let processTransferInstruction conn (file_id, ti) =
  Util.convertUnixErrorsToTransient
    "processing a transfer instruction"
    (fun () ->
       ignore ((fst (Remote.MsgIdMap.find file_id !decompressor)) ti))

let marshalTransferInstruction =
  (fun _ (file_id, (data, pos, len)) rem ->
     (Remote.encodeInt file_id :: (data, pos, len) :: rem,
      len + Remote.intSize)),
  (fun _ buf pos ->
     let len = Bytearray.length buf - pos - Remote.intSize in
     (Remote.decodeInt buf pos, (buf, pos + Remote.intSize, len)))

let streamTransferInstruction =
  Remote.registerStreamCmd
    "processTransferInstruction" marshalTransferInstruction
    processTransferInstruction

let showPrefixProgress id kind =
  match kind with
    `DATA_APPEND len -> Uutil.showProgress id len "r"
  | _                -> ()

(* There is an issue that not all threads are immediately cancelled when there
   is a connection error. A waiting thread (in this case probably a thread
   in the Lwt region for RPC streaming) may get started and open the infd but
   never complete. lwt_protect is never triggered in this scenario. Not sure
   why exactly but probably because the thread just stops (as eventually the
   connection cleanup kicks in and all threads are stopped). As a hacky
   solution, keep track of all open fds and close them when the connection
   breaks. *)
let compressorFds = Hashtbl.create 17
let closeCompressors () =
  Hashtbl.iter (fun fd _ -> close_in_noerr fd) compressorFds;
  Hashtbl.clear compressorFds
let () = Remote.at_conn_close closeCompressors

let compress conn
     ((biOpt, fspathFrom, pathFrom, fileKind), (sizeFrom, id, file_id)) =
  Lwt.catch
    (fun () ->
       streamTransferInstruction conn
         (fun processTransferInstructionRemotely ->
            (* We abort the file transfer on error if it has not
               already started *)
            if fileKind <> `RESS then Abort.check id;
            let infd = openFileIn fspathFrom pathFrom fileKind in
            Hashtbl.add compressorFds infd true;
            lwt_protect
              (fun () ->
                 showPrefixProgress id fileKind;
                 let showProgress count =
                   Uutil.showProgress id (Uutil.Filesize.ofInt count) "r" in
                 let compr =
                   match biOpt with
                     None ->
                       Transfer.send infd sizeFrom showProgress
                   | Some bi ->
                       Transfer.Rsync.rsyncCompress
                         bi infd sizeFrom showProgress
                 in
                 compr
                   (fun ti -> processTransferInstructionRemotely (file_id, ti))
                       >>= fun () ->
                 Hashtbl.remove compressorFds infd;
                 close_in infd;
                 Lwt.return ())
              (fun () ->
                 Hashtbl.remove compressorFds infd; close_in_noerr infd)))
    (fun e ->
       (* We cannot wrap the code above with the handler below,
          as the code is executed asynchronously. *)
       Util.convertUnixErrorsToTransient "transferring file contents"
         (fun () -> raise e))

let mdata = Umarshal.(sum3 unit Uutil.Filesize.m unit
                        (function
                         | `DATA -> I31 ()
                         | `DATA_APPEND a -> I32 a
                         | `RESS -> I33 ())
                        (function
                         | I31 () -> `DATA
                         | I32 a -> `DATA_APPEND a
                         | I33 () -> `RESS))

let mcompress = Umarshal.(prod2
                            (prod4 (option Transfer.Rsync.mrsync_block_info) Fspath.m Path.mlocal mdata id id)
                            (prod3 Uutil.Filesize.m Uutil.File.m int id id)
                            id id)

let convV0 = Remote.makeConvV0FunArg
  (fun ((biOpt, fspathFrom, pathFrom, fileKind), (sizeFrom, id, file_id)) ->
       (biOpt, fspathFrom, pathFrom, fileKind, sizeFrom, id, file_id))
  (fun (biOpt, fspathFrom, pathFrom, fileKind, sizeFrom, id, file_id) ->
       ((biOpt, fspathFrom, pathFrom, fileKind), (sizeFrom, id, file_id)))

let compressRemotely =
  Remote.registerServerCmd "compress" ~convV0 mcompress Umarshal.unit compress

let close_all infd outfd =
  Util.convertUnixErrorsToTransient
    "closing files"
    (fun () ->
       begin match !infd with
         Some fd -> close_in fd; infd := None
       | None    -> ()
       end;
       begin match !outfd with
         Some fd -> close_out fd; outfd := None
       | None    -> ()
       end)

let close_all_no_error infd outfd =
  begin match !infd with
    Some fd -> close_in_noerr fd
  | None    -> ()
  end;
  begin match !outfd with
    Some fd -> close_out_noerr fd
  | None    -> ()
  end

(* Lazy creation of the destination file *)
let destinationFd fspath path kind len outfd id =
  match !outfd with
    None    ->
      (* We abort the file transfer on error if it has not
         already started *)
      if kind <> `RESS then Abort.check id;
      let fd = openFileOut fspath path kind len in
      showPrefixProgress id kind;
      outfd := Some fd;
      fd
  | Some fd ->
      fd

(* Lazy opening of the reference file (for rsync algorithm) *)
let referenceFd fspath path kind infd =
  match !infd with
    None ->
      let fd = openFileIn fspath path kind in
      infd := Some fd;
      fd
  | Some fd ->
      fd

let rsyncReg = Lwt_util.make_region (40 * 1024)
let rsyncThrottle useRsync srcFileSize destFileSize f =
  if not useRsync then f () else
  let l = Transfer.Rsync.memoryFootprint srcFileSize destFileSize in
  Lwt_util.run_in_region rsyncReg l f

let transferFileContents
      connFrom fspathFrom pathFrom fspathTo pathTo realPathTo update
      fileKind srcFileSize id =
  (* We delay the opening of the files so that there are not too many
     temporary files remaining after a crash, and that they are not
     too many files simultaneously opened. *)
  let outfd = ref None in
  let infd = ref None in
  let showProgress count =
    Uutil.showProgress id (Uutil.Filesize.ofInt count) "r" in

  let destFileSize =
    match update with
      `Copy ->
        Uutil.Filesize.zero
    | `Update (destFileDataSize, destFileRessSize) ->
        match fileKind with
            `DATA | `DATA_APPEND _ -> destFileDataSize
          | `RESS -> destFileRessSize
  in
  let useRsync =
    Prefs.read rsyncActivated
      &&
    Transfer.Rsync.aboveRsyncThreshold destFileSize
      &&
    Transfer.Rsync.aboveRsyncThreshold srcFileSize
  in
  rsyncThrottle useRsync srcFileSize destFileSize (fun () ->
    let (bi, decompr) =
      if useRsync then
        Util.convertUnixErrorsToTransient
          "preprocessing file"
          (fun () ->
             let ifd = referenceFd fspathTo realPathTo fileKind infd in
             let (bi, blockSize) =
               protect
                 (fun () -> Transfer.Rsync.rsyncPreprocess
                              ifd srcFileSize destFileSize)
                 (fun () -> close_in_noerr ifd)
             in
             close_all infd outfd;
             (Some bi,
              (* Rsync decompressor *)
              fun ti ->
              let ifd = referenceFd fspathTo realPathTo fileKind infd in
              let fd =
                destinationFd
                  fspathTo pathTo fileKind srcFileSize outfd id in
              let eof =
                Transfer.Rsync.rsyncDecompress blockSize ifd fd showProgress ti
              in
              if eof then close_all infd outfd))
      else
        (None,
         (* Simple generic decompressor *)
         fun ti ->
         let fd =
           destinationFd fspathTo pathTo fileKind srcFileSize outfd id in
         let eof = Transfer.receive fd showProgress ti in
         if eof then close_all infd outfd)
    in
    let file_id = Remote.newMsgId () in
    Lwt.catch
      (fun () ->
         debug (fun () -> Util.msg "Starting the actual transfer\n");
         decompressor := Remote.MsgIdMap.add file_id (decompr, (infd, outfd)) !decompressor;
         compressRemotely connFrom
           ((bi, fspathFrom, pathFrom, fileKind), (srcFileSize, id, file_id))
           >>= fun () ->
         decompressor :=
           Remote.MsgIdMap.remove file_id !decompressor; (* For GC *)
         close_all infd outfd;
           (* JV: FIX: the file descriptors are already closed... *)
         Lwt.return ())
      (fun e ->
         decompressor :=
           Remote.MsgIdMap.remove file_id !decompressor; (* For GC *)
         close_all_no_error infd outfd;
         Lwt.fail e))

let closeDecompressors () =
  let tmp = !decompressor in
  decompressor := Remote.MsgIdMap.empty;
  tmp |> Remote.MsgIdMap.iter (fun _ (_, (infd, outfd)) ->
    close_all_no_error infd outfd)
let () = Remote.at_conn_close closeDecompressors

(****)

let transferResourceForkAndSetFileinfo
      connFrom fspathFrom pathFrom fspathTo pathTo realPathTo
      update desc fp ress id =
  (* Resource fork *)
  debug (fun() -> Util.msg "transferResourceForkAndSetFileinfo %s\n"
    (Path.toString pathTo));
  let ressLength = Osx.ressLength ress in
  begin if ressLength > Uutil.Filesize.zero then begin
    debug (fun() -> Util.msg "starting resource fork transfer for %s\n"
      (Path.toString pathTo));
    transferFileContents
      connFrom fspathFrom pathFrom fspathTo pathTo realPathTo update
      `RESS ressLength id
  end else
    Lwt.return ()
  end >>= fun () ->
  setFileinfo fspathTo pathTo realPathTo update desc;
  debug (fun() -> Util.msg "Resource fork transferred for %s; doing last paranoid check\n"
    (Path.toString realPathTo));
  paranoidCheck fspathTo pathTo realPathTo desc fp ress

let reallyTransferFile
      connFrom fspathFrom pathFrom fspathTo pathTo realPathTo
      update desc fp ress id tempInfo =
  debug (fun() -> Util.msg "reallyTransferFile(%s,%s) -> (%s,%s,%s,%s)\n"
      (Fspath.toDebugString fspathFrom) (Path.toString pathFrom)
      (Fspath.toDebugString fspathTo) (Path.toString pathTo)
      (Path.toString realPathTo) (Props.toString desc));
  validFilePrefix connFrom fspathFrom pathFrom fspathTo pathTo tempInfo desc
    >>= fun prefixLen ->
  begin match prefixLen with
    None ->
      removeOldTempFile fspathTo pathTo
  | Some len ->
      debug
        (fun() ->
           Util.msg "Keeping %s bytes previously transferred for file %s\n"
             (Uutil.Filesize.toString len) (Path.toString pathFrom))
  end;
  (* Data fork *)
  transferFileContents
    connFrom fspathFrom pathFrom fspathTo pathTo realPathTo update
    (match prefixLen with None -> `DATA | Some l -> `DATA_APPEND l)
    (Props.length desc) id >>= fun () ->
  transferResourceForkAndSetFileinfo
    connFrom fspathFrom pathFrom fspathTo pathTo realPathTo
    update desc fp ress id

(****)

let filesBeingTransferred = Hashtbl.create 17

let resetFileTransferState () =
  (* The waiting threads should be collected by GC *)
  Hashtbl.clear filesBeingTransferred
let () = Remote.at_conn_close resetFileTransferState

let wakeupNextTransfer fp =
  match
    try
      Some (Queue.take (Hashtbl.find filesBeingTransferred fp))
    with Queue.Empty ->
      None
  with
    None ->
      Hashtbl.remove filesBeingTransferred fp
  | Some next ->
      Lwt.wakeup next ()

let executeTransfer fp f =
  Lwt.try_bind f
    (fun res -> wakeupNextTransfer fp; Lwt.return res)
    (fun e -> wakeupNextTransfer fp; Lwt.fail e)

(* Keep track of which file contents are being transferred, and delay
   the transfer of a file with the same contents as another file being
   currently transferred.  This way, the second transfer can be
   skipped and replaced by a local copy. *)
let rec registerFileTransfer pathTo fp f =
  if not (Prefs.read Xferhint.xferbycopying) then f () else
  match
    try Some (Hashtbl.find filesBeingTransferred fp) with Not_found -> None
  with
    None ->
      let q = Queue.create () in
      Hashtbl.add filesBeingTransferred fp q;
      executeTransfer fp f
  | Some q ->
      debug (fun () -> Util.msg "delaying transfer of file %s\n"
               (Path.toString pathTo));
      let res = Lwt.wait () in
      Queue.push res q;
      res >>= fun () ->
      executeTransfer fp f

(****)

let copyprog =
  Prefs.createString "copyprog" "rsync --partial --inplace --compress"
    ~category:(`Advanced `General)
    "external program for copying large files"
    ("A string giving the name of an "
     ^ "external program that can be used to copy large files efficiently  "
     ^ "(plus command-line switches telling it to copy files in-place).  "
     ^ "The default setting invokes {\\tt rsync} with appropriate "
     ^ "options---most users should not need to change it.")

let copyprogrest =
  Prefs.createString
    "copyprogrest" "rsync --partial --append-verify --compress"
    ~category:(`Advanced `General)
    "variant of copyprog for resuming partial transfers"
    ("A variant of {\\tt copyprog} that names an external program "
     ^ "that should be used to continue the transfer of a large file "
     ^ "that has already been partially transferred.  Typically, "
     ^ "{\\tt copyprogrest} will just be {\\tt copyprog} "
     ^ "with one extra option (e.g., {\\tt --partial}, for rsync).  "
     ^ "The default setting invokes {\\tt rsync} with appropriate "
     ^ "options---most users should not need to change it.")

let copythreshold =
  Prefs.createInt "copythreshold" (-1)
    ~category:(`Advanced `General)
    "use copyprog on files bigger than this (if >=0, in Kb)"
    ("A number indicating above what filesize (in kilobytes) Unison should "
     ^ "use the external "
     ^ "copying utility specified by {\\tt copyprog}. Specifying 0 will cause "
     ^ "{\\em all} copies to use the external program; "
     ^ "a negative number will prevent any files from using it.  "
     ^ "The default is -1.  "
     ^ "See \\sectionref{speeding}{Making Unison Faster on Large Files} "
     ^ "for more information.")

let copyquoterem =
  Prefs.createBoolWithDefault "copyquoterem"
    ~category:(`Advanced `General)
    "add quotes to remote file name for copyprog (true/false/default)"
    ("When set to {\\tt true}, this flag causes Unison to add an extra layer "
     ^ "of quotes to the remote path passed to the external copy program. "
     ^ "This is needed by rsync, for example, which internally uses an ssh "
     ^ "connection requiring an extra level of quoting for paths containing "
     ^ "spaces. When this flag is set to {\\tt default}, extra quotes are "
     ^ "added if the value of {\\tt copyprog} contains the string "
     ^ "{\\tt rsync}.")

let copymax =
  Prefs.createInt "copymax" 1
    ~category:(`Advanced `General)
    "maximum number of simultaneous copyprog transfers"
    ("A number indicating how many instances of the external copying utility \
      Unison is allowed to run simultaneously (default to 1).")

let formatConnectionInfo root =
  match root with
    Common.Local, _ -> ""
  | Common.Remote h, _ ->
      (* Find the (unique) nonlocal root *)
      match
         Safelist.find (function Clroot.ConnectLocal _ -> false | _ -> true)
           (Safelist.map Clroot.parseRoot (Globals.rawRoots()))
      with
        Clroot.ConnectByShell (_,rawhost,uo,_,_) ->
            (match uo with None -> "" | Some u -> u ^ "@")
          ^ rawhost ^ ":"
          (* Note that we don't do anything with the port -- hopefully
             this will not affect many people.  If we did want to include it,
             we'd have to fiddle with the rsync parameters in a slightly
             deeper way. *)
      | Clroot.ConnectBySocket (h',_,_) ->
          h ^ ":"
      | Clroot.ConnectLocal _ -> assert false

let shouldUseExternalCopyprog update desc =
     Prefs.read copyprog <> ""
  && Prefs.read copythreshold >= 0
  && Props.length desc >= Uutil.Filesize.ofInt64 (Int64.of_int 1)
  && Props.length desc >=
       Uutil.Filesize.ofInt64
         (Int64.mul (Int64.of_int 1000)
            (Int64.of_int (Prefs.read copythreshold)))
  && update = `Copy

let prepareExternalTransfer fspathTo pathTo =
  let info = Fileinfo.getBasic false fspathTo pathTo in
  match info.Fileinfo.typ with
    `FILE when Props.length info.Fileinfo.desc > Uutil.Filesize.zero ->
      let perms = Props.perms info.Fileinfo.desc in
      let perms' = perms lor 0o600 in
      begin try
        Fs.chmod (Fspath.concat fspathTo pathTo) perms'
      with Unix.Unix_error _ -> () end;
      true
  | `ABSENT ->
      false
  | t ->
      debug (fun() -> Util.msg "Removing existing %s / %s\n"
               (Fspath.toDebugString fspathTo) (Path.toString pathTo));
      Os.delete fspathTo pathTo;
      false

let finishExternalTransferLocal connFrom
      ((fspathFrom, pathFrom, fspathTo, pathTo, realPathTo),
       (update, desc, fp, ress, id)) =
  let info = Fileinfo.getBasic false fspathTo pathTo in
  if
    info.Fileinfo.typ <> `FILE ||
    Props.length info.Fileinfo.desc <> Props.length desc
  then
    raise (Util.Transient (Printf.sprintf
      "External copy program did not create target file (or bad length): %s"
          (Path.toString pathTo)));
  transferResourceForkAndSetFileinfo
    connFrom fspathFrom pathFrom fspathTo pathTo realPathTo
    update desc fp ress id >>= fun res ->
  Xferhint.insertEntry fspathTo pathTo fp;
  Lwt.return res

let convV0 = Remote.makeConvV0Funs
  (fun ((fspathFrom, pathFrom, fspathTo, pathTo, realPathTo),
         (update, desc, fp, ress, id)) ->
       (fspathFrom, pathFrom, fspathTo, pathTo, realPathTo,
         update, Props.to_compat251 desc, fp, ress, id))
  (fun (fspathFrom, pathFrom, fspathTo, pathTo, realPathTo,
         update, desc, fp, ress, id) ->
       ((fspathFrom, pathFrom, fspathTo, pathTo, realPathTo),
         (update, Props.of_compat251 desc, fp, ress, id)))
  transferStatus_to_compat251
  transferStatus_of_compat251

let mcopyOrUpdate = Umarshal.(sum2 unit (prod2 Uutil.Filesize.m Uutil.Filesize.m id id)
                                (function
                                 | `Copy -> I21 ()
                                 | `Update (a, b) -> I22 (a, b))
                                (function
                                 | I21 () -> `Copy
                                 | I22 (a, b) -> `Update (a, b)))

let mfinishExternalTransfer = Umarshal.(prod2
                                          (prod5 Fspath.m Path.mlocal Fspath.m Path.mlocal Path.mlocal id id)
                                          (prod5 mcopyOrUpdate Props.m Os.mfullfingerprint Osx.mressStamp Uutil.File.m id id)
                                          id id)

let finishExternalTransferOnRoot =
  Remote.registerRootCmdWithConnection
    "finishExternalTransfer" ~convV0
    mfinishExternalTransfer mtransferStatus finishExternalTransferLocal

let copyprogReg = Lwt_util.make_region 1

let transferFileUsingExternalCopyprog
             rootFrom pathFrom rootTo fspathTo pathTo realPathTo
             update desc fp ress id useExistingTarget =
  Uutil.showProgress id Uutil.Filesize.zero "ext";
  let prog =
    if useExistingTarget then
      Prefs.read copyprogrest
    else
      Prefs.read copyprog
  in
  let extraquotes = Prefs.read copyquoterem = `True
                 || (  Prefs.read copyquoterem = `Default
                    && Util.findsubstring "rsync" prog <> None) in
  let addquotes root s =
    match root with
    | Common.Local, _ -> s
    | Common.Remote _, _ -> if extraquotes then Uutil.quotes s else s in
  let fromSpec =
      (formatConnectionInfo rootFrom)
    ^ (addquotes rootFrom
         (Fspath.toString (Fspath.concat (snd rootFrom) pathFrom))) in
  let toSpec =
      (formatConnectionInfo rootTo)
    ^ (addquotes rootTo
         (Fspath.toString (Fspath.concat fspathTo pathTo))) in
  let cmd = prog ^ " "
             ^ (Uutil.quotes fromSpec) ^ " "
             ^ (Uutil.quotes toSpec) in
  Trace.log (Printf.sprintf "%s\n" cmd);
  Lwt_util.resize_region copyprogReg (Prefs.read copymax);
  Lwt_util.run_in_region copyprogReg 1
    (fun () -> External.runExternalProgram cmd) >>= fun (_, log) ->
  debug (fun() ->
           let l = Util.trimWhitespace log in
           Util.msg "transferFileUsingExternalCopyprog %s: returned...\n%s%s"
             (Path.toString pathFrom)
             l (if l="" then "" else "\n"));
  Uutil.showProgress id (Props.length desc) "ext";
  finishExternalTransferOnRoot rootTo rootFrom
    ((snd rootFrom, pathFrom, fspathTo, pathTo, realPathTo),
     (update, desc, fp, ress, id))

(****)

let transferFileLocal connFrom
      ((fspathFrom, pathFrom, fspathTo, pathTo, realPathTo),
       (update, desc, fp, ress, id)) =
  let (tempInfo, isTransferred) =
    fileIsTransferred fspathTo pathTo desc fp ress in
  if isTransferred then begin
    (* File is already fully transferred (from some interrupted
       previous transfer).  So just make sure permissions are right. *)
    let msg =
      Printf.sprintf
        "%s/%s has already been transferred\n"
        (Fspath.toDebugString fspathTo) (Path.toString realPathTo) in
    let len = Uutil.Filesize.add (Props.length desc) (Osx.ressLength ress) in
    Uutil.showProgress id len "alr";
    setFileinfo fspathTo pathTo realPathTo update desc;
    Xferhint.insertEntry fspathTo pathTo fp;
    Lwt.return (`DONE (TransferSucceeded tempInfo, Some msg))
  end else
    registerFileTransfer pathTo fp
      (fun () ->
         match
           tryCopyMovedFile fspathTo pathTo realPathTo update desc fp ress id
         with
           Some (info, msg) ->
             (* Transfer was performed by copying *)
             Xferhint.insertEntry fspathTo pathTo fp;
             Lwt.return (`DONE (TransferSucceeded info, Some msg))
         | None ->
             debug (fun() -> Util.msg "tryCopyMovedFile didn't work, so now we actually transfer\n");
             if shouldUseExternalCopyprog update desc then
               Lwt.return (`EXTERNAL (prepareExternalTransfer fspathTo pathTo))
             else begin
               reallyTransferFile
                 connFrom fspathFrom pathFrom fspathTo pathTo realPathTo
                 update desc fp ress id tempInfo >>= fun status ->
               Xferhint.insertEntry fspathTo pathTo fp;
               Lwt.return (`DONE (status, None))
             end)

let convV0 = Remote.makeConvV0Funs
  (fun ((fspathFrom, pathFrom, fspathTo, pathTo, realPathTo),
         (update, desc, fp, ress, id)) ->
       (fspathFrom, pathFrom, fspathTo, pathTo, realPathTo,
         update, Props.to_compat251 desc, fp, ress, id))
  (fun (fspathFrom, pathFrom, fspathTo, pathTo, realPathTo,
         update, desc, fp, ress, id) ->
       ((fspathFrom, pathFrom, fspathTo, pathTo, realPathTo),
         (update, Props.of_compat251 desc, fp, ress, id)))
  (function
   | `DONE (a, b) -> `DONE (transferStatus_to_compat251 a, b)
   | `EXTERNAL a -> `EXTERNAL a)
  (function
   | `DONE (a, b) -> `DONE (transferStatus_of_compat251 a, b)
   | `EXTERNAL a -> `EXTERNAL a)

let mtransferFile = Umarshal.(sum2 (prod2 mtransferStatus (option string) id id) bool
                                (function
                                 | `DONE (a, b) -> I21 (a, b)
                                 | `EXTERNAL a -> I22 a)
                                (function
                                 | I21 (a, b) -> `DONE (a, b)
                                 | I22 a -> `EXTERNAL a))

let transferFileOnRoot =
  Remote.registerRootCmdWithConnection "transferFile" ~convV0
    mfinishExternalTransfer mtransferFile transferFileLocal

(* We limit the size of the output buffers to about 512 KB
   (we cannot go above the limit below plus 64) *)
let transferFileReg = Lwt_util.make_region 440

let bufferSize sz =
    (* Token queue *)
    min 64 ((truncate (Uutil.Filesize.toFloat sz) + 1023) / 1024)
  +
   (* Read buffer *)
   8

let transferFile
      rootFrom pathFrom rootTo fspathTo pathTo realPathTo
      update desc fp ress id =
  let f () =
    Abort.check id;
    transferFileOnRoot rootTo rootFrom
      ((snd rootFrom, pathFrom, fspathTo, pathTo, realPathTo),
       (update, desc, fp, ress, id)) >>= fun status ->
    match status with
      `DONE (status, msg) ->
         begin match msg with
           Some msg ->
             (* If the file was already present or transferred by copying
                on the server, we need to update the amount of data
                transferred so far here. *)
             if fst rootTo <> Common.Local then begin
               let len =
                 Uutil.Filesize.add (Props.length desc) (Osx.ressLength ress)
               in
               Uutil.showProgress id len "rem"
             end;
             Trace.log msg
         | None ->
             ()
         end;
         Lwt.return status
    | `EXTERNAL useExistingTarget ->
         transferFileUsingExternalCopyprog
           rootFrom pathFrom rootTo fspathTo pathTo realPathTo
           update desc fp ress id useExistingTarget
  in
  (* When streaming, we only transfer one file at a time, so we don't
     need to limit the number of concurrent transfers *)
  if Prefs.read Remote.streamingActivated then
    f ()
  else
    let bufSz = bufferSize (max (Props.length desc) (Osx.ressLength ress)) in
    Lwt_util.run_in_region transferFileReg bufSz f

(****)

let file rootFrom pathFrom rootTo fspathTo pathTo realPathTo
         update desc fp stamp ress id =
  debug (fun() -> Util.msg "copyRegFile(%s,%s) -> (%s,%s,%s,%s,%s)\n"
      (Common.root2string rootFrom) (Path.toString pathFrom)
      (Common.root2string rootTo) (Path.toString realPathTo)
      (Fspath.toDebugString fspathTo) (Path.toString pathTo)
      (Props.toString desc));
  let timer = Trace.startTimer "Transmitting file" in
  begin match rootFrom, rootTo with
    (Common.Local, fspathFrom), (Common.Local, realFspathTo) ->
      localFile
        fspathFrom pathFrom fspathTo pathTo realPathTo
        update desc (Osx.ressLength ress) (Some id);
        paranoidCheck fspathTo pathTo realPathTo desc fp ress
  | _ ->
      transferFile
        rootFrom pathFrom rootTo fspathTo pathTo realPathTo
        update desc fp ress id
  end >>= fun status ->
  Trace.showTimer timer;
  match status with
    TransferSucceeded info ->
      checkForChangesToSource rootFrom pathFrom desc fp stamp ress None false
        >>= fun () ->
      Lwt.return info
  | TransferNeedsDoubleCheckAgainstCurrentSource (info,newfp) ->
      debug (fun() -> Util.msg
               "Archive data for %s is a pseudo-fingerprint: double-checking...\n"
               (Path.toString realPathTo));

      checkForChangesToSource rootFrom pathFrom
                              desc fp stamp ress (Some newfp) false
        >>= (fun () ->
      Lwt.return info)
  | TransferFailed reason ->
      debug (fun() -> Util.msg "TRANSFER FAILED (%s) for %s (real path: %s)\n"
        reason (Path.toString pathTo) (Path.toString realPathTo));
      (* Maybe we failed because the source file was modified.
         We check this before reporting a failure *)
      checkForChangesToSource rootFrom pathFrom desc fp stamp ress None true
        >>= fun () ->
      (* This function never returns (it is supposed to fail) *)
      saveTempFileOnRoot rootTo (pathTo, realPathTo, reason) >>= fun () ->
      assert false

(****)

let recursively fspathFrom pathFrom fspathTo pathTo =
  let rec copy pFrom pTo =
    let info = Fileinfo.get true fspathFrom pFrom in
    match info.Fileinfo.typ with
    | `SYMLINK ->
        debug (fun () -> Util.msg "  Copying link %s / %s to %s / %s\n"
          (Fspath.toDebugString fspathFrom) (Path.toString pFrom)
          (Fspath.toDebugString fspathTo) (Path.toString pTo));
        Os.symlink fspathTo pTo (Os.readLink fspathFrom pFrom)
    | `FILE ->
        debug (fun () -> Util.msg "  Copying file %s / %s to %s / %s\n"
          (Fspath.toDebugString fspathFrom) (Path.toString pFrom)
          (Fspath.toDebugString fspathTo) (Path.toString pTo));
        localFile fspathFrom pFrom fspathTo pTo pTo
          `Copy info.Fileinfo.desc
          (Osx.ressLength info.Fileinfo.osX.Osx.ressInfo)  None
    | `DIRECTORY ->
        debug (fun () -> Util.msg "  Copying directory %s / %s to %s / %s\n"
          (Fspath.toDebugString fspathFrom) (Path.toString pFrom)
          (Fspath.toDebugString fspathTo) (Path.toString pTo));
        Os.createDir fspathTo pTo (Props.perms info.Fileinfo.desc);
        let ch = Os.childrenOf fspathFrom pFrom in
        Safelist.iter
          (fun n -> copy (Path.child pFrom n) (Path.child pTo n)) ch
    | `ABSENT ->
        (* BCP 4/16: Was "assert false", but this causes unison to
           crash when (1) the copyonconflict preference is used, (2) 
           there is a conflict between a deletion and a change, and
           (3) the change is propagated on top of the deletion.  Seems
           better to silently ignore the copy request. *)
        ()
    in
  debug (fun () -> Util.msg "  Copying recursively %s / %s\n"
    (Fspath.toDebugString fspathFrom) (Path.toString pathFrom));
  copy pathFrom pathTo;
  debug (fun () -> Util.msg "  Finished copying %s / %s\n"
    (Fspath.toDebugString fspathFrom) (Path.toString pathTo))
