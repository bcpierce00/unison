(* Unison file synchronizer: src/copy.ml *)
(* $Id$ *)
(* Copyright 1999-2007 (see COPYING for details) *)

let (>>=) = Lwt.bind

let debug = Trace.debug "copy"

(****)

let openFileIn fspath path kind =
  match kind with
    `DATA   -> open_in_gen [Open_rdonly; Open_binary] 0o444
                 (Fspath.concatToString fspath path)
  | `RESS _ -> Osx.openRessIn fspath path

let openFileOut fspath path kind =
  match kind with
    `DATA     ->
      let fullpath = Fspath.concatToString fspath path in
      let flags = [Unix.O_WRONLY;Unix.O_CREAT] in
      let perm = 0o600 in
      begin match Util.osType with
        `Win32 ->
          open_out_gen
            [Open_wronly; Open_creat; Open_excl; Open_binary] perm fullpath
      | `Unix ->
          let fd =
            try
              Unix.openfile fullpath (Unix.O_EXCL :: flags) perm
            with
              Unix.Unix_error
                ((Unix.EOPNOTSUPP | Unix.EUNKNOWNERR 524), _, _) ->
              (* O_EXCL not supported under a Netware NFS-mounted filesystem.
                 Solaris and Linux report different errors. *)
                Unix.openfile fullpath (Unix.O_TRUNC :: flags) perm
          in
          Unix.out_channel_of_descr fd
      end
  | `RESS len -> Osx.openRessOut fspath path len

let protect f g =
  try
    f ()
  with Sys_error _ | Unix.Unix_error _ | Util.Transient _ as e ->
    begin try g () with Sys_error _  | Unix.Unix_error _ -> () end;
    raise e

let lwt_protect f g =
  Lwt.catch f
    (fun e ->
       begin match e with
         Sys_error _ | Unix.Unix_error _ | Util.Transient _ ->
           begin try g () with Sys_error _  | Unix.Unix_error _ -> () end
       | _ ->
           ()
       end;
       Lwt.fail e)

(****)

let localFile
     fspathFrom pathFrom fspathTo pathTo realPathTo update desc ressLength ido =

  let use_id f = match ido with Some id -> f id | None -> () in
  
  Util.convertUnixErrorsToTransient
    "copying locally"
    (fun () ->
      use_id (fun id -> Uutil.showProgress id Uutil.Filesize.zero "l");
      debug (fun () ->
        Util.msg "Copy.localFile %s / %s to %s / %s\n"
          (Fspath.toString fspathFrom) (Path.toString pathFrom)
          (Fspath.toString fspathTo) (Path.toString pathTo));
      let inFd = openFileIn fspathFrom pathFrom `DATA in
      protect (fun () ->
        let outFd = openFileOut fspathTo pathTo `DATA in
        protect (fun () ->
          Uutil.readWrite inFd outFd
            (fun l ->
              use_id ( fun id ->
		Abort.check id;
		Uutil.showProgress id (Uutil.Filesize.ofInt l) "l"));
          close_in inFd;
          close_out outFd)
          (fun () -> close_out_noerr outFd))
        (fun () -> close_in_noerr inFd);
      if ressLength > Uutil.Filesize.zero then begin
        let inFd = openFileIn fspathFrom pathFrom (`RESS ressLength) in
        protect (fun () ->
          let outFd = openFileOut fspathTo pathTo (`RESS ressLength) in
          protect (fun () ->
            Uutil.readWriteBounded inFd outFd ressLength
              (fun l ->
		use_id (fun id ->
                  Abort.check id;
                  Uutil.showProgress id (Uutil.Filesize.ofInt l) "l"));
            close_in inFd;
            close_out outFd)
            (fun () -> close_out_noerr outFd))
          (fun () -> close_in_noerr inFd);
      end;
      match update with
        `Update _ ->
          Fileinfo.set fspathTo pathTo (`Copy realPathTo) desc
      | `Copy ->
          Fileinfo.set fspathTo pathTo (`Set Props.fileDefault) desc)

(****)

(* The file transfer functions here depend on an external module
   'transfer' that implements a generic transmission and the rsync
   algorithm for optimizing the file transfer in the case where a
   similar file already exists on the target. *)

let rsyncActivated =
  Prefs.createBool "rsync" true
    "activate the rsync transfer mode"
    ("Unison uses the 'rsync algorithm' for 'diffs-only' transfer "
     ^ "of updates to large files.  Setting this flag to false makes Unison "
     ^ "use whole-file transfers instead.  Under normal circumstances, "
     ^ "there is no reason to do this, but if you are having trouble with "
     ^ "repeated 'rsync failure' errors, setting it to "
     ^ "false should permit you to synchronize the offending files.")

(* Lazy creation of the destination file *)
let destinationFd fspath path kind outfd =
  match !outfd with
    None    ->
      let fd = openFileOut fspath path kind in
      outfd := Some fd;
      fd
  | Some fd ->
      fd

let decompressor = ref Remote.MsgIdMap.empty

let startReceivingFile
      fspath path realPath fileKind update srcFileSize id file_id =
  debug (fun() ->
    Util.msg "startReceivingFile: %s\n" (Fspath.concatToString fspath path));
  (* We delay the opening of the file so that there are not too many
     temporary files remaining after a crash *)
  let outfd = ref None in
  let showProgress count =
    Abort.check id;
    Uutil.showProgress id (Uutil.Filesize.ofInt count) "r" in
  (* Install a simple generic decompressor *)
  decompressor :=
    Remote.MsgIdMap.add file_id
      (fun ti ->
         let fd = destinationFd fspath path fileKind outfd in
         Transfer.receive fd showProgress ti)
      !decompressor;
  if Prefs.read rsyncActivated then begin
    match update with
      `Update (destFileDataSize, destFileRessSize) when
          let destFileSize =
            match fileKind with
              `DATA   -> destFileDataSize
            | `RESS _ -> destFileRessSize
          in
          Transfer.Rsync.aboveRsyncThreshold destFileSize
            &&
          Transfer.Rsync.aboveRsyncThreshold srcFileSize ->
        Util.convertUnixErrorsToTransient
          "preprocessing file"
          (fun () ->
             let infd = openFileIn fspath realPath fileKind in
             (* Now that we've successfully opened the original version
                of the file, install a more interesting decompressor *)
             decompressor :=
               Remote.MsgIdMap.add file_id
                 (fun ti ->
                    let fd = destinationFd fspath path fileKind outfd in
                    Transfer.Rsync.rsyncDecompress infd fd showProgress ti)
                 !decompressor;
             let bi =
               protect (fun () -> Transfer.Rsync.rsyncPreprocess infd)
                 (fun () -> close_in_noerr infd)
             in
             let (firstBi, remBi) =
               match bi with
                 []                 -> assert false
               | firstBi :: remBi -> (firstBi, remBi)
             in
             Lwt.return (outfd, ref (Some infd), Some firstBi, remBi))
    | _ ->
        Lwt.return (outfd, ref None, None, [])
  end else
    Lwt.return (outfd, ref None, None, [])

let processTransferInstruction conn (file_id, ti) =
  Util.convertUnixErrorsToTransient
    "processing a transfer instruction"
    (fun () ->
       ignore (Remote.MsgIdMap.find file_id !decompressor ti));
  Lwt.return ()

let marshalTransferInstruction =
  (fun (file_id, (data, pos, len)) rem ->
     ((Remote.encodeInt file_id, 0, 4) :: (data, pos, len) :: rem, len + 4)),
  (fun buf pos ->
     let len = String.length buf - pos - 4 in
     (Remote.decodeInt (String.sub buf pos 4), (buf, pos + 4, len)))

let processTransferInstructionRemotely =
  Remote.registerSpecialServerCmd
    "processTransferInstruction" marshalTransferInstruction
    Remote.defaultMarshalingFunctions processTransferInstruction

let blockInfos = ref Remote.MsgIdMap.empty

let compress conn
     (biOpt, fspathFrom, pathFrom, fileKind, sizeFrom, id, file_id) =
  Lwt.catch
    (fun () ->
       let infd = openFileIn fspathFrom pathFrom fileKind in
       lwt_protect (fun () ->
         let showProgress count =
           Abort.check id;
           Uutil.showProgress id (Uutil.Filesize.ofInt count) "r" in
         let compr =
           match biOpt with
             None     -> Transfer.send infd sizeFrom showProgress
           | Some bi  -> let remBi =
                           try
                             Remote.MsgIdMap.find file_id !blockInfos
                           with Not_found ->
                             []
                         in
                         let bi = bi :: remBi in
                         blockInfos :=
                           Remote.MsgIdMap.remove file_id !blockInfos;
                         Transfer.Rsync.rsyncCompress
                           bi infd sizeFrom showProgress
         in
         compr
           (fun ti -> processTransferInstructionRemotely conn (file_id, ti))
               >>= (fun () ->
         close_in infd;
         Lwt.return ()))
       (fun () ->
          close_in_noerr infd))
    (fun e ->
       Util.convertUnixErrorsToTransient
         "rsync sender" (fun () -> raise e))

let compressRemotely = Remote.registerServerCmd "compress" compress


let receiveRemBiLocally _ (file_id, bi) =
  let bil =
    try
      Remote.MsgIdMap.find file_id !blockInfos
    with Not_found ->
      []
  in
  blockInfos := Remote.MsgIdMap.add file_id (bi :: bil) !blockInfos;
  Lwt.return ()

let receiveRemBi = Remote.registerServerCmd "receiveRemBi" receiveRemBiLocally
let rec sendRemBi conn file_id remBi =
  match remBi with
    []     -> Lwt.return ()
  | x :: r -> sendRemBi conn file_id r >>= (fun () ->
              receiveRemBi conn (file_id, x))

(****)

let fileSize (fspath, path) =
  Util.convertUnixErrorsToTransient
    "getting file size"
    (fun () ->
       Lwt.return
        (Props.length (Fileinfo.get false fspath path).Fileinfo.desc))

let fileSizeOnHost =
  Remote.registerServerCmd  "fileSize" (fun _ -> fileSize)

(****)

(* We limit the size of the output buffers to about 512 KB
   (we cannot go above the limit below plus 64) *)
let transmitFileReg = Lwt_util.make_region 440

let bufferSize sz =
  min 64 ((truncate (Uutil.Filesize.toFloat sz) + 1023) / 1024)
    (* Token queue *)
    +
  8 (* Read buffer *)

(****)

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

let reallyTransmitFile
    connFrom fspathFrom pathFrom fspathTo pathTo realPathTo
    update desc ressLength id =
  debug (fun() -> Util.msg "getFile(%s,%s) -> (%s,%s,%s,%s)\n"
      (Fspath.toString fspathFrom) (Path.toString pathFrom)
      (Fspath.toString fspathTo) (Path.toString pathTo)
      (Path.toString realPathTo) (Props.toString desc));

  let srcFileSize = Props.length desc in
  debug (fun () ->
    Util.msg "src file size = %s bytes\n"
      (Uutil.Filesize.toString srcFileSize));
  let file_id = Remote.newMsgId () in
  (* Data fork *)
  startReceivingFile
    fspathTo pathTo realPathTo `DATA update srcFileSize id file_id
    >>= (fun (outfd, infd, firstBi, remBi) ->
  Lwt.catch (fun () ->
    Uutil.showProgress id Uutil.Filesize.zero "f";
    sendRemBi connFrom file_id remBi >>= (fun () ->
    compressRemotely connFrom
      (firstBi,
       fspathFrom, pathFrom, `DATA, srcFileSize, id, file_id)
            >>= (fun () ->
    decompressor :=
      Remote.MsgIdMap.remove file_id !decompressor; (* For GC *)
    close_all infd outfd;
    Lwt.return ())))
    (fun e ->
       decompressor :=
         Remote.MsgIdMap.remove file_id !decompressor; (* For GC *)
       close_all_no_error infd outfd;
       Lwt.fail e) >>= (fun () ->
  (* Ressource fork *)
  if ressLength > Uutil.Filesize.zero then begin
    startReceivingFile
      fspathTo pathTo realPathTo
      (`RESS ressLength) update ressLength id file_id
        >>= (fun (outfd, infd, firstBi, remBi) ->
    Lwt.catch (fun () ->
      Uutil.showProgress id Uutil.Filesize.zero "f";
      sendRemBi connFrom file_id remBi >>= (fun () ->
      compressRemotely connFrom
        (firstBi, fspathFrom, pathFrom,
         `RESS ressLength, ressLength, id, file_id)
            >>= (fun () ->
      decompressor :=
        Remote.MsgIdMap.remove file_id !decompressor; (* For GC *)
      close_all infd outfd;
      Lwt.return ())))
    (fun e ->
       decompressor :=
         Remote.MsgIdMap.remove file_id !decompressor; (* For GC *)
       close_all_no_error infd outfd;
       Lwt.fail e))
  end else
    Lwt.return ()) >>= (fun () ->
  begin match update with
    `Update _ -> Fileinfo.set fspathTo pathTo (`Copy realPathTo) desc
  | `Copy     -> Fileinfo.set fspathTo pathTo (`Set Props.fileDefault) desc
  end;
  Lwt.return ()))

(****)

(* TEMP NOTE: try removing logging statements as a workaround to recently discussed bug *)
let tryCopyMovedFile fspathTo pathTo realPathTo update desc fp ress id =
  Prefs.read Xferhint.xferbycopying
    &&
  begin
    Util.convertUnixErrorsToTransient "tryCopyMovedFile" (fun() ->
      debug (fun () -> Util.msg "tryCopyMovedFile: -> %s /%s/\n"
        (Path.toString pathTo) (Os.fullfingerprint_to_string fp));
      match Xferhint.lookup fp with
        None ->
          false
      | Some (candidateFspath, candidatePath) ->
          Trace.log (Printf.sprintf
            "Shortcut: copying %s from local file %s\n"
            (Path.toString realPathTo)
            (Path.toString candidatePath));
          debug (fun () ->
            Util.msg
              "tryCopyMovedFile: found match at %s,%s. Try local copying\n"
              (Fspath.toString candidateFspath)
              (Path.toString candidatePath));
          try
            if Os.exists candidateFspath candidatePath then begin
              localFile
                candidateFspath candidatePath fspathTo pathTo realPathTo
                update desc (Osx.ressLength ress) (Some id);
              let info = Fileinfo.get false fspathTo pathTo in
              let fp' = Os.fingerprint fspathTo pathTo info in
              if fp' = fp then begin
                debug (fun () -> Util.msg "tryCopyMoveFile: success.\n");
                Xferhint.insertEntry (fspathTo, pathTo) fp;
                true
              end else begin
                debug (fun () ->
                  Util.msg "tryCopyMoveFile: candidate file modified!");
                Xferhint.deleteEntry (candidateFspath, candidatePath);
                Os.delete fspathTo pathTo;
                Trace.log (Printf.sprintf
                  "Shortcut didn't work because %s was modified\n"
                  (Path.toString candidatePath));
                false
              end
            end else begin
              Trace.log (Printf.sprintf
                "Shortcut didn't work because %s disappeared!\n"
                (Path.toString candidatePath));
              Xferhint.deleteEntry (candidateFspath, candidatePath);
              false
            end 
          with
            Util.Transient s ->
              debug (fun () ->
                Util.msg "tryCopyMovedFile: local copy didn't work [%s]" s);
              Xferhint.deleteEntry (candidateFspath, candidatePath);
              Os.delete fspathTo pathTo;
              Trace.log (Printf.sprintf
                "Local copy of %s failed\n"
                (Path.toString candidatePath));
              false)
  end

let transmitFileLocal
  connFrom
  (fspathFrom, pathFrom, fspathTo, pathTo, realPathTo,
   update, desc, fp, ress, id) =
  if
    tryCopyMovedFile
      fspathTo pathTo realPathTo update desc fp ress id
  then
    Lwt.return ()
  else
    reallyTransmitFile
      connFrom fspathFrom pathFrom fspathTo pathTo realPathTo
      update desc (Osx.ressLength ress) id

let transmitFileOnRoot =
  Remote.registerRootCmdWithConnection "transmitFile" transmitFileLocal

let transmitFile
    rootFrom pathFrom rootTo fspathTo pathTo realPathTo
    update desc fp ress id =
  let bufSz = bufferSize (max (Props.length desc) (Osx.ressLength ress)) in
  (* This must be on the client: any lock on the server side may result
     in a deadlock under windows *)
  Lwt_util.run_in_region transmitFileReg bufSz (fun () ->
    Abort.check id;
    transmitFileOnRoot rootTo rootFrom
      (snd rootFrom, pathFrom, fspathTo, pathTo, realPathTo,
       update, desc, fp, ress, id))

(****)

let file
      rootFrom pathFrom rootTo fspathTo pathTo realPathTo
      update desc fp ress id =
  debug (fun() -> Util.msg "copyRegFile(%s,%s) -> (%s,%s,%s,%s,%s)\n"
      (Common.root2string rootFrom) (Path.toString pathFrom)
      (Common.root2string rootTo) (Path.toString realPathTo)
      (Fspath.toString fspathTo) (Path.toString pathTo)
      (Props.toString desc));
  let timer = Trace.startTimer "Transmitting file" in
  begin match rootFrom, rootTo with
    (Common.Local, fspathFrom), (Common.Local, realFspathTo) ->
      localFile
        fspathFrom pathFrom fspathTo pathTo realPathTo
        update desc (Osx.ressLength ress) (Some id);
      Lwt.return ()
  | _ ->
      transmitFile
        rootFrom pathFrom rootTo fspathTo pathTo realPathTo
        update desc fp ress id
  end >>= (fun () ->
  Trace.showTimer timer;
  Lwt.return ())

