(* Unison file synchronizer: src/files.ml *)
(* Copyright 1999-2012, Benjamin C. Pierce 

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
open Lwt
open Fileinfo
  
let debug = Trace.debug "files"
let debugverbose = Trace.debug "files+"
    
(* ------------------------------------------------------------ *)
    
let commitLogName = Util.fileInHomeDir "DANGER.README"
    
let writeCommitLog source target tempname =
  let sourcename = Fspath.toDebugString source in
  let targetname = Fspath.toDebugString target in
  debug (fun() -> Util.msg "Writing commit log: renaming %s to %s via %s\n"
    sourcename targetname tempname);
  Util.convertUnixErrorsToFatal
    "writing commit log"
    (fun () ->
       let c =
         System.open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_excl]
           0o600 commitLogName in
       Printf.fprintf c "Warning: the last run of %s terminated abnormally "
         Uutil.myName;
       Printf.fprintf c "while moving\n   %s\nto\n   %s\nvia\n   %s\n\n"
         sourcename targetname tempname;
       Printf.fprintf c "Please check the state of these files immediately\n";
       Printf.fprintf c "(and delete this notice when you've done so).\n";
       close_out c)

let clearCommitLog () =
  debug (fun() -> (Util.msg "Deleting commit log\n"));
  Util.convertUnixErrorsToFatal
    "clearing commit log"
      (fun () -> System.unlink commitLogName)
    
let processCommitLog () =
  if System.file_exists commitLogName then begin
    raise(Util.Fatal(
          Printf.sprintf
            "Warning: the previous run of %s terminated in a dangerous state.
            Please consult the file %s, delete it, and try again."
                Uutil.myName
                (System.fspathToPrintString commitLogName)))
  end else
    Lwt.return ()
      
let processCommitLogOnHost =
  Remote.registerHostCmd "processCommitLog" processCommitLog
    
let processCommitLogs() =
  Lwt_unix.run
    (Globals.allHostsIter (fun h -> processCommitLogOnHost h ()))

(* ------------------------------------------------------------ *)

let deleteLocal (fspathTo, (pathTo, ui)) =
  debug (fun () ->
     Util.msg "deleteLocal [%s] (None, %s)\n"
       (Fspath.toDebugString fspathTo) (Path.toString pathTo));
  let localPathTo = Update.translatePathLocal fspathTo pathTo in
  (* Make sure the target is unchanged first *)
  (* (There is an unavoidable race condition here.) *)
  let prevArch = Update.checkNoUpdates fspathTo localPathTo ui in
  Stasher.backup fspathTo localPathTo `AndRemove prevArch;
  (* Archive update must be done last *)
  Update.replaceArchiveLocal fspathTo localPathTo Update.NoArchive;
  Lwt.return ()

let deleteOnRoot = Remote.registerRootCmd "delete" deleteLocal

let delete rootFrom pathFrom rootTo pathTo ui =
  deleteOnRoot rootTo (pathTo, ui) >>= fun _ ->
  Update.replaceArchive rootFrom pathFrom Update.NoArchive

(* ------------------------------------------------------------ *)

let fileUpdated ui =
  match ui with
    Updates (File (_, ContentsUpdated _), _) -> true
  | _                                        -> false

let setPropLocal (fspath, (path, ui, newDesc, oldDesc)) =
  (* [ui] provides the modtime while [newDesc] provides the other
     file properties *)
  let localPath = Update.translatePathLocal fspath path in
  let (workingDir,realPath) = Fspath.findWorkingDir fspath localPath in
  Fileinfo.set workingDir realPath (`Update oldDesc) newDesc;
  if fileUpdated ui then Stasher.stashCurrentVersion fspath localPath None;
  (* Archive update must be done last *)
  Update.updateProps fspath localPath (Some newDesc) ui;
  Lwt.return ()

let setPropOnRoot = Remote.registerRootCmd "setProp" setPropLocal

let updatePropsOnRoot =
  Remote.registerRootCmd
   "updateProps"
     (fun (fspath, (path, propOpt, ui)) ->
        let localPath = Update.translatePathLocal fspath path in
        (* Archive update must be done first *)
        Update.updateProps fspath localPath propOpt ui;
        if fileUpdated ui then
          Stasher.stashCurrentVersion fspath localPath None;
        Lwt.return ())

let updateProps root path propOpt ui =
  updatePropsOnRoot root (path, propOpt, ui)

(* FIX: we should check there has been no update before performing the
   change *)
let setProp rootFrom pathFrom rootTo pathTo newDesc oldDesc uiFrom uiTo =
  debug (fun() ->
    Util.msg
      "setProp %s %s %s\n   %s %s %s\n"
      (root2string rootFrom) (Path.toString pathFrom)
      (Props.toString newDesc)
      (root2string rootTo) (Path.toString pathTo)
      (Props.toString oldDesc));
  setPropOnRoot rootTo (pathTo, uiTo, newDesc, oldDesc) >>= fun _ ->
  updateProps rootFrom pathFrom None uiFrom

(* ------------------------------------------------------------ *)

let mkdirOnRoot =
  Remote.registerRootCmd
    "mkdir"
    (fun (fspath,(workingDir,path)) ->
       let info = Fileinfo.get false workingDir path in
       if info.Fileinfo.typ = `DIRECTORY then begin
         begin try
           (* Make sure the directory is writable *)
           Fs.chmod (Fspath.concat workingDir path)
             (Props.perms info.Fileinfo.desc lor 0o700)
         with Unix.Unix_error _ -> () end;
         Lwt.return (true, info.Fileinfo.desc)
       end else begin
         if info.Fileinfo.typ <> `ABSENT then
           Os.delete workingDir path;
         Os.createDir workingDir path Props.dirDefault;
         Lwt.return (false, (Fileinfo.get false workingDir path).Fileinfo.desc)
       end)

let setDirPropOnRoot =
  Remote.registerRootCmd
    "setDirProp"
    (fun (_, (workingDir, path, initialDesc, newDesc)) ->
      Fileinfo.set workingDir path (`Set initialDesc) newDesc;
      Lwt.return ())

let makeSymlink =
  Remote.registerRootCmd
    "makeSymlink"
    (fun (fspath, (workingDir, path, l)) ->
       if Os.exists workingDir path then
         Os.delete workingDir path;
       Os.symlink workingDir path l;
       Lwt.return ())

(* ------------------------------------------------------------ *)

let performRename fspathTo localPathTo workingDir pathFrom pathTo prevArch =
  debug (fun () -> Util.msg "Renaming %s to %s in %s; root is %s\n"
      (Path.toString pathFrom)
      (Path.toString pathTo)
      (Fspath.toDebugString workingDir)
      (Fspath.toDebugString fspathTo));
  let source = Fspath.concat workingDir pathFrom in
  let target = Fspath.concat workingDir pathTo in
  Util.convertUnixErrorsToTransient
    (Printf.sprintf "renaming %s to %s"
       (Fspath.toDebugString source) (Fspath.toDebugString target))
    (fun () ->
      debugverbose (fun() ->
        Util.msg "calling Fileinfo.get from renameLocal\n");
      let filetypeFrom =
        (Fileinfo.get false source Path.empty).Fileinfo.typ in
      debugverbose (fun() ->
        Util.msg "back from Fileinfo.get from renameLocal\n");
      if filetypeFrom = `ABSENT then raise (Util.Transient (Printf.sprintf
           "Error while renaming %s to %s -- source file has disappeared!"
	   (Fspath.toPrintString source) (Fspath.toPrintString target)));
      let filetypeTo = (Fileinfo.get false target Path.empty).Fileinfo.typ in

       (* Windows and Unix operate differently if the target path of a
          rename already exists: in Windows an exception is raised, in
          Unix the file is clobbered.  In both Windows and Unix, if
          the target is an existing **directory**, an exception will
          be raised.  We want to avoid doing the move first, if possible,
          because this opens a "window of danger" during which the contents of
          the path is nothing. *)
      let moveFirst =
        match (filetypeFrom, filetypeTo) with
        | (_, `ABSENT)            -> false
        | ((`FILE | `SYMLINK),
           (`FILE | `SYMLINK))    -> Util.osType <> `Unix
        | _                       -> true (* Safe default *) in
      if moveFirst then begin
        debug (fun() -> Util.msg "rename: moveFirst=true\n");
        let tmpPath = Os.tempPath workingDir pathTo in
        let temp = Fspath.concat workingDir tmpPath in
        let temp' = Fspath.toDebugString temp in

        debug (fun() ->
          Util.msg "moving %s to %s\n" (Fspath.toDebugString target) temp');
        Stasher.backup fspathTo localPathTo `ByCopying prevArch;
        writeCommitLog source target temp';
        Util.finalize (fun() ->
          (* If the first rename fails, the log can be removed: the
             filesystem is in a consistent state *)
          Os.rename "renameLocal(1)" target Path.empty temp Path.empty;
          (* If the next renaming fails, we will be left with
             DANGER.README file which will make any other
             (similar) renaming fail in a cryptic way.  So it
             seems better to abort early by converting Unix errors
             to Fatal ones (rather than Transient). *)
          Util.convertUnixErrorsToFatal "renaming with commit log"
            (fun () ->
              debug (fun() -> Util.msg "rename %s to %s\n"
                       (Fspath.toDebugString source)
                       (Fspath.toDebugString target));
              Os.rename "renameLocal(2)"
                source Path.empty target Path.empty))
          (fun _ -> clearCommitLog());
        (* It is ok to leave a temporary file.  So, the log can be
           cleared before deleting it. *)
        Os.delete temp Path.empty
      end else begin
        debug (fun() -> Util.msg "rename: moveFirst=false\n");
        Stasher.backup fspathTo localPathTo `ByCopying prevArch;
        Os.rename "renameLocal(3)" source Path.empty target Path.empty;
        debug (fun() ->
	  if filetypeFrom = `FILE then
            Util.msg
              "Contents of %s after renaming = %s\n"
              (Fspath.toDebugString target)
    	      (Fingerprint.toString (Fingerprint.file target Path.empty)));
      end)

(* FIX: maybe we should rename the destination before making any check ? *)
(* JV (6/09): the window is small again...
   FIX: When this code was originally written, we assumed that the
   checkNoUpdates would happen immediately before the rename, so that
   the window of danger where other processes could invalidate the thing we
   just checked was very small.  But now that transport is multi-threaded,
   this window of danger could get very long because other transfers are
   saturating the link.  It would be better, I think, to introduce a real
   2PC protocol here, so that both sides would (locally and almost-atomically)
   check that their assumptions had not been violated and then switch the
   temp file into place, but remain able to roll back if something fails
   either locally or on the other side. *)
let renameLocal
      (fspathTo, (localPathTo, workingDir, pathFrom, pathTo, ui, archOpt)) =
  (* Make sure the target is unchanged, then do the rename.
     (Note that there is an unavoidable race condition here...) *)
  let prevArch = Update.checkNoUpdates fspathTo localPathTo ui in
  performRename fspathTo localPathTo workingDir pathFrom pathTo prevArch;
  begin match archOpt with
    Some archTo -> Stasher.stashCurrentVersion fspathTo localPathTo None;
                   Update.iterFiles fspathTo localPathTo archTo
                     Xferhint.insertEntry;
                   (* Archive update must be done last *)
                   Update.replaceArchiveLocal fspathTo localPathTo archTo
  | None        -> ()
  end;
  Lwt.return ()

let renameOnHost = Remote.registerRootCmd "rename" renameLocal

let rename root localPath workingDir pathOld pathNew ui archOpt =
  debug (fun() ->
    Util.msg "rename(root=%s, pathOld=%s, pathNew=%s)\n"
      (root2string root)
      (Path.toString pathOld) (Path.toString pathNew));
  renameOnHost root (localPath, workingDir, pathOld, pathNew, ui, archOpt)

(* ------------------------------------------------------------ *)

(* Calculate the target working directory and paths for the copy.
      workingDir  is an fspath naming the directory on the target
                  host where the copied file will actually live.
                  (In the case where pathTo names a symbolic link, this
                  will be the parent directory of the file that the
                  symlink points to, not the symlink itself.  Note that
                  this fspath may be outside of the replica, or even
                  on a different volume.)
      realPathTo  is the name of the target file relative to workingDir.
                  (If pathTo names a symlink, this will be the name of
                  the file pointed to by the symlink, not the name of the
                  link itself.)
      tempPathTo  is a temporary file name in the workingDir.  The file (or
                  directory structure) will first be copied here, then
                  "almost atomically" moved onto realPathTo. *)

let setupTargetPathsLocal (fspath, path) =
  let localPath = Update.translatePathLocal fspath path in
  let (workingDir,realPath) = Fspath.findWorkingDir fspath localPath in
  let tempPath = Os.tempPath ~fresh:false workingDir realPath in
  Lwt.return (workingDir, realPath, tempPath, localPath)

let setupTargetPaths =
  Remote.registerRootCmd "setupTargetPaths" setupTargetPathsLocal

let rec createDirectories fspath localPath props =
  match props with
    [] ->
      ()
  | desc :: rem ->
      match Path.deconstructRev localPath with
        None ->
          assert false
      | Some (_, parentPath) ->
          createDirectories fspath parentPath rem;
          try
            let absolutePath = Fspath.concat fspath parentPath in
             Fs.mkdir absolutePath (Props.perms desc)
            (* The directory may have already been created
               if there are several paths with the same prefix *)
          with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let setupTargetPathsAndCreateParentDirectoryLocal (fspath, (path, props)) =
  let localPath = Update.translatePathLocal fspath path in
  Util.convertUnixErrorsToTransient
    "creating parent directories"
    (fun () -> createDirectories fspath localPath props);
  let (workingDir,realPath) = Fspath.findWorkingDir fspath localPath in
  let tempPath = Os.tempPath ~fresh:false workingDir realPath in
  Lwt.return (workingDir, realPath, tempPath, localPath)

let setupTargetPathsAndCreateParentDirectory =
  Remote.registerRootCmd "setupTargetPathsAndCreateParentDirectory"
    setupTargetPathsAndCreateParentDirectoryLocal

(* ------------------------------------------------------------ *)

let updateSourceArchiveLocal (fspathFrom, (localPathFrom, uiFrom, errPaths)) =
  (* Archive update must be done first (before Stasher call) *)
  let newArch = Update.updateArchive fspathFrom localPathFrom uiFrom in
  (* We update the archive with what we were expected to copy *)
  Update.replaceArchiveLocal fspathFrom localPathFrom newArch;
  (* Then, we remove all pieces of which the copy failed *)
  List.iter
    (fun p ->
       debug (fun () ->
         Util.msg "Copy under %s/%s was aborted\n"
           (Fspath.toDebugString fspathFrom) (Path.toString p));
       Update.replaceArchiveLocal fspathFrom p Update.NoArchive)
    errPaths;
  Stasher.stashCurrentVersion fspathFrom localPathFrom None;
  Lwt.return ()

let updateSourceArchive =
  Remote.registerRootCmd "updateSourceArchive" updateSourceArchiveLocal

(* ------------------------------------------------------------ *)

let deleteSpuriousChild fspathTo pathTo nm =
  (* FIX: maybe we should turn them into Unison temporary files? *)
  let path = (Path.child pathTo nm) in
  debug (fun() -> Util.msg "Deleting spurious file %s/%s\n"
                    (Fspath.toDebugString fspathTo) (Path.toString path));
  Os.delete fspathTo path

let rec deleteSpuriousChildrenRec fspathTo pathTo archChildren children =
  match archChildren, children with
    archNm :: archRem, nm :: rem ->
      let c = Name.compare archNm nm in
      if c < 0 then
        deleteSpuriousChildrenRec fspathTo pathTo archRem children
      else if c = 0 then
        deleteSpuriousChildrenRec fspathTo pathTo archChildren rem
      else begin
        deleteSpuriousChild fspathTo pathTo nm;
        deleteSpuriousChildrenRec fspathTo pathTo archChildren rem
      end
  | [], nm :: rem ->
      deleteSpuriousChild fspathTo pathTo nm;
      deleteSpuriousChildrenRec fspathTo pathTo [] rem
  | _, [] ->
      ()

let deleteSpuriousChildrenLocal (_, (fspathTo, pathTo, archChildren)) =
  deleteSpuriousChildrenRec
    fspathTo pathTo archChildren
    (List.sort Name.compare (Os.childrenOf fspathTo pathTo));
  Lwt.return ()

let deleteSpuriousChildren =
  Remote.registerRootCmd "deleteSpuriousChildren" deleteSpuriousChildrenLocal

let rec normalizePropsRec propsFrom propsTo =
  match propsFrom, propsTo with
    d :: r, d' :: r' -> normalizePropsRec r r'
  | _, []            -> propsFrom
  | [], _ :: _       -> assert false

let normalizeProps propsFrom propsTo =
  normalizePropsRec (Safelist.rev propsFrom) (Safelist.rev propsTo)

(* ------------------------------------------------------------ *)

let copyReg = Lwt_util.make_region 50

let copy
      update
      rootFrom pathFrom   (* copy from here... *)
      uiFrom              (* (and then check that this updateItem still
                             describes the current state of the src replica) *)
      propsFrom           (* the properties of the parent directories, in
                             case we need to propagate them *)
      rootTo pathTo       (* ...to here *)
      uiTo                (* (but, before committing the copy, check that
                             this updateItem still describes the current
                             state of the target replica) *)
      propsTo             (* the properties of the parent directories *)
      id =                (* for progress display *)
  debug (fun() ->
    Util.msg
      "copy %s %s ---> %s %s \n"
      (root2string rootFrom) (Path.toString pathFrom)
      (root2string rootTo) (Path.toString pathTo));
  (* Calculate target paths *)
  setupTargetPathsAndCreateParentDirectory rootTo
    (pathTo, normalizeProps propsFrom propsTo)
     >>= fun (workingDir, realPathTo, tempPathTo, localPathTo) ->
  (* When in Unicode case-insensitive mode, we want to create files
     with NFC normal-form filenames. *)
  let realPathTo =
    match update with
      `Update _ ->
        realPathTo
    | `Copy ->
        match Path.deconstructRev realPathTo with
          None ->
            assert false
        | Some (name, parentPath) ->
            Path.child parentPath (Name.normalize name)
  in
  (* Calculate source path *)
  Update.translatePath rootFrom pathFrom >>= fun localPathFrom ->
  let errors = ref [] in
  (* Inner loop for recursive copy... *)
  let rec copyRec pFrom      (* Path to copy from *)
                  pTo        (* (Temp) path to copy to *)
                  realPTo    (* Path where this file will ultimately be placed
                                (needed by rsync, which uses the old contents
                                of this file to optimize transfer) *)
                  f =        (* Source archive subtree for this path *)
    debug (fun() ->
      Util.msg "copyRec %s --> %s  (really to %s)\n"
        (Path.toString pFrom) (Path.toString pTo)
        (Path.toString realPTo));
    Lwt.catch
      (fun () ->
         match f with
           Update.ArchiveFile (desc, fp, stamp, ress) ->
             Lwt_util.run_in_region copyReg 1 (fun () ->
               Abort.check id;
               let stmp =
                 if Update.useFastChecking () then Some stamp else None in
               Copy.file
                 rootFrom pFrom rootTo workingDir pTo realPTo
                 update desc fp stmp ress id
                 >>= fun info ->
               let ress' = Osx.stamp info.Fileinfo.osX in
               Lwt.return
                 (Update.ArchiveFile (Props.override info.Fileinfo.desc desc,
                                      fp, Fileinfo.stamp info, ress'),
                  []))
         | Update.ArchiveSymlink l ->
             Lwt_util.run_in_region copyReg 1 (fun () ->
               debug (fun() -> Util.msg "Making symlink %s/%s -> %s\n"
                                 (root2string rootTo) (Path.toString pTo) l);
               Abort.check id;
               makeSymlink rootTo (workingDir, pTo, l) >>= fun () ->
               Lwt.return (f, []))
         | Update.ArchiveDir (desc, children) ->
             Lwt_util.run_in_region copyReg 1 (fun () ->
               debug (fun() -> Util.msg "Creating directory %s/%s\n"
                 (root2string rootTo) (Path.toString pTo));
               mkdirOnRoot rootTo (workingDir, pTo))
                 >>= fun (dirAlreadyExisting, initialDesc) ->
             Abort.check id;
             (* We start a thread for each child *)
             let childThreads =
               Update.NameMap.mapi
                 (fun name child ->
                    let nameTo = Name.normalize name in
                    copyRec (Path.child pFrom name)
                            (Path.child pTo nameTo)
                            (Path.child realPTo nameTo)
                            child)
                 children
             in
             (* We collect the thread results *)
             Update.NameMap.fold
               (fun nm childThr remThr ->
                  childThr >>= fun (arch, paths) ->
                  remThr >>= fun (children, pathl, error) ->
                  let childErr = arch = Update.NoArchive in
                  let children =
                    if childErr then children else
                    Update.NameMap.add nm arch children
                  in
                  Lwt.return (children, paths :: pathl, error || childErr))
               childThreads
               (Lwt.return (Update.NameMap.empty, [], false))
               >>= fun (newChildren, pathl, childError) ->
             begin if dirAlreadyExisting || childError then
               let childNames =
                 Update.NameMap.fold (fun nm _ l -> nm :: l) newChildren [] in
               deleteSpuriousChildren rootTo (workingDir, pTo, childNames)
             else
               Lwt.return ()
             end >>= fun () ->
             Lwt_util.run_in_region copyReg 1 (fun () ->
               (* We use the actual file permissions so as to preserve
                  inherited bits *)
               setDirPropOnRoot rootTo
                 (workingDir, pTo, initialDesc, desc)) >>= fun () ->
             Lwt.return (Update.ArchiveDir (desc, newChildren),
                         List.flatten pathl)
         | Update.NoArchive ->
             assert false)
      (fun e ->
         match e with
           Util.Transient _ ->
             if not (Abort.testException e) then begin
               Abort.file id;
               errors := e :: !errors
             end;
             Lwt.return (Update.NoArchive, [pFrom])
         | _ ->
             Lwt.fail e)
  in
  (* Compute locally what we need to propagate *)
  let rootLocal = List.hd (Globals.rootsInCanonicalOrder ()) in
  let localArch =
    Update.updateArchive (snd rootLocal) localPathFrom uiFrom in
  copyRec localPathFrom tempPathTo realPathTo localArch
    >>= fun (archTo, errPaths) ->
  if archTo = Update.NoArchive then
    (* We were not able to transfer anything *)
    Lwt.fail (List.hd !errors)
  else begin
    (* Rename the files to their final location and then update the
       archive on the destination replica *)
    rename rootTo localPathTo workingDir tempPathTo realPathTo uiTo
      (Some archTo) >>= fun () ->
    (* Update the archive on the source replica
       FIX: we could reuse localArch if rootFrom is the same as rootLocal *)
    updateSourceArchive rootFrom (localPathFrom, uiFrom, errPaths) >>= fun () ->
    (* Return the first error, if any *)
    match Safelist.rev !errors with
      e :: _ -> Lwt.fail e
    | []     -> Lwt.return ()
  end

(* ------------------------------------------------------------ *)

let (>>=) = Lwt.bind

let diffCmd =
  Prefs.createString "diff" "diff -u CURRENT2 CURRENT1"
    "!set command for showing differences between files"
    ("This preference can be used to control the name and command-line "
     ^ "arguments of the system "
     ^ "utility used to generate displays of file differences.  The default "
     ^ "is `\\verb|diff -u CURRENT2 CURRENT1|'.  If the value of this preference contains the substrings "
     ^ "CURRENT1 and CURRENT2, these will be replaced by the names of the files to be "
     ^ "diffed.  If not, the two filenames will be appended to the command.  In both "
     ^ "cases, the filenames are suitably quoted.")

let tempName s = Os.tempFilePrefix ^ s

let rec diff root1 path1 ui1 root2 path2 ui2 showDiff id =
  debug (fun () ->
    Util.msg
      "diff %s %s %s %s ...\n"
      (root2string root1) (Path.toString path1)
      (root2string root2) (Path.toString path2));
  let displayDiff fspath1 fspath2 =
    let cmd =
      if Util.findsubstring "CURRENT1" (Prefs.read diffCmd) = None then
          (Prefs.read diffCmd)
        ^ " " ^ (Fspath.quotes fspath1)
        ^ " " ^ (Fspath.quotes fspath2)
      else
        Util.replacesubstrings (Prefs.read diffCmd)
          ["CURRENT1", Fspath.quotes fspath1;
           "CURRENT2", Fspath.quotes fspath2] in
    let c = System.open_process_in
      (if Util.osType = `Win32 && not Util.isCygwin then
        (* BCP: Proposed by Karl M. to deal with the standard windows 
           command processor's weird treatment of spaces and quotes: *)
        "\"" ^ cmd ^ "\""
       else
         cmd) in
    showDiff cmd (External.readChannelTillEof c);
    ignore (System.close_process_in c) in
  let (desc1, fp1, ress1, desc2, fp2, ress2) = Common.fileInfos ui1 ui2 in
  match root1,root2 with
    (Local,fspath1),(Local,fspath2) ->
      Util.convertUnixErrorsToTransient
        "diffing files"
        (fun () ->
           let path1 = Update.translatePathLocal fspath1 path1 in
           let path2 = Update.translatePathLocal fspath2 path2 in
           displayDiff
             (Fspath.concat fspath1 path1) (Fspath.concat fspath2 path2))
  | (Local,fspath1),(Remote host2,fspath2) ->
      Util.convertUnixErrorsToTransient
        "diffing files"
        (fun () ->
           let path1 = Update.translatePathLocal fspath1 path1 in
           let (workingDir, realPath) = Fspath.findWorkingDir fspath1 path1 in
           let tmppath =
             Path.addSuffixToFinalName realPath (tempName "diff-") in
           Os.delete workingDir tmppath;
           Lwt_unix.run
             (Update.translatePath root2 path2 >>= (fun path2 ->
              Copy.file root2 path2 root1 workingDir tmppath realPath
                `Copy (Props.setLength Props.fileSafe (Props.length desc2))
                 fp2 None ress2 id) >>= fun info ->
              Lwt.return ());
           displayDiff
	     (Fspath.concat workingDir realPath)
             (Fspath.concat workingDir tmppath);
           Os.delete workingDir tmppath)
  | (Remote host1,fspath1),(Local,fspath2) ->
      Util.convertUnixErrorsToTransient
        "diffing files"
        (fun () ->
           let path2 = Update.translatePathLocal fspath2 path2 in
           let (workingDir, realPath) = Fspath.findWorkingDir fspath2 path2 in
           let tmppath =
             Path.addSuffixToFinalName realPath "#unisondiff-" in
           Lwt_unix.run
             (Update.translatePath root1 path1 >>= (fun path1 ->
              (* Note that we don't need the resource fork *)
              Copy.file root1 path1 root2 workingDir tmppath realPath
                `Copy (Props.setLength Props.fileSafe (Props.length desc1))
                 fp1 None ress1 id >>= fun info ->
              Lwt.return ()));
           displayDiff
             (Fspath.concat workingDir tmppath)
	     (Fspath.concat workingDir realPath);
           Os.delete workingDir tmppath)
  | (Remote host1,fspath1),(Remote host2,fspath2) ->
      assert false


(**********************************************************************)

(* Taken from ocamltk/jpf/fileselect.ml *)
let get_files_in_directory dir =
  let dirh = System.opendir dir in
  let files = ref [] in
  begin try
    while true do files := dirh.System.readdir () :: !files done
  with End_of_file ->
    dirh.System.closedir ()
  end;
  Sort.list (<) !files

let ls dir pattern =
  Util.convertUnixErrorsToTransient
    "listing files"
    (fun () ->
       let files = get_files_in_directory dir in
       let re = Rx.glob pattern in
       let rec filter l =
         match l with
           [] ->
             []
         | hd :: tl ->
             if Rx.match_string re hd then hd :: filter tl else filter tl
       in
       filter files)


(***********************************************************************
                  CALL OUT TO EXTERNAL MERGE PROGRAM
************************************************************************)

let formatMergeCmd p f1 f2 backup out1 out2 outarch =
  if not (Globals.shouldMerge p) then
    raise (Util.Transient ("'merge' preference not set for "^(Path.toString p)));
  let raw =
    try Globals.mergeCmdForPath p
    with Not_found ->
      raise (Util.Transient ("'merge' preference does not provide a command "
                             ^ "template for " ^ (Path.toString p)))
  in
  let cooked = raw in
  let cooked = Util.replacesubstring cooked "CURRENT1" f1 in
  let cooked = Util.replacesubstring cooked "CURRENT2" f2 in
  let cooked =
    match backup with
      None -> begin
	let cooked = Util.replacesubstring cooked "CURRENTARCHOPT" "" in
	match Util.findsubstring "CURRENTARCH" cooked with
	  None -> cooked
	| Some _ -> raise (Util.Transient
                      ("No archive found, but the 'merge' command "
                       ^ "template expects one.  (Consider enabling "
                       ^ "'backupcurrent' for this file or using CURRENTARCHOPT "
                       ^ "instead of CURRENTARCH.)"))
      end
    | Some(s) ->
	let cooked = Util.replacesubstring cooked "CURRENTARCHOPT" s in
	let cooked = Util.replacesubstring cooked "CURRENTARCH"    s in
        cooked in
  let cooked = Util.replacesubstring cooked "NEW1"     out1 in
  let cooked = Util.replacesubstring cooked "NEW2"     out2 in
  let cooked = Util.replacesubstring cooked "NEWARCH"  outarch in
  let cooked = Util.replacesubstring cooked "NEW" out1 in
  let cooked = Util.replacesubstring cooked "PATH"
		(Uutil.quotes (Path.toString p)) in
  cooked
    
let copyBack fspathFrom pathFrom rootTo pathTo propsTo uiTo id =
  setupTargetPaths rootTo pathTo
    >>= (fun (workingDirForCopy, realPathTo, tempPathTo, localPathTo) ->
  let info = Fileinfo.get false fspathFrom pathFrom in
  let fp = Os.fingerprint fspathFrom pathFrom info in
  let stamp = Osx.stamp info.Fileinfo.osX in
  let newprops = Props.setLength propsTo (Props.length info.Fileinfo.desc) in
  Copy.file
    (Local, fspathFrom) pathFrom rootTo workingDirForCopy tempPathTo realPathTo
    `Copy newprops fp None stamp id >>= fun info ->
  rename rootTo localPathTo workingDirForCopy tempPathTo realPathTo
    uiTo None)
    
let keeptempfilesaftermerge =   
  Prefs.createBool
    "keeptempfilesaftermerge" false "*" ""

let showStatus = function
  | Unix.WEXITED i -> Printf.sprintf "exited (%d)" i
  | Unix.WSIGNALED i -> Printf.sprintf "killed with signal %d" i
  | Unix.WSTOPPED i -> Printf.sprintf "stopped with signal %d" i

let merge root1 path1 ui1 root2 path2 ui2 id showMergeFn =
  debug (fun () -> Util.msg "merge path %s between roots %s and %s\n"
      (Path.toString path1) (root2string root1) (root2string root2));

  (* The following assumes root1 is always local: switch them if needed to make this so *)
  let (root1,path1,ui1,root2,path2,ui2) = 
    match root1 with
      (Local,fspath1) -> (root1,path1,ui1,root2,path2,ui2)
    | _ -> (root2,path2,ui2,root1,path1,ui1) in

  let (localPath1, (workingDirForMerge, basep), fspath1) =
    match root1 with
      (Local,fspath1) ->
        let localPath1 = Update.translatePathLocal fspath1 path1 in
        (localPath1, Fspath.findWorkingDir fspath1 localPath1, fspath1)
    | _ -> assert false in
  
  (* We're going to be doing a lot of copying, so let's define a shorthand
     that fixes most of the arguments to Copy.localfile *)
  let copy l =
    Safelist.iter
      (fun (src,trg) ->
        debug (fun () -> Util.msg "Copying %s to %s\n" (Path.toString src) (Path.toString trg));
        Os.delete workingDirForMerge trg;
        let info = Fileinfo.get false workingDirForMerge src in
        Copy.localFile
          workingDirForMerge src
          workingDirForMerge trg trg
          `Copy info.Fileinfo.desc
          (Osx.ressLength info.Fileinfo.osX.Osx.ressInfo) (Some id))
      l in
  
  let working1 = Path.addPrefixToFinalName basep (tempName "merge1-") in
  let working2 = Path.addPrefixToFinalName basep (tempName "merge2-") in
  let workingarch = Path.addPrefixToFinalName basep (tempName "mergearch-") in
  let new1 = Path.addPrefixToFinalName basep (tempName "mergenew1-") in
  let new2 = Path.addPrefixToFinalName basep (tempName "mergenew2-") in
  let newarch = Path.addPrefixToFinalName basep (tempName "mergenewarch-") in
  
  let (desc1, fp1, ress1, desc2, fp2, ress2) = Common.fileInfos ui1 ui2 in
  
  Util.convertUnixErrorsToTransient "merging files" (fun () ->
    (* Install finalizer (below) in case we unwind the stack *)
    Util.finalize (fun () ->
      
    (* Make local copies of the two replicas *)
      Os.delete workingDirForMerge working1;
      Os.delete workingDirForMerge working2;
      Os.delete workingDirForMerge workingarch;
      Lwt_unix.run
	(Copy.file
           root1 localPath1 root1 workingDirForMerge working1 basep
           `Copy desc1 fp1 None ress1 id >>= fun info ->
         Lwt.return ());
      Lwt_unix.run
	(Update.translatePath root2 path2 >>= (fun path2 ->
	  Copy.file
	    root2 path2 root1 workingDirForMerge working2 basep
	    `Copy desc2 fp2 None ress2 id) >>= fun info ->
         Lwt.return ());
      
      (* retrieve the archive for this file, if any *)
      let arch =
	match ui1, ui2 with
	| Updates (_, Previous (_,_,fp,_)), Updates (_, Previous (_,_,fp2,_)) ->
	    if fp = fp2 then
	      Stasher.getRecentVersion fspath1 localPath1 fp 
	    else
	      assert false
	| NoUpdates, Updates(_, Previous (_,_,fp,_))
	| Updates(_, Previous (_,_,fp,_)), NoUpdates -> 
	    Stasher.getRecentVersion fspath1 localPath1 fp
	| Updates (_, New), Updates(_, New) 
	| Updates (_, New), NoUpdates
	| NoUpdates, Updates (_, New) ->
	    debug (fun () -> Util.msg "File is new, no current version will be searched");
	    None
	| _ -> assert false    in
      
      (* Make a local copy of the archive file (in case the merge program  
         overwrites it and the program crashes before the call to the Stasher). *)
      begin
        match arch with 
	  Some fspath ->
	    let info = Fileinfo.get false fspath Path.empty in
	    Copy.localFile 
	      fspath Path.empty 
	      workingDirForMerge workingarch workingarch
	      `Copy 
	      info.Fileinfo.desc
	      (Osx.ressLength info.Fileinfo.osX.Osx.ressInfo)
	      None
	| None ->
	    ()
      end;
	    
      (* run the merge command *)
      Os.delete workingDirForMerge new1;
      Os.delete workingDirForMerge new2;
      Os.delete workingDirForMerge newarch;
      let info1 = Fileinfo.get false workingDirForMerge working1 in
      (* FIX: Why split out the parts of the pair?  Why is it not abstract anyway??? *)
      let fp1 = Os.fingerprint workingDirForMerge working1 info1 in
      let info2 = Fileinfo.get false workingDirForMerge working2 in
      let fp2 = Os.fingerprint workingDirForMerge working2 info2 in
      let cmd = formatMergeCmd
          path1
          (Fspath.quotes (Fspath.concat workingDirForMerge working1))
          (Fspath.quotes (Fspath.concat workingDirForMerge working2))
          (match arch with None -> None | Some f -> Some(Fspath.quotes f))
          (Fspath.quotes (Fspath.concat workingDirForMerge new1))
          (Fspath.quotes (Fspath.concat workingDirForMerge new2))
          (Fspath.quotes (Fspath.concat workingDirForMerge newarch)) in
      Trace.log (Printf.sprintf "Merge command: %s\n" cmd);
      
      let returnValue, mergeResultLog =
        Lwt_unix.run (External.runExternalProgram cmd) in
      
      Trace.log (Printf.sprintf "Merge result (%s):\n%s\n"
                   (showStatus returnValue) mergeResultLog);
      debug (fun () -> Util.msg "Merge result = %s\n"
                   (showStatus returnValue));

      (* This query to the user probably belongs below, after we've gone through all the
         logic that might raise exceptions in various conditions.  But it has the side effect of
         *displaying* the results of the merge (or putting them in a "details" area), so we don't
         want to skip doing it if we raise one of these exceptions.  Better might be to split out
         the displaying from the querying... *)
      if not
          (showMergeFn
             (Printf.sprintf "Results of merging %s" (Path.toString path1))
             mergeResultLog) then
        raise (Util.Transient ("Merge command canceled by the user"));
      
      (* It's useful for now to be a bit verbose about what we're doing, but let's 
         keep it easy to switch this to debug-only in some later release... *)
      let say f = f() in

      (* Check which files got created by the merge command and do something appropriate
         with them *)
      debug (fun()-> Util.msg "New file 1 = %s\n" (Fspath.toDebugString (Fspath.concat workingDirForMerge new1)));
      let new1exists = Fs.file_exists (Fspath.concat workingDirForMerge new1) in
      let new2exists = Fs.file_exists (Fspath.concat workingDirForMerge new2) in
      let newarchexists = Fs.file_exists (Fspath.concat workingDirForMerge newarch) in
      
      if new1exists && new2exists then begin
        if newarchexists then 
	  say (fun () -> Util.msg "Three outputs detected \n")
	else
	  say (fun () -> Util.msg "Two outputs detected \n");
        let info1 = Fileinfo.get false workingDirForMerge new1 in
        let info2 = Fileinfo.get false workingDirForMerge new2 in
        let fp1' = Os.fingerprint workingDirForMerge new1 info1 in
        let fp2' = Os.fingerprint workingDirForMerge new2 info2 in
        if fp1'=fp2' then begin
          debug (fun () -> Util.msg "Two outputs equal => update the archive\n");
          copy [(new1,working1); (new2,working2); (new1,workingarch)];
	end else
	  if returnValue = Unix.WEXITED 0 then begin
            say (fun () -> (Util.msg "Two outputs not equal but merge command returned 0, so we will\n";
   		            Util.msg "overwrite the other replica and the archive with the first output\n"));
	    copy [(new1,working1); (new1,working2); (new1,workingarch)];
	  end else begin
            say (fun () -> (Util.msg "Two outputs not equal and the merge command exited with nonzero status, \n";
		            Util.msg "so we will copy back the new files but not update the archive\n"));
	    copy [(new1,working1); (new2,working2)];
	    
          end 
      end
	  
      else if new1exists && (not new2exists) && (not newarchexists) then begin
	  if returnValue = Unix.WEXITED 0 then begin
            say (fun () -> Util.msg "One output detected \n");
            copy [(new1,working1); (new1,working2); (new1,workingarch)];
	  end else begin
            say (fun () -> Util.msg "One output detected but merge command returned nonzero exit status\n");
            raise (Util.Transient "One output detected but merge command returned nonzero exit status\n")
          end
      end
	  
      else if (not new1exists) && new2exists && (not newarchexists) then begin
        assert false
      end
	  
      else if (not new1exists) && (not new2exists) && (not newarchexists) then begin
        say (fun () -> Util.msg "No outputs detected \n");
        let working1_still_exists = Fs.file_exists (Fspath.concat workingDirForMerge working1) in
        let working2_still_exists = Fs.file_exists (Fspath.concat workingDirForMerge working2) in
	
        if working1_still_exists && working2_still_exists then begin
          say (fun () -> Util.msg "No output from merge cmd and both original files are still present\n");
          let info1' = Fileinfo.get false workingDirForMerge working1 in
          let fp1' = Os.fingerprint workingDirForMerge working1 info1' in
          let info2' = Fileinfo.get false workingDirForMerge working2 in
          let fp2' = Os.fingerprint workingDirForMerge working2 info2' in
          if fp1 = fp1' && fp2 = fp2' then
            raise (Util.Transient "Merge program didn't change either temp file");
          if fp1' = fp2' then begin
            say (fun () -> Util.msg "Merge program made files equal\n");
            copy [(working1,workingarch)];
          end else if fp2 = fp2' then begin
            say (fun () -> Util.msg "Merge program changed just first input\n");
            copy [(working1,working2);(working1,workingarch)]
          end else if fp1 = fp1' then begin
            say (fun () -> Util.msg "Merge program changed just second input\n");
            copy [(working2,working1);(working2,workingarch)]
          end else
	    if returnValue <> Unix.WEXITED 0 then
	      raise (Util.Transient ("Error: the merge function changed both of "
                                     ^ "its inputs but did not make them equal"))
	    else begin
	      say (fun () -> (Util.msg "Merge program changed both of its inputs in";
			      Util.msg "different ways, but returned zero.\n"));
              (* Note that we assume the merge program knew what it was doing when it
                 returned 0 -- i.e., we assume a zero result means that the files are
                 "morally equal" and either can be replaced by the other; we therefore
                 choose one of them (#2) as the unique new result, so that we can update
                 Unison's archive and call the file 'in sync' again. *)
              copy [(working2,working1);(working2,workingarch)];
	    end
        end
	    
        else if working1_still_exists && (not working2_still_exists) 
	    && returnValue = Unix.WEXITED 0 then begin
              say (fun () -> Util.msg "No outputs and second replica has been deleted \n");
              copy [(working1,working2); (working1,workingarch)];
            end
	    
        else if (not working1_still_exists) && working2_still_exists 
	    && returnValue = Unix.WEXITED 0 then begin
              say (fun () -> Util.msg "No outputs and first replica has been deleted \n");
              copy [(working2,working1); (working2,workingarch)];
            end
        else if returnValue = Unix.WEXITED 0 then begin
            raise (Util.Transient ("Error: the merge program deleted both of its "
                                   ^ "inputs and generated no output!"))
        end else begin
            say (fun() -> Util.msg "The merge program exited with nonzero status and did not leave";
			  Util.msg " both files equal");
	    raise (Util.Transient ("Error: the merge program failed and did not leave"
				   ^ " both files equal"))
        end
      end else begin
        assert false
      end;

      Lwt_unix.run
	(debug (fun () -> Util.msg "Committing results of merge\n");
         copyBack workingDirForMerge working1 root1 path1 desc1 ui1 id >>= (fun () ->
         copyBack workingDirForMerge working2 root2 path2 desc2 ui2 id >>= (fun () ->
         let arch_fspath = Fspath.concat workingDirForMerge workingarch in
         if Fs.file_exists arch_fspath then begin
           debug (fun () -> Util.msg "Updating unison archives for %s to reflect results of merge\n"
                   (Path.toString path1));
           if not (Stasher.shouldBackupCurrent path1) then
             Util.msg "Warning: 'backupcurrent' is not set for path %s\n" (Path.toString path1);
           Stasher.stashCurrentVersion workingDirForMerge localPath1 (Some workingarch);
           let infoarch = Fileinfo.get false workingDirForMerge workingarch in
           let fp = Os.fingerprint arch_fspath Path.empty infoarch in
           debug (fun () -> Util.msg "New fingerprint is %s\n" (Os.fullfingerprint_to_string fp));
           let new_archive_entry =
             Update.ArchiveFile
               (Props.get (Fs.stat arch_fspath) infoarch.osX, fp,
                Fileinfo.stamp (Fileinfo.get true arch_fspath Path.empty),
                Osx.stamp infoarch.osX) in
           Update.replaceArchive root1 path1 new_archive_entry >>= fun _ ->
           Update.replaceArchive root2 path2 new_archive_entry >>= fun _ ->
           Lwt.return ()
         end else 
           (Lwt.return ()) )))) )
    (fun _ ->
      Util.ignoreTransientErrors
	(fun () ->
           if not (Prefs.read keeptempfilesaftermerge) then begin
             Os.delete workingDirForMerge working1;
             Os.delete workingDirForMerge working2;
             Os.delete workingDirForMerge workingarch;
             Os.delete workingDirForMerge new1;
             Os.delete workingDirForMerge new2;
             Os.delete workingDirForMerge newarch
           end))
