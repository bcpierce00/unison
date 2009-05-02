(* Unison file synchronizer: src/stasher.ml *)
(* $I2: Last modified by lescuyer *)
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


(* --------------------------------------------------------------------------*)
(* Preferences for backing up and stashing *)
   
let debug = Util.debug "stasher"
let verbose = Util.debug "stasher+"

let backuplocation = 
  Prefs.createString "backuploc" "central"
    "!where backups are stored ('local' or 'central')"
    ("This preference determines whether backups should be kept locally, near the "
     ^ "original files, or"
     ^" in a central directory specified by the \\texttt{backupdir} "
     ^"preference. If set to \\verb|local|, backups will be kept in "
     ^"the same directory as the original files, and if set to \\verb|central|,"
     ^" \\texttt{backupdir} will be used instead.")
    
let _ = Prefs.alias backuplocation "backuplocation"

let backup =
  Pred.create "backup" ~advanced:true
    ("Including the preference \\texttt{-backup \\ARG{pathspec}} "
     ^ "causes Unison to keep backup files for each path that matches "
     ^ "\\ARG{pathspec}.  These backup files are kept in the "
     ^ "directory specified by the \\verb|backuplocation| preference. The backups are named "
     ^ "according to the \\verb|backupprefix| and \\verb|backupsuffix| preferences."
     ^ " The number of versions that are kept is determined by the "
     ^ "\\verb|maxbackups| preference."
     ^ "\n\n The syntax of \\ARG{pathspec} is described in "
     ^ "\\sectionref{pathspec}{Path Specification}.")
    
let _ = Pred.alias backup "mirror"

let backupnot =
  Pred.create "backupnot" ~advanced:true
    ("The values of this preference specify paths or individual files or"
     ^ " regular expressions that should {\\em not} "
     ^ "be backed up, even if the {\\tt backup} preference selects "
     ^ "them---i.e., "
     ^ "it selectively overrides {\\tt backup}.  The same caveats apply here "
     ^ "as with {\\tt ignore} and {\tt ignorenot}.")

let _ = Pred.alias backupnot "mirrornot"
    
let shouldBackup p =
  let s = (Path.toString p) in
  Pred.test backup s && not (Pred.test backupnot s)
    
let backupprefix =
  Prefs.createString "backupprefix" ".bak.$VERSION."
    "!prefix for the names of backup files"
    ("When a backup for a file \\verb|NAME| is created, it is stored "
     ^ "in a directory specified by \\texttt{backuplocation}, in a file called "
     ^ "\\texttt{backupprefix}\\verb|NAME|\\texttt{backupsuffix}."
     ^ " \\texttt{backupprefix} can include a directory name (causing Unison to "
     ^ "keep all backup files for a given directory in a subdirectory with this name), and both "
     ^ " \\texttt{backupprefix} and \\texttt{backupsuffix} can contain the string"
     ^ "\\ARG{\\$VERSION}, which will be replaced by the \\emph{age} of the backup "
     ^ "(1 for the most recent, 2 for the second most recent, and so on...)."
     ^ " This keyword is ignored if it appears in a directory name"
     ^ " in the prefix; if it  does not appear anywhere"
     ^ " in the prefix or the suffix, it will be automatically"
     ^ " placed at the beginning of the suffix.  "
     ^ "\n\n"
     ^ "One thing to be careful of: If the {\\tt backuploc} preference is set "
     ^ "to {\\tt local}, Unison will automatically ignore {\\em all} files "
     ^ "whose prefix and suffix match {\\tt backupprefix} and {\\tt backupsuffix}.  "
     ^ "So be careful to choose values for these preferences that are sufficiently "
     ^ "different from the names of your real files.")
    
let backupsuffix =
  Prefs.createString "backupsuffix" ""
    "!a suffix to be added to names of backup files"
    ("See \\texttt{backupprefix} for full documentation.")

let backups =
  Prefs.createBool "backups" false
    "!keep backup copies of all files (see also 'backup')"
    ("Setting this flag to true is equivalent to "
     ^" setting \\texttt{backuplocation} to \\texttt{local}"
     ^" and \\texttt{backup} to \\verb|Name *|.")

(* The following function is used to express the old backup preference, if set,
   in the terms of the new preferences *)
let translateOldPrefs () =
  match (Pred.extern backup, Pred.extern backupnot, Prefs.read backups) with
    ([], [], true) ->
      debug (fun () -> 
	Util.msg "backups preference set: translated into backup and backuplocation\n");
      Pred.intern backup ["Name *"]; 
      Prefs.set backuplocation "local"
  | (_, _, false) -> 
      ()
  | _ -> raise (Util.Fatal ( "Both old 'backups' preference and "
			    ^ "new 'backup' preference are set!"))
	
let maxbackups =
  Prefs.createInt "maxbackups" 2
    "!number of backed up versions of a file"
    ("This preference specifies the number of backup versions that will "
     ^ "be kept by unison, for each path that matches the predicate "
     ^ "\\verb|backup|.  The default is 2.")
    
let _ = Prefs.alias maxbackups "mirrorversions"
let _ = Prefs.alias maxbackups "backupversions"
    
let backupdir =
  Prefs.createString "backupdir" ""
    "!directory for storing centralized backups"
    ("If this preference is set, Unison will use it as the name of the "
     ^ "directory used to store backup files specified by "
     ^ "the {\\tt backup} preference, when {\\tt backuplocation} is set"
     ^ " to \\verb|central|. It is checked {\\em after} the "
     ^ "{\\tt UNISONBACKUPDIR} environment variable.")

let backupDirectory () =
  Util.convertUnixErrorsToTransient "backupDirectory()" (fun () ->
    try Fspath.canonize (Some (Unix.getenv "UNISONBACKUPDIR"))
    with Not_found ->
      try Fspath.canonize (Some (Unix.getenv "UNISONMIRRORDIR"))
      with Not_found ->
	if Prefs.read backupdir <> ""
	then Fspath.canonize (Some (Prefs.read backupdir))
	else Os.fileInUnisonDir "backup")

let backupcurrent =
  Pred.create "backupcurr" ~advanced:true
    ("Including the preference \\texttt{-backupcurr \\ARG{pathspec}} "
     ^" causes Unison to keep a backup of the {\\em current} version of every file "
     ^ "matching \\ARG{pathspec}.  "
     ^" This file will be saved as a backup with version number 000. Such"
     ^" backups can be used as inputs to external merging programs, for instance.  See "
     ^ "the documentatation for the \\verb|merge| preference."
     ^" For more details, see \\sectionref{merge}{Merging Conflicting Versions}."
     ^"\n\n The syntax of \\ARG{pathspec} is described in "
     ^ "\\sectionref{pathspec}{Path Specification}.")

let backupcurrentnot =
  Pred.create "backupcurrnot" ~advanced:true
   "Exceptions to \\verb|backupcurr|, like the \\verb|ignorenot| preference."

let shouldBackupCurrent p =
     (* BCP: removed next line [Apr 2007]: causes ALL mergeable files to be backed
        up, which is probably not what users want -- the backupcurrent
        switch should be used instead.
     Globals.shouldMerge p || *)
  (let s = Path.toString p in
      Pred.test backupcurrent s && not (Pred.test backupcurrentnot s))

let _ = Pred.alias backupcurrent "backupcurrent"
let _ = Pred.alias backupcurrentnot "backupcurrentnot"

(* ---------------------------------------------------------------------------*)

(* NB: We use Str.regexp here because we need group matching to retrieve
   and increment version numbers from backup file names. We only use
   it here, though: to check if a path should be backed up or ignored, we
   use Rx instead.  (This is important because the Str regexp functions are
   terribly slow.) *)

(* A tuple of string option * string * string, describing a regular
   expression that matches the filenames of unison backups according
   to the current preferences. The first regexp is an option to match
   the local directory, if any, in which backups are stored; the second
   one matches the prefix, the third the suffix.

   Note that we always use forward slashes here (rather than using backslashes
   when running on windows) because we are constructing rx's that are going to
   be matched against Path.t's.  (Strictly speaking, we ought to ask the Path
   module what the path separator character is, rather than assuming it is slash,
   but this is never going to change.)
 *)
let backup_rx () =
  let version_rx = "\\([0-9]+\\)" in
  let prefix = Prefs.read backupprefix in
  let suffix = Str.quote (Prefs.read backupsuffix) in
  let (udir, uprefix) =
    ((match Filename.dirname prefix with
      | "." -> ""
      | s   -> (Fileutil.backslashes2forwardslashes s)^"/"), 
     Filename.basename prefix) in
  let (dir, prefix) = 
    ((match udir with "" -> None | _ -> Some(Str.quote udir)), Str.quote uprefix) in
  if Str.string_match (Str.regexp ".*\\\\\\$VERSION.*") (prefix^suffix) 0 then 
    (dir,
     Str.global_replace (Str.regexp "\\\\\\$VERSION") version_rx prefix,
     Str.global_replace (Str.regexp "\\\\\\$VERSION") version_rx suffix)
  else
    raise (Util.Fatal "Either backupprefix or backupsuffix must contain '$VERSION'")
   
(* We ignore files whose name ends in .unison.bak, since people may still have these
   lying around from using previous versions of Unison. *)
let oldBackupPrefPathspec = "Name *.unison.bak"

(* This function creates Rx regexps based on the preferences to ignore
   backups of old and current versions.  *)
let addBackupFilesToIgnorePref () =
  let (dir_rx, prefix_rx, suffix_rx) = backup_rx() in
  let regexp_to_rx s =
   Str.global_replace (Str.regexp "\\\\(") ""
     (Str.global_replace (Str.regexp "\\\\)") "" s) in
  let (full, dir) =
    let d = 
      match dir_rx with 
	None -> "/" 
      | Some s -> regexp_to_rx s in
    let p = regexp_to_rx prefix_rx in
    let s = regexp_to_rx suffix_rx in
    debug (fun() -> Util.msg "d = %s\n" d);
    ("(.*/)?"^p^".*"^s, "(.*/)?"^(String.sub d 0 (String.length d - 1))) in
  let theRegExp = 
    match dir_rx with 
      None   -> "Regex " ^ full 
    | Some _ -> "Regex " ^ dir in

  Globals.addRegexpToIgnore oldBackupPrefPathspec;
  if Prefs.read backuplocation = "local" then begin
    debug (fun () -> 
       Util.msg "New pattern being added to ignore preferences (for backup files):\n   %s\n"
         theRegExp);
    Globals.addRegexpToIgnore theRegExp
  end 

(* We use references for functions that compute the prefixes and suffixes
   in order to avoid using functions from the Str module each time we need them. *)
let make_prefix = ref (fun i -> assert false)
let make_suffix = ref (fun i -> assert false)
    
(* This function updates the function used to create prefixes and suffixes
   for naming backup files, according to the preferences. *)
let updateBackupNamingFunctions () =
  let makeFun s =
    match Str.full_split (Str.regexp "\\$VERSION") s with
      [] -> (fun _ -> "")
    | [Str.Text t] ->  
	(fun _ -> t)
    | [Str.Delim _; Str.Text t] -> 
	(fun i -> Printf.sprintf "%d%s" i t)
    | [Str.Text t; Str.Delim _] ->
	(fun i -> Printf.sprintf "%s%d" t i)
    | [Str.Text t; Str.Delim _; Str.Text t'] ->
	(fun i -> Printf.sprintf "%s%d%s" t i t')
    | _ -> raise (Util.Fatal (
        "The tag $VERSION should only appear "
       ^"once in the backupprefix and backupsuffix preferences.")) in
  
  make_prefix := makeFun (Prefs.read backupprefix);
  make_suffix := makeFun (Prefs.read backupsuffix);
  debug (fun () -> Util.msg
    "Prefix and suffix regexps for backup filenames have been updated\n")
	  
(*------------------------------------------------------------------------------------*)

let makeBackupName path i =
  (* if backups are kept centrally, the current version has exactly
     the same name as the original, for convenience. *)
  if i=0 && Prefs.read backuplocation = "central" then
    path
  else
    Path.addSuffixToFinalName 
      (Path.addPrefixToFinalName path (!make_prefix i))
      (!make_suffix i)

let stashDirectory fspath =
  match Prefs.read backuplocation with
    "central" -> backupDirectory ()
  | "local" -> fspath
  |  _ -> raise (Util.Fatal ("backuplocation preference should be set"
			     ^"to central or local."))
	
let showContent typ fspath path =
  match typ with
  | `FILE -> Fingerprint.toString (Fingerprint.file fspath path)
  | `SYMLINK -> Os.readLink fspath path
  | `DIRECTORY -> "DIR"
  | `ABSENT -> "ABSENT" 

(* Generates a file name for a backup file.  If backup file already exists,
   the old file will be renamed with the count incremented.  The newest
   backup file is always the one with version number 1, larger numbers mean
   older files. *)
(* BCP: Note that the way we keep bumping up the backup numbers on all existing
   backup files could make backups very expensive if someone sets maxbackups to a
   sufficiently large number! 
*)
let backupPath fspath path =
  let sFspath = stashDirectory fspath in

  let rec f i =
    let tempPath = makeBackupName path i in
    if Os.exists sFspath tempPath then
      if i < Prefs.read maxbackups then
        Os.rename "backupPath" sFspath tempPath sFspath (f (i + 1))
      else if i >= Prefs.read maxbackups then
        Os.delete sFspath tempPath;
    tempPath in

  let rec mkdirectories backdir =
    verbose (fun () -> Util.msg
      "mkdirectories %s %s\n" (Fspath.toString sFspath) (Path.toString backdir));
    if not (Os.exists sFspath Path.empty) then
      Os.createDir sFspath Path.empty Props.dirDefault;
    match Path.deconstructRev backdir with
      None -> ()
    | Some (_, parent) ->
        mkdirectories parent;
        let props = (Fileinfo.get false sFspath Path.empty).Fileinfo.desc in
        if not (Os.exists sFspath backdir) then Os.createDir sFspath backdir props in

  let path0 = makeBackupName path 0 in
  let sourceTyp = (Fileinfo.get true fspath path).Fileinfo.typ in
  let path0Typ = (Fileinfo.get true sFspath path0).Fileinfo.typ in

  if   (   sourceTyp = `FILE && path0Typ = `FILE
       && (Fingerprint.file fspath path) = (Fingerprint.file sFspath path0))
    || (   sourceTyp = `SYMLINK && path0Typ = `SYMLINK
       && (Os.readLink fspath path) = (Os.readLink sFspath path0))
  then begin
    debug (fun()-> Util.msg
      "[%s / %s] = [%s / %s] = %s: no need to back up\n"
      (Fspath.toString sFspath) (Path.toString path0)
      (Fspath.toString fspath) (Path.toString path)
      (showContent sourceTyp fspath path));
    None
  end else begin
    debug (fun()-> Util.msg
      "stashed [%s / %s] = %s is not equal to new [%s / %s] = %s (or one is a dir): stash!\n"
      (Fspath.toString sFspath) (Path.toString path0)
      (showContent path0Typ sFspath path0)
      (Fspath.toString fspath) (Path.toString path)
      (showContent sourceTyp fspath path));
    let sPath = f 0 in
    (* Make sure the parent directory exists *)
    begin match Path.deconstructRev sPath with
     | None -> mkdirectories Path.empty
     | Some (_, backdir) -> mkdirectories backdir
    end;
    Some(sFspath, sPath) 
  end 

(*------------------------------------------------------------------------------------*)
	  
let backup fspath path (finalDisposition : [`AndRemove | `ByCopying]) =
  debug (fun () -> Util.msg
      "backup: %s / %s\n"
      (Fspath.toString fspath)
      (Path.toString path));
  Util.convertUnixErrorsToTransient "backup" (fun () ->
    let disposeIfNeeded() =
      if finalDisposition = `AndRemove then
        Os.delete fspath path in
    if not (Os.exists fspath path) then 
      debug (fun () -> Util.msg
        "File %s in %s does not exist, so no need to back up\n"  
        (Path.toString path) (Fspath.toString fspath))
    else if shouldBackup path then begin
      match backupPath fspath path with
        None -> disposeIfNeeded()
      | Some (backRoot, backPath) ->
          debug (fun () -> Util.msg "Backing up %s / %s to %s in %s\n" 
              (Fspath.toString fspath) (Path.toString path)
              (Path.toString backPath) (Fspath.toString backRoot));
          let byCopying() = 
            let rec copy p backp =
              let info = Fileinfo.get true fspath p in
              match info.Fileinfo.typ with
              | `SYMLINK ->
                  debug (fun () -> Util.msg "  Copying link %s / %s to %s / %s\n"
                    (Fspath.toString fspath) (Path.toString p)
                    (Fspath.toString backRoot) (Path.toString backp));
                  Os.symlink backRoot backp (Os.readLink fspath p)
              | `FILE ->
                  debug (fun () -> Util.msg "  Copying file %s / %s to %s / %s\n"
                    (Fspath.toString fspath) (Path.toString p)
                    (Fspath.toString backRoot) (Path.toString backp));
                  Copy.localFile  fspath p  backRoot backp backp 
                    `Copy  info.Fileinfo.desc
                    (Osx.ressLength info.Fileinfo.osX.Osx.ressInfo)  None
              | `DIRECTORY ->
                  debug (fun () -> Util.msg "  Copying directory %s / %s to %s / %s\n"
                    (Fspath.toString fspath) (Path.toString p)
                    (Fspath.toString backRoot) (Path.toString backp));
                  Os.createDir backRoot backp info.Fileinfo.desc;
                  let ch = Os.childrenOf fspath p in
                  Safelist.iter (fun n -> copy (Path.child p n) (Path.child backp n)) ch
              | `ABSENT -> assert false in
            copy path backPath;
            debug (fun () -> Util.msg "  Finished copying; deleting %s / %s\n"
              (Fspath.toString fspath) (Path.toString path));
            disposeIfNeeded() in
          try 
            if finalDisposition = `AndRemove then
              Os.rename "backup" fspath path backRoot backPath
            else
              byCopying()
          with _ -> 
            debug (fun () -> Util.msg "Rename failed -- copying instead\n");
            byCopying()
      end else begin
	debug (fun () -> Util.msg "Path %s / %s does not need to be backed up\n"
	    (Fspath.toString fspath)
	    (Path.toString path));
        disposeIfNeeded()
      end)
	  
(*------------------------------------------------------------------------------------*)

let rec stashCurrentVersion fspath path sourcePathOpt =
  if shouldBackupCurrent path then 
    Util.convertUnixErrorsToTransient "stashCurrentVersion" (fun () ->
      let sourcePath = match sourcePathOpt with None -> path | Some p -> p in
      debug (fun () -> Util.msg "stashCurrentVersion of %s (drawn from %s) in %s\n" 
               (Path.toString path) (Path.toString sourcePath) (Fspath.toString fspath));
      let stat = Fileinfo.get true fspath sourcePath in
      match stat.Fileinfo.typ with
	`ABSENT -> ()
      |	`DIRECTORY ->
           assert (sourcePathOpt = None);
           debug (fun () -> Util.msg "Stashing recursively because file is a directory\n");
           ignore (Safelist.iter
                     (fun n ->
                       let pathChild = Path.child path n in 
                       if not (Globals.shouldIgnore pathChild) then 
                         stashCurrentVersion fspath (Path.child path n) None)
                     (Os.childrenOf fspath path))
      | `SYMLINK -> 
          begin match backupPath fspath path with
          | None -> ()
          | Some (stashFspath,stashPath) ->
	      Os.symlink stashFspath stashPath (Os.readLink fspath sourcePath)
          end 
      |	`FILE ->
          begin match backupPath fspath path with
          | None -> ()
          | Some (stashFspath, stashPath) ->
              Copy.localFile 
                fspath sourcePath 
                stashFspath stashPath stashPath 
                `Copy 
                stat.Fileinfo.desc
                (Osx.ressLength stat.Fileinfo.osX.Osx.ressInfo)
                None
          end)
      
(*------------------------------------------------------------------------------------*)    
    
(* This function tries to find a backup of a recent version of the file at location
   (fspath, path) in the current replica, matching the given fingerprint. If no file
   is found, then the functions returns None *without* searching on the other replica *)
let getRecentVersion fspath path fingerprint =
  debug (fun () ->
    Util.msg "getRecentVersion of %s in %s\n" 
      (Path.toString path) 
      (Fspath.toString fspath));
  Util.convertUnixErrorsToTransient "getRecentVersion" (fun () ->
    let dir = stashDirectory fspath in
    let rec aux_find i =
      let path = makeBackupName path i in
      if Os.exists dir path &&
	(let dig = Os.fingerprint dir path (Fileinfo.get false dir path) in 
 	 dig = fingerprint)
      then begin
	debug (fun () ->
	  Util.msg "recent version %s found in %s\n" 
	    (Path.toString path) 
	    (Fspath.toString dir));
	Some (Fspath.concat dir path)
      end else
	if i = Prefs.read maxbackups then begin
	  debug (fun () ->
	    Util.msg "No recent version was available for %s on this root.\n"
	      (Fspath.toString (Fspath.concat fspath path)));
	  None
	end else
	  aux_find (i+1)
    in
    aux_find 0)
    
(*------------------------------------------------------------------------------------*)    

(* This function initializes the Stasher module according to the preferences
   defined in the profile. It should be called whenever a profile is reloaded. *)
let initBackupsLocal () =
  debug (fun () -> Util.msg "initBackupsLocal\n");
  translateOldPrefs ();
  addBackupFilesToIgnorePref ();
  updateBackupNamingFunctions ()
  
let initBackupsRoot: Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "initBackups"
    (fun (fspath, ()) ->
      Lwt.return (initBackupsLocal ()))

let initBackups () =
  Lwt_unix.run (
    Globals.allRootsIter (fun r -> initBackupsRoot r ()))
