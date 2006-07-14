(* Unison file synchronizer: src/test.ml *)
(* $Id: $ *)
(* Copyright 1999-2006 (see COPYING for details) *)

let (>>=)  = Lwt.(>>=)

(* ---------------------------------------------------------------------- *)
(* Utility functions *)

let debug = Trace.debug "test"

let rec remove_file_or_dir d =
  match try Some(Unix.lstat d) with Unix.Unix_error((Unix.ENOENT | Unix.ENOTDIR),_,_) -> None with
  | Some(s) ->
    if s.Unix.st_kind = Unix.S_DIR then begin
      let handle = Unix.opendir d in
      let rec loop () =
        let r = try Some(Unix.readdir handle) with End_of_file -> None in
        match r with
        | Some f ->
            if f="." || f=".." then loop ()
            else begin
              remove_file_or_dir (d^"/"^f);
              loop ()
            end  
        | None ->
            Unix.closedir handle;
            Unix.rmdir d
      in loop ()
    end else 
      Sys.remove d
  | None -> ()

let read_chan chan =
  let nbytes = in_channel_length chan in
  let string = String.create nbytes in
  really_input chan string 0 nbytes;
  string

let read file =
  if file = "-" then
    read_chan stdin
  else 
    let chan = open_in_bin file in
    try
      let r = read_chan chan in
      close_in chan;
      r
    with exn ->
      close_in chan;
      raise exn

let write file s =
  if file = "-" then
    output_string stdout s
  else 
    let chan = open_out_bin file in
    try
      output_string chan s;
      close_out chan
    with exn ->
      close_out chan;
      raise exn

let read_dir d =
  let ignored = ["."; ".."] in
  let d = Unix.opendir d in
  let rec do_read acc =
    try
      (match (Unix.readdir d) with
       | s when Safelist.mem s ignored -> do_read acc
       | f -> do_read (f :: acc))
    with End_of_file -> acc
  in
  let files = do_read [] in
  Unix.closedir d;
  files

let extend p file =
  p ^ "/" ^ file

type fs =
  | File of string
  | Link of string
  | Dir of (string * fs) list

let rec equal fs1 fs2 =
  match fs1,fs2 with
    | File s1, File s2 -> s1=s2
    | Link s1, Link s2 -> s1=s2
    | Dir d1, Dir d2 ->
        let dom d = Safelist.sort String.compare (Safelist.map fst d) in
           (dom d1 = dom d2)
        && (Safelist.for_all
              (fun x ->
                equal (Safelist.assoc x d1) (Safelist.assoc x d2)))
             (dom d1)
    | _,_ -> false

let rec fs2string = function
  | File s -> "File \"" ^ s ^ "\""
  | Link s -> "Link \"" ^ s ^ "\""
  | Dir s -> "Dir [" ^ (String.concat "; "
                          (Safelist.map (fun (n,fs') -> "(\""^n^"\", "^(fs2string fs')^")") s)) ^ "]"

let rec readfs p =
  let s = Unix.lstat p in
  match s.Unix.st_kind with
    | Unix.S_REG -> File (read p)
    | Unix.S_LNK -> Link (Unix.readlink p)
    | Unix.S_DIR -> Dir (Safelist.map (fun x -> (x, readfs (extend p x))) (read_dir p))
    | _ -> assert false

let default_perm = 0o755

let writefs p fs =
  let rec loop p = function
    | File s -> write p s
    | Link s -> Unix.symlink s p
    | Dir files ->
        Unix.mkdir p default_perm;
        Safelist.iter (fun (x,cont) -> loop (extend p x) cont) files
  in
  remove_file_or_dir p;
  loop p fs

let checkRootEmpty : Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "checkRootEmpty"
    (fun (fspath, ()) ->
       if Os.exists fspath Path.empty then
         raise (Util.Fatal (Printf.sprintf
           "Path %s is not empty at start of tests!"
             (Fspath.toString fspath)));
       Lwt.return ())

let makeRootEmpty : Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "makeRootEmpty"
    (fun (fspath, ()) ->
       remove_file_or_dir (Fspath.toString fspath);
       Lwt.return ())

let getfs : Common.root -> unit -> fs Lwt.t =
  Remote.registerRootCmd
    "getfs"
    (fun (fspath, ()) ->
       Lwt.return (readfs (Fspath.toString fspath)))

let getbackup : Common.root -> unit -> fs Lwt.t =
  Remote.registerRootCmd
    "getbackup"
    (fun (fspath, ()) ->
       Lwt.return (readfs (Fspath.toString (Stasher.backupDirectory ()))))

let makeBackupEmpty : Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "makeBackupEmpty"
    (fun (fspath, ()) ->
       let b = Fspath.toString (Stasher.backupDirectory ()) in
       debug (fun () -> Util.msg "Removing %s\n" b);
       Lwt.return (remove_file_or_dir b))

let putfs : Common.root -> fs -> unit Lwt.t =
  Remote.registerRootCmd
    "putfs"
    (fun (fspath, fs) ->
       writefs (Fspath.toString fspath) fs;
       Lwt.return ())

let loadPrefs l =
  Prefs.loadStrings l;
  Lwt_unix.run (Globals.propagatePrefs ())

(* ---------------------------------------------------------------------------- *)

let displayRis ris =
  Safelist.iter
    (fun ri -> 
      Util.msg "%s\n" (Uicommon.reconItem2string Path.empty ri ""))
    ris

let sync ?(verbose=false) () = 
  let (reconItemList, _, _) =
    Recon.reconcileAll (Update.findUpdates()) in
  if verbose then begin
    Util.msg "Sync result:\n";
    displayRis reconItemList
  end;
  Lwt_unix.run (
    Lwt_util.iter
      (fun ri ->
         Transport.transportItem ri
           (Uutil.File.ofLine 0) (fun _ _ _ -> true))
      reconItemList);
  Update.commitUpdates()

let currentTest = ref ""

let runtest name f =
  Util.msg "%s...\n" name;
  Util.convertUnixErrorsToFatal "Test.test" (fun() -> 
    debug (fun() -> Util.msg "Emptying backup directory\n");
    Lwt_unix.run (Globals.allRootsIter (fun r -> makeBackupEmpty r ()));
    let savedPrefs = Prefs.dump() in
    currentTest := name;
    debug (fun() -> Util.msg "Running test\n");
    f();
    debug (fun() -> Util.msg "Restoring saved prefs\n");
    Prefs.load savedPrefs;
    Stasher.initBackups()
  )

type checkable = R1 | R2 | BACKUP1 | BACKUP2

let checkable2string = function
  R1 -> "R1" | R2 -> "R2" | BACKUP1 -> "BACKUP1" | BACKUP2 -> "BACKUP2"

let test() = 
  Util.msg "Running internal tests...\n";

  (* Paranoid checks, to make sure we do not delete anybody's filesystem! *)
  if not (Safelist.for_all
            (fun r -> Util.findsubstring "test" r <> None)
            (Globals.rawRoots())) then
    raise (Util.Fatal 
      "Self-tests can only be run if both roots include the string 'test'");
  if Util.findsubstring "test" (Fspath.toString (Stasher.backupDirectory())) = None then
    raise (Util.Fatal 
      "Self-tests can only be run if the 'backupdir' preference includes the string 'test'");
    
  Lwt_unix.run (Globals.allRootsIter (fun r -> makeRootEmpty r ()));

  let (r2,r1) = Globals.roots () in
  (* Util.msg "r1 = %s  r2 = %s...\n" (Common.root2string r1) (Common.root2string r2); *)

  let put c fs = 
    Lwt_unix.run 
      (match c with
        R1 -> putfs r1 fs | R2 -> putfs r2 fs | BACKUP1 | BACKUP2 -> assert false) in

  let check name c fs =
    debug (fun() -> Util.msg "Checking %s / %s\n" (!currentTest) name);
    let actual =
      Lwt_unix.run 
        ((match c with
          R1 -> getfs r1 | R2 -> getfs r2 | BACKUP1 -> getbackup r1 | BACKUP2 -> getbackup r2) ()) in
    if not (equal actual fs) then
      raise (Util.Fatal (Printf.sprintf
        "Test %s / %s: \nExpected %s = \n  %s\nbut found\n  %s\n"
        (!currentTest) name (checkable2string c) (fs2string fs) (fs2string actual))) in

  Util.warnPrinter := None; 
  Prefs.set Trace.logging false;
  Prefs.set Trace.terse true;
  Trace.sendLogMsgsToStderr := false;

  (* N.b.: When making up tests, it's important to choose file contents of different
     lengths.  The reason for this is that, on Unix systems, it is possible for the
     inode number of a just-deleted file to be reassigned to the very next file
     created -- i.e., to the updated version of the file that the test script has
     just written.  If the length of the contents is also the same and the test is
     running fast enough that the whole thing happens within a second, then the
     update will be missed! *)

  (* Various tests of the backup mechanism *)
  runtest "backups 1" (fun() -> 
    loadPrefs ["backup = Name *"];
    put R1 (Dir []); put R2 (Dir []); sync();
    (* Create a file and a directory *)
    put R1 (Dir ["x", File "foo"; "d", Dir ["a", File "barr"]]); sync();
    (* Delete them *)
    put R1 (Dir []); sync();
    check "1" BACKUP1 (Dir ["x", File "foo"; "d", Dir ["a", File "barr"]]);
    (* Put them back and delete them once more *)
    put R1 (Dir ["x", File "FOO"; "d", Dir ["a", File "BARR"]]); sync();
    put R1 (Dir []); sync();
    check "2" BACKUP1 (Dir [("x", File "FOO"); ("d", Dir [("a", File "BARR")]);
                            (".bak.1.x", File "foo"); (".bak.1.d", Dir [("a", File "barr")])])
  );

  runtest "backups 2" (fun() -> 
    loadPrefs ["backup = Name *"; "backuplocation = local"];
    put R1 (Dir []); put R2 (Dir []); sync();
    (* Create a file and a directory *)
    put R1 (Dir ["x", File "foo"; "d", Dir ["a", File "barr"]]); sync();
    (* Delete them *)
    put R1 (Dir []); sync();
    (* Check that they have been backed up correctly on the other side *)
    check "1" R2 (Dir [(".bak.0.x", File "foo"); (".bak.0.d", Dir [("a", File "barr")])]);
  );

  runtest "backups 3" (fun() -> 
    loadPrefs ["backup = Name *"; "backuplocation = local"; "backupcurrent = Name *"];
    put R1 (Dir []); put R2 (Dir []); sync();
    put R1 (Dir ["x", File "foo"]); sync ();
    check "1a" R1 (Dir [("x", File "foo"); (".bak.0.x", File "foo")]);
    check "1b" R2 (Dir [("x", File "foo"); (".bak.0.x", File "foo")]);
    put R2 (Dir ["x", File "barr"; (".bak.0.x", File "foo")]); sync ();
    check "2a" R1 (Dir [("x", File "barr"); (".bak.1.x", File "foo"); (".bak.0.x", File "barr")]);
    check "2b" R2 (Dir [("x", File "barr"); (".bak.1.x", File "foo"); (".bak.0.x", File "barr")]);
  );

  runtest "backups 4" (fun() -> 
    loadPrefs ["backup = Name *"; "backupcurrent = Name *"; "maxbackups = 7"];
    put R1 (Dir []); put R2 (Dir []); sync();
    put R1 (Dir ["x", File "foo"]); sync();
    check "1a" BACKUP1 (Dir [("x", File "foo")]);
    put R1 (Dir ["x", File "barr"]); sync();
    check "1b" BACKUP1 (Dir [("x", File "barr"); (".bak.1.x", File "foo")]);
    put R2 (Dir ["x", File "bazzz"]); sync();
    check "1c" BACKUP1 (Dir [("x", File "bazzz"); (".bak.2.x", File "foo"); (".bak.1.x", File "barr")]);
  );

  runtest "backups 5 (directories)" (fun() -> 
    loadPrefs ["backup = Name *"; "backupcurrent = Name *"; "maxbackups = 7"];
    put R1 (Dir []); put R2 (Dir []); sync();
    put R1 (Dir ["x", Dir ["a", File "foo"; "l", File "./foo"]]); sync();
    check "1" BACKUP1 (Dir [("x", Dir [("l", File "./foo"); ("a", File "foo")])]);
    put R2 (Dir ["x", Dir ["b", File "barr"; "l", File "./barr"]]); sync();
    check "2" BACKUP1 (Dir [("x", Dir [("l", File "./barr"); ("b", File "barr"); ("a", File "foo"); (".bak.1.l", File "./foo")])]);
    put R1 (Dir ["x", File "bazzz"]); sync();
    check "3" BACKUP1 (Dir [("x", File "bazzz"); (".bak.2.x", Dir [("l", File "./barr"); ("b", File "barr"); ("a", File "foo"); (".bak.1.l", File "./foo")]); (".bak.1.x", Dir [("l", File "./barr"); ("b", File "barr")])]);
  );

  runtest "backups 6 (backup prefix/suffix)" (fun() -> 
    loadPrefs ["backup = Name *"; 
               "backuplocation = local";
               "backupprefix = back/$VERSION-";
               "backupsuffix = .backup";
               "backupcurrent = Name *"];
    Stasher.initBackups();
    put R1 (Dir []); put R2 (Dir []); sync();
    put R1 (Dir ["x", File "foo"]); sync();
    check "1" R1 (Dir [("x", File "foo"); ("back", Dir [("0-x.backup", File "foo")])]);
  );

  if Util.osType <> `Win32 then begin
    runtest "links 1 (directories and links)" (fun() -> 
      loadPrefs ["backup = Name *"; "backupcurrent = Name *"; "maxbackups = 7"];
      put R1 (Dir []); put R2 (Dir []); sync();
      put R1 (Dir ["x", Dir ["a", File "foo"; "l", Link "./foo"]]); sync();
      check "1" BACKUP1 (Dir [("x", Dir [("l", Link "./foo"); ("a", File "foo")])]);
      put R2 (Dir ["x", Dir ["b", File "barr"; "l", Link "./barr"]]); sync();
      check "2" BACKUP1 (Dir [("x", Dir [("l", Link "./barr"); ("b", File "barr"); ("a", File "foo"); (".bak.1.l", Link "./foo")])]);
      put R1 (Dir ["x", File "bazzz"]); sync();
      check "3" BACKUP1 (Dir [("x", File "bazzz"); (".bak.2.x", Dir [("l", Link "./barr"); ("b", File "barr"); ("a", File "foo"); (".bak.1.l", Link "./foo")]); (".bak.1.x", Dir [("l", Link "./barr"); ("b", File "barr")])]);
    );

    (* Test that we correctly fail when we try to 'follow' a symlink that does not
       point to anything *)
    runtest "links 2 (symlink to nowhere)" (fun() -> 
      loadPrefs ["follow = Name y"];
      let orig = (Dir []) in
      put R1 orig; put R2 orig; sync();
      put R1 (Dir ["y", Link "x"]); sync();
      check "1" R2 orig;
    );
  end;

  Util.msg "Success :-)\n";
  ()

(* Initialization: tie the knot between this module and Uicommon *)
let _ = (Uicommon.testFunction := test)
