(* Unison file synchronizer: src/test.ml *)
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


let (>>=) = Lwt.(>>=)

(* ---------------------------------------------------------------------- *)
(* Utility functions *)

let debug = Trace.debug "test"
let verbose = Trace.debug "test"

let rec remove_file_or_dir d =
  match try Some(Fs.lstat d) with Unix.Unix_error((Unix.ENOENT | Unix.ENOTDIR),_,_) -> None with
  | Some(s) ->
    if s.Unix.LargeFile.st_kind = Unix.S_DIR then begin
      let handle = Fs.opendir d in
      let rec loop () =
        let r = try Some(handle.Fs.readdir ()) with End_of_file -> None in
        match r with
        | Some f ->
            if f="." || f=".." then loop ()
            else begin
              remove_file_or_dir (Fspath.concat d (Path.fromString f));
              loop ()
            end
        | None ->
            handle.Fs.closedir ();
            Fs.rmdir d
      in loop ()
    end else
      Fs.unlink d
  | None -> ()

let read_chan chan =
  let nbytes = in_channel_length chan in
  let string = Bytes.create nbytes in
  really_input chan string 0 nbytes;
  string

let read file =
(*
  if file = "-" then
    read_chan stdin
  else
*)
    let chan = Fs.open_in_bin file in
    try
      let r = read_chan chan in
      close_in chan;
      r
    with exn ->
      close_in chan;
      raise exn

let write file s =
(*
  if file = "-" then
    output_string stdout s
  else
*)
    let chan =
      Fs.open_out_gen
        [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o600 file in
    try
      output_string chan s;
      close_out chan
    with exn ->
      close_out chan;
      raise exn

let read_dir d =
  let ignored = ["."; ".."] in
  let d = Fs.opendir d in
  let rec do_read acc =
    try
      (match (d.Fs.readdir ()) with
       | s when Safelist.mem s ignored -> do_read acc
       | f -> do_read (f :: acc))
    with End_of_file -> acc
  in
  let files = do_read [] in
  d.Fs.closedir ();
  files

let extend p file = Fspath.concat p (Path.fromString file)

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

let fsopt2string = function
    None -> "MISSING"
  | Some(f) -> fs2string f

let readfs p =
  let rec loop p =
    let s = Fs.lstat p in
    match s.Unix.LargeFile.st_kind with
      | Unix.S_REG -> File (Bytes.to_string (read p))
      | Unix.S_LNK -> Link (Fs.readlink p)
      | Unix.S_DIR -> Dir (Safelist.map (fun x -> (x, loop (extend p x))) (read_dir p))
      | _ -> assert false
  in try Some(loop p) with
    Unix.Unix_error (Unix.ENOENT,_,_) -> None

let default_perm = 0o755

let writefs p fs =
  verbose (fun() -> Util.msg "Writing new test filesystem\n");
  let rec loop p = function
    | File s ->
        verbose (fun() -> Util.msg "Writing %s with contents %s (fingerprint %s)\n"
                   (Fspath.toDebugString p) s (Fingerprint.toString (Fingerprint.string s)));
        write p s
    | Link s -> Fs.symlink s p
    | Dir files ->
        Fs.mkdir p default_perm;
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
             (Fspath.toPrintString fspath)));
       Lwt.return ())

let makeRootEmpty : Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "makeRootEmpty"
    (fun (fspath, ()) ->
       remove_file_or_dir fspath;
       Lwt.return ())

let getfs : Common.root -> unit -> (fs option) Lwt.t =
  Remote.registerRootCmd
    "getfs"
    (fun (fspath, ()) ->
       Lwt.return (readfs fspath))

let getbackup : Common.root -> unit -> (fs option) Lwt.t =
  Remote.registerRootCmd
    "getbackup"
    (fun (fspath, ()) ->
       Lwt.return (readfs (Stasher.backupDirectory ())))

let makeBackupEmpty : Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "makeBackupEmpty"
    (fun (fspath, ()) ->
       let b = Stasher.backupDirectory () in
       debug (fun () -> Util.msg "Removing %s\n" (Fspath.toDebugString b));
       Lwt.return (remove_file_or_dir b))

let putfs : Common.root -> fs -> unit Lwt.t =
  Remote.registerRootCmd
    "putfs"
    (fun (fspath, fs) ->
       writefs fspath fs;
       Lwt.return ())

let loadPrefs l =
  Prefs.loadStrings l;
  Lwt_unix.run (Globals.propagatePrefs ());
  Stasher.initBackups()

(* ---------------------------------------------------------------------------- *)

let displayRis ris =
  Safelist.iter
    (fun ri ->
      Util.msg "%s\n" (Uicommon.reconItem2string Path.empty ri ""))
    ris

let minisleep (sec: float) =
  ignore (Unix.select [] [] [] sec)

let sync ?(verbose=false) () =
  let (reconItemList, _, _) =
    Recon.reconcileAll (Update.findUpdates None) in
  if verbose then begin
    Util.msg "Sync result:\n";
    displayRis reconItemList
  end;
  minisleep 0.1;
  Lwt_unix.run (
    Lwt_util.iter
      (fun ri ->
         Transport.transportItem ri
           (Uutil.File.ofLine 0) (fun _ _ -> true))
      reconItemList);
  Update.commitUpdates()

let currentTest = ref ""

type checkable = R1 | R2 | BACKUP1 | BACKUP2

let checkable2string = function
  R1 -> "R1" | R2 -> "R2" | BACKUP1 -> "BACKUP1" | BACKUP2 -> "BACKUP2"

let test() =
  Util.warnPrinter := None;
  Prefs.set Trace.logging false;
  Prefs.set Trace.terse true;
  Trace.sendLogMsgsToStderr := false;

  let origPrefs = Prefs.dump() in

  let runtest name prefs f =
    Util.msg "%s...\n" name;
    Util.convertUnixErrorsToFatal "Test.test" (fun() ->
      currentTest := name;
      Prefs.load origPrefs;
      loadPrefs prefs;
      debug (fun() -> Util.msg "Emptying backup directory\n");
      Lwt_unix.run (Globals.allRootsIter (fun r -> makeBackupEmpty r ()));
      debug (fun() -> Util.msg "Running test\n");
      f();
    ) in

  Util.msg "Running internal tests...\n";

  (* Paranoid checks, to make sure we do not delete anybody's filesystem! *)
  if not (Safelist.for_all
            (fun r -> Util.findsubstring "test" r <> None)
            (Globals.rawRoots())) then
    raise (Util.Fatal
      "Self-tests can only be run if both roots include the string 'test'");
  if Util.findsubstring "test" (Fspath.toPrintString (Stasher.backupDirectory())) = None then
    raise (Util.Fatal
        ("Self-tests can only be run if the 'backupdir' preference (or wherever the backup "
       ^ "directory name is coming from, e.g. the UNISONBACKUPDIR environment variable) "
       ^ "includes the string 'test'"));

  Lwt_unix.run (Globals.allRootsIter (fun r -> makeRootEmpty r ()));

  let (r2,r1) = Globals.roots () in
  (* Util.msg "r1 = %s  r2 = %s...\n" (Common.root2string r1) (Common.root2string r2); *)
  let bothRootsLocal =
    match (r1,r2) with
      (Common.Local,_),(Common.Local,_) -> true
    | _ -> false in

  let put c fs =
    Lwt_unix.run
      (match c with
        R1 -> putfs r1 fs | R2 -> putfs r2 fs | BACKUP1 | BACKUP2 -> assert false) in

  let failures = ref 0 in

  let check name c fs =
    debug (fun() -> Util.msg "Checking %s / %s\n" (!currentTest) name);
    let actual =
        Lwt_unix.run
          ((match c with
            R1 -> getfs r1 | R2 -> getfs r2 | BACKUP1 -> getbackup r1 | BACKUP2 -> getbackup r2) ())  in
    let fail () =
      Util.msg
        "Test %s / %s: \nExpected %s = \n  %s\nbut found\n  %s\n"
        (!currentTest) name (checkable2string c) (fs2string fs) (fsopt2string actual);
      failures := !failures+1;
      raise (Util.Fatal (Printf.sprintf "Self-test %s / %s failed!" (!currentTest) name)) in
    match actual with
        Some(a) -> if not (equal a fs) then fail()
      | None -> fail() in

  let checkmissing name c =
    debug (fun() -> Util.msg "Checking nonexistence %s / %s\n" (!currentTest) name);
    let actual =
      Lwt_unix.run
        ((match c with
          R1 -> getfs r1 | R2 -> getfs r2 | BACKUP1 -> getbackup r1 | BACKUP2 -> getbackup r2) ()) in
    if  actual <> None then begin
      Util.msg
        "Test %s / %s: \nExpected %s MISSING\nbut found\n  %s\n"
        (!currentTest) name (checkable2string c) (fsopt2string actual);
      failures := !failures+1;
      raise (Util.Fatal (Printf.sprintf "Self-test %s / %s failed!" (!currentTest) name))
    end in

  (* N.b.: When making up tests, it's important to choose file contents of different
     lengths.  The reason for this is that, on some Unix systems, it is possible for
     the inode number of a just-deleted file to be reassigned to the very next file
     created -- i.e., to the updated version of the file that the test script has
     just written.  If the length of the contents is also the same and the test is
     running fast enough that the whole thing happens within a second, then the
     update will be missed! *)

  (* Test that .git is treated atomically. *)
  runtest "Atomicity of certain directories 1" ["atomic = Name .git";
                                                "force = newer"] (fun() ->
      let orig = (Dir ["foo", Dir [".git", Dir ["a", File "foo";
                                                "b", File "bar";
                                                "c", File "baz"]]]) in
      put R1 orig;
      Unix.sleep 2; (* in case time granularity is coarse on this FS *)
      put R2 orig; sync();
      let expected = (Dir ["foo", Dir [".git", Dir ["a", File "modified on R1";
                                                    "b", File "bar";
                                                    "c", File "modified on R1"]]]) in
      put R2 (Dir ["foo", Dir [".git",
                               Dir ["a", File "foo";
                                    "b", File "modified on R2";
                                    "c", File "modified on R2"]]]);
      Unix.sleep 2; 
      put R1 expected;
      sync ();
      check "1" R2 expected;
      check "2" R1 expected
    );

  runtest "Atomicity of certain directories 2" ["atomic = Name .git"] (fun() ->
      let a = (Dir ["foo", Dir [".git", Dir ["a", File "foo";
                                             "b", File "bar";
                                             "c", File "baz";
                                             "d", File "quux"]]]) in
      let b = (Dir ["foo", Dir [".git", Dir ["a", File "foo";
                                             "b", File "bar";
                                             "c", File "baz";
                                             "e", File "quux"]]]) in
      put R1 a; put R2 b; sync();
      check "1" R1 a;
      check "2" R2 b
    );

  (* Check for the bug reported by Ralf Lehmann *)
  if not bothRootsLocal then
    runtest "backups 1 (remote)" ["backup = Name *"] (fun() ->
      put R1 (Dir []); put R2 (Dir []); sync();
      debug (fun () -> Util.msg "First check\n");
      checkmissing "1" BACKUP1;
      checkmissing "2" BACKUP2;
      (* Create a file *)
      put R1 (Dir ["test.txt", File "1"]); sync();
      checkmissing "3" BACKUP1;
      checkmissing "4" BACKUP2;
      (* Change it and check that the old version got backed up on the target host *)
      put R1 (Dir ["test.txt", File "2"]); sync();
      checkmissing "5" BACKUP1;
      check "6" BACKUP2 (Dir [("test.txt", File "1")]);
    );

  if bothRootsLocal then
    runtest "fastercheckUNSAFE 1" ["fastercheckUNSAFE = true"] (fun() ->
      put R1 (Dir []); put R2 (Dir []); sync();
      (* Create a file on both sides with different contents *)
      put R1 (Dir ["x", File "foo"]);
      put R2 (Dir ["x", File "bar"]); sync();
      check "1a" R1 (Dir ["x", File "foo"]);
      check "1b" R2 (Dir ["x", File "bar"]);
      (* Change contents on one side and see that we do NOT get a conflict (!) *)
      put R1 (Dir ["x", File "newcontents"]); sync();
      check "2a" R1 (Dir ["x", File "newcontents"]);
      check "2b" R2 (Dir ["x", File "newcontents"]);

      (* Start again *)
      put R1 (Dir []); put R2 (Dir []); sync();
      (* Create a file on both sides with different contents *)
      put R1 (Dir ["x", File "foo"]);
      put R2 (Dir ["x", File "bar"]); sync();
      (* Change contents without changing size and check that change is propagated *)
      put R1 (Dir ["x", File "f00"]); sync();

      check "3a" R1 (Dir ["x", File "f00"]);
      check "3b" R2 (Dir ["x", File "f00"]);

      (* Start again *)
      put R1 (Dir []); put R2 (Dir []); sync();
      (* Create a new file on one side only *)
      put R1 (Dir ["x", File "foo"]); sync();
      (* Check that change is propagated *)
      check "4" R2 (Dir ["x", File "foo"]);
    );

  if bothRootsLocal then
    runtest "backups 1 (local)" ["backup = Name *"] (fun() ->
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

  runtest "backups 2" ["backup = Name *"; "backuplocation = local"] (fun() ->
    put R1 (Dir []); put R2 (Dir []); sync();
    (* Create a file and a directory *)
    put R1 (Dir ["x", File "foo"; "d", Dir ["a", File "barr"]]); sync();
    (* Delete them *)
    put R1 (Dir []); sync();
    (* Check that they have been backed up correctly on the other side *)
    check "1" R2 (Dir [(".bak.0.x", File "foo"); (".bak.0.d", Dir [("a", File "barr")])]);
  );

  runtest "backups 2a" ["backup = Name *"; "backuplocation = local"] (fun() ->
    put R1 (Dir []); put R2 (Dir []); sync();
    (* Create a file and a directory *)
    put R1 (Dir ["foo", File "1"]); sync();
    check "1" R1 (Dir [("foo", File "1")]);
    check "2" R1 (Dir [("foo", File "1")]);
    put R1 (Dir ["foo", File "2"]); sync();
    check "3" R1 (Dir [("foo", File "2")]);
    check "4" R2 (Dir [("foo", File "2"); (".bak.0.foo", File "1")]);
  );

  runtest "backups 3" ["backup = Name *"; "backuplocation = local"; "backupcurrent = Name *"] (fun() ->
    put R1 (Dir []); put R2 (Dir []); sync();
    put R1 (Dir ["x", File "foo"]); sync ();
    check "1a" R1 (Dir [("x", File "foo"); (".bak.0.x", File "foo")]);
    check "1b" R2 (Dir [("x", File "foo"); (".bak.0.x", File "foo")]);
    put R2 (Dir ["x", File "barr"; (".bak.0.x", File "foo")]); sync ();
    check "2a" R1 (Dir [("x", File "barr"); (".bak.1.x", File "foo"); (".bak.0.x", File "barr")]);
    check "2b" R2 (Dir [("x", File "barr"); (".bak.1.x", File "foo"); (".bak.0.x", File "barr")]);
  );

  runtest "backups 4" ["backup = Name *"; "backupcurrent = Name *"; "maxbackups = 7"] (fun() ->
    put R1 (Dir []); put R2 (Dir []); sync();
    put R1 (Dir ["x", File "foo"]); sync();
    check "1a" BACKUP1 (Dir [("x", File "foo")]);
    put R1 (Dir ["x", File "barr"]); sync();
    check "1b" BACKUP1 (Dir [("x", File "barr"); (".bak.1.x", File "foo")]);
    put R2 (Dir ["x", File "bazzz"]); sync();
    check "1c" BACKUP1 (Dir [("x", File "bazzz"); (".bak.2.x", File "foo"); (".bak.1.x", File "barr")]);
  );

  runtest "backups 5 (directories)" ["backup = Name *"; "backupcurrent = Name *"; "maxbackups = 7"] (fun() ->
    put R1 (Dir []); put R2 (Dir []); sync();
    (* Create a directory x containing files a and l; check that the current version gets backed up *)
    put R1 (Dir ["x", Dir ["a", File "foo"; "l", File "./foo"]]); sync();
    check "1" BACKUP1 (Dir [("x", Dir [("l", File "./foo"); ("a", File "foo")])]);
    (* On replica 2, delete file a, create file b, and edit file l *)
    put R2 (Dir ["x", Dir ["b", File "barr"; "l", File "./barr"]]); sync();
    check "2" BACKUP1 (Dir [("x", Dir [("l", File "./barr"); ("b", File "barr"); ("a", File "foo"); (".bak.1.l", File "./foo")])]);
    (* On replica 1, replace the whole directory by a file; when we check the result, we need to know
       whether we're running the test locally or remotely; in the former case, we should see *both* the
       old and the new version as backups *)
    put R1 (Dir ["x", File "bazzz"]); sync();
    if bothRootsLocal then
      check "3" BACKUP1 (Dir [("x", File "bazzz"); (".bak.2.x", Dir [("l", File "./barr"); ("b", File "barr"); ("a", File "foo"); (".bak.1.l", File "./foo")]); (".bak.1.x", Dir [("l", File "./barr"); ("b", File "barr")])])
    else
      check "3" BACKUP1 (Dir [("x", File "bazzz"); (".bak.1.x", Dir [("l", File "./barr"); ("b", File "barr"); ("a", File "foo"); (".bak.1.l", File "./foo")])]);
  );

  runtest "backups 6 (backup prefix/suffix)" ["backup = Name *";
               "backuplocation = local";
               "backupprefix = back/$VERSION-";
               "backupsuffix = .backup";
               "backupcurrent = Name *"] (fun() ->
    put R1 (Dir []); put R2 (Dir []); sync();
    put R1 (Dir ["x", File "foo"]); sync();
    check "1" R1 (Dir [("x", File "foo"); ("back", Dir [("0-x.backup", File "foo")])]);
  );

  if not (Prefs.read Globals.someHostIsRunningWindows) then begin
    runtest "links 1 (directories and links)" ["backup = Name *"; "backupcurrent = Name *"; "maxbackups = 7"] (fun() ->
      put R1 (Dir []); put R2 (Dir []); sync();
      put R1 (Dir ["x", Dir ["a", File "foo"; "l", Link "./foo"]]); sync();
      check "1" BACKUP1 (Dir [("x", Dir [("l", Link "./foo"); ("a", File "foo")])]);
      put R2 (Dir ["x", Dir ["b", File "barr"; "l", Link "./barr"]]); sync();
      check "2" BACKUP1 (Dir [("x", Dir [("l", Link "./barr"); ("b", File "barr"); ("a", File "foo"); (".bak.1.l", Link "./foo")])]);
      put R1 (Dir ["x", File "bazzz"]); sync();
      if bothRootsLocal then
        check "3" BACKUP1
          (Dir [("x", File "bazzz");
                (".bak.2.x", Dir [("l", Link "./barr"); ("b", File "barr"); ("a", File "foo");
                                  (".bak.1.l", Link "./foo")]);
                (".bak.1.x", Dir [("l", Link "./barr"); ("b", File "barr")])])
      else
        check "3" BACKUP1
          (Dir [("x", File "bazzz");
                (".bak.1.x", Dir [("l", Link "./barr"); ("b", File "barr");
                                  ("a", File "foo"); (".bak.1.l", Link "./foo")])]);
    );

    (* Test that we correctly fail when we try to 'follow' a symlink that does not
       point to anything *)
    runtest "links 2 (symlink to nowhere)" ["follow = Name y"] (fun() ->
      let orig = (Dir []) in
      put R1 orig; put R2 orig; sync();
      put R1 (Dir ["y", Link "x"]); sync();
      check "1" R2 orig;
    );

    (* Check for the bug reported by Sebastian Elsner (Jan 2018) *)
    (* NOT POSSIBLE because the test API does not enable one to play with file
       owners, but I put the test here anyway. *)
    (*
    runtest "owner of path directories" ["owner"; "path = a/b"] (fun() ->
      put R1 (Dir ["a", Dir ["b", Dir["foo", File "Foo";
                                      "bar", File "Bar";
                                      "baz", File "Baz";]]]]);
      setOwner R1 "a/b" "testuser";  (* does not exist *)
      put R2 (Dir []);
      sync();
      checkOwner "1" R2 "a/b" "testuser";  (* does not exist *)
    );
    *)
  end;

  if !failures = 0 then
    Util.msg "Success :-)\n"
  else
    raise (Util.Fatal "Self-tests failed\n")

(* Initialization: tie the knot between this module and Uicommon *)
let _ = (Uicommon.testFunction := test)
