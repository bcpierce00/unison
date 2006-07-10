(* Unison file synchronizer: src/test.ml *)
(* $Id: $ *)
(* Copyright 1999-2006 (see COPYING for details) *)

let (>>=)  = Lwt.(>>=)

type fs =
  | File of string
  | Link of string
  | Dir of (string * fs) list

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
       Lwt.return (remove_file_or_dir (Fspath.toString (Stasher.backupDirectory ()))))

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

let sync() = 
  let (reconItemList, _, _) =
    Recon.reconcileAll (Update.findUpdates()) in
  Lwt_unix.run (
    Lwt_util.iter
      (fun ri ->
         Transport.transportItem ri
           (Uutil.File.ofLine 0) (fun _ _ _ -> true))
      reconItemList);
  Update.commitUpdates()

let runtest f =
  Util.convertUnixErrorsToFatal "Test.test" (fun() -> 
    let savedPrefs = Prefs.dump() in
    f();
    Prefs.load savedPrefs)

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

  let (r1,r2) = Globals.roots () in
  Util.msg "r1 = %s  r2 = %s...\n" (Common.root2string r1) (Common.root2string r2);

  Util.warnPrinter := None; 
  Prefs.set Trace.logging false;
  Prefs.set Trace.terse true;
  Trace.sendLogMsgsToStderr := false;

  (* Test that we correctly fail when we try to 'follow' a symlink that does not
     point to anything *)
  runtest (fun() -> 
    loadPrefs ["follow = Name y"];
    let orig = (Dir []) in
    Lwt_unix.run (putfs r1 orig);
    Lwt_unix.run (putfs r2 orig);
    sync();
    Lwt_unix.run (putfs r1 (Dir ["y", Link "x"]));
    sync();
    
    assert ((Lwt_unix.run (getfs r2 ())) = orig);
  );

  (* Various tests of the backup mechanism *)
  Lwt_unix.run (Globals.allRootsIter (fun r -> makeBackupEmpty r ()));
  runtest (fun() -> 
    Util.msg "\nTest 2\n";
    loadPrefs
      ["debug = all";
       "backup = Name *"];
    Util.msg "Original sync (empty)\n";
    let orig = (Dir []) in
    Lwt_unix.run (putfs r1 orig);
    Lwt_unix.run (putfs r2 orig);
    sync();
    Util.msg "Add x\n";
    Lwt_unix.run (putfs r1 (Dir ["x", File "foo"]));
    sync();
    Util.msg "Delete x\n";
    Lwt_unix.run (putfs r1 (Dir []));
    sync();

    assert ((Lwt_unix.run (getbackup r1 ())) = orig);
  );

  Util.msg "\nSuccess :-)\n";
  ()
