(* $I1: Unison file synchronizer: src/xferhint.ml $ *)
(* $I2: Last modified by bcpierce on Sun, 22 Aug 2004 22:29:04 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

let debug = Trace.debug "xferhint"

let xferbycopying =
  Prefs.createBool "xferbycopying" true
    "optimize transfers using local copies, if possible"
    ("When this preference is set, Unison will try to avoid transferring "
   ^ "file contents across the network by recognizing when a file with the "
   ^ "required contents already exists in the target replica.  This usually "
   ^ "allows file moves to be propagated very quickly.  The default value is"
   ^ "\\texttt{true}.  ")

module PathMap =
  Hashtbl.Make
    (struct
       type t = Fspath.t * Path.local
       let hash (fspath, path) =
         (Hashtbl.hash (Fspath.toString fspath) + 13217 * Path.hash path)
           land
         0x3FFFFFFF
       let equal = (=)
     end)
module FPMap =
  Hashtbl.Make
    (struct
       type t = Os.fullfingerprint
       let hash = Hashtbl.hash
       let equal = (=)
     end)

(* map(path, fingerprint) *)
let path2fingerprintMap =  PathMap.create 101
(* map(fingerprint, path) *)
let fingerprint2pathMap = FPMap.create 101

(*  Now we don't clear it out anymore
let initLocal () =
  debug (fun () -> Util.msg "initLocal\n");
  path2fingerprintMap := PathMap.empty;
  fingerprint2pathMap := FPMap.empty
*)

let lookup fp =
  assert (Prefs.read xferbycopying);
  debug (fun () ->
    Util.msg "lookup: fp = %s\n" (Os.fullfingerprint_to_string fp));
  try
    Some (FPMap.find fingerprint2pathMap fp)
  with Not_found ->
    None

let insertEntry p fp =
  if Prefs.read xferbycopying then begin
    debug (fun () ->
      let (fspath, path) = p in
      Util.msg "insertEntry: fspath=%s, path=%s, fp=%s\n"
        (Fspath.toString fspath)
        (Path.toString path) (Os.fullfingerprint_to_string fp));
    PathMap.replace path2fingerprintMap p fp;
    FPMap.replace fingerprint2pathMap fp p
  end

let deleteEntry p =
  if Prefs.read xferbycopying then begin
    debug (fun () ->
      let (fspath, path) = p in
      Util.msg "deleteEntry: fspath=%s, path=%s\n"
        (Fspath.toString fspath) (Path.toString path));
    try
      let fp = PathMap.find path2fingerprintMap p in
      PathMap.remove path2fingerprintMap p;
      let p' = FPMap.find fingerprint2pathMap fp in
      (* Maybe we should do this unconditionally *)
      if p' = p then FPMap.remove fingerprint2pathMap fp
    with Not_found ->
      ()
  end

let renameEntry pOrig pNew =
  if Prefs.read xferbycopying then begin
    debug (fun () ->
      let (fspathOrig, pathOrig) = pOrig in
      let (fspathNew, pathNew) = pNew in
      Util.msg "renameEntry: fsOrig=%s, pOrig=%s, fsNew=%s, pNew=%s\n"
        (Fspath.toString fspathOrig) (Path.toString pathOrig)
        (Fspath.toString fspathNew) (Path.toString pathNew));
    try
      let fp = PathMap.find path2fingerprintMap pOrig in
      PathMap.remove path2fingerprintMap pOrig;
      PathMap.replace path2fingerprintMap pNew fp;
      FPMap.replace fingerprint2pathMap fp pNew
    with Not_found ->
      ()
  end
