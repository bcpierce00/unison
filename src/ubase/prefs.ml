(* Unison file synchronizer: src/ubase/prefs.ml *)
(* $I3: Copyright 1999-2002 (see COPYING for details) $ *)

let debug = Util.debug "prefs"

type 'a t =
  { mutable value : 'a; defaultValue : 'a; mutable names : string list;
    mutable setInProfile : bool }

let read p = p.value

let set p v = p.setInProfile <- true; p.value <- v

let overrideDefault p v = if not p.setInProfile then p.value <- v

let name p = p.names

let readDefault p = p.defaultValue

let rawPref default name =
  { value = default; defaultValue = default; names = [name];
    setInProfile = false }

(* ------------------------------------------------------------------------- *)

let profileName = ref None
let profileFiles = ref []

let profilePathname n =
  let f = Util.fileInUnisonDir n in
  if System.file_exists f then f
  else Util.fileInUnisonDir (n ^ ".prf")

let thePrefsFile () = 
  match !profileName with
    None -> raise (Util.Transient("No preference file has been specified"))
  | Some(n) -> profilePathname n

let profileUnchanged () =
  List.for_all
    (fun (path, info) ->
       try
         let newInfo = System.stat path in
         newInfo.Unix.LargeFile.st_kind = Unix.S_REG &&
         info.Unix.LargeFile.st_mtime = newInfo.Unix.LargeFile.st_mtime &&
         info.Unix.LargeFile.st_size = newInfo.Unix.LargeFile.st_size
       with Unix.Unix_error _ ->
         false)
    !profileFiles

(* ------------------------------------------------------------------------- *)

(* When preferences change, we need to dump them out to the file we loaded   *)
(* them from.  This is accomplished by associating each preference with a    *)
(* printing function.                                                        *)

let printers = ref ([] : (string * (unit -> string list)) list)

let addprinter name f = printers := (name, f) :: !printers

(* ---------------------------------------------------------------------- *)

(* When we load a new profile, we need to reset all preferences to their     *)
(* default values.  Each preference has a resetter for doing this.           *)

let resetters = ref [] 

let addresetter f = resetters := f :: !resetters

let resetToDefaults () =
  Safelist.iter (fun f -> f()) !resetters; profileFiles := []

(* ------------------------------------------------------------------------- *)

(* When the server starts up, we need to ship it the current state of all    *)
(* the preference settings.  This is accomplished by dumping them on the     *)
(* client side and loading on the server side; as each preference is         *)
(* created, a dumper (marshaler) and a loader (parser) are added to the list *)
(* kept here...                                                              *)

type dumpedPrefs = (string * bool * string) list

let dumpers = ref ([] : (string * bool * (unit->string)) list)
let loaders = ref (Util.StringMap.empty : (string->unit) Util.StringMap.t)

let adddumper name optional f =
  dumpers := (name,optional,f) :: !dumpers

let addloader name f =
  loaders := Util.StringMap.add name f !loaders

let dump () = Safelist.map (fun (name, opt, f) -> (name, opt, f())) !dumpers

let load d =
  Safelist.iter
    (fun (name, opt, dumpedval) ->
       match
         try Some (Util.StringMap.find name !loaders) with Not_found -> None
       with
         Some loaderfn ->
           loaderfn dumpedval
       | None ->
           if not opt then
             raise (Util.Fatal
                      ("Preference "^name^" not found: \
                        inconsistent Unison versions??")))
    d

(* For debugging *)
let dumpPrefsToStderr() =
  Printf.eprintf "Preferences:\n";
  Safelist.iter
    (fun (name,f) ->
       Safelist.iter
         (fun s -> Printf.eprintf "%s = %s\n" name s)
         (f()))
    !printers

(* ------------------------------------------------------------------------- *)

(* Each preference is associated with a handler function taking an argument  *)
(* of appropriate type.  These functions should raise IllegalValue if they   *)
(* are invoked with a value that falls outside the range they expect.  This  *)
(* exception will be caught within the preferences module and used to        *)
(* generate an appropriate usage message.                                    *)
exception IllegalValue of string

(* aliasMap: prefName -> prefName *)
let aliasMap = ref (Util.StringMap.empty : string Util.StringMap.t)

let canonicalName nm =
  try Util.StringMap.find nm !aliasMap with Not_found -> nm

type typ =
  [`BOOL | `INT | `STRING | `STRING_LIST | `BOOLDEF | `CUSTOM | `UNKNOWN]

(* prefType : prefName -> type *)
let prefType = ref (Util.StringMap.empty : typ Util.StringMap.t)

let typ nm = try Util.StringMap.find nm !prefType with Not_found -> `UNKNOWN

(* prefs: prefName -> (doc, pspec, fulldoc)                                  *)
let prefs =
  ref (Util.StringMap.empty : (string * Uarg.spec * string) Util.StringMap.t)

let documentation nm =
  try
    let (doc, _, fulldoc) = Util.StringMap.find nm !prefs in
    if doc <> "" && doc.[0] = '*' then raise Not_found;
    let basic = doc = "" || doc.[0] <> '!' in
    let doc =
      if not basic then
        String.sub doc 1 (String.length doc - 1)
      else
        doc
    in
    (doc, fulldoc, basic)
  with Not_found ->
    ("", "", false)

let list () =
  List.sort String.compare
    (Util.StringMap.fold (fun nm _ l -> nm :: l) !prefType [])

(* aliased pref has *-prefixed doc and empty fulldoc                         *)
let alias pref newname =
  (* pref must have been registered, so name pref is not empty, and will be *)
  (* found in the map, no need for catching exception                       *)
  let (_,pspec,_) = Util.StringMap.find (Safelist.hd (name pref)) !prefs in
  prefs := Util.StringMap.add newname ("*", pspec, "") !prefs;
  aliasMap := Util.StringMap.add newname (Safelist.hd (name pref)) !aliasMap;
  pref.names <- newname :: pref.names

let registerPref name typ pspec doc fulldoc =
  if Util.StringMap.mem name !prefs then
    raise (Util.Fatal ("Preference " ^ name ^ " registered twice"));
  prefs := Util.StringMap.add name (doc, pspec, fulldoc) !prefs;
  (* Ignore internal preferences *)
  if doc = "" || doc.[0] <> '*' then
    prefType := Util.StringMap.add name typ !prefType

let createPrefInternal name typ local default doc fulldoc printer parsefn =
  let newCell = rawPref default name in
  registerPref name typ (parsefn newCell) doc fulldoc;
  adddumper name local
    (fun () -> Marshal.to_string (newCell.value, newCell.names) []);
  addprinter name (fun () -> printer newCell.value);
  addresetter
    (fun () ->
       newCell.setInProfile <- false; newCell.value <- newCell.defaultValue);
  addloader name
    (fun s ->
       let (value, names) = Marshal.from_string s 0 in
       newCell.value <- value);
  newCell

let create name ?(local=false) default doc fulldoc intern printer =
  createPrefInternal name `CUSTOM local default doc fulldoc printer
    (fun cell -> Uarg.String (fun s -> set cell (intern (read cell) s)))

let createBool name ?(local=false) default doc fulldoc =
  let doc = if default then doc ^ " (default true)" else doc in
  createPrefInternal name `BOOL local default doc fulldoc
    (fun v -> [if v then "true" else "false"])
    (fun cell -> Uarg.Bool (fun b -> set cell b))

let createInt name ?(local=false) default doc fulldoc =
  createPrefInternal name `INT local default doc fulldoc
    (fun v -> [string_of_int v])
    (fun cell -> Uarg.Int (fun i -> set cell i))

let createString name ?(local=false) default doc fulldoc =
  createPrefInternal name `STRING local default doc fulldoc
    (fun v -> [v])
    (fun cell -> Uarg.String (fun s -> set cell s))

let createFspath name ?(local=false) default doc fulldoc =
  createPrefInternal name `STRING local default doc fulldoc
    (fun v -> [System.fspathToString v])
    (fun cell -> Uarg.String (fun s -> set cell (System.fspathFromString s)))

let createStringList name ?(local=false) doc fulldoc =
  createPrefInternal name `STRING_LIST local [] doc fulldoc
    (fun v -> v)
    (fun cell -> Uarg.String (fun s -> set cell (s:: read cell)))

let createBoolWithDefault name ?(local=false) doc fulldoc =
  createPrefInternal name `BOOLDEF local `Default doc fulldoc
    (fun v -> [match v with
                 `True    -> "true"
               | `False   -> "false"
               | `Default -> "default"])
    (fun cell ->
       Uarg.String
         (fun s ->
            let v =
              match s with
                "yes" | "true"     -> `True
              | "default" | "auto" -> `Default
              | _                  -> `False
            in
            set cell v))

(*****************************************************************************)
(*                      Command-line parsing                                 *)
(*****************************************************************************)

let prefArg = function
    Uarg.Bool(_)   -> ""
  | Uarg.Int(_)    -> "n"
  | Uarg.String(_) -> "xxx"
  | _             -> assert false

let argspecs hook =
  Util.StringMap.fold
    (fun name (doc, pspec, _) l ->
       ("-" ^ name, hook name pspec, "")::l)
    !prefs []

let oneLineDocs u =
  let formatOne name pspec doc p =
    if not p then "" else
    let doc = if doc.[0] = '!'
                then String.sub doc 1 ((String.length doc) - 1)
                else doc in
    let arg = prefArg pspec in
    let arg = if arg = "" then "" else " " ^ arg in
    let spaces =
      String.make (max 1 (18 - String.length (name ^ arg))) ' ' in
    " -" ^ name ^ arg ^ spaces ^ doc ^ "\n" in
  let formatAll p =
    String.concat ""
      (Safelist.rev
         (Util.StringMap.fold 
            (fun name (doc, pspec, _) l ->
               (formatOne name pspec doc
                  (String.length doc > 0 && doc.[0] <> '*' && p doc)) :: l)
            !prefs []))
  in
    u ^ "\n" 
  ^ "Basic options: \n"
  ^ formatAll (fun doc -> doc.[0] <> '!')
  ^ "\nAdvanced options: \n"
  ^ formatAll (fun doc -> doc.[0] = '!')

let printUsage usage = Uarg.usage (argspecs (fun _ s -> s))
                         (oneLineDocs usage)

let processCmdLine usage hook =
  Uarg.current := 0;
  let argspecs = argspecs hook in
  let defaultanonfun _ =
    print_string "Anonymous arguments not allowed\n";
    Uarg.usage argspecs (oneLineDocs usage);
    exit 2
  in
  let anonfun =
    try
      let (_, p, _) = Util.StringMap.find "rest" !prefs in 
      match hook "rest" p with
        Uarg.String stringFunction -> stringFunction
      | _                         -> defaultanonfun
    with
      Not_found -> defaultanonfun
  in 
  try
    Uarg.parse argspecs anonfun (oneLineDocs usage)
  with IllegalValue str -> 
    raise(Util.Fatal(Printf.sprintf "%s \n%s\n" (oneLineDocs usage) str))

let parseCmdLine usage =
  processCmdLine usage (fun _ sp -> sp)

(* Scan command line without actually setting any preferences; return a      *)
(* string map associating a list of strings with each option appearing on    *)
(* the command line.                                                         *)
let scanCmdLine usage = 
  let m = ref (Util.StringMap.empty : (string list) Util.StringMap.t) in
  let insert name s =
    let old = try Util.StringMap.find name !m with Not_found -> [] in
    m := Util.StringMap.add name (s :: old) !m   in
  processCmdLine usage
    (fun name p ->
       match p with
         Uarg.Bool _   -> Uarg.Bool   (fun b -> insert name (string_of_bool b))
       | Uarg.Int _    -> Uarg.Int    (fun i -> insert name (string_of_int i))
       | Uarg.String _ -> Uarg.String (fun s -> insert name s)
       | _             -> assert false);
  !m

(*****************************************************************************)
(*                     Preferences file parsing                              *)
(*****************************************************************************)

let string2bool name = function
   "true"  -> true 
 | "false" -> false
 | other   -> raise (Util.Fatal (name^" expects a boolean value, but \n"^other
                                ^ " is not a boolean"))

let string2int name string =
 try
   int_of_string string
 with Failure "int_of_string" -> 
   raise (Util.Fatal (name ^ " expects an integer value, but\n" 
                 ^ string ^ " is not an integer"))

(* Takes a filename and returns a list of "parsed lines" containing
      (filename, lineno, varname, value)
   in the same order as in the file. *)
let rec readAFile filename : (string * int * string * string) list =
  let chan =
    try
      let path = profilePathname filename in
        profileFiles := (path, System.stat path) :: !profileFiles;
        System.open_in_bin path
    with Unix.Unix_error _ | Sys_error _ ->
      raise(Util.Fatal(Printf.sprintf "Preference file %s not found" filename))
  in
  let bom = "\xef\xbb\xbf" in (* BOM: UTF-8 byte-order mark *)
  let rec loop lines =
    match (try Some(input_line chan) with End_of_file -> None) with
      None -> close_in chan; parseLines filename lines
    | Some(theLine) ->
        let theLine =
          (* A lot of Windows tools start a UTF-8 encoded file by a
             byte-order mark.  We skip it. *)
          if lines = [] && Util.startswith theLine bom then
            String.sub theLine 3 (String.length theLine - 3)
          else
            theLine
        in
        loop (theLine::lines) in
  loop []

(* Takes a list of strings in reverse order and yields a list of "parsed lines"
   in correct order *)
and parseLines filename lines = 
  let rec loop lines lineNum res =
    match lines with
      [] -> res
    | theLine :: rest ->
        let theLine = Util.removeTrailingCR theLine in
        let l = Util.trimWhitespace theLine in
        if l = "" || l.[0]='#' then
          loop rest (lineNum+1) res
        else if Util.startswith theLine "include " then
          match Util.splitIntoWords theLine ' ' with
            [_;f] ->
              let sublines = readAFile f in
              loop rest (lineNum+1) (Safelist.append sublines res)
          | _ -> raise (Util.Fatal(Printf.sprintf
				     "File \"%s\", line %d:\nGarbled 'include' directive: %s" 
				     filename lineNum theLine))
        else try
          let pos = String.index theLine '=' in
          let varName = Util.trimWhitespace (String.sub theLine 0 pos) in
          let theResult =
            Util.trimWhitespace (String.sub theLine (pos+1)
                              (String.length theLine - pos - 1)) in
          loop rest (lineNum+1) ((filename, lineNum, varName, theResult)::res)
        with Not_found -> (* theLine does not contain '=' *)
          raise(Util.Fatal(Printf.sprintf
			     "File \"%s\", line %d:\nGarbled line (no '='):\n%s" filename lineNum theLine)) in
  loop lines 1 []

let processLines lines =
  Safelist.iter
    (fun (fileName, lineNum, varName,theResult) ->
       try
         let _, theFunction, _ = Util.StringMap.find varName !prefs in
         match theFunction with
           Uarg.Bool boolFunction ->
             boolFunction (string2bool varName theResult)
         | Uarg.Int intFunction ->
             intFunction (string2int varName theResult)
         | Uarg.String stringFunction ->
             stringFunction theResult
         | _ -> assert false
       with Not_found ->
         raise (Util.Fatal ("File \""^ fileName ^ "\", line " ^ 
                            string_of_int lineNum ^ ": `" ^
                            varName ^ "' is not a valid option"))
       | IllegalValue str -> 
           raise(Util.Fatal("File \""^ fileName ^ "\", line " ^ 
                            string_of_int lineNum ^ ": " ^ str)))
    lines

let loadTheFile () =
  match !profileName with
    None -> ()
  | Some(n) -> processLines(readAFile n)

let loadStrings l =
  processLines (parseLines "<internal>" l)

(*****************************************************************************)
(*                            Printing                                       *)
(*****************************************************************************)

let listVisiblePrefs () =
  let l =
    Util.StringMap.fold
      (fun name (_, pspec, fulldoc) l ->
         if String.length fulldoc > 0 then begin
           (name, pspec, fulldoc) :: l
         end else l) !prefs [] in
  Safelist.stable_sort (fun (name1,_,_) (name2,_,_) -> compare name1 name2) l

let printFullDocs () =
  Printf.eprintf "\\begin{description}\n";
  Safelist.iter
    (fun (name, pspec, fulldoc) ->
       Printf.eprintf "\\item [{%s \\tt %s}]\n%s\n\n"
         name (prefArg pspec) fulldoc)
    (listVisiblePrefs());
  Printf.eprintf "\\end{description}\n"


(*****************************************************************************)
(*                  Adding stuff to the prefs file                           *)
(*****************************************************************************)

let addprefsto = createString "addprefsto" ""
  "!file to add new prefs to"
  "By default, new preferences added by Unison (e.g., new \\verb|ignore| \
   clauses) will be appended to whatever preference file Unison was told \
   to load at the beginning of the run.  Setting the preference \
   \\texttt{addprefsto \\ARG{filename}} makes Unison \
   add new preferences to the file named \\ARG{filename} instead."

let addLine l = 
  let filename =
    if read addprefsto <> ""
      then profilePathname (read addprefsto)
      else thePrefsFile() in
  try
    debug (fun() ->
      Util.msg "Adding '%s' to %s\n" l (System.fspathToDebugString filename));
    let resultmsg =
      l ^ "' added to profile " ^ System.fspathToPrintString filename in
    let ochan =
      System.open_out_gen [Open_wronly; Open_creat; Open_append] 0o600 filename
    in
    output_string ochan l;
    output_string ochan "\n";
    close_out ochan;
    resultmsg
  with
    Sys_error e ->
      begin
        let resultmsg =
          (Printf.sprintf "Could not write preferences file (%s)\n" e) in
        Util.warn resultmsg;
        resultmsg
      end 

let add name value = addLine (name ^ " = " ^ value)

let addComment c = ignore (addLine ("# " ^ c))
