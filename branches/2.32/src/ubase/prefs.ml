(* Unison file synchronizer: src/ubase/prefs.ml *)
(* $I3: Copyright 1999-2002 (see COPYING for details) $ *)

let debug = Util.debug "prefs"

type 'a t = ('a * string list) ref

let read p = fst !p

let set p v = p:=(v, snd !p)

let name p = snd !p

let rawPref default = ref default

(* ------------------------------------------------------------------------- *)

let profileName = ref None

let profilePathname n =
  let f = Util.fileInUnisonDir n in
  if Sys.file_exists f then f
  else Util.fileInUnisonDir (n ^ ".prf")

let thePrefsFile () = 
  match !profileName with
    None -> raise (Util.Transient("No preference file has been specified"))
  | Some(n) -> profilePathname n

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

let resetToDefaults () = Safelist.iter (fun f -> f()) !resetters
  
(* ------------------------------------------------------------------------- *)

(* When the server starts up, we need to ship it the current state of all    *)
(* the preference settings.  This is accomplished by dumping them on the     *)
(* client side and loading on the server side; as each preference is         *)
(* created, a dumper (marshaler) and a loader (parser) are added to the list *)
(* kept here...                                                              *)

type dumpedPrefs = (string * string) list

let dumpers = ref ([] : (string * (unit->string)) list)
let loaders = ref (Util.StringMap.empty : (string->unit) Util.StringMap.t)

let adddumper name f =
  dumpers := (name,f) :: !dumpers

let addloader name f =
  loaders := Util.StringMap.add name f !loaders

let dump () = Safelist.map (fun (name,f) -> (name, f())) !dumpers
  
let load d =
  begin
    Safelist.iter
      (fun (name, dumpedval) ->
        let loaderfn =
          try Util.StringMap.find name !loaders
          with Not_found -> raise (Util.Fatal
            ("Preference "^name^" not found: inconsistent Unison versions??"))
        in loaderfn dumpedval)
      d
  end

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

(* prefs: prefName -> (doc, pspec, fulldoc)                                  *)
let prefs =
  ref (Util.StringMap.empty : (string * Uarg.spec * string) Util.StringMap.t)

(* aliased pref has *-prefixed doc and empty fulldoc                         *)
let alias pref newname =
  (* pref must have been registered, so name pref is not empty, and will be *)
  (* found in the map, no need for catching exception                       *)
  let (_,pspec,_) = Util.StringMap.find (Safelist.hd (name pref)) !prefs in
  prefs := Util.StringMap.add newname ("*", pspec, "") !prefs;
  pref := (fst !pref, newname::(snd !pref))

let registerPref name pspec doc fulldoc =
  if Util.StringMap.mem name !prefs then
    raise (Util.Fatal ("Preference " ^ name ^ " registered twice"));
  prefs := Util.StringMap.add name (doc, pspec, fulldoc) !prefs

let createPrefInternal name default doc fulldoc printer parsefn =
  let newCell = rawPref (default, [name]) in
  registerPref name (parsefn newCell) doc fulldoc;
  adddumper name (fun () -> Marshal.to_string !newCell []);
  addprinter name (fun () -> printer (fst !newCell));
  addresetter (fun () -> newCell := (default, [name]));
  addloader name (fun s -> newCell := Marshal.from_string s 0);
  newCell

let create name default doc fulldoc intern printer =
  createPrefInternal name default doc fulldoc printer
    (fun cell -> Uarg.String (fun s -> set cell (intern (fst !cell) s)))

let createBool name default doc fulldoc =
  let doc = if default then doc ^ " (default true)" else doc in
  createPrefInternal name default doc fulldoc
    (fun v -> [if v then "true" else "false"])
    (fun cell -> Uarg.Bool (fun b -> set cell b))

let createInt name default doc fulldoc =
  createPrefInternal name default doc fulldoc
    (fun v -> [string_of_int v])  
    (fun cell -> Uarg.Int (fun i -> set cell i))

let createString name default doc fulldoc =
  createPrefInternal name default doc fulldoc 
    (fun v -> [v])
    (fun cell -> Uarg.String (fun s -> set cell s))

let createStringList name doc fulldoc =
  createPrefInternal name [] doc fulldoc
    (fun v -> v)
    (fun cell -> Uarg.String (fun s -> set cell (s::(fst !cell))))

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
    try open_in (profilePathname filename)
    with Sys_error _ ->
      raise(Util.Fatal(Printf.sprintf "Preference file %s not found" filename)) in
  let rec loop lines =
    match (try Some(input_line chan) with End_of_file -> None) with
      None -> close_in chan; parseLines filename lines
    | Some(theLine) -> loop (theLine::lines) in
  loop []

(* Takes a list of strings in reverse order and yields a list of "parsed lines"
   in correct order *)
and parseLines filename lines = 
  let rec loop lines lineNum res =
    match lines with
      [] -> res
    | theLine :: rest ->
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
    debug (fun() -> Util.msg "Adding '%s' to %s\n" l filename);
    let resultmsg = l ^ "' added to profile " ^ filename in 
    let ochan =
      open_out_gen [Open_wronly; Open_append; Open_creat] 0o600 filename
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
