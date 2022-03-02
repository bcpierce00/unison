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

let profilePathname ?(add_ext=true) n =
  let f = Util.fileInUnisonDir n in
  if (not add_ext) || System.file_exists f then f
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

let mdumpedPrefs = Umarshal.(list (prod3 string bool string id id))

let dumpers = ref ([] : (string * bool * (unit->bool) * (int->string)) list)
let loaders = ref (Util.StringMap.empty : (int->string->unit) Util.StringMap.t)
let ignored = ref []

let adddumper name optional send f =
  dumpers := (name,optional,send,f) :: !dumpers

let addloader name f =
  loaders := Util.StringMap.add name f !loaders

let addignored name =
  ignored := name :: !ignored

let dump rpcVer =
  Safelist.filter (fun (_, _, sf, _) -> sf ()) !dumpers
  |> Safelist.map (fun (name, opt, _, f) -> (name, opt, f rpcVer))

let load d rpcVer =
  Safelist.iter
    (fun (name, opt, dumpedval) ->
       match
         try Some (Util.StringMap.find name !loaders) with Not_found -> None
       with
         Some loaderfn ->
           loaderfn rpcVer dumpedval
       | None ->
           if not opt && not (Safelist.mem name !ignored) then
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

type apref =
  {
    doc : string;
    pspec : Uarg.spec;
    fulldoc : string;
    typ : typ;
    cli_only : bool;
    deprec : bool;
  }

(* prefs: prefName -> apref                                                  *)
let prefs =
  ref (Util.StringMap.empty : apref Util.StringMap.t)

let typ nm =
  try let {typ; _} = Util.StringMap.find nm !prefs in typ with
  | Not_found -> `UNKNOWN

let documentation nm =
  try
    let {doc; fulldoc; deprec; _} = Util.StringMap.find nm !prefs in
    if doc <> "" && doc.[0] = '*' then raise Not_found;
    let basic = doc = "" || doc.[0] <> '!' in
    let doc =
      if not basic then
        String.sub doc 1 (String.length doc - 1)
      else
        doc
    in
    let doc =
      if not deprec then doc
      else "(Deprecated) " ^ doc
    in
    let fulldoc =
      if not deprec then fulldoc
      else "{\\em (Deprecated)} " ^ fulldoc
    in
    (doc, fulldoc, basic)
  with Not_found ->
    ("", "", false)

let list include_cli_only =
  List.sort String.compare
    (Util.StringMap.fold
      (fun nm {doc; cli_only; _} l ->
        if (not cli_only || include_cli_only) && (doc = "" || doc.[0] <> '*') then
          nm :: l
        else l)
      !prefs [])

(* aliased pref has *-prefixed doc and empty fulldoc                         *)
let alias pref newname =
  (* pref must have been registered, so name pref is not empty, and will be *)
  (* found in the map, no need for catching exception                       *)
  let pref' = Util.StringMap.find (Safelist.hd (name pref)) !prefs in
  prefs := Util.StringMap.add newname {pref' with doc = "*"; fulldoc = ""} !prefs;
  let () =
    try
      let loader = Util.StringMap.find (Safelist.hd (name pref)) !loaders in
      addloader newname loader
    with Not_found -> ()
  in
  aliasMap := Util.StringMap.add newname (Safelist.hd (name pref)) !aliasMap;
  pref.names <- newname :: pref.names

let combine_pspec f = function
  | Uarg.Bool f' -> Uarg.Bool (fun x -> f' x; f ())
  | Uarg.String f' -> Uarg.String (fun x -> f' x; f ())
  | Uarg.Int f' -> Uarg.Int (fun x -> f' x; f ())
  | _ -> assert false

let deprecatedPref name p =
  combine_pspec @@ fun () ->
  Util.warn ("Preference \"" ^ name ^ "\" is deprecated!\n"
    ^ "It may be removed in the next release, so you should\n"
    ^ "stop using this preference on the command line and\n"
    ^ "in the profiles."
    ^ (if read p <> readDefault p then "" else
         "\nYou will not lose out on anything; you have currently\n"
       ^ "set this preference to its default value."))

let registerPref name typ cell pspec cli_only deprec doc fulldoc =
  if Util.StringMap.mem name !prefs then
    raise (Util.Fatal ("Preference " ^ name ^ " registered twice"));
  let pspec =
    if not deprec then pspec
    else deprecatedPref name cell pspec in
  let pref = {doc; pspec; fulldoc; typ; cli_only; deprec} in
  prefs := Util.StringMap.add name pref !prefs

let createPrefInternal name typ cli_only local send default deprecated doc fulldoc printer parsefn m =
  let m = Umarshal.(prod2 m (list string) id id) in
  let newCell = rawPref default name in
  registerPref name typ newCell (parsefn newCell) cli_only deprecated doc fulldoc;
  let (local, send) =
    if not cli_only then (local, send)
    else (true, Some (fun () -> false))
  in
  adddumper name local
    (fun () -> match send with None -> true | Some f -> f ())
    (function
     | 0 -> Marshal.to_string (newCell.value, newCell.names) []
     | _ -> Umarshal.to_string m (newCell.value, newCell.names));
  addprinter name (fun () -> printer newCell.value);
  addresetter
    (fun () ->
       newCell.setInProfile <- false; newCell.value <- newCell.defaultValue);
  addloader name
    (fun rpcVer s ->
       if not cli_only then   (* Better for compatibility to not fail if cli_only *)
       let (value, names) =
         match rpcVer with
         | 0 -> Marshal.from_string s 0
         | _ -> Umarshal.from_string m s 0
       in
       newCell.value <- value);
  newCell

let create name ?(cli_only=false) ?(local=false) ?send default ?(deprecated=false) doc fulldoc intern printer m =
  createPrefInternal name `CUSTOM cli_only local send default deprecated doc fulldoc printer
    (fun cell -> Uarg.String (fun s -> set cell (intern (read cell) s)))
    m

let createBool name ?(cli_only=false) ?(local=false) ?send default ?(deprecated=false) doc fulldoc =
  let doc = if default then doc ^ " (default true)" else doc in
  createPrefInternal name `BOOL cli_only local send default deprecated doc fulldoc
    (fun v -> [if v then "true" else "false"])
    (fun cell -> Uarg.Bool (fun b -> set cell b))
    Umarshal.bool

let createInt name ?(cli_only=false) ?(local=false) ?send default ?(deprecated=false) doc fulldoc =
  createPrefInternal name `INT cli_only local send default deprecated doc fulldoc
    (fun v -> [string_of_int v])
    (fun cell -> Uarg.Int (fun i -> set cell i))
    Umarshal.int

let createString name ?(cli_only=false) ?(local=false) ?send default ?(deprecated=false) doc fulldoc =
  createPrefInternal name `STRING cli_only local send default deprecated doc fulldoc
    (fun v -> [v])
    (fun cell -> Uarg.String (fun s -> set cell s))
    Umarshal.string

let createFspath name ?(cli_only=false) ?(local=false) ?send default ?(deprecated=false) doc fulldoc =
  createPrefInternal name `STRING cli_only local send default deprecated doc fulldoc
    (fun v -> [System.fspathToString v])
    (fun cell -> Uarg.String (fun s -> set cell (System.fspathFromString s)))
    System.mfspath

let createStringList name ?(cli_only=false) ?(local=false) ?send ?(deprecated=false) doc fulldoc =
  createPrefInternal name `STRING_LIST cli_only local send [] deprecated doc fulldoc
    (fun v -> v)
    (fun cell -> Uarg.String (fun s -> set cell (s:: read cell)))
    Umarshal.(list string)

let createBoolWithDefault name ?(cli_only=false) ?(local=false) ?send ?(deprecated=false) doc fulldoc =
  createPrefInternal name `BOOLDEF cli_only local send `Default deprecated doc fulldoc
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
    Umarshal.(sum3 unit unit unit
                (function
                 | `True -> I31 ()
                 | `False -> I32 ()
                 | `Default -> I33 ())
                (function
                 | I31 () -> `True
                 | I32 () -> `False
                 | I33 () -> `Default))

let markRemoved name =
  addignored name

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
 with Failure _ ->
   raise (Util.Fatal (name ^ " expects an integer value, but\n"
                 ^ string ^ " is not an integer"))

(* Takes a filename and returns a list of "parsed lines" containing
      (filename, lineno, varname, value)
   in the same order as in the file. *)
let rec readAFile ?(fail=true) ?(add_ext=true) filename
    : (string * int * string * string) list =
  let bom = "\xef\xbb\xbf" in (* BOM: UTF-8 byte-order mark *)
  let rec loop chan lines =
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
        loop chan (theLine::lines)
  in
  let chan =
    try
      let path = profilePathname ~add_ext:add_ext filename in
      profileFiles := (path, System.stat path) :: !profileFiles;
      Some (System.open_in_bin path)
    with Unix.Unix_error _ | Sys_error _ -> None
  in
  match chan, fail with
    None, true ->
      raise(Util.Fatal(Printf.sprintf "Preference file %s not found" filename))
  | None, false -> []
  | Some chan, _ -> loop chan []

(* Takes a list of strings in reverse order and yields a list of "parsed lines"
   in correct order *)
and parseLines filename lines =
  let rec loop lines lineNum res =
    match lines with
      [] -> res
    | theLine :: rest ->
        let theLine = Util.removeTrailingCR theLine in
        let l = Util.trimWhitespace theLine in
        let includes ~fail ~add_ext =
          match Util.splitIntoWords theLine ' ' with
            [_;f] ->
              let sublines = readAFile f ~fail:fail ~add_ext:add_ext in
              loop rest (lineNum+1) (Safelist.append sublines res)
          | _ -> raise (Util.Fatal(Printf.sprintf
                                     "File \"%s\", line %d:\nGarbled 'include' directive: %s"
                                     filename lineNum theLine)) in
        if l = "" || l.[0]='#' then
          loop rest (lineNum+1) res
        else if Util.startswith theLine "include " then
          includes ~fail:true ~add_ext:true
        else if Util.startswith theLine "source " then
          includes ~fail:true ~add_ext:false
        else if Util.startswith theLine "include? " then
          includes ~fail:false ~add_ext:true
        else if Util.startswith theLine "source? " then
          includes ~fail:false ~add_ext:false
        else
          match Util.splitAtChar theLine '=' with
            i, Some j -> let (varName, theResult) = (fun f (i,j) -> (f i,f j))
                  Util.trimWhitespace (i,j) in
              loop rest (lineNum+1) ((filename, lineNum, varName, theResult)::res)
          | _ -> (* theLine does not contain '=' *)
              raise (Util.Fatal(Printf.sprintf
                                  "File \"%s\", line %d:\nGarbled line (no '='):\n%s"
                                  filename lineNum theLine)) in
  loop lines 1 []

let processLines lines =
  Safelist.iter
    (fun (fileName, lineNum, varName,theResult) ->
       try
         let {pspec = theFunction; cli_only; _} = Util.StringMap.find varName !prefs in
         if cli_only then
           raise (IllegalValue ("\"" ^ varName
             ^ "\" is a command line-only option; "
             ^ "it must not be present in a profile."));
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
(*                      Command-line parsing                                 *)
(*****************************************************************************)

let _ = create "source" ()
  ~cli_only:true
  "!include a file's preferences"
  "Include preferences from a file.  \\texttt{source \\ARG{name}} reads the \
   file \\showtt{name} in the \\texttt{.unison} directory and includes its \
   contents as if it was part of a profile or given directly on command line."
  (fun _ s -> processLines (readAFile ~add_ext:false s))
  (fun v -> []) Umarshal.unit

let _ = create "include" ()
  ~cli_only:true
  "!include a profile's preferences"
  "Include preferences from a profile.  \\texttt{include \\ARG{name}} reads \
   the profile \\showtt{name} (or file \\showtt{name} in the \\texttt{.unison} \
   directory if profile \\showtt{name} does not exist) and includes its \
   contents as if it was part of a profile or given directly on command line."
  (fun _ s -> processLines (readAFile s))
  (fun v -> []) Umarshal.unit

let prefArg = function
    Uarg.Bool(_)   -> ""
  | Uarg.Int(_)    -> "n"
  | Uarg.String(_) -> "xxx"
  | _             -> assert false

(* [argspecs hook] returns a list of specs for [Uarg.parse] *)
let argspecs hook =
  Util.StringMap.fold
    (fun name pref l ->
       ("-" ^ name, hook name pref.pspec, "") :: l)
    !prefs []

let oneLineDocs u =
  let formatOne name {pspec; doc; deprec; _ } p =
    (* if [p] format a one line message documenting a preference *)
    if not (String.length doc > 0 && doc.[0] <> '*' && p doc) then "" else
    let doc = if doc.[0] = '!'
                then String.sub doc 1 ((String.length doc) - 1)
                else doc in
    let doc = if deprec then "(deprecated) " ^ doc else doc in
    let arg = prefArg pspec in
    let arg = if arg = "" then "" else " " ^ arg in
    let spaces =
      String.make (max 1 (18 - String.length (name ^ arg))) ' ' in
    " -" ^ name ^ arg ^ spaces ^ doc ^ "\n" in
  let formatAll p =
    (* format a message documenting non hidden preferences matching [p] *)
    String.concat "" @@
    Safelist.rev @@
    Util.StringMap.fold
      (fun name pref l -> (formatOne name pref p) :: l) !prefs []
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
      let {pspec = p; _} = Util.StringMap.find "rest" !prefs in
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
(*                            Printing                                       *)
(*****************************************************************************)

let listVisiblePrefs () =
  let l =
    Util.StringMap.fold
      (fun name ({fulldoc; _} as pref) l ->
         if String.length fulldoc > 0 then begin
           (name, pref) :: l
         end else l) !prefs [] in
  Safelist.stable_sort (fun (name1, _) (name2, _) -> compare name1 name2) l

let printFullDocs () =
  Printf.eprintf "\\begin{description}\n";
  Safelist.iter
    (fun (name, {pspec; fulldoc; deprec; _}) ->
       Printf.eprintf "\\item [{%s \\tt %s}]\n%s%s\n\n"
         name (prefArg pspec) (if deprec then "{\\em (Deprecated)} " else "") fulldoc)
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
    output_string ochan "\n";
    output_string ochan l;
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
