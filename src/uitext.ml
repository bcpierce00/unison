(* Unison file synchronizer: src/uitext.ml *)
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

open Common
open Lwt

module Body : Uicommon.UI = struct

let debug = Trace.debug "ui"

let dumbtty =
  Prefs.createBool "dumbtty"
    (try System.getenv "EMACS" <> "" with Not_found -> false)
    ~category:(`Advanced `CLI)
    "do not change terminal settings in text UI"
    ("When set to \\verb|true|, this flag makes the text mode user "
     ^ "interface avoid trying to change any of the terminal settings.  "
     ^ "(Normally, Unison puts the terminal in `raw mode', so that it can "
     ^ "do things like overwriting the current line.) This is useful, for "
     ^ "example, when Unison runs in a shell inside of Emacs.  "
     ^ "\n\n"
     ^ "When \\verb|dumbtty| is set, commands to the user interface need to "
     ^ "be followed by a carriage return before Unison will execute them.  "
     ^ "(When it is off, Unison "
     ^ "recognizes keystrokes as soon as they are typed.)\n\n"
     ^ "This preference has no effect on the graphical user "
     ^ "interface.")

let silent =
  Prefs.createBool "silent" false
    ~category:(`Basic `Syncprocess_CLI)
    "print nothing except error messages"
    ("When this preference is set to {\\tt true}, the textual user "
     ^ "interface will print nothing at all, except in the case of errors.  "
     ^ "Setting \\texttt{silent} to true automatically sets the "
     ^ "\\texttt{batch} preference to {\\tt true}.")

let cbreakMode = ref None

let supportSignals = Util.osType = `Unix || Util.isCygwin

let rawTerminal () =
  match !cbreakMode with
    None      -> ()
  | Some funs -> funs.System.rawTerminal ()

let defaultTerminal () =
  match !cbreakMode with
    None      -> ()
  | Some funs -> funs.System.defaultTerminal ()

let restoreTerminal() =
  if supportSignals && not (Prefs.read dumbtty) then
    Sys.set_signal Sys.sigcont Sys.Signal_default;
  defaultTerminal ();
  cbreakMode := None

let setupTerminal() =
  if not (Prefs.read dumbtty) then
    try
      cbreakMode := Some (System.terminalStateFunctions ());
      let suspend _ =
        defaultTerminal ();
        Sys.set_signal Sys.sigtstp Sys.Signal_default;
        Unix.kill (Unix.getpid ()) Sys.sigtstp
      in
      let resume _ =
        if supportSignals then
          Sys.set_signal Sys.sigtstp (Sys.Signal_handle suspend);
        rawTerminal ()
      in
      if supportSignals then
        Sys.set_signal Sys.sigcont (Sys.Signal_handle resume);
      resume ()
    with Unix.Unix_error _ ->
      restoreTerminal ()

let colorMode =
  Prefs.createBoolWithDefault "color"
    ~category:(`Advanced `CLI) ~local:true
    "use color output for text UI (true/false/default)"
    ("When set to {\\tt true}, this flag enables color output in "
     ^ "text mode user interface. When set to {\\tt false}, all "
     ^ "color output is disabled. Default is to enable color if "
     ^ "the {\\tt NO\\_COLOR} environment variable is not set.")

let colorEnabled = ref false

let setColorPreference () =
  let envOk = try let _ = System.getenv "NO_COLOR" in false
    with Not_found -> true
  and termOk = try System.getenv "TERM" <> "dumb" with Not_found -> true
  and ttyOk = (Unix.isatty Unix.stdout) && (Unix.isatty Unix.stderr) in
  let colorOk = envOk && termOk && ttyOk && not (Prefs.read dumbtty) in
  colorEnabled :=
    match Prefs.read colorMode with
    | `True    -> true
    | `False   -> false
    | `Default -> colorOk && (not Sys.win32
                    || (System.termVtCapable Unix.stdout
                        && System.termVtCapable Unix.stderr))

let color t =
  if not !colorEnabled then "" else
  match t with
    `Reset       -> "\027[0m"
  | `Focus       -> "\027[1m"
  | `Success     -> "\027[1;32m"
  | `Information -> "\027[1;34m"
  | `Warning     -> "\027[1;33m"
  | `Failure     -> "\027[1;31m"
  | `AError      -> "\027[31m"
  | `ASkip       -> "\027[1;35m"
  | `ALtoRf      -> "\027[1;32m"
  | `ALtoRt      -> "\027[1;33m"
  | `ARtoLf      -> "\027[1;34m"
  | `ARtoLt      -> "\027[1;33m"
  | `AMerge      -> "\027[1;36m"
  | `DiffHead    -> "\027[1m"
  | `DiffAdd     -> "\027[32m"
  | `DiffDel     -> "\027[31m"
  | `DiffLoc     -> "\027[36m"
  | _            -> ""

let lineRegexp = Str.regexp "^"

let colorDiff text =
  let result = Buffer.create (String.length text) in
  let a s = Buffer.add_string result s in
  let p = Str.full_split lineRegexp text in
  Safelist.iter (fun t ->
              match t with
                Str.Delim s -> a s
              | Str.Text s -> (let lineSt = s.[0] in
                               match lineSt with
                               | '+' -> a (color `DiffAdd); a s; a (color `Reset)
                               | '-' -> a (color `DiffDel); a s; a (color `Reset)
                               | '@' -> a (color `DiffLoc); a s; a (color `Reset)
                               | _   -> a s)
            ) p;
  Buffer.contents result

let alwaysDisplay message =
  print_string message;
  flush stdout

let alwaysDisplayAndLog message =
(*  alwaysDisplay message;*)
  Trace.log (message ^ "\n")

let display message =
  if not (Prefs.read silent) then alwaysDisplay message

let displayWhenInteractive message =
  if not (Prefs.read Globals.batch) then alwaysDisplay message

let getInput () =
  match !cbreakMode with
    None -> input_line stdin
  | Some funs ->
      (* Raw terminal mode, we want to read the input directly, without the line
         buffering. We can't use [Stdlib.input_char] because OCaml 'char' equals
         one byte and this is not what we want to read. Not all characters are
         one byte (mainly thinking of UTF-8). We also want to make sure that we
         properly read in any input ANSI escape sequences. *)
      let input_char () =
        (* We cannot used buffered I/Os under Windows, as character
           '\r' is not passed through (probably due to the code that
           turns \r\n into \n) *)
        let l = 9 in (* This should suffice to fit a complete escape sequence *)
        let s = Bytes.create l in
        let n = Unix.read Unix.stdin s 0 l in
        if n = 0 then raise End_of_file;
        if Bytes.get s 0 = '\003' then raise Sys.Break;
        Bytes.sub_string s 0 n
      in
      funs.System.startReading ();
      let c = input_char () in
      funs.System.stopReading ();
      let c = match c with
        | "\000" -> "(invalid input)" (* Windows*)
        | "\n" | "\r" -> ""
        | c when Sys.win32 -> Unicode.protect c
                 (* This is not correct because [Unicode.protect] assumes
                    Latin1 encoding. But it does not matter here as currently
                    non-ASCII input is not expected to be processed anyway. *)
                 (* FIX: Must reassess this once proper UTF-8 input becomes
                    possible and widespread on Windows. *)
        | c -> c in
      if c <> "" && c.[0] <> '\027' then
        display c;
      c

let newLine () =
  (* If in dumb mode (i.e. not in cbreak mode) the newline is entered by the
     user to validate the input *)
  if !cbreakMode <> None then display "\n"

let overwrite () =
  if !cbreakMode <> None then display "\r"


let keyEsc = "\027"
let keyF1 = "\027OP"
let keyF2 = "\027OQ"
let keyF3 = "\027OR"
let keyF4 = "\027OS"
let keyF5 = "\027[15~"
let keyF6 = "\027[17~"
let keyF7 = "\027[18~"
let keyF8 = "\027[19~"
let keyF9 = "\027[20~"
let keyF10 = "\027[21~"
let keyF11 = "\027[23~"
let keyF12 = "\027[24~"
let keyInsert = "\027[2~"
let keyDelete = "\027[3~"
let keyHome = "\027[H"
let keyEnd = "\027[F"
let keyPgUp = "\027[5~"
let keyPgDn = "\027[6~"
let keyUp = "\027[A"
let keyDn = "\027[B"
let keyLeft = "\027[D"
let keyRight = "\027[C"
let keyShiftUp = "\027[1;2A"
let keyShiftDn = "\027[1;2B"
let keyTab = "\t"
let keyRvTab = "\027[Z"


let rec selectAction batch actions tryagain =
  let formatname = function
      "" -> "<ret>"
    | " " -> "<spc>"
    | "\x7f" | "\027[3~" -> "<del>"
    | "\b" -> "<bsp>"
    | "\t" -> "<tab>"
    | "\027[Z" -> "<shift+tab>"
    | "\027" -> "<esc>"
    | "\027[A" -> "<up>"
    | "\027[B" -> "<down>"
    | "\027[D" -> "<left>"
    | "\027[C" -> "<right>"
    | "\027[5~" -> "<pg up>"
    | "\027[6~" -> "<pg down>"
    | "\027[H" -> "<home>"
    | "\027[F" -> "<end>"
    | n when n.[0] = '\027' -> "^" ^ String.map (function | '\027' -> '[' | c -> c) n
    | n -> n in
  let summarizeChoices() =
    display "[";
    Safelist.iter
      (fun (names,doc,action) ->
         if (Safelist.nth names 0) = "" then
           display (formatname (Safelist.nth names 1)))
      actions;
    display "] " in
  let tryagainOrLoop() =
    tryagain ();
    selectAction batch actions tryagain in
  let rec find n = function
      [] -> raise Not_found
    | (names,doc,action)::rest ->
        if Safelist.mem n names then action else find n rest
  in
  let doAction a =
    if a="?" || a = "\027OP" then
      (newLine ();
       display "Commands:\n";
       Safelist.iter (fun (names,doc,action) ->
         let n = Util.concatmap " or " formatname names in
         let space = String.make (max 2 (22 - String.length n)) ' ' in
         display ("  " ^ n ^ space ^ doc ^ "\n"))
         actions;
       tryagainOrLoop())
    else
      let action = try Some (find a actions) with Not_found -> None in
      match action with
        Some action ->
          action ()
      | None ->
          newLine ();
          if a="" then
            display ("No default command [type '?' or F1 for help]\n")
          else
            display ("Unrecognized command '" ^ String.escaped a
                     ^ "': try again  [type '?' or F1 for help]\n");
          tryagainOrLoop()
  in
  let handleExn s =
    (* Make sure that the error messages start on their own lines and not
     * after the prompt. *)
    alwaysDisplay "\n";
    raise (Util.Fatal ("Failure reading from the standard input ("^s^")\n"))
  in
  let userInput () =
    try
      Some (getInput ())
    with
      (* Restart an interrupted system call (which can happen notably when
       * the process is put in the background by SIGTSTP). *)
    | Unix.Unix_error (Unix.EINTR, _, _) -> None
      (* Simply print a slightly more informative message than the exception
       * itself (e.g. "Uncaught unix error: read failed: Resource temporarily
       * unavailable" or "Uncaught exception End_of_file"). *)
    | End_of_file -> handleExn "End of file"
    | Unix.Unix_error (err, _, _) -> handleExn (Unix.error_message err)
  in
  let a =
    match batch with
    | None ->
      summarizeChoices();
      userInput ()
    | _ -> batch
  in
  match a with
  | Some a -> doAction a
  | None -> tryagainOrLoop()

let alwaysDisplayErrors prefix l =
  List.iter
    (fun err -> alwaysDisplay (Format.sprintf "%s%s\n" prefix err)) l

let alwaysDisplayDetails ri =
  alwaysDisplay ((Uicommon.details2string ri "  ") ^ "\n");
  match ri.replicas with
    Problem _ ->
      ()
  | Different diff ->
      alwaysDisplayErrors "[root 1]: " diff.errors1;
      alwaysDisplayErrors "[root 2]: " diff.errors2

let displayDetails ri =
  if not (Prefs.read silent) then alwaysDisplayDetails ri

let displayri ri =
  let (r1, action, r2, path) = Uicommon.reconItem2stringList Path.empty ri in
  let forced =
    match ri.replicas with
      Different diff -> diff.direction <> diff.default_direction
    | Problem _      -> false
  in
  let (defaultAction, forcedAction) =
    match action with
      Uicommon.AError      -> ((color `AError) ^ "error" ^ (color `Reset), (color `AError) ^ "error" ^ (color `Reset))
    | Uicommon.ASkip _     -> ((color `ASkip)  ^ "<-?->" ^ (color `Reset), (color `ASkip)  ^ "<=?=>" ^ (color `Reset))
    | Uicommon.ALtoR false -> ((color `ALtoRf) ^ "---->" ^ (color `Reset), (color `ALtoRf) ^ "====>" ^ (color `Reset))
    | Uicommon.ALtoR true  -> ((color `ALtoRt) ^ "--?->" ^ (color `Reset), (color `ALtoRt) ^ "==?=>" ^ (color `Reset))
    | Uicommon.ARtoL false -> ((color `ARtoLf) ^ "<----" ^ (color `Reset), (color `ARtoLf) ^ "<====" ^ (color `Reset))
    | Uicommon.ARtoL true  -> ((color `ARtoLt) ^ "<-?--" ^ (color `Reset), (color `ARtoLt) ^ "<=?==" ^ (color `Reset))
    | Uicommon.AMerge      -> ((color `AMerge) ^ "<-M->" ^ (color `Reset), (color `AMerge) ^ "<=M=>" ^ (color `Reset))
  in
  let action = if forced then forcedAction else defaultAction in
  let s = Format.sprintf "%s %s %s   %s  " r1 action r2 path in
  match ri.replicas with
    Problem _ ->
      alwaysDisplay s
  | Different {direction = d} when isConflict d ->
      alwaysDisplay s
  | _ ->
      display s

type proceed = ConfirmBeforeProceeding | ProceedImmediately

(* "interact [] rilist" interactively reconciles each list item *)
let interact prilist rilist =
  if not (Prefs.read Globals.batch) then display ("\n" ^ Uicommon.roots2string() ^ "\n");
  let (r1,r2) = Globals.roots() in
  let (host1, host2) = root2hostname r1, root2hostname r2 in
  let showdiffs ri =
    Uicommon.showDiffs ri
      (fun title text ->
         let colorText = colorDiff text in
         try
           let pager = System.getenv "PAGER" in
           restoreTerminal ();
           let out = System.open_process_out pager in
           Printf.fprintf out "\n%s\n\n%s\n\n" title colorText;
           let _ = System.close_process_out out in
           setupTerminal ()
         with Not_found ->
           Printf.printf "\n%s\n\n%s\n\n" title colorText)
      (fun s -> Printf.printf "%s\n" s)
      Uutil.File.dummy;
      true
  and ispropschanged = function
      {replicas = Different {rc1 = rc1; rc2 = rc2}}
      when rc1.status = `PropsChanged &&
           (rc2.status = `PropsChanged || rc2.status = `Unchanged) -> true
    | {replicas = Different {rc1 = rc1; rc2 = rc2}}
      when rc1.status = `Unchanged && rc2.status = `PropsChanged -> true
    | _ -> false
  and setdirchanged = function
      {replicas = Different ({rc1 = rc1; rc2 = rc2} as diff)}
      when rc1.status = `Modified && rc2.status = `PropsChanged ->
        diff.direction <- Replica1ToReplica2; true
    | {replicas = Different ({rc1 = rc1; rc2 = rc2} as diff)}
      when rc1.status = `PropsChanged && rc2.status = `Modified ->
        diff.direction <- Replica2ToReplica1; true
    | {replicas = Different _} -> false
    | _ -> true
  and setskip = function
      {replicas = Different ({direction = Conflict _})} -> true
    | {replicas = Different diff} ->
        begin diff.direction <- Conflict "skip requested"; true end
    | _ -> true
  and setdir dir = function
      {replicas = Different diff} -> begin diff.direction <- dir; true end
    | _ -> true
  and invertdir = function
      {replicas = Different ({direction = Replica1ToReplica2} as diff)}
        -> diff.direction <- Replica2ToReplica1; true
    | {replicas = Different ({direction = Replica2ToReplica1} as diff)}
        -> diff.direction <- Replica1ToReplica2; true
    | {replicas = Different _} -> false
    | _ -> true
  and setDirectionIfConflict dir = function
      {replicas = Different ({direction = Conflict _})} as ri ->
        begin Recon.setDirection ri dir `Force; true end
    | ri -> begin Recon.setDirection ri dir `Prefer; true end
  in
  let ripred = ref [] in
  let ritest ri = match !ripred with
      [] -> true
    | test::_ -> test ri in
  let rec loop prev =
    let rec previous prev ril =
      match prev with
        ({ replicas = Problem s } as pri)::pril ->
          displayri pri; display "\n"; display s; display "\n";
          previous pril (pri::ril)
      | pri::pril -> loop pril (pri::ril)
      | [] -> display ("\n" ^ Uicommon.roots2string() ^ "\n"); loop prev ril in
    let rec forward n prev ril =
      match n, prev, ril with
        0, prev, ril -> loop prev ril
      | n, [], ril when n < 0 -> loop [] ril
      | n, pri::pril, ril when n < 0 -> forward (n+1) pril (pri::ril)
      | _, [], [] -> loop [] []
      | n, pri::pril, [] when n > 0 -> loop pril [pri]
      | n, prev, ri::rest when n > 0 -> forward (n-1) (ri::prev) rest
      | _ -> assert false (* to silence the compiler *) in
    function
      [] -> (ConfirmBeforeProceeding, Safelist.rev prev)
    | ri::rest as ril ->
        let next() = loop (ri::prev) rest in
        let repeat() = loop prev ril in
        let ignore_pref pat rest what =
          display "  ";
          Uicommon.addIgnorePattern pat;
          display ("  Permanently ignoring " ^ what ^ "\n");
          begin match !Prefs.profileName with None -> assert false |
            Some(n) ->
              display ("  To un-ignore, edit "
                       ^ Prefs.profilePathname n
                       ^ " and restart " ^ Uutil.myName ^ "\n") end;
          let nukeIgnoredRis =
            Safelist.filter (fun ri -> not (Globals.shouldIgnore ri.path1)) in
          loop (nukeIgnoredRis (ri::prev)) (nukeIgnoredRis ril) in
        (* This should work on most terminals: *)
        let redisplayri() = overwrite (); displayri ri; display "\n" in
        let setripred cmd =
          ripred := match cmd, !ripred with
              `Unset, [] -> display "Matching condition already disabled\n"; []
            | `Unset, _ | `Pop, [_] -> display "  Disabling matching condition\n"; []
            | `Pop, p::pp::t -> pp::t
            | `Push rp, [] -> display "  Enabling matching condition\n"; [rp]
            | `Push rp, p -> rp::p
            | _, [] -> display "Matching condition not enabled\n"; []
            | `Op1 op, p::t -> (fun ri -> op (p ri))::t
            | `Op2 op, [p] -> display "Missing previous matching condition\n"; [p]
            | `Op2 op, p::pp::t -> (fun ri -> op (p ri) (pp ri))::t
            | _ -> assert false in
        let actOnMatching ?(change=true) ?(fail=Some(fun()->())) f =
          (* [f] can have effects on the ri and return false to run [fail] (if
             the matching condition is disabled) *)
          (* When [fail] is [None] if [f] returns false then instead of
             executing [fail] and repeating we discard the item (even when the
             matching condition is disabled) and go to the next *)
          (* Disabling [change] avoids to redisplay the item, allows [f] to
             print a message (info or error) on a separate line and repeats
             instead of going to the next item *)
          let discard, err =
            match fail with Some e -> false, e | None -> true, fun()->() in
          match !ripred with
          | [] -> if not change then newLine();
              let t = f ri in
              if t || not discard
              then begin
                if change then redisplayri();
                if not t then err();
                if t && change then next() else repeat()
              end else begin
                if change then newLine();
                loop prev rest
              end
          | test::_ -> newLine();
              let filt = fun ri -> if test ri then f ri || not discard else true in
              loop prev (ri::Safelist.filter filt rest)
        in
        displayri ri;
        match ri.replicas with
          Problem s -> display "\n"; display s; display "\n"; next()
        | Different {rc1 = _; rc2 = _; direction = dir} ->
            if Prefs.read Uicommon.auto && not (isConflict dir) then begin
              display "\n"; next()
            end else
              let (descr, descl) =
                if host1 = host2 then
                  "left to right", "right to left"
                else
                  "from "^host1^" to "^host2,
                  "from "^host2^" to "^host1
              in
              if Prefs.read Globals.batch then begin
                display "\n";
                if not (Prefs.read Trace.terse) then
                  displayDetails ri
              end;
              if Prefs.read Globals.batch then next () else
              selectAction
                (if Prefs.read Globals.batch then Some " " else None)
                [((if (isConflict dir) && not (Prefs.read Globals.batch)
                   then ["f"]  (* Offer no default behavior if we've got a
                                  conflict and we're in interactive mode *)
                   else ["";"f";" "]),
                  ("follow " ^ Uutil.myName ^ "'s recommendation (if any)"),
                  (fun () -> newLine();
                     if (isConflict dir) && not (Prefs.read Globals.batch)
                     then begin
                       display "No default action [type '?' for help]\n";
                       repeat()
                     end else
                       next()));
                 (["n";"j"; keyDn; keyTab],
                  ("go to the next item"),
                  (fun () -> newLine();
                     next()));
                 (["p";"b";"k"; keyUp; keyRvTab],
                  ("go back to previous item"),
                  (fun () -> newLine();
                     previous prev ril));
                 (["\x7f";"\b"; keyDelete],
                  ("revert then go back to previous item"),
                  (fun () ->
                     Recon.revertToDefaultDirection ri; redisplayri();
                     previous prev ril));
                 (["0"; keyHome],
                  ("go to the start of the list"),
                  (fun () -> newLine();
                     loop [] (Safelist.rev_append prev ril)));
                 (["9"; keyEnd],
                  ("go to the end of the list"),
                  (fun () -> newLine();
                     match Safelist.rev_append ril prev with
                       [] -> loop [] []
                     | lri::prev -> loop prev [lri]));
                 (["5"; keyPgDn],
                  ("go forward to the middle of the following items"),
                  (fun () -> newLine();
                     let l = (Safelist.length ril)/2 in
                     display ("  Moving "^(string_of_int l)^" items forward\n");
                     forward l prev ril));
                 (["6"; keyPgUp],
                  ("go backward to the middle of the preceding items"),
                  (fun () -> newLine();
                     let l = -((Safelist.length prev)+1)/2 in
                     display ("  Moving "^(string_of_int l)^" items backward\n");
                     forward l prev ril));
                 (["R"],
                  ("reverse the list of paths"),
                  (fun () -> newLine();
                     loop rest (ri::prev)));
                 (["d"],
                  ("show differences (curr or match)"),
                  (fun () ->
                     actOnMatching ~change:false showdiffs));
                 (["x"],
                  ("show details (curr or match)"),
                  (fun () ->
                      actOnMatching ~change:false
                        (fun ri -> displayDetails ri; true)));
                 (["L"],
                  ("list all (or matching) following changes tersely"),
                  (fun () -> newLine();
                     Safelist.iter
                       (fun ri -> display "  "; displayri ri; display "\n")
                       (Safelist.filter ritest ril);
                     repeat()));
                 (["l"],
                  ("list all (or matching) following changes with details"),
                  (fun () -> newLine();
                     Safelist.iter
                       (fun ri -> display "  "; displayri ri; display "\n";
                                  alwaysDisplayDetails ri)
                       (Safelist.filter ritest ril);
                     repeat()));
                 (["A";"*"],
                  ("match all the following"),
                  (fun () -> newLine();
                     setripred (`Push (fun _ -> true));
                     repeat()));
                 (["1"],
                  ("match all the following that propagate " ^ descr),
                  (fun () -> newLine();
                     setripred (`Push (function
                         {replicas = Different ({direction = Replica1ToReplica2})} -> true
                       | _ -> false));
                     repeat()));
                 (["2"],
                  ("match all the following that propagate " ^ descl),
                  (fun () -> newLine();
                     setripred (`Push (function
                         {replicas = Different ({direction = Replica2ToReplica1})} -> true
                       | _ -> false));
                     repeat()));
                 (["C"],
                  ("match all the following conflicts"),
                  (fun () -> newLine();
                     setripred (`Push (function
                         {replicas = Different ({direction = Conflict _})} -> true
                       | _ -> false));
                     repeat()));
                 (["P";"="],
                  ("match all the following with only props changes"),
                  (fun () -> newLine();
                     setripred (`Push ispropschanged);
                     repeat()));
                 (["M"],
                  ("match all the following merges"),
                  (fun () -> newLine();
                     setripred (`Push (function
                         {replicas = Different ({direction = Merge})} -> true
                       | _ -> false));
                     repeat()));
                 (["X";"!"],
                  ("invert the matching condition"),
                  (fun () -> newLine();
                     setripred (`Op1 not);
                     repeat()));
                 (["&"],
                  ("and the last two matching conditions"),
                  (fun () -> newLine();
                     setripred (`Op2 (&&));
                     repeat()));
                 (["|"],
                  ("or the last two matching conditions"),
                  (fun () -> newLine();
                     setripred (`Op2 (||));
                     repeat()));
                 (["D";"_"],
                  ("delete/pop the active matching condition"),
                  (fun () -> newLine();
                     setripred `Pop;
                     repeat()));
                 (["U";"$"],
                  ("unmatch (select current)"),
                  (fun () -> newLine();
                     setripred `Unset;
                     repeat()));
                 (["r";"u"],
                  ("revert to " ^ Uutil.myName ^ "'s default recommendation (curr or match)"),
                  (fun () ->
                     actOnMatching
                       (fun ri->Recon.revertToDefaultDirection ri; true)));
                 (["m"],
                  ("merge the versions (curr or match)"),
                  (fun () ->
                     actOnMatching (setdir Merge)));
                 ([">";"."; keyRight],
                  ("propagate from " ^ descr ^ " (curr or match)"),
                  (fun () ->
                     actOnMatching (setdir Replica1ToReplica2)));
                 (["<";","; keyLeft],
                  ("propagate from " ^ descl ^ " (curr or match)"),
                  (fun () ->
                     actOnMatching (setdir Replica2ToReplica1)));
                 (["]";"\""],
                  ("resolve conflicts in favor of the newer (curr or match)"),
                  (fun () ->
                     actOnMatching (setDirectionIfConflict `Newer)));
                 (["[";"'"],
                  ("resolve conflicts in favor of the older (curr or match)"),
                  (fun () ->
                     actOnMatching (setDirectionIfConflict `Older)));
                 (["c"],
                  ("resolve conflicts in favor of changed (curr or match)"),
                  (fun () ->
                     actOnMatching
                       ~fail:(Some (fun()->display "Cannot set direction\n"))
                       setdirchanged));
                 (["i"],
                  ("invert direction of propagation (curr or match)"),
                  (fun () ->
                     actOnMatching
                       ~fail:(Some (fun()->display "Cannot invert direction\n"))
                       invertdir));
                 (["/";":"],
                  ("skip (curr or match)"),
                  (fun () ->
                     actOnMatching setskip));
                 (["%"],
                  ("skip all the following"),
                  (fun () -> newLine();
                     Safelist.iter (fun ri -> ignore (setskip ri); ()) rest;
                     repeat()));
                 (["-"],
                  ("skip and discard for this session (curr or match)"),
                  (fun () ->
                     actOnMatching ~fail:None (fun _->false)));
                 (["+"],
                  ("skip and discard all the following"),
                  (fun () -> newLine();
                     loop prev [ri]));
                 (["I"],
                  ("ignore this path permanently"),
                  (fun () -> newLine();
                     ignore_pref (Uicommon.ignorePath ri.path1) rest
                       "this path"));
                 (["E"],
                  ("permanently ignore files with this extension"),
                  (fun () -> newLine();
                     ignore_pref (Uicommon.ignoreExt ri.path1) rest
                       "files with this extension"));
                 (["N"],
                  ("permanently ignore paths ending with this name"),
                  (fun () -> newLine();
                     ignore_pref (Uicommon.ignoreName ri.path1) rest
                       "files with this name"));
                 (["s"],
                  ("stop reconciling and go to the proceed menu"),
                  (fun () -> newLine();
                     (ConfirmBeforeProceeding, Safelist.rev_append prev ril)));
                 (["g"],
                  ("proceed immediately to propagating changes"),
                  (fun () -> newLine();
                     (ProceedImmediately, Safelist.rev_append prev ril)));
                 (["q"; keyEsc],
                  ("exit " ^ Uutil.myName ^ " without propagating any changes"),
                  (fun () -> newLine();
                     raise Sys.Break))
                ]
                (fun () -> displayri ri)
  in loop prilist rilist

let verifyMerge title text =
  Util.set_infos "";
  Printf.printf "%s\n" text;
  if Prefs.read Globals.batch then
    true
  else begin
    if Prefs.read Uicommon.confirmmerge then begin
      display "Commit results of merge? ";
      selectAction
        None   (* Maybe better: (Some "n") *)
        [(["y";"g"],
          "Yes: commit",
          (fun() -> newLine();
             true));
         (["n"],
          "No: leave this file unchanged",
          (fun () -> newLine();
             false));
        ]
        (fun () -> display "Commit results of merge? ")
    end else
      true
  end

type stateItem =
  { mutable ri : reconItem;
    mutable bytesTransferred : Uutil.Filesize.t;
    mutable bytesToTransfer : Uutil.Filesize.t }

let doTransport reconItemList =
  let items =
    Array.map
      (fun ri ->
         {ri = ri;
          bytesTransferred = Uutil.Filesize.zero;
          bytesToTransfer = Common.riLength ri})
      (Array.of_list reconItemList)
  in
  let totalBytesTransferred = ref Uutil.Filesize.zero in
  let totalBytesToTransfer =
    ref
      (Array.fold_left
         (fun s item -> Uutil.Filesize.add item.bytesToTransfer s)
         Uutil.Filesize.zero items)
  in
  let totalBytesToTransferStr = Util.bytes2string
    (Uutil.Filesize.toInt64 !totalBytesToTransfer) in
  let t0 = Unix.gettimeofday () in
  let calcProgress i bytes dbg =
    let i = Uutil.File.toLine i in
    let item = items.(i) in
    item.bytesTransferred <- Uutil.Filesize.add item.bytesTransferred bytes;
    totalBytesTransferred := Uutil.Filesize.add !totalBytesTransferred bytes;
    let totalBytesTransferredStr = Util.bytes2string
      (Uutil.Filesize.toInt64 !totalBytesTransferred) in
    let v =
      (Uutil.Filesize.percentageOfTotalSize
         !totalBytesTransferred !totalBytesToTransfer)
    in
    let t1 = Unix.gettimeofday () in
    let remTime =
      if v <= 0. then "--:--"
      else if v >= 100. then "00:00:00"
      else
        let t = truncate ((t1 -. t0) *. (100. -. v) /. v +. 0.5) in
        let u = t mod 3600 in
        let h = t / 3600 in
        let m = u / 60 in
        let sec = u mod 60 in
        Format.sprintf "%02d:%02d:%02d" h m sec
    in
    let stat = Format.sprintf "%s  (%s of %s)  %s ETA" (Util.percent2string v)
      totalBytesTransferredStr totalBytesToTransferStr remTime in
    t1, stat
  in
  let tlog = ref t0 in
  let showProgress i bytes dbg =
    let t1, s = calcProgress i bytes dbg in
    if not (Prefs.read Trace.terse) && (Prefs.read Trace.debugmods = []) then
      Util.set_infos s;
    if (Prefs.read Trace.terse) || (Prefs.read Globals.batch) then
      if (t1 -. !tlog) >= 60. then
      begin
        Trace.logonly (s ^ "\n");
        tlog := t1
      end
  in
  Uutil.setProgressPrinter showProgress;

  let intrcount = ref 0 in
  let sigtermHandler _ =
    if !intrcount >= 3 then raise Sys.Break;
    Abort.all ();
    incr intrcount
  in
  let ctrlCHandler n =
    sigtermHandler n;
    if !intrcount = 1 then
      let s = "\n\nUpdate propagation interrupted. It may take a while \
        to stop.\nIf the process doesn't stop soon then wait or press \
        Ctrl-C\n3 more times to force immediate termination.\n\n\n" in
      (* Don't use [Printf.*printf] or [Format.*printf] (or other functions
         which use [Stdlib.out_channel]) because this can cause a deadlock
         with other outputting functions (in this case most likely at
         [Util.set_infos] called in [showProgress]) before OCaml 4.12. *)
      try Unix.write_substring Unix.stdout s 0 (String.length s) |> ignore
      with Unix.Unix_error _ -> ()
  in
  let stopAtIntr f =
    let signal_noerr signa behv =
      try Some (Sys.signal signa behv)
      with Sys_error _ | Invalid_argument _ -> None
    in
    let restore_noerr signa = function
    | Some prevSig -> ignore (signal_noerr signa prevSig)
    | None -> ()
    in
    let prevSigInt = signal_noerr Sys.sigint (Signal_handle ctrlCHandler) in
    let prevSigTerm = signal_noerr Sys.sigterm (Signal_handle sigtermHandler) in
    let restoreSig () =
      (* Set handlers will still raise [Sys.Break]; can ignore errors here. *)
      restore_noerr Sys.sigint prevSigInt;
      restore_noerr Sys.sigterm prevSigTerm
    in

    try f (); restoreSig ()
    with e ->
      let origbt = Printexc.get_raw_backtrace () in
      restoreSig ();
      Printexc.raise_with_backtrace e origbt
  in

  Uicommon.transportStart ();
  let fFailedPaths = ref [] in
  let fPartialPaths = ref [] in
  let notstarted = ref (Array.length items) in
  let uiWrapper i item =
    Lwt.try_bind
      (fun () -> decr notstarted;
                 Transport.transportItem item.ri
                   (Uutil.File.ofLine i) verifyMerge)
      (fun () ->
         if partiallyProblematic item.ri && not (problematic item.ri) then
           fPartialPaths := item.ri.path1 :: !fPartialPaths;
         Lwt.return ())
      (fun e ->
        match e with
          Util.Transient s ->
            let rem =
              Uutil.Filesize.sub
                item.bytesToTransfer item.bytesTransferred
            in
            if rem <> Uutil.Filesize.zero then
              showProgress (Uutil.File.ofLine i) rem "done";
            let m = "[" ^ (Path.toString item.ri.path1)  ^ "]: " ^ s in
            Util.set_infos "";
            alwaysDisplay ("Failed " ^ m ^ "\n");
            fFailedPaths := item.ri.path1 :: !fFailedPaths;
            return ()
        | _ ->
            fail e) in
  stopAtIntr begin fun () ->
    Uicommon.transportItems items (fun {ri; _} -> not (Common.isDeletion ri)) uiWrapper;
    Uicommon.transportItems items (fun {ri; _} -> Common.isDeletion ri) uiWrapper
  end;
  Uicommon.transportFinish ();

  Uutil.setProgressPrinter (fun _ _ _ -> ());
  Util.set_infos "";

  (Safelist.rev !fFailedPaths, Safelist.rev !fPartialPaths, !notstarted, !intrcount > 0)

let setWarnPrinterForInitialization()=
  Util.warnPrinter :=
    Some (fun s -> alwaysDisplay ("Warning: " ^ s ^ "\n\n"))

let setWarnPrinter() =
  Util.warnPrinter :=
    Some(fun s ->
           Util.set_infos "";
           alwaysDisplay "Warning: ";
           alwaysDisplay (s^"\n");
           if not (Prefs.read Globals.batch) then begin
             display "Press return to continue.";
             selectAction None
               [(["";" ";"y"],
                 ("Continue"),
                 (fun () -> newLine()));
                (["n";"q";"x"],
                 ("Exit"),
                 (fun () -> newLine();
                     restoreTerminal ();
                     Lwt_unix.run (Update.unlockArchives ());
                     exit Uicommon.fatalExit))]
               (fun () -> display  "Press return to continue.")
           end)

let lastMajor = ref ""

let formatStatus major minor =
  let s =
    if major = !lastMajor then "  " ^ minor
    else major ^ (if minor="" then "" else "\n  " ^ minor)
  in
    lastMajor := major;
    s

let rec interactAndPropagateChanges prevItemList reconItemList
      : bool * bool * bool * bool * (Path.t list)
        (* anySkipped?, anyPartial?, anyFailures?, anyCancels?, failingPaths *) =
  let (proceed,newReconItemList) = interact prevItemList reconItemList in
  let (updatesToDo, skipped) =
    Safelist.fold_left
      (fun (howmany, skipped) ri ->
        if problematic ri then (howmany, skipped + 1)
        else (howmany + 1, skipped))
      (0, 0) newReconItemList in
  let doTransp newReconItemList =
    try
      doTransport newReconItemList
    with e ->
      let origbt = Printexc.get_raw_backtrace () in
      let summary =
        "\nSynchronization "
          ^ (color `Failure)
          ^ (match e with Sys.Break -> "interrupted" | _ -> "failed")
          ^ (color `Reset)
          ^ (try let tm = Util.localtime (Util.time ()) in
             Printf.sprintf " at %02d:%02d:%02d"
               tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec with _ -> "")
          ^ (match e with Sys.Break -> " by user request" | _ -> " due to a fatal error")
          ^ "\n\n"
      in
      Trace.log_color summary;
      Printexc.raise_with_backtrace e origbt
  in
  let doit() =
    if not (Prefs.read Globals.batch || Prefs.read Trace.terse) then newLine();
    if not (Prefs.read Trace.terse) then Trace.status "Propagating updates";
    let timer = Trace.startTimer "Transmitting all files" in
    let (failedPaths, partialPaths, notstarted, intr) = doTransp newReconItemList in
    let failures = Safelist.length failedPaths in
    let partials = Safelist.length partialPaths in
    Trace.showTimer timer;
    if not (Prefs.read Trace.terse) then Trace.status "Saving synchronizer state";
    Update.commitUpdates ();
    let trans = updatesToDo - notstarted - failures in
    let summary =
      Printf.sprintf
       "Synchronization %s at %s  (%d item%s transferred, %s%s, %s%s)"
       (if failures = 0 && notstarted = 0 then (color `Success) ^ "complete" ^ (color `Reset)
        else (color `Failure) ^ "incomplete" ^ (color `Reset))
       (let tm = Util.localtime (Util.time()) in
        Printf.sprintf "%02d:%02d:%02d"
          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
       trans (if trans=1 then "" else "s")
       (if partials <> 0 then
          Format.sprintf "%d partially transferred, " partials
        else
          "")
       (if skipped = 0 then "0 skipped" else (color `Information) ^ (Printf.sprintf "%d skipped" skipped) ^ (color `Reset))
       (if failures = 0 then "0 failed" else (color `Failure) ^ (Printf.sprintf "%d failed" failures) ^ (color `Reset))
       (if notstarted = 0 then "" else ", " ^ (color `Information) ^ (Printf.sprintf "%d not started" notstarted) ^ (color `Reset)) in
    Trace.log_color (summary ^ "\n");
    if skipped>0 then
      Safelist.iter
        (fun ri ->
         match ri.replicas with
           Problem r
         | Different {rc1 = _; rc2 = _; direction = Conflict r; default_direction = _} ->
            alwaysDisplayAndLog (Printf.sprintf "  skipped: %s (%s)"
                                                (Path.toString ri.path1) r)
         | _ -> ())
        newReconItemList;
    if partials>0 then
      Safelist.iter
        (fun p ->
           alwaysDisplayAndLog ("  partially transferred: " ^ Path.toString p))
        partialPaths;
    if failures>0 then
      Safelist.iter
        (fun p -> alwaysDisplayAndLog ("  failed: " ^ (Path.toString p)))
        failedPaths;
    if intr then raise Sys.Break; (* Make sure repeat mode is stopped *)
    (skipped > 0, partials > 0, failures > 0, notstarted > 0, failedPaths) in
  if updatesToDo = 0 then begin
    (* BCP (3/09): We need to commit the archives even if there are
       no updates to propagate because some files (in fact, if we've
       just switched to DST on windows, a LOT of files) might have new
       modtimes in the archive. *)
    (* JV (5/09): Don't save the archive in repeat mode as it has some
       costs and its unlikely there is much change to the archives in
       this mode. *)
    if !Update.foundArchives && Prefs.read Uicommon.repeat = `NoRepeat then
      Update.commitUpdates ();
    display "No updates to propagate\n";
    if skipped > 0 then begin
      let summary =
        Printf.sprintf
          "Synchronization %scomplete%s at %s  (0 items transferred, %s%d skipped%s, 0 failed)"
          (color `Success)
          (color `Reset)
          (let tm = Util.localtime (Util.time()) in
           Printf.sprintf "%02d:%02d:%02d"
                          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
          (color `Information)
          skipped
          (color `Reset) in
      Trace.log_color (summary ^ "\n");
      Safelist.iter
        (fun ri ->
         match ri.replicas with
           Problem r
         | Different {rc1 = _; rc2 = _; direction = Conflict r; default_direction = _} ->
            alwaysDisplayAndLog (Printf.sprintf "  skipped: %s (%s)"
                                                (Path.toString ri.path1) r)
         | _ -> ())
        newReconItemList
      end;
    (skipped > 0, false, false, false, [])
  end else if proceed=ProceedImmediately then begin
    doit()
  end else
    let rec askagain newReconItemList =
      displayWhenInteractive "\nProceed with propagating updates? ";
      selectAction
        (* BCP: I find it counterintuitive that every other prompt except this one
           would expect <CR> as a default.  But I got talked out of offering a
           default here, because of safety considerations (too easy to press
           <CR> one time too many). *)
        (if Prefs.read Globals.batch then Some "y" else None)
        [(["y";"g"],
          "Yes: proceed with updates as selected above",
          doit);
         (["n"],
          "No: go through reconciliation process again",
          (fun () -> newLine();
             Prefs.set Uicommon.auto false;
             interactAndPropagateChanges [] newReconItemList));
         (["p";"b"],
          "go back to the last item of the reconciliation",
          (fun () -> newLine();
             Prefs.set Uicommon.auto false;
             match Safelist.rev newReconItemList with
               [] -> interactAndPropagateChanges [] []
             | lastri::prev -> interactAndPropagateChanges prev [lastri]));
         (["N"],
          "sort by Name",
          (fun () ->
             Sortri.sortByName();
             askagain (Sortri.sortReconItems newReconItemList)));
         (["S"],
          "sort by Size",
          (fun () ->
             Sortri.sortBySize();
             askagain (Sortri.sortReconItems newReconItemList)));
         (["W"],
          "sort neW first (toggle)",
          (fun () ->
             Sortri.sortNewFirst();
             askagain (Sortri.sortReconItems newReconItemList)));
         (["D"],
          "Default ordering",
          (fun () ->
             Sortri.restoreDefaultSettings();
             askagain (Sortri.sortReconItems newReconItemList)));
         (["R"],
          "Reverse the sort order",
          (fun () -> askagain (Safelist.rev newReconItemList)));
         (["q"; keyEsc],
          ("exit " ^ Uutil.myName ^ " without propagating any changes"),
          (fun () -> newLine();
             raise Sys.Break))
        ]
        (fun () -> display "Proceed with propagating updates? ")
    in askagain newReconItemList

let checkForDangerousPath dangerousPaths =
  if Prefs.read Globals.confirmBigDeletes then begin
    if dangerousPaths <> [] then begin
      alwaysDisplayAndLog (Uicommon.dangerousPathMsg dangerousPaths);
      if Prefs.read Globals.batch then begin
          alwaysDisplay "Aborting...\n"; restoreTerminal ();
          exit Uicommon.fatalExit
      end else begin
        displayWhenInteractive "Do you really want to proceed? ";
        selectAction
          None
          [(["y"],
            "Continue",
            (fun () -> ()));
           (["n";"q";"x";""],
            "Exit",
            (fun () -> alwaysDisplay "\n";
               restoreTerminal ();
               exit Uicommon.fatalExit))]
          (fun () -> display "Do you really want to proceed? ")
      end
    end
  end

let displayWaitMessage () =
  if not (Prefs.read silent) then
    Util.msg "%s\n" (Uicommon.contactingServerMsg ())

let synchronizeOnce ?wantWatcher pathsOpt =
  let showStatus path =
    if path = "" then Util.set_infos "" else
    let max_len = 70 in
    let mid = (max_len - 3) / 2 in
    let path =
      let l = String.length path in
      if l <= max_len then path else
      String.sub path 0 (max_len - mid - 3) ^ "..." ^
      String.sub path (l - mid) mid
    in
    let c = "-\\|/".[truncate (mod_float (4. *. Unix.gettimeofday ()) 4.)] in
    Util.set_infos (Format.sprintf "%c %s" c path)
  in
  Uicommon.connectRoots ~displayWaitMessage ();
  Trace.status "Looking for changes";
  if not (Prefs.read Trace.terse) && (Prefs.read Trace.debugmods = []) then
    Uutil.setUpdateStatusPrinter (Some showStatus);

  debug (fun() -> Util.msg "temp: Globals.paths = %s\n"
           (String.concat " "
              (Safelist.map Path.toString (Prefs.read Globals.paths))));
  let updates = Update.findUpdates ?wantWatcher pathsOpt in

  Uutil.setUpdateStatusPrinter None;
  Util.set_infos "";

  let (reconItemList, anyEqualUpdates, dangerousPaths) =
    Recon.reconcileAll ~allowPartial:true updates in

  if not !Update.foundArchives then Update.commitUpdates ();
  if reconItemList = [] then begin
    if !Update.foundArchives && Prefs.read Uicommon.repeat = `NoRepeat then
      Update.commitUpdates ();
    (if anyEqualUpdates then
      Trace.status ("Nothing to do: replicas have been changed only "
                    ^ "in identical ways since last sync.")
     else
       Trace.status "Nothing to do: replicas have not changed since last sync.");
    (Uicommon.perfectExit, [])
  end else begin
    checkForDangerousPath dangerousPaths;
    let (anySkipped, anyPartial, anyFailures, anyCancel, failedPaths) =
      interactAndPropagateChanges [] reconItemList in
    let exitStatus = Uicommon.exitCode (anySkipped || anyPartial || anyCancel, anyFailures) in
    (exitStatus, failedPaths)
  end

(* ----------------- Filesystem watching mode ---------------- *)

let watchinterval = 1.    (* Minimal interval between two synchronizations *)
let retrydelay = 5.       (* Minimal delay to retry failed paths *)
let maxdelay = 30. *. 60. (* Maximal delay to retry failed paths *)

module PathMap = Map.Make (Path)

let waitForChangesRoot: Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "waitForChanges" Umarshal.unit Umarshal.unit
    (fun (fspath, _) -> Fswatchold.wait (Update.archiveHash fspath))

let waitForChanges t =
  let dt = t -. Unix.gettimeofday () in
  if dt > 0. then begin
    let timeout = if dt <= maxdelay then [Lwt_unix.sleep dt] else [] in
    Lwt_unix.run
      (Globals.allRootsMap (fun r -> Lwt.return (waitForChangesRoot r ()))
         >>= fun l ->
       Lwt.choose (timeout @ l))
  end

let synchronizePathsFromFilesystemWatcher () =
  let rec loop isStart delayInfo =
    let t = Unix.gettimeofday () in
    let (delayedPaths, readyPaths) =
      PathMap.fold
        (fun p (t', _) (delayed, ready) ->
           if t' <= t then (delayed, p :: ready) else (p :: delayed, ready))
        delayInfo ([], [])
    in
    let (exitStatus, failedPaths) =
      synchronizeOnce ~wantWatcher:true
        (if isStart then None else Some (readyPaths, delayedPaths))
    in
    (* After a failure, we retry at once, then use an exponential backoff *)
    let delayInfo =
      Safelist.fold_left
        (fun newDelayInfo p ->
           PathMap.add p
             (try
                let (t', d) = PathMap.find p delayInfo in
                if t' > t then (t', d) else
                let d = max retrydelay (min maxdelay (2. *. d)) in
                (t +. d, d)
              with Not_found ->
                (t, 0.))
             newDelayInfo)
        PathMap.empty
        (Safelist.append delayedPaths failedPaths)
    in
    Lwt_unix.run (Lwt_unix.sleep watchinterval);
    let nextTime =
      PathMap.fold (fun _ (t, d) t' -> min t t') delayInfo 1e20 in
    waitForChanges nextTime;
    loop false delayInfo
  in
  loop true PathMap.empty

(* ----------------- Repetition ---------------- *)

let synchronizeUntilNoFailures repeatMode =
  let wantWatcher = repeatMode in
  let rec loop triesLeft pathsOpt =
    let (exitStatus, failedPaths) =
      synchronizeOnce ~wantWatcher pathsOpt in
    if failedPaths <> [] && triesLeft <> 0 then begin
      loop (triesLeft - 1) (Some (failedPaths, []))
    end else begin
      exitStatus
    end in
  loop (Prefs.read Uicommon.retry) None

let rec synchronizeUntilDone repeatinterval =
  let exitStatus = synchronizeUntilNoFailures(repeatinterval >= 0) in
  if repeatinterval < 0 then
    exitStatus
  else begin
    (* Do it again *)
    Trace.status (Printf.sprintf
       "\nSleeping for %d seconds...\n" repeatinterval);
    Unix.sleep repeatinterval;
    synchronizeUntilDone repeatinterval
  end

let synchronizeUntilDone () =
  match Prefs.read Uicommon.repeat with
  | `Watch -> synchronizePathsFromFilesystemWatcher ()
  | `Interval i -> synchronizeUntilDone i
  | `NoRepeat -> synchronizeUntilDone (-1)

(* ----------------- Startup ---------------- *)

let profmgrPrefName = "i"
let profmgrPref =
  Prefs.createBool profmgrPrefName false
    ~category:(`Basic `CLI)
    ~cli_only:true
    "interactive profile mode (text UI); command-line only"
    ("Provide this preference in the command line arguments to enable "
     ^ "interactive profile manager in the text user interface. Currently "
     ^ "only profile listing and interactive selection are available. "
     ^ "Preferences like \\texttt{batch} and \\texttt{silent} remain "
     ^ "applicable to synchronization functionality.")
let profmgrUsageMsg = "To start interactive profile selection, type \""
  ^ Uutil.myName ^ " -" ^ profmgrPrefName ^ "\"."

let addProfileKeys list default =
  let rec nextAvailKey i =
    let n = i + 1 in
    if n >= (Array.length Uicommon.profileKeymap) then
      n
    else
      match Uicommon.profileKeymap.(n) with
          None   -> n
        | Some _ -> nextAvailKey n
  in
  let keyAndNext (p, info) i =
    match info.Uicommon.key with
      Some k -> (k, i)
    | None   -> if p = default then ("d", i)
                else ((string_of_int i), (nextAvailKey i))
  in
  let rec addKey i acc = function
  | []           -> []
  | [prof]       -> let (key, _) = keyAndNext prof i in
                      (key, prof) :: acc
  | prof :: rest -> let (key, next) = keyAndNext prof i in
                      addKey next ((key, prof) :: acc) rest
  in
  addKey 0 [] list

let scanProfiles () =
  let wp = !Util.warnPrinter in
  (* Replace warn printer with something that doesn't quit
     the UI just for errors in random scanned profiles. *)
  Util.warnPrinter := Some (fun s -> alwaysDisplay ("Warning: " ^ s ^ "\n\n"));
  let () = Uicommon.scanProfiles () in
  Util.warnPrinter := wp

let getProfile default =
  let cmdArgs = Prefs.scanCmdLine Uicommon.shortUsageMsg in
  if Util.StringMap.mem Uicommon.runTestsPrefName cmdArgs ||
    not (Util.StringMap.mem profmgrPrefName cmdArgs) then
    Some default
  else
  let () = scanProfiles () in
  if (List.length !Uicommon.profilesAndRoots) > 10 then begin
    Trace.log (Format.sprintf "You have too many profiles in %s \
                for interactive selection. Please specify profile \
                or roots on command line.\n"
                Util.unisonDir);
    Trace.log "The profile names are:\n";
    Safelist.iter (fun (p, _) -> Trace.log (Format.sprintf "  %s\n" p))
      !Uicommon.profilesAndRoots;
    Trace.log "\n";
    Some default
  end else if (List.length !Uicommon.profilesAndRoots) = 0 then
    Some default
  else

  let keyedProfileList = addProfileKeys
    (Safelist.sort (fun (p, _) (p', _) -> compare p p')
      !Uicommon.profilesAndRoots)
    default in
  let profileList = (Safelist.sort (fun (k, _) (k', _) -> compare k k')
                      keyedProfileList)
  in

  (* Must parse command line to get dumbtty and color preferences *)
  Prefs.parseCmdLine Uicommon.shortUsageMsg;
  setupTerminal(); setColorPreference ();
  Prefs.resetToDefaults();

  display "Available profiles:\n key:  profilename         label\n";
  Safelist.iteri
    (fun n (key, (profile, info)) ->
      let labeltext =
          match info.Uicommon.label with None -> "" | Some l -> l in
      display (Format.sprintf "  %s%s%s :"
                (color `Focus) key (color `Reset));
      display (Format.sprintf "  %s%-18s%s  %s%s%s\n"
                (color `Focus) profile (color `Reset)
                (color `Information) labeltext (color `Reset));
      Safelist.iteri
          (fun i root -> display (Format.sprintf "         root %i = %s\n"
                                   (i + 1) root))
          info.Uicommon.roots
    )
    profileList;
  display "\n";

  let selection = ref (Some default) in
  let actions = Safelist.append
    [(["";"n";"/"],
      "Don't select any profile",
      (fun () -> selection := None; newLine();
                   display "\nNo profile selected\n\n"));
     (["q"],
      ("exit " ^ Uutil.myName),
      (fun () -> newLine(); raise Sys.Break))]
    (Safelist.map (fun (key, (profile, info)) ->
        ([key],
        "Profile: " ^ profile,
        (fun () -> selection := Some profile; newLine();
                     display ("\nProfile " ^ profile ^ " selected\n\n")))
      )
      profileList);
  in
  let rec askProfile () =
    display "Select a profile ";
    selectAction None actions (fun () -> display "Select a profile ")
  in
  askProfile ();
  !selection

let handleException e =
  restoreTerminal();
  let msg = Uicommon.exn2string e in
  let () =
    try Trace.log (msg ^ "\n")
    with Util.Fatal _ -> () in (* Can't allow fatal errors in fatal error handler *)
  if not !Trace.sendLogMsgsToStderr then alwaysDisplay ("\n" ^ msg ^ "\n")

let rec start interface =
  if interface <> Uicommon.Text then
    Util.msg "This Unison binary only provides the text GUI...\n";
  begin try
    Sys.catch_break true;
    (* Just to make sure something is there... *)
    setWarnPrinterForInitialization();
    let errorOut s =
      Util.msg "%s%s%s\n" Uicommon.shortUsageMsg profmgrUsageMsg s;
      exit 1
    in
    let profileName = match Uicommon.uiInitClRootsAndProfile () with
      | Error s -> errorOut ("\n\n" ^ s)
      | Ok None ->
          let profile = getProfile "default" in
          let () = restoreTerminal () in
          begin
            match profile with
            | None -> exit 0
            | Some x -> x
          end
      | Ok (Some s) -> s
    in
    Uicommon.initPrefs ~profileName ~promptForRoots:(fun () -> errorOut "") ()
  with e ->
    handleException e;
    exit Uicommon.fatalExit
  end;

  (* Uncaught exceptions up to this point are non-recoverable, treated
     as permanent and will inevitably exit the process. Uncaught exceptions
     from here onwards are treated as potentially temporary or recoverable.
     The process does not have to exit if in repeat mode and can try again. *)
  begin try
    if Prefs.read silent then Prefs.set Trace.terse true;

    Uicommon.connectRoots ~displayWaitMessage ();

    if Prefs.read Uicommon.testServer then exit 0;

    (* Run unit tests if requested *)
    if Prefs.read Uicommon.runtests then begin
      !Uicommon.testFunction ();
      exit 0
    end;

    (* Some preference settings imply others... *)
    if Prefs.read silent then begin
      Prefs.set Globals.batch true;
      Prefs.set Trace.terse true;
      Prefs.set dumbtty true;
      Trace.sendLogMsgsToStderr := false;
    end;
    if Prefs.read Uicommon.repeat <> `NoRepeat then begin
      Prefs.set Globals.batch true;
    end;
    setColorPreference ();

    (* Tell OCaml that we want to catch Control-C ourselves, so that
       we get a chance to reset the terminal before exiting *)
    Sys.catch_break true;
    (* Put the terminal in cbreak mode if possible *)
    if not (Prefs.read Globals.batch) then setupTerminal();
    setWarnPrinter();
    Trace.statusFormatter := formatStatus;

    let exitStatus = synchronizeUntilDone() in

    (* Put the terminal back in "sane" mode, if necessary, and quit. *)
    restoreTerminal();
    exit exitStatus

  with
    Sys.Break -> begin
      (* If we've been killed, then die *)
      handleException Sys.Break;
      exit Uicommon.fatalExit
    end
  | e when breakRepeat e -> begin
      handleException e;
      exit Uicommon.fatalExit
    end
  | e -> begin
      (* If any other bad thing happened and the -repeat preference is
         set, then restart *)
      handleException e;
      if Prefs.read Uicommon.repeat = `NoRepeat
          || Prefs.read Uicommon.runtests then
        exit Uicommon.fatalExit;

      Util.msg "\nRestarting in 10 seconds...\n\n";
      begin try Unix.sleep 10 with Sys.Break -> exit Uicommon.fatalExit end;
      start interface
    end
  end

(* Though in some cases we could, there's no point in recovering
   and continuing at any of these exceptions. *)
and breakRepeat = function
  (* Programming errors *)
  | Assert_failure _
  | Match_failure _
  | Invalid_argument _
  | Fun.Finally_raised _
  (* Async exceptions *)
  | Out_of_memory
  | Stack_overflow
  | Sys.Break -> true
  | _ -> false

let defaultUi = Uicommon.Text

end
