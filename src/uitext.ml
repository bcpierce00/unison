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
    "!do not change terminal settings in text UI"
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
  Prefs.createBool "silent" false "print nothing except error messages"
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
  Prefs.createBoolWithDefault "color" ~local:true
    "!use color output for text UI (true/false/default)"
    ("When set to {\\tt true}, this flag enables color output in "
     ^ "text mode user interface. When set to {\\tt false}, all "
     ^ "color output is disabled. Default is to enable color if "
     ^ "the {\\tt NO\\_COLOR} environment variable is not set.")

let colorEnabled = ref false

let setColorPreference () =
  let envOk = try let _ = System.getenv "NO_COLOR" in false
    with Not_found -> true
  and termOk = try System.getenv "TERM" <> "dumb" with Not_found -> true
  and ttyOk = (Unix.isatty Unix.stdin) && (Unix.isatty Unix.stderr) in
  let colorOk = envOk && termOk && ttyOk && not (Prefs.read dumbtty) in
  colorEnabled :=
    match Prefs.read colorMode with
    | `True    -> true
    | `False   -> false
    | `Default -> colorOk && Sys.os_type <> "Win32"

let color t =
  if not !colorEnabled then "" else
  match t with
    `Reset       -> "\o033[0m"
  | `Focus       -> "\o033[1m"
  | `Success     -> "\o033[1;32m"
  | `Information -> "\o033[1;34m"
  | `Warning     -> "\o033[1;33m"
  | `Failure     -> "\o033[1;31m"
  | `AError      -> "\o033[31m"
  | `ASkip       -> "\o033[1;35m"
  | `ALtoRf      -> "\o033[1;32m"
  | `ALtoRt      -> "\o033[1;33m"
  | `ARtoLf      -> "\o033[1;34m"
  | `ARtoLt      -> "\o033[1;33m"
  | `AMerge      -> "\o033[1;36m"
  | `DiffHead    -> "\o033[1m"
  | `DiffAdd     -> "\o033[32m"
  | `DiffDel     -> "\o033[31m"
  | `DiffLoc     -> "\o033[36m"
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
    None ->
      let l = input_line stdin in
      if l="" then "" else String.sub l 0 1
  | Some funs ->
      let input_char () =
        (* We cannot used buffered I/Os under Windows, as character
           '\r' is not passed through (probably due to the code that
           turns \r\n into \n) *)
        let s = Bytes.create 1 in
        let n = Unix.read Unix.stdin s 0 1 in
        if n = 0 then raise End_of_file;
        if Bytes.get s 0 = '\003' then raise Sys.Break;
        Bytes.get s 0
      in
      funs.System.startReading ();
      let c = input_char () in
      funs.System.stopReading ();
      let c = if c='\n' || c = '\r' then "" else String.make 1 c in
      display c;
      c

let newLine () =
  (* If in dumb mode (i.e. not in cbreak mode) the newline is entered by the
     user to validate the input *)
  if !cbreakMode <> None then display "\n"

let overwrite () =
  if !cbreakMode <> None then display "\r"

let rec selectAction batch actions tryagain =
  let formatname = function
      "" -> "<ret>"
    | " " -> "<spc>"
    | "\x7f" -> "<del>"
    | "\b" -> "<bsp>"
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
    if a="?" then
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
            display ("No default command [type '?' for help]\n")
          else
            display ("Unrecognized command '" ^ String.escaped a
                     ^ "': try again  [type '?' for help]\n");
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
                       ^ System.fspathToPrintString (Prefs.profilePathname n)
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
                 (["n";"j"],
                  ("go to the next item"),
                  (fun () -> newLine();
                     next()));
                 (["p";"b";"k"],
                  ("go back to previous item"),
                  (fun () -> newLine();
                     previous prev ril));
                 (["\x7f";"\b"],
                  ("revert then go back to previous item"),
                  (fun () ->
                     Recon.revertToDefaultDirection ri; redisplayri();
                     previous prev ril));
                 (["0"],
                  ("go to the start of the list"),
                  (fun () -> newLine();
                     loop [] (Safelist.rev_append prev ril)));
                 (["9"],
                  ("go to the end of the list"),
                  (fun () -> newLine();
                     match Safelist.rev_append ril prev with
                       [] -> loop [] []
                     | lri::prev -> loop prev [lri]));
                 (["5"],
                  ("go forward to the middle of the following items"),
                  (fun () -> newLine();
                     let l = (Safelist.length ril)/2 in
                     display ("  Moving "^(string_of_int l)^" items forward\n");
                     forward l prev ril));
                 (["6"],
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
                     actOnMatching
                       ~fail:(Some (fun() ->
                           display ((Uicommon.cannotMergeMsg ~path:None)^"\n")))
                       (fun ri -> if Globals.shouldMerge ri.path1
                                  then setdir Merge ri else false)));
                 ([">";"."],
                  ("propagate from " ^ descr ^ " (curr or match)"),
                  (fun () ->
                     actOnMatching (setdir Replica1ToReplica2)));
                 (["<";","],
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
                 (["q"],
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
  let t0 = Unix.gettimeofday () in
  let calcProgress i bytes dbg =
    let i = Uutil.File.toLine i in
    let item = items.(i) in
    item.bytesTransferred <- Uutil.Filesize.add item.bytesTransferred bytes;
    totalBytesTransferred := Uutil.Filesize.add !totalBytesTransferred bytes;
    let v =
      (Uutil.Filesize.percentageOfTotalSize
         !totalBytesTransferred !totalBytesToTransfer)
    in
    let t1 = Unix.gettimeofday () in
    let remTime =
      if v <= 0. then "--:--"
      else if v >= 100. then "00:00"
      else
        let t = truncate ((t1 -. t0) *. (100. -. v) /. v +. 0.5) in
        Format.sprintf "%02d:%02d" (t / 60) (t mod 60)
    in
    t1, Format.sprintf "%s  %s ETA" (Util.percent2string v) remTime
  in
  let logOnly s =
    let temp = !Trace.sendLogMsgsToStderr in
    Trace.sendLogMsgsToStderr := false;
    Trace.log (s ^ "\n");
    Trace.sendLogMsgsToStderr := temp
  in
  let tlog = ref t0 in
  let showProgress i bytes dbg =
    let t1, s = calcProgress i bytes dbg in
    if not (Prefs.read Trace.terse) && (Prefs.read Trace.debugmods = []) then
      Util.set_infos s;
    if (Prefs.read Trace.terse) || (Prefs.read Globals.batch) then
      if (t1 -. !tlog) >= 60. then
      begin
        logOnly s;
        tlog := t1
      end
  in
  Uutil.setProgressPrinter showProgress;

  Transport.logStart ();
  let fFailedPaths = ref [] in
  let fPartialPaths = ref [] in
  let uiWrapper i item f =
    Lwt.try_bind f
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
  let im = Array.length items in
  let rec loop i actions pRiThisRound =
    if i < im then begin
      let item = items.(i) in
      let actions =
        if pRiThisRound item.ri then
          uiWrapper i item
            (fun () -> Transport.transportItem item.ri
                         (Uutil.File.ofLine i) verifyMerge)
          :: actions
        else
          actions
      in
      loop (i + 1) actions pRiThisRound
    end else
      actions
  in
  Lwt_unix.run
    (let actions = loop 0 [] (fun ri -> not (Common.isDeletion ri)) in
     Lwt_util.join actions);
  Lwt_unix.run
    (let actions = loop 0 [] Common.isDeletion in
     Lwt_util.join actions);
  Transport.logFinish ();

  Uutil.setProgressPrinter (fun _ _ _ -> ());
  Util.set_infos "";

  (Safelist.rev !fFailedPaths, Safelist.rev !fPartialPaths)

let setWarnPrinterForInitialization()=
  Util.warnPrinter :=
     Some(fun s ->
            alwaysDisplay "Error: ";
            alwaysDisplay (s^"\n");
            exit Uicommon.fatalExit)

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
            : bool * bool * bool * (Path.t list)
              (* anySkipped?, anyPartial?, anyFailures?, failingPaths *) =
  let (proceed,newReconItemList) = interact prevItemList reconItemList in
  let (updatesToDo, skipped) =
    Safelist.fold_left
      (fun (howmany, skipped) ri ->
        if problematic ri then (howmany, skipped + 1)
        else (howmany + 1, skipped))
      (0, 0) newReconItemList in
  let doit() =
    if not (Prefs.read Globals.batch || Prefs.read Trace.terse) then newLine();
    if not (Prefs.read Trace.terse) then Trace.status "Propagating updates";
    let timer = Trace.startTimer "Transmitting all files" in
    let (failedPaths, partialPaths) = doTransport newReconItemList in
    let failures = Safelist.length failedPaths in
    let partials = Safelist.length partialPaths in
    Trace.showTimer timer;
    if not (Prefs.read Trace.terse) then Trace.status "Saving synchronizer state";
    Update.commitUpdates ();
    let trans = updatesToDo - failures in
    let summary =
      Printf.sprintf
       "Synchronization %s at %s  (%d item%s transferred, %s%s, %s)"
       (if failures = 0 then (color `Success) ^ "complete" ^ (color `Reset) else (color `Failure) ^ "incomplete" ^ (color `Reset))
       (let tm = Util.localtime (Util.time()) in
        Printf.sprintf "%02d:%02d:%02d"
          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
       trans (if trans=1 then "" else "s")
       (if partials <> 0 then
          Format.sprintf "%d partially transferred, " partials
        else
          "")
       (if skipped = 0 then "0 skipped" else (color `Information) ^ (Printf.sprintf "%d skipped" skipped) ^ (color `Reset))
       (if failures = 0 then "0 failed" else (color `Failure) ^ (Printf.sprintf "%d failed" failures) ^ (color `Reset)) in
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
    (skipped > 0, partials > 0, failures > 0, failedPaths) in
  if updatesToDo = 0 then begin
    (* BCP (3/09): We need to commit the archives even if there are
       no updates to propagate because some files (in fact, if we've
       just switched to DST on windows, a LOT of files) might have new
       modtimes in the archive. *)
    (* JV (5/09): Don't save the archive in repeat mode as it has some
       costs and its unlikely there is much change to the archives in
       this mode. *)
    if !Update.foundArchives && Prefs.read Uicommon.repeat = "" then
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
    (skipped > 0, false, false, [])
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
         (["q"],
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

let synchronizeOnce ?wantWatcher ?skipRecentFiles pathsOpt =
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
  Uicommon.refreshConnection
    ~displayWaitMessage:(fun () -> if not (Prefs.read silent)
                         then Util.msg "%s\n" (Uicommon.contactingServerMsg()))
    ~termInteract:None;
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
    if !Update.foundArchives && Prefs.read Uicommon.repeat = "" then
      Update.commitUpdates ();
    (if anyEqualUpdates then
      Trace.status ("Nothing to do: replicas have been changed only "
                    ^ "in identical ways since last sync.")
     else
       Trace.status "Nothing to do: replicas have not changed since last sync.");
    (Uicommon.perfectExit, [])
  end else begin
    checkForDangerousPath dangerousPaths;
    let (anySkipped, anyPartial, anyFailures, failedPaths) =
      interactAndPropagateChanges [] reconItemList in
    let exitStatus = Uicommon.exitCode(anySkipped || anyPartial,anyFailures) in
    (exitStatus, failedPaths)
  end

(* ----------------- Filesystem watching mode ---------------- *)

let watchinterval = 1.    (* Minimal interval between two synchronizations *)
let retrydelay = 5.       (* Minimal delay to retry failed paths *)
let maxdelay = 30. *. 60. (* Maximal delay to retry failed paths *)

module PathMap = Map.Make (Path)

let waitForChangesRoot: Common.root -> unit -> unit Lwt.t =
  Remote.registerRootCmd
    "waitForChanges"
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
      synchronizeOnce ~wantWatcher:() ~skipRecentFiles:()
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
  let rec loop triesLeft pathsOpt =
    let (exitStatus, failedPaths) =
      synchronizeOnce
        ?wantWatcher:(if repeatMode then Some () else None) pathsOpt in
    if failedPaths <> [] && triesLeft <> 0 then begin
      loop (triesLeft - 1) (Some (failedPaths, []))
    end else begin
      exitStatus
    end in
  loop (Prefs.read Uicommon.retry) None

let rec synchronizeUntilDone () =
  let repeatinterval =
    if Prefs.read Uicommon.repeat = "" then -1 else
    try int_of_string (Prefs.read Uicommon.repeat)
    with Failure _ ->
      (* If the 'repeat' pref is not a valid number, switch modes... *)
      if Prefs.read Uicommon.repeat = "watch" then
        synchronizePathsFromFilesystemWatcher()
      else
        raise (Util.Fatal ("Value of 'repeat' preference ("
                           ^Prefs.read Uicommon.repeat
                           ^") should be either a number or 'watch'\n")) in

  let exitStatus = synchronizeUntilNoFailures(repeatinterval >= 0) in
  if repeatinterval < 0 then
    exitStatus
  else begin
    (* Do it again *)
    Trace.status (Printf.sprintf
       "\nSleeping for %d seconds...\n" repeatinterval);
    Unix.sleep repeatinterval;
    synchronizeUntilDone ()
  end

(* ----------------- Startup ---------------- *)

let profmgrPrefName = "i"
let profmgrPref =
  Prefs.createBool profmgrPrefName false ~local:true
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

let getProfile default =
  let cmdArgs = Prefs.scanCmdLine Uicommon.shortUsageMsg in
  Uicommon.scanProfiles ();
  if Util.StringMap.mem Uicommon.runTestsPrefName cmdArgs ||
    not (Util.StringMap.mem profmgrPrefName cmdArgs) then
    Some default
  else
  if (List.length !Uicommon.profilesAndRoots) > 10 then begin
    Trace.log (Format.sprintf "You have too many profiles in %s \
                for interactive selection. Please specify profile \
                or roots on command line.\n"
                (System.fspathToPrintString Util.unisonDir));
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
  Trace.log (msg ^ "\n");
  if not !Trace.sendLogMsgsToStderr then alwaysDisplay ("\n" ^ msg ^ "\n")

let rec start interface =
  if interface <> Uicommon.Text then
    Util.msg "This Unison binary only provides the text GUI...\n";
  begin try
    (* Just to make sure something is there... *)
    setWarnPrinterForInitialization();
    Uicommon.uiInit
      ~reportError:
      (fun s -> Util.msg "%s%s\n\n%s\n" Uicommon.shortUsageMsg profmgrUsageMsg s; exit 1)
      ~tryAgainOrQuit:
      (fun s -> Util.msg "%s" Uicommon.shortUsageMsg; exit 1)
      ~displayWaitMessage:
      (fun () -> setWarnPrinter();
                 if Prefs.read silent then Prefs.set Trace.terse true;
                 if not (Prefs.read silent)
                 then Util.msg "%s\n" (Uicommon.contactingServerMsg()))
      ~getProfile:
      (fun () -> let prof = getProfile "default" in restoreTerminal(); prof)
      ~getFirstRoot:
      (fun () -> Util.msg "%s%s\n" Uicommon.shortUsageMsg profmgrUsageMsg; exit 1)
      ~getSecondRoot:
      (fun () -> Util.msg "%s%s\n" Uicommon.shortUsageMsg profmgrUsageMsg; exit 1)
      ~termInteract:
      None;

    (* Some preference settings imply others... *)
    if Prefs.read silent then begin
      Prefs.set Globals.batch true;
      Prefs.set Trace.terse true;
      Prefs.set dumbtty true;
      Trace.sendLogMsgsToStderr := false;
    end;
    if Prefs.read Uicommon.repeat <> "" then begin
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
  | e -> begin
      (* If any other bad thing happened and the -repeat preference is
         set, then restart *)
      (* JV: it seems safer to just abort here, as we don't know in which
         state Unison is; for instance, if the connection is lost, there
         is no point in restarting as Unison will currently not attempt to
         establish a new connection. *)
      handleException e;
      if false (*Prefs.read Uicommon.repeat <> ""*) then begin
        Util.msg "Restarting in 10 seconds...\n";
        Unix.sleep 10;
        start interface
      end else
        exit Uicommon.fatalExit
    end
  end

let defaultUi = Uicommon.Text

end
