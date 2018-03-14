(* Unison file synchronizer: src/pred.ml *)
(* Copyright 1999-2018, Benjamin C. Pierce

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


let debug = Util.debug "pred"

(********************************************************************)
(*                              TYPES                               *)
(********************************************************************)

type t =
  { pref: string list Prefs.t;
    name: string;                  (* XXX better to get it from Prefs! *)
    mutable default: string list;
    mutable last_pref : string list;
    mutable last_def : string list;
    mutable last_mode : Case.mode;
    mutable compiled: Rx.t;
    mutable associated_strings : (Rx.t * string) list;
  }

let error_msg s =
   Printf.sprintf "bad pattern: %s\n\
    A pattern must be introduced by one of the following keywords:\n\
 \032   Regex, Name, Path, BelowPath, NameString, String, BelowString\n\
 \032   (or add <KEYWORD> or del <KEYWORD> or assoc <KEYWORD>)." s

let addPref = "add "
let delPref = "del "
let assocPref = "assoc "

(* [select_pattern str [(p1, f1), ..., (pN, fN)] fO]: (roughly) *)
(* match str with                                               *)
(*  p1 p' -> f1 p'                                              *)
(*  ...                                                         *)
(*  pN p' -> fN p'                                              *)
(*  otherwise -> fO str                                         *)
let rec select_pattern str l err =
  let rest realpref g =
    let l = String.length realpref in
    let s =
      Util.trimWhitespace (String.sub str l (String.length str - l)) in
    g (Util.trimWhitespace realpref) ((Case.ops())#normalizePattern s) in
  match l with
    [] -> err str
  | (pref, g)::r ->
      if Util.startswith str pref then `Alt (rest pref g)
      else if Util.startswith str (addPref^pref) then `Alt (rest (addPref^pref) g)
      else if Util.startswith str (delPref^pref) then `Dif (rest (delPref^pref) g)
      else if Util.startswith str (assocPref^pref) then `Nul (rest (assocPref^pref) g)
      else select_pattern str r err

let mapSeparator = "->"

(* Compile a pattern (in string form) to a regular expression *)
let compile_pattern clause =
  let (p,v) =
    match Util.splitIntoWordsByString clause mapSeparator with
      [p] -> (p,None)
    | [p;v] -> (p, Some (Util.trimWhitespace v))
    | [] -> raise (Prefs.IllegalValue "Empty pattern")
    | _ -> raise (Prefs.IllegalValue ("Malformed pattern: "
                  ^ "\"" ^ clause ^ "\"\n"
                  ^ "Only one instance of " ^ mapSeparator ^ " allowed.")) in
  let compiled =
    begin try
      let checkpath prefix str =
        let msg =
          "Malformed pattern: \"" ^ p ^ "\"\n"
          ^ "'" ^ prefix ^ "' patterns may not begin with a slash; "
          ^ "only relative paths are allowed." in
        if str<>"" && str.[0] = '/' then
          raise (Prefs.IllegalValue msg) in
      let name rx = Rx.seq [Rx.rx "(.*/)?"; rx]
      and below rx = Rx.seq [rx; Rx.rx "(/.*)?"]
      and del_quotes c str =
        let l = String.length str in
        if l >= 2 && str.[0] = c && str.[l-1] = c
        then String.sub str 1 (l-2)
        else str
      in
      select_pattern p
        [("Name ", fun realpref str -> checkpath realpref str;
            name (Rx.globx str));
         ("Path ", fun realpref str -> checkpath realpref str;
            Rx.globx str);
         ("BelowPath ", fun realpref str -> checkpath realpref str;
            below (Rx.globx str));
         ("NameString ", fun realpref str -> checkpath realpref (del_quotes '\'' str);
            name (Rx.str (del_quotes '\'' str)));
         ("String ", fun realpref str -> checkpath realpref (del_quotes '\'' str);
            Rx.str (del_quotes '\'' str));
         ("BelowString ", fun realpref str -> checkpath realpref (del_quotes '\'' str);
            below (Rx.str (del_quotes '\'' str)));
         ("Regex ", fun realpref str -> checkpath realpref str;
            Rx.rx str)]
        (fun str -> raise (Prefs.IllegalValue (error_msg p)))
    with
      Rx.Parse_error | Rx.Not_supported ->
        raise (Prefs.IllegalValue ("Malformed pattern \"" ^ p ^ "\"."))
    end in
  (compiled, v)

let create name ?(local=false) ?(advanced=false) fulldoc =
  let pref =
    Prefs.create name ~local []
      ((if advanced then "!" else "")
       ^ "add a pattern to the " ^ name ^ " list")
      fulldoc
      (fun oldList string ->
         ignore (compile_pattern string); (* Check well-formedness *)
        string :: oldList)
      (fun l -> l) in
  {pref = pref; name = name;
   last_pref = []; default = []; last_def = []; last_mode = (Case.ops())#mode;
   compiled = Rx.empty; associated_strings = []}

let addDefaultPatterns p pats =
  p.default <- Safelist.append pats p.default

let alias p n = Prefs.alias p.pref n

let recompile mode p =
  (* Accumulate consecutive pathspec regexps with the same sign and discard
     null patterns *)
  let rev_acc_alt_or_dif acc r =
    match acc, r with
      (`Alt rl :: t), `Alt rx -> `Alt (rx::rl) :: t
    | (`Dif rl :: t), `Dif rx -> `Dif (rx::rl) :: t
    | _             , `Alt rx -> `Alt [rx]     :: acc
    | _             , `Dif rx -> `Dif [rx]     :: acc
    | _             , `Nul rx ->                  acc
  (* Combine newer positive or negative pathspec regexps with the older ones *)
  and combine_alt_or_dif rx = function
      `Alt rl -> Rx.alt [Rx.alt rl; rx]
    | `Dif rl -> Rx.diff rx (Rx.alt rl)
        (* A negative pattern is diff'ed from the former ones only *)
  in
  let pref = Prefs.read p.pref in
  let compiledList = Safelist.append p.default pref
    |> Safelist.rev_map compile_pattern in
  let compiled = Safelist.rev compiledList
    |> Safelist.fold_left (fun a (r, _) -> rev_acc_alt_or_dif a r) []
    |> Safelist.fold_left combine_alt_or_dif Rx.empty in
         (* The patterns are processed in order of appearance so that later
            preferences override the previous ones. *)
  let handleCase rx =
    if (Case.ops())#caseInsensitiveMatch then Rx.case_insensitive rx
    else rx
  in
  let nodif_string = function
      `Alt rx, Some v | `Nul rx, Some v -> Some (handleCase rx, v)
    | _ -> None in
  let strings = Safelist.rev_filterMap nodif_string compiledList in
  p.compiled <- handleCase compiled;
  p.associated_strings <- strings;
  p.last_pref <- pref;
  p.last_def <- p.default;
  p.last_mode <- mode

let recompile_if_needed p =
  let mode = (Case.ops())#mode in
  if
    p.last_mode <> mode ||
    p.last_pref != Prefs.read p.pref ||
    p.last_def != p.default
  then
    recompile mode p

(********************************************************************)
(*                         IMPORT / EXPORT                          *)
(********************************************************************)

let intern p regexpStringList = Prefs.set p.pref regexpStringList

let extern p = Prefs.read p.pref

let extern_associated_strings p =
  recompile_if_needed p;
  Safelist.map snd p.associated_strings

(********************************************************************)
(*                             TESTING                              *)
(********************************************************************)

let test p s =
  recompile_if_needed p;
  let res = Rx.match_string p.compiled ((Case.ops())#normalizeMatchedString s) in
  debug (fun() -> Util.msg "%s '%s' = %b\n" p.name s res);
  res

let assoc p s =
  recompile_if_needed p;
  let s = (Case.ops())#normalizeMatchedString s in
  snd (Safelist.find (fun (rx,v) -> Rx.match_string rx s) p.associated_strings)

let assoc_all p s =
  recompile_if_needed p;
  let s = (Case.ops())#normalizeMatchedString s in
  Safelist.map snd
    (Safelist.filter (fun (rx,v) -> Rx.match_string rx s) p.associated_strings)
