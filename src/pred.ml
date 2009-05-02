(* Unison file synchronizer: src/pred.ml *)
(* Copyright 1999-2009, Benjamin C. Pierce 

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
    mutable last_mode : bool;
    mutable compiled: Rx.t;
    mutable associated_strings : (Rx.t * string) list;
  }

let error_msg s =
   Printf.sprintf "bad pattern: %s\n\
    A pattern must be introduced by one of the following keywords:\n\
 \032   Name, Path, or Regex." s

(* [select str [(p1, f1), ..., (pN, fN)] fO]: (roughly) *)
(* match str with                                       *)
(*  p1 p' -> f1 p'                                      *) 
(*  ...		       	       	       	       	       	*) 
(*  pN p' -> fN p'   					*)
(*  otherwise -> fO str	       	       	       	        *)
let rec select str l f =
  match l with
    [] -> f str
  | (pref, g)::r ->
      if Util.startswith str pref then
        let l = String.length pref in
        g (Util.trimWhitespace (String.sub str l (String.length str - l)))
      else
        select str r f

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
      select p
        [("Name ", fun str -> Rx.seq [Rx.rx "(.*/)?"; Rx.globx str]);
         ("Path ", fun str ->
            if str<>"" && str.[0] = '/' then
              raise (Prefs.IllegalValue
                       ("Malformed pattern: "
                        ^ "\"" ^ p ^ "\"\n"
                        ^ "'Path' patterns may not begin with a slash; "
                        ^ "only relative paths are allowed."));
            Rx.globx str);
         ("Regex ", Rx.rx)]
        (fun str -> raise (Prefs.IllegalValue (error_msg p)))
    with
      Rx.Parse_error | Rx.Not_supported ->
        raise (Prefs.IllegalValue ("Malformed pattern \"" ^ p ^ "\"."))
    end in
  (compiled, v)

let create name ?(advanced=false) fulldoc =
  let pref = 
    Prefs.create name []
      ((if advanced then "!" else "")
       ^ "add a pattern to the " ^ name ^ " list")
      fulldoc
      (fun oldList string ->
         ignore (compile_pattern string); (* Check well-formedness *)
        string :: oldList)
      (fun l -> l) in
  {pref = pref; name = name;
   last_pref = []; default = []; last_def = []; last_mode = false;
   compiled = Rx.empty; associated_strings = []} 

let addDefaultPatterns p pats =
  p.default <- Safelist.append pats p.default

let alias p n = Prefs.alias p.pref n

let recompile mode p =
  let pref = Prefs.read p.pref in
  let compiledList = Safelist.map compile_pattern (Safelist.append p.default pref) in
  let compiled = Rx.alt (Safelist.map fst compiledList) in
  let strings = Safelist.filterMap
                  (fun (rx,vo) ->
                     match vo with
                       None -> None
                     | Some v -> Some (rx,v))
                  compiledList in
  p.compiled <- if mode then Rx.case_insensitive compiled else compiled;
  p.associated_strings <- strings;
  p.last_pref <- pref;
  p.last_def <- p.default;
  p.last_mode <- mode

let recompile_if_needed p =
  let mode = Case.insensitive () in
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
  let res = Rx.match_string p.compiled (Case.normalize s) in
  debug (fun() -> Util.msg "%s '%s' = %b\n" p.name s res);
  res

let assoc p s =
  recompile_if_needed p;
  snd (Safelist.find (fun (rx,v) -> Rx.match_string rx s) p.associated_strings)
