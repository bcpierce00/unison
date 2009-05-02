(* Unison file synchronizer: src/ubase/rx.ml *)
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

(*
  Inspired by some code and algorithms from Mark William Hopkins
  (regexp.tar.gz, available in the comp.compilers file archive)
*)

(*
Missing POSIX features
----------------------
- Collating sequences
*)

type v =
    Cst of int list
  | Alt of u list
  | Seq of u list
  | Rep of u * int * int option
  | Bol | Eol
  | Int of u list
  | Dif of u * u

and u = { desc : v; hash : int }

(****)

let hash x =
  match x with
    Cst l -> List.fold_left (fun h i -> h + 757 * i) 0 l
  | Alt l -> 199 * List.fold_left (fun h y -> h + 883 * y.hash) 0 l
  | Seq l -> 821 * List.fold_left (fun h y -> h + 883 * y.hash) 0 l
  | Rep (y, i, Some j) -> 197 * y.hash + 137 * i + j
  | Rep (y, i, None) -> 197 * y.hash + 137 * i + 552556457
  | Bol -> 165160782
  | Eol -> 152410806
  | Int l -> 71 * List.fold_left (fun h y -> h + 883 * y.hash) 0 l
  | Dif (y, z) -> 379 * y.hash + 563 * z.hash

let make x = {desc = x; hash = hash x}

let epsilon = make (Seq [])
let empty = make (Alt [])

(**** Printing ****)

open Format

let print_list sep print l =
  match l with
    [] -> ()
  | v::r -> print v; List.iter (fun v -> sep (); print v) r

let rec print n t =
  match t.desc with
    Cst l ->
      open_box 1; print_string "[";
      print_list print_space print_int l;
      print_string "]"; close_box ()
  | Alt tl ->
      if n > 0 then begin open_box 1; print_string "(" end;
      print_list (fun () -> print_string "|"; print_cut ()) (print 1) tl;
      if n > 0 then begin print_string ")"; close_box () end
  | Seq tl ->
      if n > 1 then begin open_box 1; print_string "(" end;
      print_list (fun () -> print_cut ()) (print 2) tl;
      if n > 1 then begin print_string ")"; close_box () end
  | Rep (t, 0, None) ->
      print 2 t; print_string "*"
  | Rep (t, i, None) ->
      print 2 t; print_string "{"; print_int i; print_string ",}"
  | Rep (t, i, Some j) ->
      print 2 t;
      print_string "{"; print_int i; print_string ",";
      print_int j; print_string "}"
  | _ -> assert false

(**** Constructors for regular expressions *)

let seq2 x y =
  match x.desc, y.desc with
    Alt [], _ | _, Alt [] -> empty
  | Seq [], s             -> y
  | r, Seq []             -> x
  | Seq r, Seq s          -> make (Seq (r @ s))
  | Seq r, _              -> make (Seq (r @ [y]))
  | _, Seq s              -> make (Seq (x :: s))
  | r, s                  -> make (Seq [x; y])

let seq l = List.fold_right seq2 l epsilon

let seq' l = match l with [] -> epsilon | [x] -> x | _ -> make (Seq l)

let rec alt_merge r s =
  match r, s with
    [], _ -> s
  | _, [] -> r
  | {desc = Seq (x::m)} :: s, {desc = Seq (y::n)} :: r when x = y ->
      alt_merge (seq2 x (alt2 (seq' m) (seq' n))::s) r
  | x :: r', y :: s' ->
      let c = compare x y in
      if c = 0 then x :: alt_merge r' s'
      else if c < 0 then x :: alt_merge r' s
      else (* if c > 0 then *) y :: alt_merge r s'

and alt2 x y =
  let c = compare x y in
  if c = 0 then x else
  match x.desc, y.desc with
    Alt [], _             -> y
  | _, Alt []             -> x
  | Alt r, Alt s          -> make (Alt (alt_merge r s))
  | Alt [r], _ when r = y -> y
  | _, Alt [s] when x = s -> x
  | Alt r, _              -> make (Alt (alt_merge r [y]))
  | _, Alt s              -> make (Alt (alt_merge [x] s))
  | Seq (r::m), Seq (s::n) when r = s -> seq2 r (alt2 (seq' m) (seq' n))
  | _, _                  -> make (if c < 0 then Alt [x; y] else Alt [y; x])

let alt l = List.fold_right alt2 l empty

let rep x i j =
  match x.desc with
    Alt [] when i > 0 -> empty
  | Alt [] | Seq []   -> epsilon
  | _                 ->
      match i, j with
        _, Some 0 -> epsilon
      | 0, Some 1 -> alt2 epsilon x
      | 1, Some 1 -> x
      | _         -> make (Rep (x, i, j))

let rec int2 x y =
  let c = compare x y in
  if c = 0 then x else
  match x.desc, y.desc with
    Int [], _             -> y
  | _, Int []             -> x
  | Int r, Int s          -> make (Int (alt_merge r s))
  | Int [r], _ when r = y -> y
  | _, Int [s] when s = x -> x
  | Int r, _              -> make (Int (alt_merge r [y]))
  | _, Int s              -> make (Int (alt_merge [x] s))
  | _, _                  -> make (if c < 0 then Int [x; y] else Int [y; x])

let int l = List.fold_right int2 l empty

let cst c = Cst [Char.code c]

let rec dif x y =
  if x = y then empty else
  match x.desc, y.desc with
    Dif (x1, y1), _ -> dif x1 (alt2 y1 y)
  | Alt [], _       -> empty
  | _, Alt []       -> x 
  | _               -> make (Dif (x, y))

(**** Computation of the next states of an automata ****)

type pos = Pos_bol | Pos_other
let never = 0
let always = (-1)
let when_eol = 2

let combine top bot op f l =
  let rec combine v l =
    match l with
      [] -> v
    | a::r ->
        let c = f a in
        if c = bot then c else combine (op v c) r
  in
  combine top l

module ReTbl =
  Hashtbl.Make
    (struct
       type t = u
       let equal x y = x.hash = y.hash && x = y
       let hash x = x.hash
     end)

let h = ReTbl.create 101
let rec contains_epsilon pos x =
try ReTbl.find h x with Not_found ->
let res =
  match x.desc with
    Cst _         -> never
  | Alt l         -> combine never always (lor) (contains_epsilon pos) l
  | Seq l         -> combine always never (land) (contains_epsilon pos) l
  | Rep (_, 0, _) -> always
  | Rep (y, _, _) -> contains_epsilon pos y
  | Bol           -> if pos = Pos_bol then always else never
  | Eol           -> when_eol
  | Int l         -> combine always never (land) (contains_epsilon pos) l
  | Dif (y, z)    -> contains_epsilon pos y land
                     (lnot (contains_epsilon pos z))
in
ReTbl.add h x res; res

module DiffTbl =
  Hashtbl.Make
    (struct
       type t = int * u
       let equal ((c : int), x) (d, y) = c = d && x.hash = y.hash && x = y
       let hash (c, x) = x.hash + 11 * c
     end)

let diff_cache = DiffTbl.create 101

let rec delta_seq nl pos c l =
  match l with
    [] ->
      empty
  | x::r ->
      let rdx = seq2 (delta nl pos c x) (seq' r) in
      let eps = contains_epsilon pos x in
      if eps land always = always then
        alt2 rdx (delta_seq nl pos c r)
      else if eps land when_eol = when_eol && c = nl then
        alt2 rdx (delta_seq nl pos c r)
      else
        rdx

and delta nl pos c x =
let p = (c, x) in
try DiffTbl.find diff_cache p with Not_found ->
let res =
  match x.desc with
    Cst l -> if List.mem c l then epsilon else empty
  | Alt l -> alt (List.map (delta nl pos c) l)
  | Seq l -> delta_seq nl pos c l
  | Rep (y, 0, None) -> seq2 (delta nl pos c y) x
  | Rep (y, i, None) -> seq2 (delta nl pos c y) (rep y (i - 1) None)
  | Rep (y, 0, Some j) -> seq2 (delta nl pos c y) (rep y 0 (Some (j - 1)))
  | Rep (y, i, Some j) -> seq2 (delta nl pos c y) (rep y (i - 1) (Some (j-1)))
  | Eol | Bol -> empty
  | Int l -> int (List.map (delta nl pos c) l)
  | Dif (y, z) -> dif (delta nl pos c y) (delta nl pos c z)
in
DiffTbl.add diff_cache p res;
res

(**** String matching ****)

type state =
  { mutable valid : bool;
    mutable next : state array;
            pos : pos;
            final : bool;
            desc : u }

type rx =
  { initial : state;
    categ   : int array;
    ncat    : int;
    states  : state ReTbl.t }

let unknown =
  { valid = false; next = [||]; desc = empty ; pos = Pos_bol; final = false }

let mk_state ncat pos desc =
  { valid = desc <> empty;
    next = Array.make ncat unknown;
    pos = pos;
    desc = desc;
    final = contains_epsilon pos desc <> 0 }

let find_state states ncat pos desc =
  try
    ReTbl.find states desc
  with Not_found ->
    let st = mk_state ncat pos desc in
    ReTbl.add states desc st;
    st

let rec validate s i l rx cat st c =
  let nl = cat.(Char.code '\n') in
  let desc = delta nl st.pos c st.desc in
  st.next.(c) <-
    find_state rx.states rx.ncat (if c = nl then Pos_bol else Pos_other) desc;
  loop s i l rx cat st

and loop s i l rx cat st =
  let rec loop i st =
    let c = Array.unsafe_get cat (Char.code (String.unsafe_get s i)) in
    let st' = Array.unsafe_get st.next c in
    if st'.valid then begin
      let i = i + 1 in
      if i < l then
        loop i st'
      else
        st'.final
    end else if st' != unknown then
      false
    else
      validate s i l rx cat st c
  in
  loop i st

let match_str rx s =
  let l = String.length s in
  if l = 0 then rx.initial.final else
  loop s 0 l rx rx.categ rx.initial

(* Combining the final and valid fields may make things slightly faster
   (one less memory access) *)
let rec validate_pref s i l l0 rx cat st c =
  let nl = cat.(Char.code '\n') in
  let desc = delta nl st.pos c st.desc in
  st.next.(c) <-
    find_state rx.states rx.ncat (if c = nl then Pos_bol else Pos_other) desc;
  loop_pref s i l l0 rx cat st

and loop_pref s i l l0 rx cat st =
  let rec loop i l0 st =
    let c = Array.unsafe_get cat (Char.code (String.unsafe_get s i)) in
    let st' = Array.unsafe_get st.next c in
    if st'.valid then begin
      let i = i + 1 in
      let l0 = if st'.final then i else l0 in
      if i < l then
        loop i l0 st'
      else
        l0
    end else if st' != unknown then
      l0
    else
      validate_pref s i l l0 rx cat st c
  in
  loop i l0 st

let match_pref rx s p =
  let l = String.length s in
  if p < 0 || p > l then invalid_arg "Rx.rep";
  let l0 = if rx.initial.final then p else -1 in
  let l0 =
    if l = p then l0 else
    loop_pref s p l l0 rx rx.categ rx.initial
  in
  if l0 >= 0 then Some (l0 - p) else None

let mk_rx init categ ncat =
  let states = ReTbl.create 97 in
  { initial = find_state states ncat Pos_bol init;
    categ = categ;
    ncat = ncat;
    states = states }

(**** Character sets ****)

let rec cunion l l' =
  match l, l' with
    _, [] -> l
  | [], _ -> l'
  | (c1, c2)::r, (c1', c2')::r' ->
      if c2 + 1 < c1' then
        (c1, c2)::cunion r l'
      else if c2' + 1 < c1 then
        (c1', c2')::cunion l r'
      else if c2 < c2' then
        cunion r ((min c1 c1', c2')::r')
      else
        cunion ((min c1 c1', c2)::r) r'

let rec cinter l l' =
  match l, l' with
    _, [] -> []
  | [], _ -> []
  | (c1, c2)::r, (c1', c2')::r' ->
      if c2 < c1' then
        cinter r l'
      else if c2' < c1 then
        cinter l r'
      else if c2 < c2' then
        (max c1 c1', c2)::cinter r l'
      else
        (max c1 c1', c2')::cinter l r'

let rec cnegate mi ma l =
  match l with
    [] ->
      if mi <= ma then [(mi, ma)] else []
  | (c1, c2)::r when ma < c1 ->
      if mi <= ma then [(mi, ma)] else []
  | (c1, c2)::r when mi < c1 ->
      (mi, c1 - 1) :: cnegate c1 ma l
  | (c1, c2)::r (* when c1 <= mi *) ->
      cnegate (max mi (c2 + 1)) ma r

let csingle c = let i = Char.code c in [i, i]

let cadd c l = cunion (csingle c) l

let cseq c c' =
  let i = Char.code c in let i' = Char.code c' in
  if i <= i' then [i, i'] else [i', i]

let rec ctrans o l =
  match l with
    [] -> []
  | (c1, c2) :: r ->
      if c2 + o < 0 || c1 + o > 255 then
        ctrans o r
      else
        (c1 + o, c2 + o) :: ctrans o r

let cany = [0, 255]

type cset = (int * int) list

(**** Compilation of a regular expression ****)

type regexp =
    Set of cset
  | Sequence of regexp list
  | Alternative of regexp list
  | Repeat of regexp * int * int option
  | Beg_of_line | End_of_line
  | Intersection of regexp list
  | Difference of regexp * regexp

let rec split s cm =
  match s with
    []    -> ()
  | (i, j)::r -> cm.(i) <- true; cm.(j + 1) <- true; split r cm

let rec colorize c regexp =
  let rec colorize regexp =
    match regexp with
      Set s                     -> split s c
    | Sequence l                -> List.iter colorize l
    | Alternative l             -> List.iter colorize l
    | Repeat (r, _, _)          -> colorize r
    | Beg_of_line | End_of_line -> split (csingle '\n') c
    | Intersection l            -> List.iter colorize l
    | Difference (s, t)         -> colorize s; colorize t
  in
  colorize regexp

let make_cmap () = Array.make 257 false

let flatten_cmap cm =
  let c = Array.make 256 0 in
  let v = ref 0 in
  for i = 1 to 255 do
    if cm.(i) then incr v;
    c.(i) <- !v
  done;
  (c, !v + 1)

let rec interval i j = if i > j then [] else i :: interval (i + 1) j

let rec cset_hash_rec l =
  match l with
    []        -> 0
  | (i, j)::r -> i + 13 * j + 257 * cset_hash_rec r
let cset_hash l = (cset_hash_rec l) land 0x3FFFFFFF

module CSetMap =
  Map.Make
  (struct
    type t = int * (int * int) list
    let compare (i, u) (j, v) =
      let c = compare i j in if c <> 0 then c else compare u v
   end)

let trans_set cache cm s =
  match s with
    [i, j] when i = j ->
      [cm.(i)] 
  | _ ->
      let v = (cset_hash_rec s, s) in
      try
        CSetMap.find v !cache
      with Not_found ->
        let l =
          List.fold_right (fun (i, j) l -> cunion [cm.(i), cm.(j)] l) s []
        in
        let res =
          List.flatten (List.map (fun (i, j) -> interval i j) l)
        in
        cache := CSetMap.add v res !cache;
        res

let rec trans_seq cache c r rem =
  match r with
    Sequence l -> List.fold_right (trans_seq cache c) l rem
  | _ -> seq2 (translate cache c r) rem

and translate cache c r =
  match r with
    Set s -> make (Cst (trans_set cache c s))
  | Alternative l -> alt (List.map (translate cache c) l)
  | Sequence l -> trans_seq cache c r epsilon
  | Repeat (r', i, j) -> rep (translate cache c r') i j
  | Beg_of_line -> make Bol
  | End_of_line -> make Eol
  | Intersection l -> int (List.map (translate cache c) l)
  | Difference (r', r'') -> dif (translate cache c r') (translate cache c r'')

let compile regexp =
  let c = make_cmap () in
  colorize c regexp;
  let (cat, ncat) = flatten_cmap c in
  let r = translate (ref (CSetMap.empty)) cat regexp in
  mk_rx r cat ncat

(**** Regexp type ****)

type t = {def : regexp; mutable comp: rx option; mutable comp': rx option}

let force r =
  match r.comp with
    Some r' -> r'
  | None -> let r' = compile r.def in r.comp <- Some r'; r'

let anything = Repeat (Set [0, 255], 0, None)
let force' r =
  match r.comp' with
    Some r' -> r'
  | None ->
      let r1 = Sequence [anything; r.def; anything] in
      let r' = compile r1 in r.comp' <- Some r'; r'

let wrap r = {def = r; comp = None; comp' = None}
let def r = r.def

let alt rl = wrap (Alternative (List.map def rl))
let seq rl = wrap (Sequence (List.map def rl))
let empty = alt []
let epsilon = seq []
let rep r i j =
  if i < 0 then invalid_arg "Rx.rep";
  begin match j with Some j when j < i -> invalid_arg "Rx.rep" | _ -> () end;
  wrap (Repeat (def r, i, j))
let rep0 r = rep r 0 None
let rep1 r = rep r 1 None
let opt r = alt [epsilon; r]
let bol = wrap Beg_of_line
let eol = wrap End_of_line
let any = wrap (Set [0, 255])
let notnl = wrap (Set (cnegate 0 255 (csingle '\n')))
let inter rl = wrap (Intersection (List.map def rl))
let diff r r' = wrap (Difference (def r, def r'))

let set str =
  let s = ref [] in
  for i = 0 to String.length str - 1 do
    s := cunion (csingle str.[i]) !s
  done;
  wrap (Set !s)

let str s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := Set (csingle s.[i]) :: !l
  done;
  wrap (Sequence !l)

let match_string t s = match_str (force t) s
let match_substring t s = match_str (force' t) s
let match_prefix t s p = match_pref (force t) s p

let uppercase =
  cunion (cseq 'A' 'Z') (cunion (cseq '\192' '\214') (cseq '\216' '\222'))

let lowercase = ctrans 32 uppercase

let rec case_insens r =
  match r with
    Set s ->
      Set (cunion s (cunion (ctrans 32 (cinter s uppercase))
                            (ctrans (-32) (cinter s lowercase))))
  | Sequence l ->
      Sequence (List.map case_insens l)
  | Alternative l ->
      Alternative (List.map case_insens l)
  | Repeat (r, i, j) ->
      Repeat (case_insens r, i, j)
  | Beg_of_line | End_of_line ->
      r
  | Intersection l ->
      Intersection (List.map case_insens l)
  | Difference (r, r') ->
      Difference (case_insens r, case_insens r')

let case_insensitive r =
  wrap (case_insens (def r))

(**** Parser ****)

exception Parse_error
exception Not_supported

let parse s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let accept c = let r = test c in if r then incr i; r in
  let get () = let r = s.[!i] in incr i; r in
  let unget () = decr i in

  let rec regexp () = regexp' (branch ())
  and regexp' left =
    if accept '|' then regexp' (Alternative [left; branch ()]) else left
  and branch () = branch' (piece ())
  and branch' left =
    if eos () || test '|' || test ')' then left
    else branch' (Sequence [left; piece ()])
  and piece () =
    let r = atom () in
    if accept '*' then Repeat (r, 0, None) else
    if accept '+' then Repeat (r, 1, None) else
    if accept '?' then Alternative [Sequence []; r] else
    if accept '{' then
      match integer () with
        Some i ->
          let j = if accept ',' then integer () else Some i in
          if not (accept '}') then raise Parse_error;
          begin match j with
            Some j when j < i -> raise Parse_error | _ -> ()
          end;
          Repeat (r, i, j)
      | None ->
          unget (); r
    else
      r
  and atom () =
    if accept '.' then Set cany else
    if accept '(' then begin
      let r = regexp () in
      if not (accept ')') then raise Parse_error;
      r
    end else
    if accept '^' then Beg_of_line else
    if accept '$' then End_of_line else
    if accept '[' then begin
      if accept '^' then
        Set (cnegate 0 255 (bracket []))
      else
        Set (bracket [])
    end else
    if accept '\\' then begin
      if eos () then raise Parse_error;
      match get () with
        '|' | '(' | ')' | '*' | '+' | '?'
      | '[' | '.' | '^' | '$' | '{' | '\\' as c -> Set (csingle c)
      |                 _                       -> raise Parse_error
    end else begin
      if eos () then raise Parse_error;
      match get () with
        '*' | '+' | '?' | '{' | '\\' -> raise Parse_error
      |                 c            -> Set (csingle c)
    end
  and integer () =
    if eos () then None else
    match get () with
      '0'..'9' as d -> integer' (Char.code d - Char.code '0')
    |     _        -> unget (); None
  and integer' i =
    if eos () then Some i else
    match get () with
      '0'..'9' as d ->
        let i' = 10 * i + (Char.code d - Char.code '0') in
        if i' < i then raise Parse_error;
        integer' i'
    | _ ->
        unget (); Some i
  and bracket s =
    if s <> [] && accept ']' then s else begin
      let c = char () in
      if accept '-' then begin
        if accept ']' then (cadd c (cadd '-' s)) else begin
          let c' = char () in
          bracket (cunion (cseq c c') s)
        end
      end else
        bracket (cadd c s)
    end
  and char () =
    if eos () then raise Parse_error;
    let c = get () in
    if c = '[' then begin
      if accept '=' || accept ':' then raise Not_supported;
      if accept '.' then begin
        if eos () then raise Parse_error;
        let c = get () in
        if not (accept '.') then raise Not_supported;
        if not (accept ']') then raise Parse_error;
        c
      end else
        c
    end else
      c
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res

let rx s = wrap (parse s)

(**** File globbing ****)

let gany = cnegate 0 255 (csingle '/')
let notdot = cnegate 0 255 (cunion (csingle '.') (csingle '/'))
let dot = csingle '.'

type loc = Beg | BegAny | Mid

let beg_start =
  Alternative [Sequence []; Sequence [Set notdot; Repeat (Set gany, 0, None)]]

let beg_start' =
  Sequence [Set notdot; Repeat (Set gany, 0, None)]

let glob_parse init s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let accept c = let r = test c in if r then incr i; r in
  let get () = let r = s.[!i] in incr i; r in
  (* let unget () = decr i in *)

  let rec expr () = expr' init (Sequence [])
  and expr' beg left =
    if eos () then
      match beg with
        Mid | Beg -> left
      | BegAny -> Sequence [left; beg_start]
    else
      let (piec, beg) = piece beg in expr' beg (Sequence [left; piec])
  and piece beg =
    if accept '*' then begin
      if beg <> Mid then
        (Sequence [], BegAny)
      else
        (Repeat (Set gany, 0, None), Mid)
    end else if accept '?' then
      (begin match beg with
         Beg    -> Set notdot
       | BegAny -> Sequence [Set notdot; Repeat (Set gany, 0, None)]
       | Mid    -> Set gany
       end,
       Mid)
    else if accept '[' then begin
      (* let mask = if beg <> Mid then notdot else gany in *)
      let set =
        if accept '^' || accept '!' then
          cnegate 0 255 (bracket [])
        else
          bracket []
      in
      (begin match beg with
         Beg -> Set (cinter notdot set)
       | BegAny -> Alternative [Sequence [beg_start; Set (cinter notdot set)];
                                Sequence [beg_start'; Set (cinter dot set)]]
       | Mid -> Set (cinter gany set)
       end,
       Mid)
    end else
      let c = char () in
      ((if beg <> BegAny then
          Set (csingle c)
        else if c = '.' then
          Sequence [beg_start'; Set (csingle c)]
        else
          Sequence [beg_start; Set (csingle c)]),
       if c = '/' then init else Mid)
  and bracket s =
    if s <> [] && accept ']' then s else begin
      let c = char () in
      if accept '-' then begin
        if accept ']' then (cadd c (cadd '-' s)) else begin
          let c' = char () in
          bracket (cunion (cseq c c') s)
        end
      end else
        bracket (cadd c s)
    end
  and char () =
    ignore (accept '\\');
    if eos () then raise Parse_error;
    get ()
  in
  let res = expr () in
  res

let rec mul l l' =
  List.flatten (List.map (fun s -> List.map (fun s' -> s ^ s') l') l)

let explode str =
  let l = String.length str in
  let rec expl inner s i acc beg =
    if i >= l then begin
      if inner then raise Parse_error;
      (mul beg [String.sub str s (i - s)], i)
    end else
    match str.[i] with
      '\\' -> expl inner s (i + 2) acc beg
    | '{' ->
        let (t, i') = expl true (i + 1) (i + 1) [] [""] in
        expl inner i' i' acc
          (mul beg (mul [String.sub str s (i - s)] t))
    | ',' when inner ->
        expl inner (i + 1) (i + 1)
          (mul beg [String.sub str s (i - s)] @ acc) [""]
    | '}' when inner ->
        (mul beg [String.sub str s (i - s)] @ acc, i + 1)
    | _ ->
        expl inner s (i + 1) acc beg
  in
  List.rev (fst (expl false 0 0 [] [""]))

let glob' nodot s = wrap (glob_parse (if nodot then Beg else Mid) s)
let glob s = glob' true s
let globx' nodot s = alt (List.map (glob' nodot) (explode s))
let globx s = globx' true s
