(* Unison file synchronizer: src/sortri.ml *)
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


open Common  

let dbgsort = Util.debug "sort"

(* Preferences *)

let bysize =
  Prefs.createBool "sortbysize" false
    "!list changed files by size, not name"
    ("When this flag is set, the user interface will list changed files "
     ^ "by size (smallest first) rather than by name.  This is useful, for "
     ^ "example, for synchronizing over slow links, since it puts very "
     ^ "large files at the end of the list where they will not prevent "
     ^ "smaller files from being transferred quickly.\n\n"
     ^ "This preference (as well as the other sorting flags, but not the "
     ^ "sorting preferences that require patterns as arguments) can be "
     ^ "set interactively and temporarily using the  'Sort' menu in the "
     ^ "graphical user interface.")

let newfirst =
  Prefs.createBool "sortnewfirst" false
    "!list new before changed files"
    ("When this flag is set, the user interface will list newly created "
     ^ "files before all others.  This is useful, for example, for checking "
     ^ "that newly created files are not `junk', i.e., ones that should be "
     ^ "ignored or deleted rather than synchronized.")

let sortfirst = Pred.create "sortfirst" ~advanced:true
    ("Each argument to \\texttt{sortfirst} is a pattern \\ARG{pathspec}, "
     ^ "which describes a set of paths.  "
     ^ "Files matching any of these patterns will be listed first in the "
     ^ "user interface. "
     ^ "The syntax of \\ARG{pathspec} is "
     ^ "described in \\sectionref{pathspec}{Path Specification}.")

let sortlast = Pred.create "sortlast" ~advanced:true
    ("Similar to \\verb|sortfirst|, except that files matching one of these "
     ^ "patterns will be listed at the very end.")

type savedPrefs = {nf:bool; bs:bool; sf:string list; sl:string list}
let savedPrefs = ref(None)

let saveSortingPrefs () =
  if !savedPrefs = None then
    savedPrefs := Some {
      sf = Pred.extern sortfirst;
      sl = Pred.extern sortlast;
      bs = Prefs.read bysize;
      nf = Prefs.read newfirst }

let restoreDefaultSettings () = 
  match !savedPrefs with
    None -> ()
  | Some {nf=nf; bs=bs; sf=sf; sl=sl} ->
      Prefs.set newfirst nf;
      Prefs.set bysize bs;
      Pred.intern sortfirst sf;
      Pred.intern sortlast sl

let zeroSortingPrefs () =
  Prefs.set newfirst false;
  Prefs.set bysize false;
  Pred.intern sortfirst [];
  Pred.intern sortlast []

(* ------------------- *)

let sortByName () =
  saveSortingPrefs();
  zeroSortingPrefs()
  
let sortBySize () =
  saveSortingPrefs();
  zeroSortingPrefs();
  Prefs.set bysize true
 
let sortNewFirst () =
  saveSortingPrefs();
  Prefs.set newfirst (not (Prefs.read newfirst))

(* ---------------------------------------------------------------------- *)
(* Main sorting functions *)

let shouldSortFirst ri =
  Pred.test sortfirst (Path.toString ri.path)
let shouldSortLast ri =
  Pred.test sortlast (Path.toString ri.path)

let newItem ri = 
  let newItem1 ri =
    match ri.replicas with
      Different((_, `Created, _, _), _, _, _) -> true
    | _ -> false in
  let newItem2 ri =
    match ri.replicas with
      Different(_, (_, `Created, _, _), _, _) -> true
    | _ -> false
  in newItem1 ri || newItem2 ri

(* Should these go somewhere else? *)
let rec combineCmp = function
    [] -> 0
  | c::cs -> if c<>0 then c else combineCmp cs
let invertCmp c = c * -1

let compareReconItems () =
  let newfirst = Prefs.read newfirst in
  fun ri1 ri2 ->
    let pred p =
      let b1 = p ri1 in let b2 = p ri2 in
      if b1 && b2 then 0 else if b1 then -1 else if b2 then 1 else 0 in
    let cmp = 
      combineCmp [
        pred problematic;
        pred shouldSortFirst;
        invertCmp (pred shouldSortLast);
        if newfirst then pred newItem else 0;
        (if Prefs.read bysize then
          let l1 = Common.riLength ri1 in
          let l2 = Common.riLength ri2 in
          if l1<l2 then -1 else if l2<l1 then 1 else 0
         else 0);
        (compare (Path.toString ri1.path) (Path.toString ri2.path))
      ] in
    dbgsort (fun() -> Util.msg "%s <= %s --> %d\n"
               (Path.toString ri1.path) (Path.toString ri2.path) cmp);
    cmp

let sortReconItems items = Safelist.stable_sort (compareReconItems()) items

