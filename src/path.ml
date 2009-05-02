(* Unison file synchronizer: src/path.ml *)
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


(* Defines an abstract type of relative pathnames *)

type 'a path = string
type t = string
type local = string

let pathSeparatorChar = '/'
let pathSeparatorString = "/"

let concat p p' =
  let l = String.length p in
  if l = 0 then p' else
  let l' = String.length p' in
  if l' = 0 then p else
  let p'' = String.create (l + l' + 1) in
  String.blit p 0 p'' 0 l;
  p''.[l] <- pathSeparatorChar;
  String.blit p' 0 p'' (l + 1) l';
  p''

let empty = ""

let isEmpty p = String.length p = 0

let length p =
  let l = ref 0 in
  for i = 0 to String.length p - 1 do
    if p.[i] = pathSeparatorChar then incr l
  done;
  !l

(* Add a name to the end of a path *)
let rcons n path = concat (Name.toString n) path

let toStringList p = Str.split (Str.regexp pathSeparatorString) p

(* Give a left-to-right list of names in the path *)
let toNames p = Safelist.map Name.fromString (toStringList p)

let child path name = concat path (Name.toString name)

let parent path =
  try
    let i = String.rindex path pathSeparatorChar in
    String.sub path 0 i
  with Not_found ->
    empty

let finalName path =
  try
    let i = String.rindex path pathSeparatorChar + 1 in
    Some (Name.fromString (String.sub path i (String.length path - i)))
  with Not_found ->
    if isEmpty path then
      None
    else
      Some (Name.fromString path)

(* pathDeconstruct : path -> (name * path) option *)
let deconstruct path =
  try
    let i = String.index path pathSeparatorChar in
    Some (Name.fromString (String.sub path 0 i),
          String.sub path (i + 1) (String.length path - i - 1))
  with Not_found ->
    if isEmpty path then
      None
    else
      Some (Name.fromString path, empty)

let deconstructRev path =
  try
    let i = String.rindex path pathSeparatorChar in
    Some (Name.fromString
            (String.sub path (i + 1) (String.length path - i - 1)),
          String.sub path 0 i)
  with Not_found ->
    if path = "" then
      None
    else
      Some (Name.fromString path, empty)

let winAbspathRx = Rx.rx "([a-zA-Z]:)?(/|\\\\).*"
let unixAbspathRx = Rx.rx "/.*"
let is_absolute s =
  if Util.osType=`Win32 then Rx.match_string winAbspathRx s
  else Rx.match_string unixAbspathRx s

(* Function string2path: string -> path

   THIS IS THE CRITICAL FUNCTION.

   Problem: What to do on argument "" ?
   What we do: we raise Invalid_argument.

   Problem: double slash within the argument, e.g., "foo//bar".
   What we do: we raise Invalid_argument.

   Problem: What if string2path is applied to an absolute path?  We
   want to disallow this, but, relative is relative.  E.g., on Unix it
   makes sense to have a directory with subdirectory "c:".  Then, it
   makes sense to synchronize on the path "c:".  But this will go
   badly if the Unix system synchronizes with a Windows system.
   What we do: we check whether a path is relative using local
   conventions, and raise Invalid_argument if not.  If we synchronize
   with a system with other conventions, then problems must be caught
   elsewhere.  E.g., the system should refuse to create a directory
   "c:" on a Windows machine.

   Problem: spaces in the argument, e.g., " ".  Still not sure what to
   do here.  Is it possible to create a file with this name in Unix or
   Windows?

   Problem: trailing slashes, e.g., "foo/bar/".  Shells with
   command-line completion may produce these routinely.
   What we do: we remove them.  Moreover, we remove as many as
   necessary, e.g., "foo/bar///" becomes "foo/bar".  This may be
   counter to conventions of some shells/os's, where "foo/bar///"
   might mean "/".

   Examples:
     loop "hello/there" -> ["hello"; "there"]
     loop "/hello/there" -> [""; "hello"; "there"]
     loop "" -> [""]
     loop "/" -> [""; ""]
     loop "//" -> [""; ""; ""]
     loop "c:/" ->["c:"; ""]
     loop "c:/foo" -> ["c:"; "foo"]
*)
let fromString str =
  let str = if Util.osType = `Win32 then Fileutil.backslashes2forwardslashes str else str in
  if is_absolute str then
    raise (Util.Transient
             (Printf.sprintf "The path '%s' is not a relative path" str));
  let str = Fileutil.removeTrailingSlashes str in
  if str = "" then empty else
  let rec loop p str =
    try
      let pos = String.index str pathSeparatorChar in
      let name1 = String.sub str 0 pos in
      let str_res =
        String.sub str (pos + 1) (String.length str - pos - 1) in
      if pos = 0 then begin
        loop p str_res
      end else
        loop (child p (Name.fromString name1)) str_res
    with
      Not_found -> child p (Name.fromString str)
    | Invalid_argument _ ->
        raise(Invalid_argument "Path.fromString") in
  loop empty str

let toString path = path

let compare p1 p2 =
  if Case.insensitive () then Util.nocase_cmp p1 p2 else compare p1 p2

let toDebugString path = String.concat " / " (toStringList path)

let addSuffixToFinalName path suffix = path ^ suffix

let addPrefixToFinalName path prefix =
  try
    let i = String.rindex path pathSeparatorChar + 1 in
    let l = String.length path in
    let l' = String.length prefix in
    let p = String.create (l + l') in
    String.blit path 0 p 0 i;
    String.blit prefix 0 p i l';
    String.blit path i p (i + l') (l - i);
    p
  with Not_found ->
    assert (not (isEmpty path));
    prefix ^ path

let hash p = Hashtbl.hash p

(* Pref controlling whether symlinks are followed. *)
let follow = Pred.create "follow"
    ("Including the preference \\texttt{-follow \\ARG{pathspec}} causes Unison to \
      treat symbolic links matching \\ARG{pathspec} as `invisible' and \
      behave as if the object pointed to by the link had appeared literally \
      at this position in the replica.  See \
      \\sectionref{symlinks}{Symbolic Links} for more details. \
      The syntax of \\ARG{pathspec>} is \
      described in \\sectionref{pathspec}{Path Specification}.")

let followLink path =
     (Util.osType = `Unix || Util.isCygwin)
  && Pred.test follow (toString path)

let magic p = p
let magic' p = p
