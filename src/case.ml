(* Unison file synchronizer: src/case.ml *)
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


(* The update detector, reconciler, and transporter behave differently       *)
(* depending on whether the local and/or remote file system is case          *)
(* insensitive.  This pref is set during the initial handshake if any one of *)
(* the hosts is case insensitive.                                            *)
let caseInsensitiveMode =
  Prefs.createString "ignorecase" "default"
    "!identify upper/lowercase filenames (true/false/default)"
    ("When set to {\\tt true}, this flag causes Unison to treat "
     ^ "filenames as case insensitive---i.e., files in the two "
     ^ "replicas whose names differ in (upper- and lower-case) `spelling' "
     ^ "are treated as the same file.  When the flag is set to {\\tt false}, Unison "
     ^ "will treat all filenames as case sensitive.  Ordinarily, when the flag is "
     ^ "set to {\\tt default}, "
     ^ "filenames are automatically taken to be case-insensitive if "
     ^ "either host is running Windows or OSX.  In rare circumstances it is  "
     ^ "useful to set the flag manually (e.g. when running Unison on a  "
     ^ "Unix system with a FAT [Windows] volume mounted).")

(* Defining this variable as a preference ensures that it will be propagated
   to the other host during initialization *)
let someHostIsInsensitive =
  Prefs.createBool "someHostIsInsensitive" false
    "*Pseudo-preference for internal use only" ""

(* Note: this function must be fast *)
let insensitive () = Prefs.read someHostIsInsensitive

let needNormalization s =
  let rec iter s pos len wasDot =
    if pos = len then wasDot else
    let c = s.[pos] in
    (wasDot && c = '/') || iter s (pos + 1) len (c = '.')
  in
  iter s 0 (String.length s) false

let removeTrailingDots s =
  let len = String.length s in
  let s' = String.create len in
  let pos = ref (len - 1) in
  let pos' = ref (len - 1) in
  while !pos >= 0 do
    while !pos >= 0 && s.[!pos] = '.' do decr pos done;
    while !pos >= 0 && s.[!pos] <> '/' do
      s'.[!pos'] <- s.[!pos]; decr pos; decr pos'
    done;
    while !pos >= 0 && s.[!pos] = '/' do
      s'.[!pos'] <- s.[!pos]; decr pos; decr pos'
    done
  done;
  String.sub s' (!pos' + 1) (len - !pos' - 1)

(* Dots are ignored at the end of filenames under Windows. *)
let normalize s =
  s
(*FIX: disabled for know -- requires an archive version change
  if
    insensitive () &&
(*FIX: should only be done when one host is running under Windows...
(should be OK for now as it seems unlikely to have a file ending with
 a dot and the same file with the same name but no dot at the end)
    Prefs.read someHostIsRunningWindows &&
    not (Prefs.read allHostsAreRunningWindows) &&
*)
    needNormalization s
  then
    removeTrailingDots s
  else
    s
*)

(* During startup the client determines the case sensitivity of each root.   *)
(* If any root is case insensitive, all roots must know it; we ensure this   *)
(* by storing the information in a pref so that it is propagated to the      *)
(* server with the rest of the prefs.                                        *)
let init b =
  Prefs.set someHostIsInsensitive
    (Prefs.read caseInsensitiveMode = "yes" ||
     Prefs.read caseInsensitiveMode = "true" ||
     (Prefs.read caseInsensitiveMode = "default" && b))
