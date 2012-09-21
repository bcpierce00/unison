(* Unison file synchronizer: src/case.ml *)
(* Copyright 1999-2012, Benjamin C. Pierce 

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
  Prefs.createBoolWithDefault "ignorecase"
    "!identify upper/lowercase filenames (true/false/default)"
    ("When set to {\\tt true}, this flag causes Unison to treat "
     ^ "filenames as case insensitive---i.e., files in the two "
     ^ "replicas whose names differ in (upper- and lower-case) `spelling' "
     ^ "are treated as the same file.  When the flag is set to {\\tt false}, Unison "
     ^ "will treat all filenames as case sensitive.  Ordinarily, when the flag is "
     ^ "set to {\\tt default}, "
     ^ "filenames are automatically taken to be case-insensitive if "
     ^ "either host is running Windows or OSX.  In rare circumstances it may be  "
     ^ "useful to set the flag manually.")

(* Defining this variable as a preference ensures that it will be propagated
   to the other host during initialization *)
let someHostIsInsensitive =
  Prefs.createBool "someHostIsInsensitive" false
    "*Pseudo-preference for internal use only" ""

let unicode =
  Prefs.createBoolWithDefault "unicode"
    "!assume Unicode encoding in case insensitive mode"
    "When set to {\\tt true}, this flag causes Unison to perform \
     case insensitive file comparisons assuming Unicode encoding.  \
     This is the default.  When the flag is set to {\\tt false}, \
     a Latin 1 encoding is assumed.  When Unison runs in case sensitive \
     mode, this flag only makes a difference if one host is running \
     Windows or Mac OS X.  Under Windows, the flag selects between using \
     the Unicode or 8bit Windows API for accessing the filesystem. \
     Under Mac OS X, it selects whether comparing the filenames up to \
     decomposition, or byte-for-byte."

let unicodeEncoding =
  Prefs.createBool "unicodeEnc" false
    "*Pseudo-preference for internal use only" ""

let useUnicode () =
  let pref = Prefs.read unicode in
  pref = `True || pref = `Default

let useUnicodeAPI = useUnicode

let unicodeCaseSensitive =
  Prefs.createBool "unicodeCS" ~local:true false
    "*Pseudo-preference for internal use only" ""

(* During startup the client determines the case sensitivity of each root.   *)
(* If any root is case insensitive, all roots must know it; we ensure this   *)
(* by storing the information in a pref so that it is propagated to the      *)
(* server with the rest of the prefs.                                        *)
let init b someHostRunningOsX =
  Prefs.set someHostIsInsensitive
    (Prefs.read caseInsensitiveMode = `True ||
     (Prefs.read caseInsensitiveMode = `Default && b));
  Prefs.set unicodeCaseSensitive (useUnicode () && someHostRunningOsX);
  Prefs.set unicodeEncoding (useUnicode ())

(****)

(* Dots are ignored at the end of filenames under Windows. *)

(* FIX: for the moment, simply disallow files ending with a dot.
   This is more efficient, and this may well be good enough.
   We should reconsider this is people start complaining...

let hasTrailingDots s =
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

let rmTrailDots s =
  s
(*FIX: disabled for now -- requires an archive version change
  if
    Prefs.read someHostIsRunningWindows &&
    not (Prefs.read allHostsAreRunningWindows) &&
    hasTrailingDots s
  then
    removeTrailingDots s
  else
    s
*)
*)

(****)

type mode = Sensitive | Insensitive | UnicodeSensitive | UnicodeInsensitive

(*
Important invariant:
  if [compare s s' = 0],
  then [hash s = hash s'] and
  and  [Rx.match_string rx (normalizeMatchedString s) =
        Rx.match_string rx (normalizeMatchedString s')]
  (when [rx] has been compiled using the [caseInsensitiveMatch] mode)
*)

let sensitiveOps = object
  method mode = Sensitive
  method modeDesc = "case sensitive"
  method compare s s' = compare (s : string) s'
  method hash s = Uutil.hash s
  method normalizePattern s = s
  method caseInsensitiveMatch = false
  method normalizeMatchedString s = s
  method normalizeFilename s = s
  method badEncoding s = false
end

let insensitiveOps = object
  method mode = Insensitive
  method modeDesc = "Latin-1 case insensitive"
  method compare s s' = Util.nocase_cmp s s'
  method hash s = Uutil.hash (String.lowercase s)
  method normalizePattern s = s
  method caseInsensitiveMatch = true
  method normalizeMatchedString s = s
  method normalizeFilename s = s
  method badEncoding s = false
end

let unicodeSensitiveOps = object
  method mode = UnicodeSensitive
  method modeDesc = "Unicode case sensitive"
  method compare s s' = Unicode.case_sensitive_compare s s'
  method hash s = Uutil.hash (Unicode.decompose s)
  method normalizePattern p = Unicode.decompose p
  method caseInsensitiveMatch = false
  method normalizeMatchedString s = Unicode.decompose s
  method normalizeFilename s = Unicode.compose s
  method badEncoding s = not (Unicode.check_utf_8 s)
end

let unicodeInsensitiveOps = object
  method mode = UnicodeInsensitive
  method modeDesc = "Unicode case insensitive"
  method compare s s' = Unicode.case_insensitive_compare s s'
  method hash s = Uutil.hash (Unicode.normalize s)
  method normalizePattern p = Unicode.normalize p
  method caseInsensitiveMatch = false
  method normalizeMatchedString s = Unicode.normalize s
  method normalizeFilename s = Unicode.compose s
  method badEncoding s = not (Unicode.check_utf_8 s)
end

(* Note: the dispatch must be fast *)
let ops () =
  if Prefs.read someHostIsInsensitive then begin
    if Prefs.read unicodeEncoding then
      unicodeInsensitiveOps
    else
      insensitiveOps
  end else
    if Prefs.read unicodeCaseSensitive then
      unicodeSensitiveOps
    else
      sensitiveOps

let caseSensitiveModeDesc = sensitiveOps#modeDesc
