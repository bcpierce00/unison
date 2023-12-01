(* Unison file synchronizer: src/ubase/util.ml *)
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


(*****************************************************************************)
(*                        CASE INSENSITIVE COMPARISON                        *)
(*****************************************************************************)
(* Latin1 (ISO 8859-1) string functions have been deprecated in OCaml. Latin1
   being supported by Unison, the deprecated Stdlib has been replaced with
   this lowercase_latin1 function. *)
let lowercase_latin1 = function
  | 'A' .. 'Z'
  | '\192' .. '\214'
  | '\216' .. '\222' as c ->
    Char.chr(Char.code c + 32)
  | c -> c

let nocase_cmp a b =
  let alen = String.length a in
  let blen = String.length b in
  let minlen = if alen<blen then alen else blen in
  let rec loop i =
    if i>=minlen then compare alen blen
    else
      let c =
        compare (lowercase_latin1(String.get a i)) (lowercase_latin1(String.get b i)) in
      if c<>0 then c else loop (i+1) in
  loop 0
let nocase_eq a b = (0 = (nocase_cmp a b))


(*****************************************************************************)
(*                        PRE-BUILT MAP AND SET MODULES                      *)
(*****************************************************************************)

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let stringSetFromList l =
  Safelist.fold_right StringSet.add l StringSet.empty

(*****************************************************************************)
(*                    Debugging / error messages                             *)
(*****************************************************************************)

type infos = { s : string; clr : string }
let infos = ref { s = ""; clr = "" }

let clear_infos () =
  if !infos.clr <> "" then begin
    print_string !infos.clr;
    flush stdout
  end else if !infos.s <> "" then begin
    print_string "\r";
    print_string (String.make (String.length !infos.s) ' ');
    print_string "\r";
    flush stdout
  end
let show_infos () =
  if !infos.s <> "" then begin print_string !infos.s; flush stdout end
let set_infos ?(clr = "") s =
  if s <> !infos.s then begin clear_infos (); infos := {s; clr}; show_infos () end

let msg f =
  clear_infos ();
  Printf.kfprintf (fun _ -> flush stderr; show_infos ()) stderr f

let msg : ('a, out_channel, unit) format -> 'a = msg

(* ------------- Formatting stuff --------------- *)

let curr_formatter = ref Format.std_formatter

let format f = Format.fprintf (!curr_formatter) f
let format : ('a, Format.formatter, unit) format -> 'a = format

let format_to_string f =
  let old_formatter = !curr_formatter in
  curr_formatter := Format.str_formatter;
  f ();
  let s = Format.flush_str_formatter () in
  curr_formatter := old_formatter;
  s

let flush () = Format.pp_print_flush (!curr_formatter) ()

(*****************************************************************************)
(*                  GLOBAL DEBUGGING SWITCH                                  *)
(*****************************************************************************)

let debugPrinter = ref None

let debug s th =
  match !debugPrinter with
    None -> assert false
  | Some p -> p s th

(* This should be set by the UI to a function that can be used to warn users *)
let warnPrinter = ref (Some (msg "Warning: %s"))

(* The rest of the program invokes this function to warn users.              *)
let warn message =
  match !warnPrinter with
    None -> ()
  | Some p -> p message

(*****************************************************************************)
(*                    EXCEPTION HANDLING                                     *)
(*****************************************************************************)

exception Fatal of string
exception Transient of string

let encodeException m kind e =
  let reraise s =
    match kind with
      `Fatal     -> raise (Fatal s)
    | `Transient -> raise (Transient s)
  in
  let kindStr =
    match kind with
      `Fatal     -> "Fatal"
    | `Transient -> "Transient"
  in
  match e with
    Unix.Unix_error(err,fnname,param) ->
      let s =   "Error in " ^ m ^ ":\n"
              ^ (Unix.error_message err)
              ^ " [" ^ fnname ^ "(" ^ param ^ ")]" ^
              (match err with
                 Unix.EUNKNOWNERR n -> Format.sprintf " (code %d)" n
               | _                  -> "")
      in
      debug "exn"
        (fun() -> msg "Converting a Unix error to %s:\n%s\n" kindStr s);
      reraise s
  | Transient(s) ->
      debug "exn" (fun() ->
        if kind = `Fatal then
          msg "In %s: Converting a Transient error to %s:\n%s\n" m kindStr s
        else
          msg "In %s: Propagating Transient error\n" m);
      reraise s
  | Not_found ->
      let s = "Not_found raised in " ^ m
              ^ " (this indicates a bug!)" in
      debug "exn"
        (fun() -> msg "Converting a Not_found to %s:\n%s\n" kindStr s);
      reraise s
  | Invalid_argument a ->
      let s = "Invalid_argument("^a^") raised in " ^ m
              ^ " (this indicates a bug!)" in
      debug "exn"
        (fun() -> msg "Converting an Invalid_argument to %s:\n%s\n" kindStr s);
      reraise s
  | Sys_error(s) ->
      let s = "Error in " ^ m ^ ":\n" ^ s in
      debug "exn"
        (fun() -> msg "Converting a Sys_error to %s:\n%s\n" kindStr s);
      reraise s
  | Sys_blocked_io ->
      let s = "Blocked IO error in " ^ m in
      debug "exn"
        (fun() -> msg "Converting a Sys_blocked_io to %s:\n%s\n" kindStr s);
      reraise s
  | _ ->
      raise e

let convertUnixErrorsToExn m f n e =
  try f()
  with
    Unix.Unix_error(err,fnname,param) ->
      let s =   "Error in " ^ m ^ ":\n"
              ^ (Unix.error_message err)
              ^ " [" ^ fnname ^ "(" ^ param ^ ")]" in
      debug "exn"
        (fun() -> msg "Converting a Unix error to %s:\n%s\n" n s);
      raise (e s)
  | Transient(s) ->
      debug "exn" (fun() ->
        if n="Fatal" then
          msg "In %s: Converting a Transient error to %s:\n%s\n" m n s
        else
          msg "In %s: Propagating Transient error\n" m);
      raise (e s)
  | Not_found ->
      let s = "Not_found raised in " ^ m
              ^ " (this indicates a bug!)" in
        debug "exn" (fun() -> msg "Converting a Not_found to %s:\n%s\n" n s);
        raise (e s)
  | End_of_file ->
      let s = "End_of_file exception raised in " ^ m
              ^ " (this indicates a bug!)" in
        debug "exn" (fun() -> msg "Converting an End_of_file to %s:\n%s\n" n s);
        raise (e s)
  | Sys_error(s) ->
      let s = "Error in " ^ m ^ ":\n" ^ s in
      debug "exn" (fun() -> msg "Converting a Sys_error to %s:\n%s\n" n s);
      raise (e s)
  | Sys_blocked_io ->
      let s = "Blocked IO error in " ^ m in
      debug "exn" (fun() -> msg "Converting a Sys_blocked_io to %s:\n%s\n"
                     n s);
      raise (e s)

let convertUnixErrorsToFatal m f =
  convertUnixErrorsToExn m f "Fatal" (fun str -> Fatal(str))

let convertUnixErrorsToTransient m f =
  convertUnixErrorsToExn m f "Transient" (fun str -> Transient(str))

let unwindProtect f cleanup =
  try
    f ()
  with
    Transient _ as e ->
      debug "exn" (fun () -> msg "Exception caught by unwindProtect\n");
      convertUnixErrorsToFatal "unwindProtect" (fun()-> cleanup e);
      raise e

let finalize f cleanup =
  try
    let res = f () in
    cleanup ();
    res
  with
    Transient _ as e ->
      debug "exn" (fun () -> msg "Exception caught by finalize\n");
      convertUnixErrorsToFatal "finalize" cleanup;
      raise e

type confirmation =
   Succeeded
 | Failed of string

let ignoreTransientErrors thunk =
  try
    thunk()
  with
    Transient(s) -> ()

let printException e =
  try
    raise e
  with
    Transient s -> s
  | Fatal s -> s
  | e -> Printexc.to_string e

(* Safe version of Unix getenv -- raises a comprehensible error message if
   called with an env variable that doesn't exist                            *)
let safeGetenv var =
  convertUnixErrorsToFatal
    "querying environment"
    (fun () ->
       try System.getenv var
       with Not_found ->
         raise (Fatal ("Environment variable " ^ var ^ " not found")))

let process_status_to_string = function
    Unix.WEXITED i   -> Printf.sprintf "Exited with status %d" i
  | Unix.WSIGNALED i -> Printf.sprintf "Killed by signal %d" i
  | Unix.WSTOPPED i  -> Printf.sprintf "Stopped by signal %d" i


let blockSignals sigs f =
  let (prevMask, ok) =
    try (Unix.sigprocmask SIG_BLOCK sigs, true)
    with Invalid_argument _ -> ([], false) in
  let restoreMask () =
    if ok then Unix.sigprocmask SIG_SETMASK prevMask |> ignore in
  try let r = f () in restoreMask (); r
  with e ->
    let origbt = Printexc.get_raw_backtrace () in
    restoreMask ();
    Printexc.raise_with_backtrace e origbt

(*****************************************************************************)
(*                      MISCELLANEOUS                                        *)
(*****************************************************************************)

let monthname n =
  Safelist.nth
    ["Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"]
    n

let localtime f =
  convertUnixErrorsToTransient "localtime" (fun()-> Unix.localtime f)

let time () =
  convertUnixErrorsToTransient "time" Unix.time

let time2string timef =
  try
    let time = localtime timef in
(* Old-style:
    Printf.sprintf
      "%2d:%.2d:%.2d on %2d %3s, %4d"
      time.Unix.tm_hour
      time.Unix.tm_min
      time.Unix.tm_sec
      time.Unix.tm_mday
      (monthname time.Unix.tm_mon)
      (time.Unix.tm_year + 1900)
*)
    Printf.sprintf
      "%4d-%02d-%02d at %2d:%.2d:%.2d"
      (time.Unix.tm_year + 1900)
      (time.Unix.tm_mon + 1)
      time.Unix.tm_mday
      time.Unix.tm_hour
      time.Unix.tm_min
      time.Unix.tm_sec
  with Transient _ ->
    "(invalid date)"

let percentageOfTotal current total =
  (int_of_float ((float current) *. 100.0 /. (float total)))

let percent2string p = Printf.sprintf "%3d%%" (truncate (max 0. (min 100. p)))

let gib = 1073741824.
let mib = 1048576.
let kib = 1024.
let bytes2string v =
  if v > 1_048_051_711L then
    Printf.sprintf "%.2f GiB" (Int64.to_float v /. gib)
  else if v > 104_805_171L then
    Printf.sprintf "%.0f MiB" (Int64.to_float v /. mib)
  else if v > 1_023_487L then
    Printf.sprintf "%.1f MiB" (Int64.to_float v /. mib)
  else if v > 102_348L then
    Printf.sprintf "%.0f KiB" (Int64.to_float v /. kib)
  else if v > 999L then
    Printf.sprintf "%.1f KiB" (Int64.to_float v /. kib)
  else
    Printf.sprintf "%Ld B" v

let extractValueFromOption = function
    None -> raise (Fatal "extractValueFromOption failed")
  | Some(v) -> v

let option2string (prt: 'a -> string) = function
    Some x -> prt x
  | None   -> "N.A."

(*****************************************************************************)
(*                    String utility functions                               *)
(*****************************************************************************)

let truncateString s count =
  (* Truncate a string by counting code points instead of bytes. *)
  let rec subValidUTF8 ?(extra = 0) s pos len =
    (* Like [String.sub] but tries to keep the substring a valid UTF-8
       string (it may not be meaningful in any way but the encoding is not
       broken). Requires the input string to be valid UTF-8 to work
       properly.
       If the initial substring (like a simple [String.sub]) is not valid
       UTF-8 then it tries to blindly extend (never reduce) the substring
       until it becomes valid UTF-8. This is a very simple implementation
       that works without knowing anything about the UTF-8 encoding. *)
    let totl = String.length s in
    if pos >= totl then
      None
    else if pos + len > totl then
      Some (String.sub s pos (totl - pos))
    else
      let s' = String.sub s pos len in
      if Unicode.check_utf_8 s' || extra > 5 then
        Some s'
      else
        subValidUTF8 s pos (len + 1) ~extra:(extra + 1)
  in
  let rec extractCodepoints pos count s' s =
    (* Somewhat like [String.sub] but instead of number of bytes, extracts
       [count] number of code points from the string while [pos] is still
       counted in bytes. *)
    match subValidUTF8 s pos 1 with
    | None -> s'
    | Some s'' ->
        if count > 1 then
          extractCodepoints (pos + String.length s'') (count - 1) (s' ^ s'') s
        else s' ^ s''
  in
  let s = Unicode.compose (Unicode.protect s) in
  let s' = extractCodepoints 0 (count - 3) "" s in
  let s'' = extractCodepoints (String.length s') 3 "" s in
  if String.length s' + String.length s'' < String.length s then
    s' ^ "..."
  else
    s' ^ s''

let findsubstring ?reverse:(rev=false) s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let rec loop i =
    if i+l1 > l2 || i < 0 then None
    else if s1 = String.sub s2 i l1 then Some(i)
    else loop (if rev then i-1 else i+1)
  in loop (if rev then l2-l1 else 0)

let rec replacesubstring s fromstring tostring =
  match findsubstring fromstring s with
    None -> s
  | Some(i) ->
      let before = String.sub s 0 i in
      let afterpos = i + (String.length fromstring) in
      let after = String.sub s afterpos ((String.length s) - afterpos) in
      before ^ tostring ^ (replacesubstring after fromstring tostring)

let replacesubstrings s pairs =
  Safelist.fold_left
    (fun s' (froms,tos) -> replacesubstring s' froms tos)
    s pairs

let startswith s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  if l1 < l2 then false else
    let rec loop i =
      if i>=l2 then true
      else if s1.[i] <> s2.[i] then false
      else loop (i+1)
    in loop 0

let endswith s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let offset = l1 - l2 in
  if l1 < l2 then false else
    let rec loop i =
      if i>=l2 then true
      else if s1.[i+offset] <> s2.[i] then false
      else loop (i+1)
    in loop 0

let concatmap sep f l =
  String.concat sep (Safelist.map f l)

let removeTrailingCR s =
  let l = String.length s in
  if l = 0 || s.[l - 1] <> '\r' then s else
  String.sub s 0 (l - 1)

let trimWhitespace s =
  let l = String.length s in
  let rec loop lp rp =
    if lp > rp then ""
    else if s.[lp]=' ' || s.[lp]='\t' || s.[lp]='\n' || s.[lp]='\r' then
      loop (lp+1) rp
    else if s.[rp]=' ' || s.[rp]='\t' || s.[rp]='\n' || s.[rp]='\r' then
      loop lp (rp-1)
    else
      String.sub s lp (rp+1-lp)
   in
   loop 0 (l-1)

let splitAtChar ?reverse:(rev=false) (s:string) (c:char) =
  try
    let i = if rev then String.rindex s c else String.index s c
    and l = String.length s in
    (* rest is possibly the empty string *)
    (String.sub s 0 i, Some (String.sub s (i+1) (l-i-1)))
  with Not_found -> (s, None)

let splitIntoWords ?esc:(e='\\') (s:string) (c:char) =
  let rec inword acc eacc start pos =
    if pos >= String.length s || s.[pos] = c then
      let word =
        String.concat "" (Safelist.rev (String.sub s start (pos-start)::eacc)) in
      betweenwords (word::acc) pos
    else if s.[pos] = e then inescape acc eacc start pos
    else inword acc eacc start (pos+1)
  and inescape acc eacc start pos =
    let eword = String.sub s start (pos-start) in
    if pos+1 >= String.length s
    then inword acc (eword::eacc) (pos+1) (pos+1) (* ignore final esc *)
    else (* take any following char *)
      let echar = String.make 1 (String.get s (pos+1)) in
      inword acc (echar::eword::eacc) (pos+2) (pos+2)
  and betweenwords acc pos =
    if pos >= String.length s then (Safelist.rev acc)
    else if s.[pos]=c then betweenwords acc (pos+1)
    else inword acc [] pos pos
  in betweenwords [] 0

let splitAtString ?(reverse=false) s sep =
  match findsubstring ~reverse:reverse sep s with
    None -> (s, None)
  | Some(i) ->
      let before = String.sub s 0 i in
      let afterpos = i + (String.length sep) in
      let after = String.sub s afterpos ((String.length s) - afterpos) in
      (* rest is possibly the empty string *)
      (before, Some after)

let rec splitIntoWordsByString s sep =
  match splitAtString s sep with
    (s, None) -> [s]
  | (before, Some after) -> before :: (splitIntoWordsByString after sep)

let padto n s = s ^ (String.make (max 0 (n - String.length s)) ' ')

(*****************************************************************************)
(*              Building pathnames in the user's home dir                    *)
(*****************************************************************************)

let homeDir () =
    (if Sys.unix || Sys.cygwin then
       safeGetenv "HOME"
     else if Sys.win32 then
(*We don't want the behavior of Unison to depends on whether it is run
  from a Cygwin shell (where HOME is set) or in any other way (where
  HOME is usually not set)
       try System.getenv "HOME" (* Windows 9x with Cygwin HOME set *)
       with Not_found ->
*)
       try System.getenv "USERPROFILE" (* Windows NT/2K standard *)
       with Not_found ->
       try System.getenv "UNISON" 
          (* Use custom UNISON dir if it is set.  This can be a path 
             or just the name of the folder you want to use in the 
             current directory *)
       with Not_found ->
       "c:/" (* Default *)
     else
       assert false (* osType can't be anything else *))

let fileInHomeDir n = Filename.concat (homeDir ()) n

(*****************************************************************************)
(*                       .unison dir                                         *)
(*****************************************************************************)

external isMacOSXPred : unit -> bool = "isMacOSX"

let isMacOSX = isMacOSXPred ()

let unisonDir =
  try
    System.getenv "UNISON"
  with Not_found ->
    let genericName =
      fileInHomeDir (Printf.sprintf ".%s" ProjectInfo.myName) in
    if isMacOSX && not (System.file_exists genericName) then
      fileInHomeDir "Library/Application Support/Unison"
    else
      genericName

let fileInUnisonDir str = Filename.concat unisonDir str

let fileMaybeRelToUnisonDir n =
  if Filename.is_relative n
  then fileInUnisonDir n
  else n
