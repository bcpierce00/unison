(* Unison file synchronizer: src/external.ml *)
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
(*                     RUNNING EXTERNAL PROGRAMS                             *)
(*****************************************************************************)

let debug = Util.debug "external"

let (>>=) = Lwt.bind
open Lwt

(* For backwards compatibility with OCaml < 4.12 *)
let path =
  try
    Str.split (Str.regexp (if Sys.win32 then ";" else ":"))
      (Sys.getenv "PATH")
  with Not_found ->
    []

let search_in_path ?(path = path) name =
  if String.contains name '/' then name else
  Filename.concat
    (List.find (fun dir ->
       let p = Filename.concat dir name in
       let found = System.file_exists p in
       debug (fun () -> Util.msg "'%s' ...%s\n" p
         (match found with true -> "found" | false -> "not found"));
       found)
    path)
    name

(* Make sure external process resources are collected and zombie processes
   reaped when the Lwt thread calling the external program is stopped
   suddenly due to remote connection being closed. *)
let close_process_noerr close pid x =
  let pid = pid x in
  begin try
    Unix.kill pid (if Sys.win32 then Sys.sigkill else Sys.sigterm)
    with Unix.Unix_error _ -> () end;
  begin try ignore (Terminal.safe_waitpid pid) with Unix.Unix_error _ -> () end;
  try ignore (close x) with Sys_error _ | Unix.Unix_error _ -> ()

let inProcRes =
  Remote.resourceWithConnCleanup System.close_process_in
    (close_process_noerr System.close_process_in System.process_in_pid)
let fullProcRes =
  Remote.resourceWithConnCleanup System.close_process_full
    (close_process_noerr System.close_process_full System.process_full_pid)

let openProcessIn cmd = inProcRes.register (System.open_process_in cmd)
let closeProcessIn = inProcRes.release

(* Remove call to search_in_path once we require OCaml >= 4.12. *)
let openProcessArgsIn cmd args = inProcRes.register (System.open_process_args_in (search_in_path cmd) args)
let closeProcessArgsIn = inProcRes.release

let openProcessFull cmd = fullProcRes.register (System.open_process_full cmd)
let closeProcessFull = fullProcRes.release

(* Remove call to search_in_path once we require OCaml >= 4.12. *)
let openProcessArgsFull cmd args = fullProcRes.register (System.open_process_args_full (search_in_path cmd) args)
let closeProcessArgsFull = fullProcRes.release

let readChannelTillEof c =
  let lst = ref [] in
  let rec loop () =
    lst := input_line c :: !lst;
    loop ()
  in
  begin try loop () with End_of_file -> () end;
  String.concat "\n" (Safelist.rev !lst)

let readChannelTillEof_lwt c =
  let rec loop lines =
    Lwt.try_bind
      (fun () -> Lwt_unix.input_line c)
      (fun l  -> loop (l :: lines))
      (fun e  -> if e = End_of_file then Lwt.return lines else Lwt.fail e)
  in
  String.concat "\n" (Safelist.rev (Lwt_unix.run (loop [])))

let readChannelsTillEof l =
  let rec suckitdry lines c =
    Lwt.try_bind
      (fun () -> Lwt_unix.input_line c)
      (fun l -> suckitdry (l :: lines) c)
      (fun e -> match e with End_of_file -> Lwt.return lines | _ -> raise e)
  in
  Lwt_util.map
    (fun c ->
       suckitdry [] c
       >>= (fun res -> return (String.concat "\n" (Safelist.rev res))))
    l


let runExternalProgramAux ~winProc ~posixProc =
  if Sys.win32 then begin
    debug (fun()-> Util.msg "Executing external program windows-style\n");
    let c = winProc () in
    let log = Util.trimWhitespace (readChannelTillEof c) in
    let returnValue = closeProcessIn c in
    let resultLog =
      (*cmd ^
      (if log <> "" then "\n\n" ^*) log (*else "")*) ^
      (if returnValue <> Unix.WEXITED 0 then
         "\n\n" ^ Util.process_status_to_string returnValue
       else
         "") in
    Lwt.return (returnValue, resultLog)
  end else
    let (out, ipt, err) as desc = posixProc () in
    let out = Lwt_unix.intern_in_channel out in
    let err = Lwt_unix.intern_in_channel err in
    readChannelsTillEof [out;err]
    >>= (function [logOut;logErr] ->
    let returnValue = closeProcessFull desc in
    let logOut = Util.trimWhitespace logOut in
    let logErr = Util.trimWhitespace logErr in
    return (returnValue, (
      (*  cmd
      ^ "\n\n" ^ *)
        (if logOut = "" || logErr = ""
           then logOut ^ logErr
         else logOut ^ "\n\n" ^ ("Error Output:" ^ logErr))
      ^ (if returnValue = Unix.WEXITED 0
         then ""
         else "\n\n" ^ Util.process_status_to_string returnValue)))
      (* Stop typechechecker from complaining about non-exhaustive pattern above *)
      | _ -> assert false)

let runExternalProgram cmd =
  runExternalProgramAux
    ~winProc:(fun () -> openProcessIn ("\"" ^ cmd ^ "\""))
    ~posixProc:(fun () -> openProcessFull cmd)

let runExternalProgramArgs cmd args =
  runExternalProgramAux
    ~winProc:(fun () -> openProcessArgsIn cmd args)
    ~posixProc:(fun () -> openProcessArgsFull cmd args)
