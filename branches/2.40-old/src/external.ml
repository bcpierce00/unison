(* Unison file synchronizer: src/external.ml *)
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


(*****************************************************************************)
(*                     RUNNING EXTERNAL PROGRAMS                             *)
(*****************************************************************************)

let debug = Util.debug "external"

let (>>=) = Lwt.bind
open Lwt

let readChannelTillEof c =
  let rec loop lines =
    try let l = input_line c in
        (* Util.msg "%s\n" l; *)
        loop (l::lines)
    with End_of_file -> lines in
  String.concat "\n" (Safelist.rev (loop []))

let readChannelTillEof_lwt c =
  let rec loop lines =
    let lo =
      try
        Some(Lwt_unix.run (Lwt_unix.input_line c))
      with End_of_file -> None
    in
    match lo with
      Some l -> loop (l :: lines)
    | None   -> lines
  in
  String.concat "\n" (Safelist.rev (loop []))

let readChannelsTillEof l =
  let rec suckitdry lines c =
    Lwt.catch
      (fun() -> Lwt_unix.input_line c >>= (fun l -> return (Some l)))
      (fun e -> match e with End_of_file -> return None | _ -> raise e)
    >>= (fun lo ->
           match lo with
             None -> return lines
           | Some l -> suckitdry (l :: lines) c) in
  Lwt_util.map
    (fun c ->
       suckitdry [] c
       >>= (fun res -> return (String.concat "\n" (Safelist.rev res))))
    l

let runExternalProgram cmd =
  if Util.osType = `Win32 && not Util.isCygwin then begin
    debug (fun()-> Util.msg "Executing external program windows-style\n");
    let c = System.open_process_in ("\"" ^ cmd ^ "\"") in
    let log = readChannelTillEof c in
    let returnValue = System.close_process_in c in
    let mergeResultLog =
      cmd ^
      (if log <> "" then "\n\n" ^ log else "") ^
      (if returnValue <> Unix.WEXITED 0 then
         "\n\n" ^ Util.process_status_to_string returnValue
       else
         "") in
    Lwt.return (returnValue,mergeResultLog) 
  end else
    let (out, ipt, err) as desc = System.open_process_full cmd in
    let out = Lwt_unix.intern_in_channel out in
    let err = Lwt_unix.intern_in_channel err in
    readChannelsTillEof [out;err]
    >>= (function [logOut;logErr] ->
    let returnValue = System.close_process_full desc in
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
