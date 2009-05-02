(* Unison file synchronizer: src/clroot.ml *)
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
  This file parses the unison command-line arguments that
  specify replicas.  The syntax for replicas is based on that of
  URI's, described in RFC 2396.  They have the following grammar:

  replica ::= [protocol:]//[user@][host][:port][/path]
           |  path

  protocol ::= file
            |  socket
            |  ssh
            |  rsh

  user ::= [-_a-zA-Z0-9]+

  host ::= [-_a-zA-Z0-9.]+

  port ::= [0-9]+

  path is any string that does not begin with protocol: or //.

*)

(* Command-line roots *)
type clroot =
    ConnectLocal of
          string option (* root *)
  | ConnectByShell of
          string        (* shell = "rsh" or "ssh" *)
        * string        (* name of host *)
        * string option (* user name to log in as *)
        * string option (* port *)
        * string option (* root of replica in host fs *)
  | ConnectBySocket of
          string        (* name of host *)
        * string        (* port where server should be listening *)
        * string option (* root of replica in host fs *)

(* Internal datatypes used in parsing command-line roots *)
type protocol = File | Rsh | Socket | Ssh
type uri = protocol       (*   - a protocol *)
         * string option  (*   - an optional user *)
         * string option  (*   - an optional host *)
         * int option     (*   - an optional port *)
         * string option  (*   - an optional path *)

(* Regular expressions, used in parsing *)
let protocolColonSlashSlashRegexp = Str.regexp "[a-zA-Z]+://"
let protocolColonRegexp = Str.regexp "[a-zA-Z]+:"
let slashSlashRegexp = Str.regexp "//"

let getProtocolSlashSlash s =
  if Str.string_match protocolColonSlashSlashRegexp s 0
  then
    let matched = Str.matched_string s in
    let len = String.length matched in
    let remainder = Str.string_after s len in
    let protocolName = String.sub matched 0 (len-3) in
    let protocol =
      match protocolName with
        "file" -> File
      | "rsh" -> Rsh
      | "socket" -> Socket
      | "ssh" -> Ssh
      | "unison" ->
          raise(Invalid_argument
                  (Printf.sprintf "protocol unison has been deprecated, use file, ssh, rsh, or socket instead" ))
      | _ ->
          raise(Invalid_argument
                  (Printf.sprintf "unrecognized protocol %s" protocolName)) in
    Some(protocol,remainder)
  else if Str.string_match slashSlashRegexp s 0
  then Some(File,String.sub s 2 (String.length s - 2))
  else if Str.string_match protocolColonRegexp s 0
  then
    let matched = Str.matched_string s in
    match matched with
      "file:" | "ssh:" | "rsh:" | "socket:" ->
        raise(Util.Fatal
                (Printf.sprintf
                   "ill-formed root specification %s (%s must be followed by //)"
                   s matched))
    | _ -> None
  else None

let userAtRegexp = Str.regexp "[-_a-zA-Z0-9.]+@"
let getUser s =
  if Str.string_match userAtRegexp s 0
  then
    let userAt = Str.matched_string s in
    let len = String.length userAt in
    let afterAt = Str.string_after s len in
    let beforeAt = String.sub userAt 0 (len-1) in
    (Some beforeAt,afterAt)
  else (None,s)

let hostRegexp = Str.regexp "[-_a-zA-Z0-9.]+"
let getHost s =
  if Str.string_match hostRegexp s 0
  then
    let host = Str.matched_string s in
    let s' = Str.string_after s (String.length host) in
    (Some host,s')
  else (None,s)

let colonPortRegexp = Str.regexp ":[^/]+"
let getPort s =
  if Str.string_match colonPortRegexp s 0
  then
    let colonPort = Str.matched_string s in
    let len = String.length colonPort in
    let port = String.sub colonPort 1 (len-1) in
    let s' = Str.string_after s len in
    (Some port,s')
  else (None,s)

(* parseUri : string
   -> protocol
   * user option
   * host option
   * port option
   * path option

   where user, host, port, and path are strings,
   and path is guaranteed to be non-empty
*)
let parseUri s =
  match getProtocolSlashSlash s with
    None ->
      (File,None,None,None,Some s)
  | Some(protocol,s0) ->
      let (userOpt,s1) = getUser s0 in
      let (hostOpt,s2) = getHost s1 in
      let (portOpt,s3) = getPort s2 in
      let pathOpt =
        let len = String.length s3 in
        if  len <= 0 then None
        else if String.get s3 0 = '/' then
          if len=1 then None
          else Some(String.sub s3 1 (len-1))
        else
          raise(Util.Fatal
                  (Printf.sprintf "ill-formed root specification %s" s)) in
      (protocol,userOpt,hostOpt,portOpt,pathOpt)

(* These should succeed *)
let t1 = "socket://tjim@saul.cis.upenn.edu:4040/hello/world"
let t2 = "ssh://tjim@saul/hello/world"
let t3 = "rsh://saul:4040/hello/world"
let t4 = "rsh://saul/hello/world"
let t5 = "rsh://saul"
let t6 = "rsh:///hello/world"
let t7 = "///hello/world"
let t8 = "//raptor/usr/local/bin"
let t9 = "file://raptor/usr/local/bin"
let t9 = "//turtle/c:/winnt/"
let t9 = "file://turtle/c:/winnt/"

(* These should fail *)
let b1 = "//saul:40a4/hello"
let b2 = "RSH://saul/hello"
let b3 = "rsh:/saul/hello"
let b4 = "//s%aul/hello"

let cannotAbbrevFileRx = Rx.rx "(file:|ssh:|rsh:|socket:).*"
let networkNameRx = Rx.rx "//.*"
(* Main external printing function *)
let clroot2string = function
  ConnectLocal None -> "."
| ConnectLocal(Some s) ->
    if Rx.match_string cannotAbbrevFileRx s
    then if Rx.match_string networkNameRx s
    then Printf.sprintf "file:%s" s
    else Printf.sprintf "file:///%s" s
    else s
| ConnectBySocket(h,p,s) ->
    Printf.sprintf "socket://%s:%s/%s" h p
      (match s with None -> "" | Some x -> x)
| ConnectByShell(sh,h,u,p,s) ->
    let user = match u with None -> "" | Some x -> x^"@" in
    let port = match p with None -> "" | Some x -> ":"^x in
    let path = match s with None -> "" | Some x -> x in
    Printf.sprintf "%s://%s%s%s/%s" sh user h port path

let sshversion = Prefs.createString "sshversion" ""
                "*optional version suffix for ssh command [1 or 2]"
    ("This preference can be used to control which version "
     ^ "of ssh should be used to connect to the server.  Legal values are "
     ^ "1 and 2, which will cause unison to try to use \\verb|ssh1| or"
     ^ "\\verb|ssh2| instead of just \\verb|ssh| to invoke ssh.  "
     ^ "The default value is empty, which will make unison use whatever "
     ^ "version of ssh is installed as the default `ssh' command.")

(* Main external function *)
let parseRoot string =
  let illegal2 s = raise(Prefs.IllegalValue
                           (Printf.sprintf
                              "%s: %s" string s)) in
  let (protocol,user,host,port,path) = parseUri string in
  let clroot =
    match protocol,user,host,port with
    | _,_,None,Some _
    | _,Some _,None,None
    | Rsh,_,None,_
    | Ssh,_,None,_ ->
        illegal2 "missing host"
    | Rsh,_,_,Some _ ->
        illegal2 "ill-formed (cannot use a port number with rsh)"
    | File,_,_,Some _ ->
        illegal2 "ill-formed (cannot use a port number with file)"
    | File,_,Some h,None ->
        let prefix = "//"^h^"/" in
        (match path with
          None -> ConnectLocal(Some prefix)
        | Some p -> ConnectLocal(Some(prefix^p)))
    | File,None,None,None ->
        ConnectLocal(path)
    | Socket,None,Some h,Some p ->
        ConnectBySocket(h,p,path)
    | Socket,Some _,_,_ ->
        illegal2 "ill-formed (cannot use a user with socket)"
    | Socket,_,_,None ->
        illegal2 "ill-formed (must give a port number with socket)"
    | Rsh,_,Some h,_ ->
        ConnectByShell("rsh",h,user,port,path)
    | Ssh,_,Some h,_ ->
        ConnectByShell("ssh"^(Prefs.read sshversion),h,user,port,path) in
  clroot
