(* Unison file synchronizer: src/clroot.ml *)
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


(*
  This file parses the unison command-line arguments that
  specify replicas.  The syntax for replicas is based on that of
  URI's, described in RFC 2396.  They have the following grammar:

  replica ::= [protocol:]//[user@][host][:port][/path]
           |  path

  protocol ::= file
            |  socket
            |  ssh

  user ::= [-_a-zA-Z0-9%@]+

  host ::= [-_a-zA-Z0-9.]+
        |  \[ [a-f0-9:.]+ zone? \]     IPv6 literals (no future format).
        |  { [^}]+ }                   For Unix domain sockets only.

  zone ::= %[-_a-zA-Z0-9~%.]+

  port ::= [0-9]+

  path is any string that does not begin with protocol: or //.

*)

(* Command-line roots *)
type clroot =
    ConnectLocal of
          string option (* root *)
  | ConnectByShell of
          string        (* shell = "ssh" *)
        * string        (* name of host *)
        * string option (* user name to log in as *)
        * string option (* port *)
        * string option (* root of replica in host fs *)
  | ConnectBySocket of
          string        (* name of host *)
        * string        (* port where server should be listening *)
        * string option (* root of replica in host fs *)

(* Internal datatypes used in parsing command-line roots *)
type protocol = File | Socket | Ssh
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
      | "rsh" ->
          raise (Invalid_argument
                  (Printf.sprintf "protocol rsh has been deprecated, use ssh instead (optionally specifying a different sshcmd preference)"))
      | "socket" -> Socket
      | "ssh" -> Ssh
      | "unison" ->
          raise(Invalid_argument
                  (Printf.sprintf "protocol unison has been deprecated, use file, ssh, or socket instead" ))
      | _ ->
          raise(Invalid_argument
                  (Printf.sprintf "\"%s\": unrecognized protocol %s" s protocolName)) in
    Some(protocol,remainder)
  else if Str.string_match slashSlashRegexp s 0
  then Some(File,String.sub s 2 (String.length s - 2))
  else if Str.string_match protocolColonRegexp s 0
  then
    let matched = Str.matched_string s in
    match matched with
      "file:" | "ssh:" | "socket:" ->
        raise(Util.Fatal
                (Printf.sprintf
                   "ill-formed root specification \"%s\" (%s must be followed by //)"
                   s matched))
    | _ -> None
  else None

let userAtRegexp = Str.regexp "[-_a-zA-Z0-9.%@]+@"
let getUser s =
  if Str.string_match userAtRegexp s 0
  then
    let userAt = Str.matched_string s in
    let len = String.length userAt in
    let afterAt = Str.string_after s len in
    let beforeAt = String.sub userAt 0 (len-1) in
    (Some beforeAt,afterAt)
  else (None,s)

let ipv6Regexp = "[a-f0-9:.]+\\(%[-_a-zA-Z0-9~%.]+\\)?"
(* Hostname, IP or Unix domain socket path *)
let hostRegexp = Str.regexp ("[-_a-zA-Z0-9%.]+\\|{[^}]+}\\|\\[\\(" ^ ipv6Regexp ^ "\\)\\]")
let getHost s =
  if Str.string_match hostRegexp s 0
  then
    let host = Str.matched_string s in
    let host' = try Str.matched_group 1 s with Not_found -> host in
    let s' = Str.string_after s (String.length host) in
    (Some host', s')
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
  let s = Util.trimWhitespace s in
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

let parseHostPort s =
  let (hostOpt, s1) = getHost s in
  let (portOpt, s2) = getPort s1 in
  if String.length s2 > 0 then
    raise (Util.Transient
              (Printf.sprintf "ill-formed host specification %s" s));
  ((match hostOpt with Some h -> h | None -> ""), portOpt)

(* These should succeed *)
let t1 = "socket://tjim@saul.cis.upenn.edu:4040/hello/world"
let t2 = "ssh://tjim@saul/hello/world"
(*let t3 = "rsh://saul:4040/hello/world"
let t4 = "rsh://saul/hello/world"
let t5 = "rsh://saul"
let t6 = "rsh:///hello/world"*)
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

let cannotAbbrevFileRx = Rx.rx "(file:|ssh:|socket:).*"
let networkNameRx = Rx.rx "//.*"
(* Main external printing function *)
let clroot2string = function
| ConnectLocal None | ConnectLocal (Some "") -> "."
| ConnectLocal(Some s) ->
    if Rx.match_string cannotAbbrevFileRx s
    then if Rx.match_string networkNameRx s
    then Printf.sprintf "file:%s" s
    else Printf.sprintf "file:///%s" s
    else s
| ConnectBySocket(h,p,s) ->
    let p = if p <> "" then ":" ^ p else p in
    let h = if String.contains h ':' && h.[0] <> '{' then "[" ^ h ^ "]" else h in
    Printf.sprintf "socket://%s%s/%s" h p
      (match s with None -> "" | Some x -> x)
| ConnectByShell(sh,h,u,p,s) ->
    let user = match u with None -> "" | Some x -> x^"@" in
    let port = match p with None -> "" | Some x -> ":"^x in
    let path = match s with None -> "" | Some x -> x in
    let h = if String.contains h ':' then "[" ^ h ^ "]" else h in
    Printf.sprintf "%s://%s%s%s/%s" sh user h port path

(* Pref sshversion removed since 2.52 *)
let () = Prefs.markRemoved "sshversion"

let fixHost = function
  | ConnectLocal _ as r -> r
  | ConnectBySocket (h, "", s) ->
      (match parseHostPort h with
      | h, Some p -> ConnectBySocket (h, p, s)
      | h, None -> ConnectBySocket (h, "", s))
  | ConnectBySocket _ as r -> r
  | ConnectByShell (sh, h, u, None, s) ->
      let (h, p) = parseHostPort h in
      ConnectByShell (sh, h, u, p, s)
  | ConnectByShell _ as r -> r

(* Main external function *)
let parseRoot string =
  let illegal2 s = raise(Prefs.IllegalValue
                           (Printf.sprintf
                              "\"%s\": %s" string s)) in
  let (protocol,user,host,port,path) = parseUri string in
  let clroot =
    match protocol,user,host,port with
    | _,_,None,Some _
    | _,Some _,None,None
    | Socket, _, None, None
    | Ssh,_,None,_ ->
        illegal2 "missing host"
    | File,_,_,Some _ ->
        illegal2 "ill-formed (cannot use a port number with file)"
    | File,_,Some h,None ->
        let prefix = "//"^h^"/" in
        (match path with
          None -> ConnectLocal(Some prefix)
        | Some p -> ConnectLocal(Some(prefix^p)))
    | File,None,None,None ->
        ConnectLocal(path)
    | Socket, None, Some h, Some p when h.[0] <> '{' ->
        ConnectBySocket(h,p,path)
    | Socket, None, Some h, None when h.[0] = '{' ->
        ConnectBySocket (h, "", path)
    | Socket,Some _,_,_ ->
        illegal2 "ill-formed (cannot use a user with socket)"
    | Socket,_,_,None ->
        illegal2 "ill-formed (must give a port number with socket)"
    | Socket, _, Some _, Some _ ->
        illegal2 "ill-formed (must not give a port number with Unix domain socket)"
    | Ssh,_,Some h,_ ->
        ConnectByShell("ssh",h,user,port,path) in
  clroot
