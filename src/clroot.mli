(* Unison file synchronizer: src/clroot.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

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

val clroot2string : clroot -> string

val parseRoot : string -> clroot

(* Parse a clroot with manually constructed host
   which may or may not include the port number *)
val fixHost : clroot -> clroot
