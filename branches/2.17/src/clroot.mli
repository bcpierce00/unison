(* $I1: Unison file synchronizer: src/clroot.mli $ *)
(* $I2: Last modified by tjim on Wed, 06 Nov 2002 18:48:47 -0500 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

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

val clroot2string : clroot -> string

val parseRoot : string -> clroot
