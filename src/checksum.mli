(* Unison file synchronizer: src/checksum.mli *)
(* $Id$ *)
(* Copyright 1999-2006 (see COPYING for details) *)

type t = int
type u = int array

val init : int             (* blockSize *)
        -> u               (* pre-computed table *) 

val substring : string
             -> int        (* offset in string *)
             -> int        (* substring length *)
             -> t

val roll : u               (* string length *)
        -> t               (* previous checksum *)
        -> char            (* outgoing char *)
        -> char            (* incoming char *)
        -> t
