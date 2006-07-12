(* Unison file synchronizer: src/fileutil.ml *)
(* $Id$ *)
(* Copyright 1999-2006 (see COPYING for details) *)

(* Convert backslashes in a string to forward slashes.  Useful in Windows.   *)
let backslashes2forwardslashes s0 =
  try
    ignore(String.index s0 '\\'); (* avoid alloc if possible *)
    let n = String.length s0 in
    let s = String.create n in
    for i = 0 to n-1 do
      let c = String.get s0 i in
      if c = '\\'
      then String.set s i '/'
      else String.set s i c
    done;
    s
  with Not_found -> s0

let rec removeTrailingSlashes s =
  let len = String.length s in
  if len>0 && String.get s (len-1) = '/'
  then removeTrailingSlashes (String.sub s 0 (len-1))
  else s
