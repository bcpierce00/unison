(* Unison file synchronizer: src/abort.ml *)
(* $Id$ *)
(* Copyright 1999-2006 (see COPYING for details) *)

let debug = Trace.debug "abort"

let files = ref ([] : Uutil.File.t list)
let abortAll = ref false

(****)

let reset () = files := []; abortAll := false

(****)

let file id =
  debug (fun() -> Util.msg "Aborting line %d\n" (Uutil.File.toLine id));
  files := id :: !files

let all () = abortAll := true

(****)

let check id =
  debug (fun() -> Util.msg "Checking line %d\n" (Uutil.File.toLine id));
  if !abortAll || Safelist.mem id !files then begin
    debug (fun() ->
      Util.msg "Abort failure for line %d\n" (Uutil.File.toLine id));
    raise (Util.Transient "Aborted")
  end

let testException e = e = Util.Transient "Aborted"
