(* Unison file synchronizer: src/abort.ml *)
(* $Id$ *)
(* Copyright 1999-2007 (see COPYING for details) *)

let debug = Trace.debug "abort"

let files = ref ([] : Uutil.File.t list)
let abortAll = ref false

(****)

let reset () = files := []; abortAll := false

(****)

let file id =
  debug (fun() -> Util.msg "Aborting line %s\n" (Uutil.File.toString id));
  files := id :: !files

let all () = abortAll := true

(****)

let check id =
  debug (fun() -> Util.msg "Checking line %s\n" (Uutil.File.toString id));
  if !abortAll || Safelist.mem id !files then begin
    debug (fun() ->
      Util.msg "Abort failure for line %s\n" (Uutil.File.toString id));
    raise (Util.Transient "Aborted")
  end

let testException e = e = Util.Transient "Aborted"
