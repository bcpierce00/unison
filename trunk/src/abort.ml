(* Unison file synchronizer: src/abort.ml *)
(* Copyright 1999-2012, Benjamin C. Pierce 

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

let debug = Trace.debug "abort"

(****)

let maxerrors =
  Prefs.createInt "maxerrors" 1
    "!maximum number of errors before a directory transfer is aborted"
    "This preference controls after how many errors Unison aborts a \
     directory transfer.  Setting it to a large number allows Unison \
     to transfer most of a directory even when some files fail to be \
     copied.  The default is 1.  If the preference is set too high, \
     Unison may take a long time to abort in case of repeated \
     failures (for instance, when the disk is full)."

(****)

let files = Hashtbl.create 17
let abortAll = ref false

let errorCountCell id =
  try
    Hashtbl.find files id
  with Not_found ->
    let c = ref 0 in
    Hashtbl.add files id c;
    c

let errorCount id = !(errorCountCell id)
let bumpErrorCount id = incr (errorCountCell id)

(****)

let reset () = Hashtbl.clear files; abortAll := false

(****)

let file id =
  debug (fun() -> Util.msg "Aborting line %s\n" (Uutil.File.toString id));
  bumpErrorCount id

let all () = abortAll := true

(****)

let check id =
  debug (fun() -> Util.msg "Checking line %s\n" (Uutil.File.toString id));
  if !abortAll || errorCount id >= Prefs.read maxerrors then begin
    debug (fun() ->
      Util.msg "Abort failure for line %s\n" (Uutil.File.toString id));
    raise (Util.Transient "Aborted")
  end

let testException e = (e = Util.Transient "Aborted")
