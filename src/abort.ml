(* Unison file synchronizer: src/abort.ml *)
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
