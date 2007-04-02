(* Unison file synchronizer: src/ui.mli *)
(* Copyright 1999-2007 (see COPYING for details) *)

(* The module Ui provides only the user interface signature.
   Implementations are provided by Uitext and Uitk. *)

module type SIG = sig
 val start : unit -> unit 
end


