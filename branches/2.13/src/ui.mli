(* $I1: Unison file synchronizer: src/ui.mli $ *)
(* $I2: Last modified by bcpierce on Mon, 19 Jul 1999 18:14:04 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

(* The module Ui provides only the user interface signature.
   Implementations are provided by Uitext and Uitk. *)

module type SIG = sig
 val start : unit -> unit 
end


