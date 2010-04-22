

(* Fake functions *)

let s_ s = s

let f_ fmt =
  fmt^^""

let init_text () =
  "", []

let init_gtk () =
  "", []

IFDEF ENABLE_NLS THEN
include Gettext.Program 
  (struct 
     let textdomain = "unison"
     let codeset    = None
     let dependencies = Gettext.init
     let dir = 
       try 
         Some (Sys.getenv "UNISON_LOCALE")
       with Not_found ->
         None
   end)
  (GettextStub.Native)
ENDIF
