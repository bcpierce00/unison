(* $I1: Unison file synchronizer: src/case.ml $ *)
(* $I2: Last modified by vouillon on Wed, 19 May 2004 16:05:24 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

(* The update detector, reconciler, and transporter behave differently       *)
(* depending on whether the local and/or remote file system is case          *)
(* insensitive.  This pref is set during the initial handshake if any one of *)
(* the hosts is case insensitive.                                            *)
let caseInsensitiveMode =
  Prefs.createString "ignorecase" "default"
    "ignore upper/lowercase in filenames (`true', `false', or `default')"
    ("When set to {\\tt true}, this flag causes Unison to treat "
     ^ "filenames as case insensitive---i.e., files in the two "
     ^ "replicas whose names differ in (upper- and lower-case) `spelling' "
     ^ "are treated as the same file.  When the flag is set to {\\tt false}, Unison "
     ^ "will treat all filenames as case sensitive.  Ordinarily, when the flag is "
     ^ "set to {\tt default}, "
     ^ "filenames are automatically taken to be case-insensitive if "
     ^ "either host is running Windows or OSX.  In rare circumstances it is  "
     ^ "useful to set the flag manually (e.g. when running Unison on a  "
     ^ "Unix system with a FAT [Windows] volume mounted).")

(* Defining this variable as a preference ensures that it will be propagated
   to the other host during initialization *)
let someHostIsInsensitive =
  Prefs.createBool "someHostIsInsensitive" false
    "*Pseudo-preference for internal use only" ""

(* Note: this function must be fast *)
let insensitive () = Prefs.read someHostIsInsensitive

(* During startup the client determines the case sensitivity of each root.   *)
(* If any root is case insensitive, all roots must know it; we ensure this   *)
(* by storing the information in a pref so that it is propagated to the      *)
(* server with the rest of the prefs.                                        *)
let init b =
  Prefs.set someHostIsInsensitive
    (Prefs.read caseInsensitiveMode = "yes" ||
     Prefs.read caseInsensitiveMode = "true" ||
     (Prefs.read caseInsensitiveMode = "default" && b))
