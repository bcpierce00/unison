(* Unison file synchronizer: src/main.ml *)
(* Copyright 1999-2020, Benjamin C. Pierce

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


(* ---------------------------------------------------------------------- *)

(* This is the main program -- the thing that gets executed first when
   unison is run.

   The Main module is actually a functor that takes the user interface
   (e.g., Uitext or Uigtk) as a parameter.  This allows us to build with
   just one user interface at a time, which avoids having to always link
   in all the libraries needed by all the user interfaces.

   A non-functor interface is provided to allow the Mac GUI to reuse the
   startup code for non-GUI options.
 *)

(* ---------------------------------------------------------------------- *)

(* Some command-line arguments are handled specially during startup, e.g.,
   -doc
   -help
   -version
   -server
   -socket
   -ui
   They are expected to appear on the command-line only, not in a
   profile. In particular, -version and -doc will print to the
   standard output, so they only make sense if invoked from the
   command-line (and not a click-launched gui that has no standard
   output).

   Furthermore, the actions associated with these command-line
   arguments are executed without loading a profile or doing the usual
   command-line parsing. This is because we want to run the actions
   without loading a profile; and then we can't do command-line
   parsing because it is intertwined with profile loading.

   NB: the Mac GUI handles these options itself and needs to change
   if any more are added.
*)

let versionPrefName = "version"
let printVersionAndExit =
  Prefs.createBool versionPrefName false
    ~category:(`Basic `General)
    ~cli_only:true
    "print version and exit"
    ("Print the current version number and exit.  "
     ^ "(This option only makes sense on the command line.)")

let docsPrefName = "doc"
let docs =
  Prefs.createString docsPrefName ""
    ~category:(`Basic `General)
    ~cli_only:true
    "show documentation ('-doc topics' lists topics)"
    (  "The command-line argument \\texttt{-doc \\ARG{secname}} causes unison to "
       ^ "display section  \\ARG{secname} of the manual on the standard output "
       ^ "and then exit.   Use \\verb|-doc all| to display the whole manual, "
       ^ "which includes exactly the same information as the printed and HTML "
       ^ "manuals, modulo "
       ^ "formatting.  Use \\verb|-doc topics| to obtain a list of the "
       ^ "names of the various sections that can be printed.")

let prefsdocsPrefName = "prefsdocs"
let prefsdocs =
  Prefs.createBool prefsdocsPrefName false
    ~category:(`Internal `Devel)
    ~cli_only:true
    "*show full documentation for all preferences (and then exit)"
    ""

let prefsmanPrefName = "prefsman"
let prefsman =
  Prefs.createString prefsmanPrefName ""
    ~category:(`Internal `Devel)
    ~cli_only:true
    "*show manpage documentation for all preferences (and then exit)"
    ""

let serverPrefName = "server"
let server =
  Prefs.createBool serverPrefName false
    ~category:(`Internal `Other)
    ~cli_only:true
    "*normal or server mode" ""

let socketPrefName = "socket"
let socket =
  Prefs.createString socketPrefName ""
    ~category:(`Advanced `Remote)
    ~cli_only:true
    "act as a server on a socket"
    ("Start " ^ Uutil.myName ^ " as a server listening on a TCP socket "
     ^ "(with TCP port number as argument) or a local socket (aka Unix "
     ^ "domain socket) (with socket path as argument).")

let serverHostNameAlias = "host"
let serverHostName = "listen"
let serverHost =
  Prefs.createString serverHostName ""
    ~category:(`Advanced `Remote)
    ~cli_only:true
    "listen on this name or addr in server socket mode (can repeat)"
    ("When acting as a server on a TCP socket, Unison will by default listen "
     ^ "on \"any\" address (0.0.0.0 and [::]).  This command-line argument "
     ^ "allows to specify a different listening address and can be repeated "
     ^ "to listen on multiple addresses.  Listening address can be specified "
     ^ "as a host name or an IP address.")
let () = Prefs.alias serverHost serverHostNameAlias

(* User preference for which UI to use if there is a choice *)
let uiPrefName = "ui"
let interface =
  Prefs.create uiPrefName Uicommon.Graphic
    ~category:(`Advanced `General)
    ~cli_only:true
    "select UI ('text' or 'graphic'); command-line only"
    ("This preference selects either the graphical or the textual user "
     ^ "interface.  Legal values are \\verb|graphic| or \\verb|text|.  "
     ^ "\n\nBecause this option is processed specially during Unison's "
     ^ "start-up sequence, it can {\\em only} be used on the command line.  "
     ^ "In preference files it has no effect."
     ^ "\n\nIf "
     ^ "the Unison executable was compiled with only a textual interface, "
     ^ "this option has "
     ^ "no effect.  (The pre-compiled binaries are all compiled with both "
     ^ "interfaces available.)")
    (fun _ -> function
        "text" -> Uicommon.Text
      | "graphic" -> Uicommon.Graphic
      | other ->
          raise (Prefs.IllegalValue ("option ui :\n\
                                      text -> textual user interface\n\
                                      graphic -> graphic user interface\n"
                                      ^other^ " is not a legal value")))
    (function Uicommon.Text -> ["text"]
      | Uicommon.Graphic -> ["graphic"])
    Uicommon.minterface

let catch_all f =
  try
    try
      (* Util.msg "Starting catch_all...\n"; *)
      f ();
      (* Util.msg "Done catch_all...\n"; *)
    with Prefs.IllegalValue str -> raise (Util.Fatal str)
  with e ->
    Util.msg "Unison server failed: %s\n" (Uicommon.exn2string e); exit 1;;

let gui_safe_printf fmt =
  Printf.ksprintf (fun s ->
    if System.has_stdout ~info:s then Printf.printf "%s" s) fmt

let verify_stdout () =
  if not (System.has_stdout ~info:"") then exit 37

let init () = begin
  ignore (Gc.set {(Gc.get ()) with Gc.max_overhead = 150});
  (* Make sure exception descriptions include backtraces *)
  Printexc.record_backtrace true;

  let argv = Prefs.scanCmdLine Uicommon.usageMsg in

  (* Print version if requested *)
  if Util.StringMap.mem versionPrefName argv then begin
    gui_safe_printf "%s version %s\n" Uutil.myName Uutil.myVersion;
    exit 0
  end;

  (* Print docs for all preferences if requested (this is used when building
     the manual) *)
  if Util.StringMap.mem prefsdocsPrefName argv then begin
    Prefs.printFullDocs `TeX;
    exit 0
  end;

  if Util.StringMap.mem prefsmanPrefName argv then begin
    begin match Util.StringMap.find prefsmanPrefName argv with
      | "short" :: _ -> Prefs.printUsageForMan ()
      | "full" :: _ -> Prefs.printFullDocs `man
      | _ -> ()
    end;
    exit 0
  end;

  (* Display documentation if requested *)
  begin try
    let docv = Util.StringMap.find docsPrefName argv in
    verify_stdout ();
    begin match docv with
      [] ->
        assert false
    | "topics"::_ ->
        Printf.printf "Documentation topics:\n";
        Safelist.iter
          (fun (sn,(n,doc)) ->
            if sn<>"" then Printf.printf "   %12s %s\n" sn n)
          Strings.docs;
        Printf.printf
          "\nType \"%s -doc <topic>\" for detailed information about <topic>\n"
          Uutil.myName;
        Printf.printf
          "or \"%s -doc all\" for the whole manual\n\n"
          Uutil.myName
    | "all"::_ ->
        Printf.printf "\n";
        Safelist.iter
          (fun (sn,(n,doc)) -> if n<>"Junk" then Printf.printf "%s\n" doc)
          Strings.docs
    | topic::_ ->
        (try
          let (_,d) = Safelist.assoc topic Strings.docs in
          Printf.printf "\n%s\n" d
        with
          Not_found ->
            Printf.printf "Documentation topic %s not recognized:"
              topic;
            Printf.printf "\nType \"%s -doc topics\" for a list\n"
              Uutil.myName)
    end;
    exit 0
  with Not_found -> () end;

  (* Start a server if requested *)
  if Util.StringMap.mem serverPrefName argv then begin
    catch_all (fun () ->
      Os.createUnisonDir();
      Remote.beAServer();
      exit 0)
  end;

  (* Start a socket server if requested *)
  begin try
    let i = List.hd (Util.StringMap.find socketPrefName argv) in
    catch_all (fun () ->
     Os.createUnisonDir();
      Remote.waitOnPort
        ((try Util.StringMap.find serverHostName argv with Not_found -> []) @
         (try Util.StringMap.find serverHostNameAlias argv with Not_found -> []))
        i);
    exit 0
  with Not_found -> () end;
  argv
end

(* non-GUI startup for Mac GUI version *)
let nonGuiStartup () = begin
  let argv = init() in (* might not return *)
  (* if it returns start a UI *)
  (try
    (match Util.StringMap.find uiPrefName argv with
      "text"::_    -> (Uitext.Body.start Uicommon.Text; exit 0)
    | "graphic"::_ -> () (* fallthru *)
    | _            -> Prefs.printUsage Uicommon.usageMsg; exit 1)
  with Not_found -> ());
  ()
end

module Body = functor (Ui : Uicommon.UI) -> struct
  let argv = init() in (* might not return *)
  (* if it returns start a UI *)
  Ui.start
    (try
      (match Util.StringMap.find uiPrefName argv with
      | "text"::_    -> verify_stdout (); Uicommon.Text
      | "graphic"::_ -> Uicommon.Graphic
      | _ -> verify_stdout (); Prefs.printUsage Uicommon.usageMsg; exit 1)
    with Not_found -> Ui.defaultUi)
end
