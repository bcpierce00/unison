(* $I1: Unison file synchronizer: src/uigtk.ml $ *)
(* $I2: Last modified by vouillon on Thu, 09 Sep 2004 08:43:03 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

open Common
open Lwt

module Private = struct

let debug = Trace.debug "ui"

(**********************************************************************
                           LOW-LEVEL STUFF
 **********************************************************************)

(**********************************************************************
 Some message strings (build them here because they look ugly in the
 middle of other code.
 **********************************************************************)

let tryAgainMessage =
  Printf.sprintf
"You can use %s to synchronize a local directory with another local directory,
or with a remote directory.

Please enter the first (local) directory that you want to synchronize."
Uutil.myName

(* ---- *)

let helpmessage = Printf.sprintf
"%s can synchronize a local directory with another local directory, or with
a directory on a remote machine.

To synchronize with a local directory, just enter the file name.

To synchronize with a remote directory, you must first choose a protocol
that %s will use to connect to the remote machine.  Each protocol has
different requirements:

1) To synchronize using SSH, there must be an SSH client installed on
this machine and an SSH server installed on the remote machine.  You
must enter the host to connect to, a user name (if different from
your user name on this machine), and the directory on the remote machine
(relative to your home directory on that machine).

2) To synchronize using RSH, there must be an RSH client installed on
this machine and an RSH server installed on the remote machine.  You
must enter the host to connect to, a user name (if different from
your user name on this machine), and the directory on the remote machine
(relative to your home directory on that machine).

3) To synchronize using %s's socket protocol, there must be a %s
server running on the remote machine, listening to the port that you
specify here.  (Use \"%s -socket xxx\" on the remote machine to
start the %s server.)  You must enter the host, port, and the directory
on the remote machine (relative to the working directory of the
%s server running on that machine)."
Uutil.myName Uutil.myName Uutil.myName Uutil.myName Uutil.myName Uutil.myName Uutil.myName

(**********************************************************************
 Font preferences
 **********************************************************************)

let fontMonospaceMedium =
  if Util.osType = `Win32 then
    lazy (Gdk.Font.load "-*-Courier New-Medium-R-Normal--*-110-*-*-*-*-*-*")
  else
    lazy (Gdk.Font.load "-*-Clean-Medium-R-Normal--*-130-*-*-*-*-*-*")

let fontMonospaceBold =
  if Util.osType = `Win32 then
    lazy (Gdk.Font.load "-*-Courier New-Bold-R-Normal--*-110-*-*-*-*-*-*")
  else
    lazy (Gdk.Font.load "-*-Courier-Bold-R-Normal--*-120-*-*-*-*-*-*")

(*********************************************************************
  UI state variables
 *********************************************************************)

type stateItem = { mutable ri : reconItem;
                   mutable bytesTransferred : Uutil.Filesize.t;
                   mutable whatHappened : Util.confirmation option }
let theState = ref [||]

let current = ref None

(* ---- *)

let currentWindow = ref None

let grabFocus t =
  match !currentWindow with
    Some w -> t#set_transient_for w;
              w#misc#set_sensitive false
  | None   -> ()

let releaseFocus () =
  begin match !currentWindow with
    Some w -> w#misc#set_sensitive true
  | None   -> ()
  end

(*********************************************************************
  Lock management
 *********************************************************************)

let busy = ref false

let getLock f =
  if !busy then
    Trace.status "Synchronizer is busy, please wait.."
  else begin
    busy := true; f (); busy := false
  end

(**********************************************************************
  Miscellaneous
 **********************************************************************)

let gtk_sync () = while Glib.Main.iteration false do () done

(**********************************************************************
                       USEFUL LOW-LEVEL WIDGETS
 **********************************************************************)

class scrolled_text ?editable ?word_wrap ?width ?height ?packing ?show
    () =
  let sw =
    GBin.scrolled_window ?width ?height ?packing ~show:false
      ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC ()
  in
  let text = GEdit.text ?editable ?word_wrap ~packing:sw#add () in
  object
    inherit GObj.widget_full sw#as_widget
    method text = text
    method insert ?(font=fontMonospaceMedium) s =
      text#freeze ();
      text#delete_text ~start:0 ~stop:text#length;
      text#insert ~font:(Lazy.force font) s;
      text#thaw ()
    method show () = sw#misc#show ()
    initializer
      if show <> Some false then sw#misc#show ()
  end

(* ------ *)

(* oneBox: Display a message in a window and wait for the user
   to hit the button. *)
let oneBox ~title ~message ~label =
  let t = GWindow.dialog ~title ~wm_name:title
      ~modal:true ~position:`CENTER () in
  grabFocus t;
  let h = GPack.hbox ~packing:(t#vbox#pack ~expand:false ~padding:20) () in
  ignore(GMisc.label ~justify:`LEFT ~text:message
           ~packing:(h#pack ~expand:false ~padding:20) ());
  let b = GButton.button ~label ~packing:t#action_area#add () in
  b#grab_default ();
  ignore (b#connect#clicked ~callback:(fun () -> t#destroy()));
  t#show ();
  (* Do nothing until user destroys window *)
  ignore (t#connect#destroy ~callback:GMain.Main.quit);
  GMain.Main.main ();
  releaseFocus ()

let okBox ~title ~message = oneBox ~title ~message ~label:"OK"

(* ------ *)

(* twoBox: Display a message in a window and wait for the user
   to hit one of two buttons.  Return true if the first button is
   chosen, false if the second button is chosen. *)
let twoBox ~title ~message ~alabel ~blabel =
  let result = ref false in
  let t = GWindow.dialog ~title ~wm_name:title ~modal:true
      ~position:`CENTER () in
  grabFocus t;
  let h = GPack.hbox ~packing:(t#vbox#pack ~expand:false ~padding:20) () in
  ignore(GMisc.label ~justify:`LEFT ~text:message
           ~packing:(h#pack ~expand:false ~padding:20) ());
  (*
  ignore(GMisc.label ~text:message
           ~packing:(t#vbox#pack ~expand:false ~padding:4) ());
  *)
  let yes = GButton.button ~label:alabel ~packing:t#action_area#add ()
  and no = GButton.button ~label:blabel ~packing:t#action_area#add () in
  yes#grab_default ();
  ignore (yes#connect#clicked
            ~callback:(fun () -> t#destroy (); result := true));
  ignore (no#connect#clicked
            ~callback:(fun () -> t#destroy (); result := false));
  t#show ();
  (* Do nothing until user destroys window *)
  ignore (t#connect#destroy ~callback:GMain.Main.quit);
  GMain.Main.main ();
  releaseFocus ();
  !result

(* ------ *)

(* Avoid recursive invocations of the function below (a window receives
   delete events even when it is not sensitive) *)
let inExit = ref false

let doExit () = Lwt_unix.run (Update.unlockArchives ()); exit 0

let safeExit () =
  if not !inExit then begin
    inExit := true;
    if not !busy then exit 0 else
    if twoBox ~title:"Premature exit"
        ~message:"Unison is working, exit anyway ?"
        ~alabel:"Yes" ~blabel:"No"
    then exit 0;
    inExit := false
  end

(* ------ *)

(* warnBox: Display a warning message in a window and wait (unless
   we're in batch mode) for the user to hit "OK" or "Exit". *)
let warnBox title message =
  if Prefs.read Globals.batch then begin
    (* In batch mode, just pop up a window and go ahead *)
    let t = GWindow.dialog ~title ~wm_name:title ~position:`CENTER () in
    let h = GPack.hbox ~packing:(t#vbox#pack ~expand:false ~padding:20) () in
    ignore(GMisc.label ~justify:`LEFT ~text:message
             ~packing:(h#pack ~expand:false ~padding:20) ());
    let t_dismiss =
      GButton.button ~label:"Dismiss" ~packing:t#action_area#add () in
    t_dismiss#grab_default ();
    let dismiss () = t#destroy () in
    ignore (t_dismiss#connect#clicked ~callback:dismiss);
    ignore (t#event#connect#delete ~callback:(fun _ -> dismiss (); true));
    t#show ()
  end else begin
    inExit := true;
    let ok = twoBox ~title ~message ~alabel:"OK" ~blabel:"Exit" in
    if not(ok) then doExit ();
    inExit := false
  end

(**********************************************************************
                      CHARACTER SET TRANSCODING
***********************************************************************)

(* Transcodage from Microsoft Windows Codepage 1252 to Unicode *)

(* Unison currently uses the "ASCII" Windows filesystem API.  With
   this API, filenames are encoded using a proprietary character
   encoding.  This encoding depends on the Windows setup, but in
   Western Europe, the Windows Codepage 1252 is usually used.
   GTK, on the other hand, uses the UTF-8 encoding.  This code perform
   the translation from Codepage 1252 to UTF-8.  A call to [transcode]
   should be wrapped around every string below that might contain
   non-ASCII characters. *)

let code =
  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18;
     19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34;
     35; 36; 37; 38; 39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50;
     51; 52; 53; 54; 55; 56; 57; 58; 59; 60; 61; 62; 63; 64; 65; 66;
     67; 68; 69; 70; 71; 72; 73; 74; 75; 76; 77; 78; 79; 80; 81; 82;
     83; 84; 85; 86; 87; 88; 89; 90; 91; 92; 93; 94; 95; 96; 97; 98;
     99; 100; 101; 102; 103; 104; 105; 106; 107; 108; 109; 110; 111;
     112; 113; 114; 115; 116; 117; 118; 119; 120; 121; 122; 123; 124;
     125; 126; 127; 8364; 129; 8218; 131; 8222; 8230; 8224; 8225; 136;
     8240; 352; 8249; 346; 356; 381; 377; 144; 8216; 8217; 8220; 8221;
     8226; 8211; 8212; 152; 8482; 353; 8250; 347; 357; 382; 378; 160;
     711; 728; 321; 164; 260; 166; 167; 168; 169; 350; 171; 172; 173;
     174; 379; 176; 177; 731; 322; 180; 181; 182; 183; 184; 261; 351;
     187; 376; 733; 317; 380; 340; 193; 194; 258; 196; 313; 262; 199;
     268; 201; 280; 203; 282; 205; 206; 270; 272; 323; 327; 211; 212;
     336; 214; 215; 344; 366; 218; 368; 220; 221; 354; 223; 341; 225;
     226; 259; 228; 314; 263; 231; 269; 233; 281; 235; 283; 237; 238;
     271; 273; 324; 328; 243; 244; 337; 246; 247; 345; 367; 250; 369;
     252; 253; 355; 729 |]

let rec transcode_rec buf s i l =
  if i < l then begin
    let c = code.(Char.code s.[i]) in
    if c < 0x80 then
      Buffer.add_char buf (Char.chr c)
    else if c < 0x800 then begin
      Buffer.add_char buf (Char.chr (c lsr 6 + 0xC0));
      Buffer.add_char buf (Char.chr (c land 0x3f + 0x80))
    end else if c < 0x10000 then begin
      Buffer.add_char buf (Char.chr (c lsr 12 + 0xE0));
      Buffer.add_char buf (Char.chr ((c lsr 6) land 0x3f + 0x80));
      Buffer.add_char buf (Char.chr (c land 0x3f + 0x80))
    end;
    transcode_rec buf s (i + 1) l
  end

let transcode s =
  if Util.osType = `Win32 then
    let buf = Buffer.create 32 in
    transcode_rec buf s 0 (String.length s);
    Buffer.contents buf
  else
    s

(**********************************************************************
                         HIGHER-LEVEL WIDGETS
***********************************************************************)

(*
XXX
* Accurate write accounting:
  - Local copies on the remote side are ignored
  - What about failures?
*)
class stats width height =
  let pixmap = GDraw.pixmap ~width ~height () in
  let area =
    pixmap#set_foreground `WHITE;
    pixmap#rectangle ~filled:true ~x:0 ~y:0 ~width ~height ();
    GMisc.pixmap pixmap ~width ~height ~xpad:4 ~ypad:8 ()
  in
  object (self)
    inherit GObj.widget_full area#as_widget
    val mutable maxim = ref 0.
    val mutable scale = ref 1.
    val mutable min_scale = 1.
    val values = Array.make width 0.
    val mutable active = false

    method activate a = active <- a

    method scale h = truncate ((float height) *. h /. !scale)

    method private rect i v' v =
      let h = self#scale v in
      let h' = self#scale v' in
      let h1 = min h' h in
      let h2 = max h' h in
      pixmap#set_foreground `BLACK;
      pixmap#rectangle
        ~filled:true ~x:i ~y:(height - h1) ~width:1 ~height:h1 ();
      for h = h1 + 1 to h2 do
        let v = truncate (65535. *. (float (h - h1) /. float (h2 - h1))) in
        pixmap#set_foreground (`RGB (v, v, v));
        pixmap#rectangle
          ~filled:true ~x:i ~y:(height - h) ~width:1 ~height:1 ();
      done

    method push v =
      let need_max = values.(0) = !maxim in
      for i = 0 to width - 2 do
        values.(i) <- values.(i + 1)
      done;
      values.(width - 1) <- v;
      if need_max then begin
        maxim := 0.;
        for i = 0 to width - 1 do maxim := max !maxim values.(i) done
      end else
        maxim := max !maxim v;
      if active then begin
        let need_resize =
          !maxim > !scale || (!maxim > min_scale && !maxim < !scale /. 1.5) in
        if need_resize then begin
          scale := min_scale;
          while !maxim > !scale do
            scale := !scale *. 1.5
          done;
          pixmap#set_foreground `WHITE;
          pixmap#rectangle ~filled:true ~x:0 ~y:0 ~width ~height ();
          pixmap#set_foreground `BLACK;
          for i = 0 to width - 1 do
            self#rect i values.(max 0 (i - 1)) values.(i)
          done
        end else begin
          pixmap#put_pixmap ~x:0 ~y:0 ~xsrc:1 (pixmap#pixmap);
          pixmap#set_foreground `WHITE;
          pixmap#rectangle
            ~filled:true ~x:(width - 1) ~y:0 ~width:1 ~height ();
          self#rect (width - 1) values.(width - 2) values.(width - 1)
        end;
        area#misc#draw None
      end
  end

let clientWritten = ref 0.
let serverWritten = ref 0.

let statistics () =
  let title = "Statistics" in
  let t = GWindow.dialog ~title ~wm_name:title () in
  let t_dismiss =
    GButton.button ~label:"Dismiss" ~packing:t#action_area#add () in
  t_dismiss#grab_default ();
  let dismiss () = t#misc#hide () in
  ignore (t_dismiss#connect#clicked ~callback:dismiss);
  ignore (t#event#connect#delete ~callback:(fun _ -> dismiss (); true));

  let emission = new stats 320 50 in
  t#vbox#pack ~expand:false ~padding:4 (emission :> GObj.widget);
  let reception = new stats 320 50 in
  t#vbox#pack ~expand:false ~padding:4 (reception :> GObj.widget);

  let lst =
    GList.clist
      ~packing:(t#vbox#add)
      ~titles_active:false
      ~titles:[""; "Client"; "Server"; "Total"] ()
  in
  lst#set_column ~auto_resize:true 0;
  lst#set_column ~auto_resize:true ~justification:`RIGHT 1;
  lst#set_column ~auto_resize:true ~justification:`RIGHT 2;
  lst#set_column ~auto_resize:true ~justification:`RIGHT 3;
  ignore (lst#append ["Reception rate"]);
  ignore (lst#append ["Data received"]);
  ignore (lst#append ["File data written"]);
  let style = lst#misc#style#copy in
  style#set_font (Lazy.force fontMonospaceMedium);
  for r = 0 to 2 do
    lst#set_row ~selectable:false r;
    for c = 1 to 3 do
      lst#set_cell ~style r c
    done
  done;

  ignore (t#event#connect#map (fun _ ->
    emission#activate true;
    reception#activate true;
    false));
  ignore (t#event#connect#unmap (fun _ ->
    emission#activate false;
    reception#activate false;
    false));

  let delay = 0.5 in
  let a = 0.5 in
  let b = 0.8 in

  let emittedBytes = ref 0. in
  let emitRate = ref 0. in
  let emitRate2 = ref 0. in
  let receivedBytes = ref 0. in
  let receiveRate = ref 0. in
  let receiveRate2 = ref 0. in
  let timeout _ =
    emitRate :=
      a *. !emitRate +.
      (1. -. a) *. (!Remote.emittedBytes -. !emittedBytes) /. delay;
    emitRate2 :=
      b *. !emitRate2 +.
      (1. -. b) *. (!Remote.emittedBytes -. !emittedBytes) /. delay;
    emission#push !emitRate;
    receiveRate :=
      a *. !receiveRate +.
      (1. -. a) *. (!Remote.receivedBytes -. !receivedBytes) /. delay;
    receiveRate2 :=
      b *. !receiveRate2 +.
      (1. -. b) *. (!Remote.receivedBytes -. !receivedBytes) /. delay;
    reception#push !receiveRate;
    emittedBytes := !Remote.emittedBytes;
    receivedBytes := !Remote.receivedBytes;
    let kib2str v = Format.sprintf "%.0f B" v in
    let rate2str v =
      if v > 9.9e3 then begin
        if v > 9.9e6 then
          Format.sprintf "%4.0f MiB/s" (v /. 1e6)
        else if v > 999e3 then
          Format.sprintf "%4.1f MiB/s" (v /. 1e6)
        else
          Format.sprintf "%4.0f KiB/s" (v /. 1e3)
      end else begin
        if v > 990. then
          Format.sprintf "%4.1f KiB/s" (v /. 1e3)
        else if v > 99. then
          Format.sprintf "%4.2f KiB/s" (v /. 1e3)
        else
          "          "
      end
    in
    lst#set_cell ~text:(rate2str !receiveRate2) 0 1;
    lst#set_cell ~text:(rate2str !emitRate2) 0 2;
    lst#set_cell ~text:
      (rate2str (!receiveRate2 +. !emitRate2)) 0 3;
    lst#set_cell ~text:(kib2str !receivedBytes) 1 1;
    lst#set_cell ~text:(kib2str !emittedBytes) 1 2;
    lst#set_cell ~text:
      (kib2str (!receivedBytes +. !emittedBytes)) 1 3;
    lst#set_cell ~text:(kib2str !clientWritten) 2 1;
    lst#set_cell ~text:(kib2str !serverWritten) 2 2;
    lst#set_cell ~text:
      (kib2str (!clientWritten +. !serverWritten)) 2 3;
    true
  in
  ignore (GMain.Timeout.add ~ms:(truncate (delay *. 1000.)) ~callback:timeout);

  t

(****)

(* Standard file dialog *)
let file_dialog ~title ~callback ?filename () =
  let sel = GWindow.file_selection ~title ~modal:true ?filename () in
  grabFocus sel;
  ignore (sel#cancel_button#connect#clicked ~callback:sel#destroy);
  ignore (sel#ok_button#connect#clicked ~callback:
            (fun () ->
               let name = sel#get_filename in
               sel#destroy ();
               callback name));
  sel#show ();
  ignore (sel#connect#destroy ~callback:GMain.Main.quit);
  GMain.Main.main ();
  releaseFocus ()

(* ------ *)

let fatalError message =
  Trace.log ((transcode message) ^ "\n");
  oneBox ~title:(Printf.sprintf "%s: Fatal error"
                   (String.capitalize Uutil.myName))
    ~message ~label:"Quit"

(* ------ *)

let tryAgainOrQuit message =
  twoBox ~title:"Error" ~message ~alabel:"Try again" ~blabel:"Quit";;

(* ------ *)

let getFirstRoot() =
  let t = GWindow.dialog ~title:"Root selection" ~wm_name:"Root selection"
      ~modal:true ~allow_grow:true () in
  t#misc#grab_focus ();

  let hb = GPack.hbox
      ~packing:(t#vbox#pack ~expand:false ~padding:15) () in
  ignore(GMisc.label ~text:tryAgainMessage
           ~justify:`LEFT
           ~packing:(hb#pack ~expand:false ~padding:15) ());

  let f1 = GPack.hbox ~spacing:4
      ~packing:(t#vbox#pack ~expand:true ~padding:4) () in
  ignore (GMisc.label ~text:"Dir:" ~packing:(f1#pack ~expand:false) ());
  let fileE = GEdit.entry ~packing:f1#add () in
  fileE#misc#grab_focus ();
  let browseCommand() =
    file_dialog ~title:"Select a local directory"
      ~callback:fileE#set_text ~filename:fileE#text () in
  let b = GButton.button ~label:"Browse"
      ~packing:(f1#pack ~expand:false) () in
  ignore (b#connect#clicked ~callback:browseCommand);

  let f3 = t#action_area in
  let result = ref None in
  let contCommand() =
    result := Some(fileE#text);
    t#destroy () in
  let contButton = GButton.button ~label:"Continue" ~packing:f3#add () in
  ignore (contButton#connect#clicked ~callback:contCommand);
  ignore (fileE#connect#activate ~callback:contCommand);
  contButton#grab_default ();
  let quitButton = GButton.button ~label:"Quit" ~packing:f3#add () in
  ignore (quitButton#connect#clicked
            ~callback:(fun () -> result := None; t#destroy()));
  t#show ();
  ignore (t#connect#destroy ~callback:GMain.Main.quit);
  GMain.Main.main ();
  match !result with None -> None
  | Some file ->
      Some(Clroot.clroot2string(Clroot.ConnectLocal(Some file)))

(* ------ *)

let getSecondRoot () =
  let t = GWindow.dialog ~title:"Root selection" ~wm_name:"Root selection"
      ~modal:true ~allow_grow:true () in
  t#misc#grab_focus ();

  let message = "Please enter the second directory you want to synchronize." in

  let vb = t#vbox in
  let hb = GPack.hbox ~packing:(vb#pack ~expand:false ~padding:15) () in
  ignore(GMisc.label ~text:message
           ~justify:`LEFT
           ~packing:(hb#pack ~expand:false ~padding:15) ());
  let helpB = GButton.button ~label:"Help" ~packing:hb#add () in
  ignore (helpB#connect#clicked
            ~callback:(fun () -> okBox ~title:"Picking roots"
                ~message:helpmessage));

  let result = ref None in

  let f = GPack.vbox ~packing:(vb#pack ~expand:false) () in

  let f1 = GPack.hbox ~spacing:4 ~packing:f#add () in
  ignore (GMisc.label ~text:"Directory:" ~packing:(f1#pack ~expand:false) ());
  let fileE = GEdit.entry ~packing:f1#add () in
  fileE#misc#grab_focus ();
  let browseCommand() =
    file_dialog ~title:"Select a local directory"
      ~callback:fileE#set_text ~filename:fileE#text () in
  let b = GButton.button ~label:"Browse"
      ~packing:(f1#pack ~expand:false) () in
  ignore (b#connect#clicked ~callback:browseCommand);

  let f0 = GPack.hbox ~spacing:4 ~packing:f#add () in
  let localB = GButton.radio_button ~packing:(f0#pack ~expand:false)
      ~label:"Local" () in
  let sshB = GButton.radio_button ~group:localB#group
      ~packing:(f0#pack ~expand:false)
      ~label:"SSH" () in
  let rshB = GButton.radio_button ~group:localB#group
      ~packing:(f0#pack ~expand:false) ~label:"RSH" () in
  let socketB = GButton.radio_button ~group:sshB#group
      ~packing:(f0#pack ~expand:false) ~label:"Socket" () in

  let f2 = GPack.hbox ~spacing:4 ~packing:f#add () in
  ignore (GMisc.label ~text:"Host:" ~packing:(f2#pack ~expand:false) ());
  let hostE = GEdit.entry ~packing:f2#add () in

  ignore (GMisc.label ~text:"(Optional) User:"
            ~packing:(f2#pack ~expand:false) ());
  let userE = GEdit.entry ~packing:f2#add () in

  ignore (GMisc.label ~text:"Port:"
            ~packing:(f2#pack ~expand:false) ());
  let portE = GEdit.entry ~packing:f2#add () in

  let varLocalRemote = ref (`Local : [`Local|`SSH|`RSH|`SOCKET]) in
  let localState() =
    varLocalRemote := `Local;
    hostE#misc#set_sensitive false;
    userE#misc#set_sensitive false;
    portE#misc#set_sensitive false;
    b#misc#set_sensitive true in
  let remoteState() =
    hostE#misc#set_sensitive true;
    b#misc#set_sensitive false;
    match !varLocalRemote with
      `SOCKET ->
        (portE#misc#set_sensitive true; userE#misc#set_sensitive false)
    | _ ->
        (portE#misc#set_sensitive false; userE#misc#set_sensitive true) in
  let protoState x =
    varLocalRemote := x;
    remoteState() in
  ignore (localB#connect#clicked ~callback:localState);
  ignore (sshB#connect#clicked ~callback:(fun () -> protoState(`SSH)));
  ignore (rshB#connect#clicked ~callback:(fun () -> protoState(`RSH)));
  ignore (socketB#connect#clicked ~callback:(fun () -> protoState(`SOCKET)));
  localState();
  let getRoot() =
    let file = fileE#text in
    let user = userE#text in
    let host = hostE#text in
    match !varLocalRemote with
      `Local ->
        Clroot.clroot2string(Clroot.ConnectLocal(Some file))
    | `SSH | `RSH ->
        Clroot.clroot2string(
        Clroot.ConnectByShell((if !varLocalRemote=`SSH then "ssh" else "rsh"),
                              host,
                              (if user="" then None else Some user),
                              Some portE#text,
                              Some file))
    | `SOCKET ->
        Clroot.clroot2string(
        (* FIX: report an error if the port entry is not well formed *)
        Clroot.ConnectBySocket(host,
                               portE#text,
                               Some file)) in
  let contCommand() =
    try
      let root = getRoot() in
      result := Some root;
      t#destroy ()
    with Failure "int_of_string" ->
      if portE#text="" then
        okBox ~title:"Error" ~message:"Please enter a port"
      else okBox ~title:"Error"
          ~message:"The port you specify must be an integer"
    | _ ->
      okBox ~title:"Error"
        ~message:"Something's wrong with the values you entered, try again" in
  let f3 = t#action_area in
  let contButton = GButton.button ~label:"Continue" ~packing:f3#add () in
  ignore (contButton#connect#clicked ~callback:contCommand);
  contButton#grab_default ();
  ignore (fileE#connect#activate ~callback:contCommand);
  let quitButton = GButton.button ~label:"Quit" ~packing:f3#add () in
  ignore (quitButton#connect#clicked ~callback:safeExit);

  t#show ();
  ignore (t#connect#destroy ~callback:GMain.Main.quit);
  GMain.Main.main ();
  !result

(* ------ *)

type profileInfo = {roots:string list; label:string option}

(* ------ *)

let termInteract() =
(*
  if Util.isOSX then Some(fun s -> "") (*FIXTJ*)
  else
*)
    None

(* ------ *)

let profileKeymap = Array.create 10 None

let provideProfileKey filename k profile info =
  try
    let i = int_of_string k in
    if 0<=i && i<=9 then
      match profileKeymap.(i) with
        None -> profileKeymap.(i) <- Some(profile,info)
      | Some(otherProfile,_) ->
          raise (Util.Fatal
            ("Error scanning profile "^filename^":\n"
             ^ "shortcut key "^k^" is already bound to profile "
             ^ otherProfile))
    else
      raise (Util.Fatal
        ("Error scanning profile "^filename^":\n"
         ^ "Value of 'key' preference must be a single digit (0-9), "
         ^ "not " ^ k))
  with int_of_string -> raise (Util.Fatal
    ("Error scanning profile "^filename^":\n"
     ^ "Value of 'key' preference must be a single digit (0-9), "
     ^ "not " ^ k))

(* ------ *)

let profilesAndRoots = ref []

let scanProfiles () =
  Array.iteri (fun i _ -> profileKeymap.(i) <- None) profileKeymap;
  profilesAndRoots :=
    (Safelist.map
       (fun f ->
          let f = Filename.chop_suffix f ".prf" in
          let filename = Prefs.profilePathname f in
          let fileContents = Safelist.map (fun (_, _, n, v) -> (n, v)) (Prefs.readAFile f) in
          let roots =
            Safelist.map snd
              (Safelist.filter (fun (n, _) -> n = "root") fileContents) in
          let label =
            try Some(Safelist.assoc "label" fileContents)
            with Not_found -> None in
          let info = {roots=roots; label=label} in
          (* If this profile has a 'key' binding, put it in the keymap *)
          (try
             let k = Safelist.assoc "key" fileContents in
             provideProfileKey filename k f info
           with Not_found -> ());
          (f, info))
       (Safelist.filter (fun name -> not (   Util.startswith name ".#"
                                          || Util.startswith name Os.tempFilePrefix))
          (Files.ls (Fspath.toString Os.unisonDir)
             "*.prf")))

let getProfile () =
  (* The selected profile *)
  let result = ref None in

  (* Build the dialog *)
  let t = GWindow.dialog ~title:"Profiles" ~wm_name:"Profiles" ~width:400 () in

  let okCommand() =
    currentWindow := None;
    t#destroy () in
  let okButton = GButton.button ~label:"OK" ~packing:t#action_area#add () in
  ignore (okButton#connect#clicked ~callback:okCommand);
  okButton#misc#set_sensitive false;
  okButton#grab_default ();
  let cancelCommand() = t#destroy (); exit 0 in
  let cancelButton = GButton.button ~label:"Cancel"
      ~packing:t#action_area#add () in
  ignore (cancelButton#connect#clicked ~callback:cancelCommand);
  cancelButton#misc#set_can_default true;

  let vb = t#vbox in

  ignore (GMisc.label
            ~text:"Select an existing profile or create a new one"
            ~xpad:2 ~ypad:5 ~packing:(vb#pack ~expand:false) ());

  let sw =
    GBin.scrolled_window ~packing:(vb#add) ~height:200
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
  let lst = GList.clist_poly ~selection_mode:`BROWSE ~packing:(sw#add) () in
  let selRow = ref 0 in
  let fillLst default =
    scanProfiles();
    lst#freeze ();
    lst#clear ();
    let i = ref 0 in (* FIX: Work around a lablgtk bug *)
    Safelist.iter
      (fun (profile, info) ->
         let labeltext =
           match info.label with None -> "" | Some(l) -> " ("^l^")" in
         let s = profile ^ labeltext in
         ignore (lst#append [s]);
         if profile = default then selRow := !i;
         lst#set_row_data !i (profile, info);
         incr i)
      (Safelist.sort (fun (p, _) (p', _) -> compare p p') !profilesAndRoots);
    let r = lst#rows in
    let p = if r < 2 then 0. else float !selRow /. float (r - 1) in
    lst#scroll_vertical `JUMP p;
    lst#thaw () in
  let tbl =
    GPack.table ~rows:2 ~columns:2 ~packing:(vb#pack ~expand:true) () in
  tbl#misc#set_sensitive false;
  ignore (GMisc.label ~text:"Root 1:" ~xpad:2
            ~packing:(tbl#attach ~left:0 ~top:0 ~expand:`NONE) ());
  ignore (GMisc.label ~text:"Root 2:" ~xpad:2
            ~packing:(tbl#attach ~left:0 ~top:1 ~expand:`NONE) ());
  let root1 =
    GEdit.entry ~packing:(tbl#attach ~left:1 ~top:0 ~expand:`X)
      ~editable:false () in
  let root2 =
    GEdit.entry ~packing:(tbl#attach ~left:1 ~top:1 ~expand:`X)
      ~editable:false () in
  root1#misc#set_can_focus false;
  root2#misc#set_can_focus false;
  let hb =
    GPack.hbox ~border_width:2 ~spacing:2 ~packing:(vb#pack ~expand:false) ()
  in
  let nw =
    GButton.button ~label:"Create new profile"
      ~packing:(hb#pack ~expand:false) () in
  ignore (nw#connect#clicked ~callback:(fun () ->
    let t =
      GWindow.dialog ~title:"New profile" ~wm_name:"New profile" ~modal:true ()
    in
    let vb = GPack.vbox ~border_width:4 ~packing:t#vbox#add () in
    let f = GPack.vbox ~packing:(vb#pack ~expand:true ~padding:4) () in
    let f0 = GPack.hbox ~spacing:4 ~packing:f#add () in
    ignore (GMisc.label ~text:"Profile name:"
              ~packing:(f0#pack ~expand:false) ());
    let prof = GEdit.entry ~packing:f0#add () in
    prof#misc#grab_focus ();

    let exit () = t#destroy (); GMain.Main.quit () in
    ignore (t#event#connect#delete ~callback:(fun _ -> exit (); true));

    let f3 = t#action_area in
    let okCommand () =
      let profile = prof#text in
      if profile <> "" then
        let filename = Prefs.profilePathname profile in
        if Sys.file_exists filename then
          okBox
            ~title:(Uutil.myName ^ " error")
            ~message:("Profile \""
                      ^ profile
                      ^ "\" already exists!\nPlease select another name.")
        else
          (* Make an empty file *)
          let ch =
            open_out_gen
              [Open_wronly; Open_creat; Open_trunc] 0o600 filename in
          close_out ch;
          fillLst profile;
          exit () in
    let okButton = GButton.button ~label:"OK" ~packing:f3#add () in
    ignore (okButton#connect#clicked ~callback:okCommand);
    okButton#grab_default ();
    let cancelButton = GButton.button ~label:"Cancel" ~packing:f3#add () in
    ignore (cancelButton#connect#clicked ~callback:exit);

    t#show ();
    grabFocus t;
    GMain.Main.main ();
    releaseFocus ()));

  ignore (lst#connect#unselect_row ~callback:(fun ~row:_ ~column:_ ~event:_ ->
    root1#set_text ""; root2#set_text "";
    result := None;
    tbl#misc#set_sensitive false;
    okButton#misc#set_sensitive false));

  let select_row i =
    (* Inserting the first row triggers the signal, even before the row
       data is set. So, we need to catch the corresponding exception *)
    (try
      let (profile, info) = lst#get_row_data i in
      result := Some profile;
      begin match info.roots with
        [r1; r2] -> root1#set_text r1; root2#set_text r2;
                    tbl#misc#set_sensitive true
      | _        -> root1#set_text ""; root2#set_text "";
                    tbl#misc#set_sensitive false
      end;
      okButton#misc#set_sensitive true
    with Gpointer.Null -> ()) in

  ignore (lst#connect#select_row
            ~callback:(fun ~row:i ~column:_ ~event:_ -> select_row i));

  ignore (lst#event#connect#button_press ~callback:(fun ev ->
    match GdkEvent.get_type ev with
      `TWO_BUTTON_PRESS ->
        okCommand ();
        true
    | _ ->
        false));
  fillLst "default";
  select_row !selRow;
  lst#misc#grab_focus ();
  currentWindow := Some (t :> GWindow.window);
  ignore (t#connect#destroy ~callback:GMain.Main.quit);
  t#show ();
  GMain.Main.main ();
  !result

(* ------ *)

let documentation sect =
  let title = "Documentation" in
  let t = GWindow.dialog ~title ~wm_name:title () in
  let t_dismiss =
    GButton.button ~label:"Dismiss" ~packing:t#action_area#add () in
  t_dismiss#grab_default ();
  let dismiss () = t#destroy () in
  ignore (t_dismiss#connect#clicked ~callback:dismiss);
  ignore (t#event#connect#delete ~callback:(fun _ -> dismiss (); true));

  let (name, docstr) = List.assoc sect Strings.docs in
  let hb = GPack.hbox ~packing:(t#vbox#pack ~expand:false ~padding:2) () in
  let optionmenu =
    GMenu.option_menu ~packing:(hb#pack ~expand:true ~fill:false) () in

  let charW = Gdk.Font.char_width (Lazy.force fontMonospaceMedium) 'M' in
  let charH = 16 in
  let t_text =
    new scrolled_text ~editable:false
      ~width:(charW * 80) ~height:(charH * 20) ~packing:t#vbox#add ()
  in
  t_text#insert docstr;

  let sect_idx = ref 0 in
  let idx = ref 0 in
  let menu = GMenu.menu () in
  let addDocSection (shortname, (name, docstr)) =
    if shortname <> "" && name <> "" then begin
      if shortname = sect then sect_idx := !idx;
      incr idx;
      let item = GMenu.menu_item ~label:name ~packing:menu#append () in
      ignore
        (item#connect#activate ~callback:(fun () -> t_text#insert docstr))
    end
  in
  Safelist.iter addDocSection Strings.docs;
  optionmenu#set_menu menu;
  optionmenu#set_history !sect_idx;

  t#show ()

(* ------ *)

let messageBox ~title ?(label = "Dismiss") ?(action = fun t -> t#destroy)
    ?(modal = false) message =
  let t = GWindow.dialog ~title ~wm_name:title ~modal ~position:`CENTER () in
  let t_dismiss = GButton.button ~label ~packing:t#action_area#add () in
  t_dismiss#grab_default ();
  ignore (t_dismiss#connect#clicked ~callback:(action t));
  let charW = Gdk.Font.char_width (Lazy.force fontMonospaceMedium) 'M' in
  let charH = 16 in
  let t_text =
    new scrolled_text ~editable:false
      ~width:(charW * 80) ~height:(charH * 20) ~packing:t#vbox#add ()
  in
  t_text#insert (transcode message);
  ignore (t#event#connect#delete ~callback:(fun _ -> action t (); true));
  t#show ();
  if modal then begin
    grabFocus t;
    GMain.Main.main ();
    releaseFocus ()
  end


(**********************************************************************
                             TOP-LEVEL WINDOW
 **********************************************************************)

let myWindow = ref None

let getMyWindow () =
  if not (Prefs.read Uicommon.reuseToplevelWindows) then begin
    (match !myWindow with Some(w) -> w#destroy() | None -> ());
    myWindow := None;
  end;
  let w = match !myWindow with
            Some(w) ->
              Safelist.iter w#remove w#children;
              w
          | None ->
              (* Used to be ~position:`CENTER -- maybe that was better... *)
              GWindow.window ~kind:`TOPLEVEL ~position:`CENTER
                ~wm_name:Uutil.myName () in
  myWindow := Some(w);
  w#set_border_width 4;
  w

(* ------ *)

let displayWaitMessage () =
  if not (Prefs.read Uicommon.contactquietly) then begin
    let w = getMyWindow() in
    ignore (GMisc.label ~text: (Uicommon.contactingServerMsg()) ~packing:(w#add) ());
    w#set_border_width 20;
    w#show();
    ignore (w#event#connect#delete ~callback:(fun _ -> exit 0))
  end

(* ------ *)

let rec createToplevelWindow () =
  let toplevelWindow = getMyWindow() in
  let toplevelVBox = GPack.vbox ~packing:toplevelWindow#add () in

  (*******************************************************************
   Statistic window
   *******************************************************************)

  (* FIX: currently statistics window unavailable in the Cygwin version;
     enabling it causes core dump. *)
  let stat_win = 
    (if Util.isCygwin then
      GWindow.dialog () 
     else
      statistics ())
  in

  (*******************************************************************
   Groups of things that are sensitive to interaction at the same time
   *******************************************************************)
  let grAction = ref [] in
  let grDiff = ref [] in
  let grGo = ref [] in
  let grRestart = ref [] in
  let grAdd gr w = gr := w#misc::!gr in
  let grSet gr st = List.iter (fun x -> x#set_sensitive st) !gr in

  (*********************************************************************
    Create the menu bar
   *********************************************************************)
  let topHBox = GPack.hbox ~packing:(toplevelVBox#pack ~expand:false) () in

  let menuBar =
    GMenu.menu_bar ~border_width:0
      ~packing:(topHBox#pack ~expand:true) () in
  let menus = new GMenu.factory ~accel_modi:[] menuBar in
  let accel_group = menus#accel_group in
  toplevelWindow#add_accel_group accel_group;
  let add_submenu ?(modi=[]) ~label () =
    new GMenu.factory ~accel_group ~accel_modi:modi (menus#add_submenu label)
  in

  let profileLabel =
    GMisc.label ~text:"" ~packing:(topHBox#pack ~expand:false ~padding:2) () in

  let displayNewProfileLabel p =
    let label = Prefs.read Uicommon.profileLabel in
    let s =
      if p="" then ""
      else if p="default" then label
      else if label="" then p
      else p ^ " (" ^ label ^ ")" in
    let s = if s="" then "" else "Profile: " ^ s in
    profileLabel#set_text s
  in

  begin match !Prefs.profileName with
    None -> ()
  | Some(p) -> displayNewProfileLabel p
  end;

  (*********************************************************************
    Create the menus
   *********************************************************************)
  let fileMenu = add_submenu ~label:"Synchronization" ()
  and actionsMenu = add_submenu ~label:"Actions" ()
  and ignoreMenu = add_submenu ~modi:[`SHIFT] ~label:"Ignore" ()
  and sortMenu = add_submenu ~label:"Sort" ()
  and helpMenu = add_submenu ~label:"Help" () in

  (*********************************************************************
    Create the main window
   *********************************************************************)
  let mainWindow =
    let sw =
      GBin.scrolled_window ~packing:(toplevelVBox#add)
        ~height:(Prefs.read Uicommon.mainWindowHeight * 12)
        ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
    GList.clist ~columns:5 ~titles_show:true
      ~selection_mode:`BROWSE ~packing:sw#add () in
  mainWindow#misc#grab_focus ();
  let setMainWindowColumnHeaders () =
    (* FIX: roots2string should return a pair *)
    let s = Uicommon.roots2string () in
    Array.iteri
      (fun i data ->
         mainWindow#set_column
           ~title_active:false ~auto_resize:true ~title:data i)
      [| " " ^ String.sub s  0 12 ^ " "; "  Action  ";
         " " ^ String.sub s 15 12 ^ " "; "  Status  "; " Path" |];
    let status_width =
      let font = mainWindow#misc#style#font in
      4 + max (Gdk.Font.string_width font "working")
              (Gdk.Font.string_width font "skipped") in
    mainWindow#set_column ~justification:`CENTER 1;
    mainWindow#set_column
      ~justification:`CENTER ~auto_resize:false ~width:status_width 3 in
  setMainWindowColumnHeaders();

  (*********************************************************************
    Create the details window
   *********************************************************************)

  let charW = Gdk.Font.char_width (Lazy.force fontMonospaceMedium) 'M' in
  let charH = if Util.osType = `Win32 then 20 else 16 in

  let detailsWindow =
    let sw =
      GBin.scrolled_window ~packing:(toplevelVBox#pack ~expand:false)
        ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
    GEdit.text ~editable:false ~height:(3 * charH) ~width: (128 * charW)
      ~line_wrap:false ~packing:sw#add () in
  detailsWindow#misc#set_can_focus false;
  let style = detailsWindow#misc#style#copy in
  style#set_font (Lazy.force fontMonospaceMedium);
  detailsWindow#misc#set_style style;

  let updateButtons () =
    match !current with
      None ->
        grSet grAction false;
        grSet grDiff false
    | Some row ->
        let (activate1, activate2) =
          match !theState.(row).whatHappened, !theState.(row).ri.replicas with
          | None,   Different((`FILE, _, _, _),(`FILE, _, _, _), _, _) ->
              (true, true)
          | Some _,   Different((`FILE, _, _, _),(`FILE, _, _, _), _, _) ->
              (false, true)
          | Some _, _ ->
              (false, false)
          | None,   _ ->
              (true, false) in
        grSet grAction activate1;
        grSet grDiff activate2 in

  let makeRowVisible row =
    if mainWindow#row_is_visible row <> `FULL then begin
      let adj = mainWindow#vadjustment in
      let current = adj#value
      and upper = adj#upper and lower = adj#lower in
      let v =
        float row /. float (mainWindow#rows + 1) *. (upper-.lower) +. lower
      in
      adj#set_value (min v (upper -. adj#page_size))
    end in

  let makeFirstUnfinishedVisible pRiInFocus =
    let im = Array.length !theState in
    let rec find i =
      if i >= im then () else
      match pRiInFocus (!theState.(i).ri), !theState.(i).whatHappened with
        true, None -> makeRowVisible i
      | _ -> find (i+1) in
    find 0
  in

  let updateDetails () =
    detailsWindow#freeze ();
    detailsWindow#delete_text ~start:0 ~stop:detailsWindow#length;
    begin match !current with
      None ->
        ()
    | Some row ->
        makeRowVisible row;
        let details =
          match !theState.(row).whatHappened with
            None -> Uicommon.details2string !theState.(row).ri "  "
          | Some(Util.Succeeded) -> Uicommon.details2string !theState.(row).ri "  "
          | Some(Util.Failed(s)) -> s in
        detailsWindow#insert
          (transcode (Path.toString !theState.(row).ri.path));
        detailsWindow#insert "\n";
        detailsWindow#insert details
    end;
    (* Display text *)
    detailsWindow#thaw ();
    updateButtons () in

  (*********************************************************************
    Status window
   *********************************************************************)

  let statusHBox = GPack.hbox ~packing:(toplevelVBox#pack ~expand:false) () in
  let statusWindow =
    GMisc.statusbar ~packing:(statusHBox#pack ~expand:true) () in
  let statusContext = statusWindow#new_context ~name:"status" in
  ignore (statusContext#push "");

  let displayStatus m =
    statusContext#pop ();
    ignore (statusContext#push m);
    (* Force message to be displayed immediately *)
    gtk_sync ()
  in

  let formatStatus major minor = (Util.padto 30 (major ^ "  ")) ^ minor in

  (* Tell the Trace module about the status printer *)
  Trace.messageDisplayer := displayStatus;
  Trace.statusFormatter := formatStatus;
  Trace.sendLogMsgsToStderr := false;

  (*********************************************************************
    Functions used to print in the main window
   *********************************************************************)

  let select i =
    let r = mainWindow#rows in
    let p = if r < 2 then 0. else (float i +. 0.5) /. float (r - 1) in
    mainWindow#scroll_vertical `JUMP (min p 1.) in

  ignore (mainWindow#connect#unselect_row ~callback:
      (fun ~row ~column ~event -> current := None; updateDetails ()));
  ignore (mainWindow#connect#select_row ~callback:
      (fun ~row ~column ~event -> current := Some row; updateDetails ()));

  let nextInteresting () =
    let l = Array.length !theState in
    let start = match !current with Some i -> i + 1 | None -> 0 in
    let rec loop i =
      if i < l then
        match !theState.(i).ri.replicas with
          Different (_, _, dir, _)
              when not (Prefs.read Uicommon.auto) || !dir = Conflict ->
            select i
        | _ ->
            loop (i + 1) in
    loop start in
  let selectSomethingIfPossible () =
    if !current=None then nextInteresting () in

  let columnsOf i =
    let oldPath = if i = 0 then Path.empty else !theState.(i-1).ri.path in
    let status =
      match !theState.(i).whatHappened with
        None -> "      "
      | Some conf ->
          match !theState.(i).ri.replicas with
            Different(_,_,{contents=Conflict},_) | Problem _ ->
              "      "
          | _ ->
              match conf with
                Util.Succeeded -> "done  "
              | Util.Failed _  -> "failed" in
    let s = Uicommon.reconItem2string oldPath !theState.(i).ri status in
    (* FIX: This is ugly *)
    (String.sub s  0 8,
     String.sub s  9 5,
     String.sub s 15 8,
     String.sub s 25 6,
     String.sub s 32 (String.length s - 32)) in

  let greenPixel  = "00dd00" in
  let redPixel    = "ff2040" in
  let yellowPixel = "999900" in
  let lightbluePixel = "8888FF" in
  let blackPixel  = "000000" in
  let buildPixmap p =
    GDraw.pixmap_from_xpm_d ~window:toplevelWindow ~data:p () in
  let buildPixmaps f c1 =
    (buildPixmap (f c1), buildPixmap (f lightbluePixel)) in

  let rightArrow = buildPixmaps Pixmaps.copyAB greenPixel in
  let leftArrow = buildPixmaps Pixmaps.copyBA greenPixel in
  let ignoreAct = buildPixmaps Pixmaps.ignore redPixel in
  let doneIcon = buildPixmap Pixmaps.success in
  let failedIcon = buildPixmap Pixmaps.failure in
  let rightArrowBlack = buildPixmap (Pixmaps.copyAB blackPixel) in
  let leftArrowBlack = buildPixmap (Pixmaps.copyBA blackPixel) in
  let mergeLogo = buildPixmaps Pixmaps.mergeLogo greenPixel in
  let mergeLogoBlack = buildPixmap (Pixmaps.mergeLogo blackPixel) in

  let displayArrow i j action =
    let changedFromDefault = match !theState.(j).ri.replicas with
        Different(_,_,{contents=curr},default) -> curr<>default
      | _ -> false in
    let sel pixmaps =
      if changedFromDefault then snd pixmaps else fst pixmaps in
    match action with
	"<-?->" -> mainWindow#set_cell ~pixmap:(sel ignoreAct) i 1
      | "<-M->" -> mainWindow#set_cell ~pixmap:(sel mergeLogo) i 1
      | "---->" -> mainWindow#set_cell ~pixmap:(sel rightArrow) i 1
      | "<----" -> mainWindow#set_cell ~pixmap:(sel leftArrow) i 1
      | "error" -> mainWindow#set_cell ~pixmap:failedIcon i 1
      |    _    -> assert false in

  let displayStatusIcon i status =
    match status with
    | "failed" -> mainWindow#set_cell ~pixmap:failedIcon i 3
    | "done  " -> mainWindow#set_cell ~pixmap:doneIcon i 3
    | _        -> mainWindow#set_cell ~text:status i 3 in

  let displayMain() =
    (* The call to mainWindow#clear below side-effect current,
       so we save the current value before we clear out the main window and
       rebuild it. *)
    let savedCurrent = !current in
    mainWindow#freeze ();
    mainWindow#clear ();
    for i = Array.length !theState - 1 downto 0 do
      let (r1, action, r2, status, path) = columnsOf i in
      ignore (mainWindow#prepend [ r1; ""; r2; status; transcode path ]);
      displayArrow 0 i action
    done;
    debug (fun()-> Util.msg "reset current to %s\n"
             (match savedCurrent with None->"None" | Some(i) -> string_of_int i));
    if savedCurrent <> None then current := savedCurrent;
    selectSomethingIfPossible ();
    begin match !current with Some idx -> select idx | None -> () end;
    mainWindow#thaw ();
    updateDetails ();
 in

  let redisplay i =
    let (r1, action, r2, status, path) = columnsOf i in
    mainWindow#freeze ();
    mainWindow#set_cell ~text:r1     i 0;
    displayArrow i i action;
    mainWindow#set_cell ~text:r2     i 2;
    displayStatusIcon i status;
    mainWindow#set_cell ~text:(transcode path)   i 4;
    if status = "failed" then begin
      mainWindow#set_cell
        ~text:(path ^ "       [failed: click on this line for details]") i 4
    end;
    mainWindow#thaw ();
    if !current = Some i then updateDetails ();
    updateButtons () in

  let globalProgressBar =
    GMisc.statusbar ~packing:(statusHBox#pack ~expand:false) () in
  let globalProgressContext = globalProgressBar#new_context ~name:"prog" in
  ignore (globalProgressContext#push "");

  let totalBytesToTransfer = ref Uutil.Filesize.zero in
  let totalBytesTransferred = ref Uutil.Filesize.zero in

  let displayGlobalProgress s =
    globalProgressContext#pop ();
    ignore (globalProgressContext#push s);
    (* Force message to be displayed immediately *)
    gtk_sync () in

  let showGlobalProgress b =
    (* Concatenate the new message *)
    totalBytesTransferred := Uutil.Filesize.add !totalBytesTransferred b;
    let s =
      Util.percent2string
        (Uutil.Filesize.percentageOfTotalSize
           !totalBytesTransferred !totalBytesToTransfer)
    in
    displayGlobalProgress (s^" ")
  in

  let initGlobalProgress b =
    totalBytesToTransfer := b;
    totalBytesTransferred := Uutil.Filesize.zero;
    showGlobalProgress Uutil.Filesize.zero
  in

  let (root1,root2) = Globals.roots () in
  let root1IsLocal = fst root1 = Local in
  let root2IsLocal = fst root2 = Local in

  let showProgress i bytes dbg =
(* XXX There should be a way to reset the amount of bytes transferred... *)
    let i = Uutil.File.toLine i in
    let item = !theState.(i) in
    item.bytesTransferred <- Uutil.Filesize.add item.bytesTransferred bytes;
    let b = item.bytesTransferred in
    let len = Common.riLength item.ri in
    let newstatus =
      if b = Uutil.Filesize.zero || len = Uutil.Filesize.zero then "start "
      else if len = Uutil.Filesize.zero then
        Printf.sprintf "%5s " (Uutil.Filesize.toString b)
      else Util.percent2string (Uutil.Filesize.percentageOfTotalSize b len) in
    let dbg = if Trace.enabled "progress" then dbg ^ "/" else "" in
    let newstatus = dbg ^ newstatus in
    mainWindow#set_cell ~text:newstatus i 3;
    showGlobalProgress bytes;
    gtk_sync ();
    begin match item.ri.replicas with
      Different (_, _, dir, _) ->
        begin match !dir with
          Replica1ToReplica2 ->
            if root2IsLocal then
              clientWritten := !clientWritten +. Uutil.Filesize.toFloat bytes
            else
              serverWritten := !serverWritten +. Uutil.Filesize.toFloat bytes
        | Replica2ToReplica1 ->
            if root1IsLocal then
              clientWritten := !clientWritten +. Uutil.Filesize.toFloat bytes
            else
              serverWritten := !serverWritten +. Uutil.Filesize.toFloat bytes
        | Conflict | Merge ->
            (* Diff / merge *)
            clientWritten := !clientWritten +. Uutil.Filesize.toFloat bytes
        end
    | _ ->
        assert false
    end
  in

  (* Install showProgress so that we get called back by low-level
     file transfer stuff *)
  Uutil.setProgressPrinter showProgress;

  (* Apply new ignore patterns to the current state, expecting that the
     number of reconitems will grow smaller. Adjust the display, being
     careful to keep the cursor as near as possible to its position
     before the new ignore patterns take effect. *)
  let ignoreAndRedisplay () =
    let lst = Array.to_list !theState in
    (* FIX: we should actually test whether any prefix is now ignored *)
    let keep sI = not (Globals.shouldIgnore sI.ri.path) in
    begin match !current with
      None ->
        theState := Array.of_list (Safelist.filter keep lst)
    | Some index ->
        let i = ref index in
        let l = ref [] in
        Array.iteri
          (fun j sI -> if keep sI then l := sI::!l
                       else if j < !i then decr i)
          !theState;
        theState := Array.of_list (Safelist.rev !l);
        current := if !l = [] then None
                   else Some (min (!i) ((Array.length !theState) - 1));
    end;
    displayMain() in

  let sortAndRedisplay () =
    current := None;
    let compareRIs = Sortri.compareReconItems() in
    Array.stable_sort (fun si1 si2 -> compareRIs si1.ri si2.ri) !theState;
    displayMain() in

  (******************************************************************
   Main detect-updates-and-reconcile logic
   ******************************************************************)

  let detectUpdatesAndReconcile () =
    grSet grAction false;
    grSet grDiff false;
    grSet grGo false;
    grSet grRestart false;

    let (r1,r2) = Globals.roots () in
    let t = Trace.startTimer "Checking for updates" in
    let findUpdates () =
      Trace.status "Looking for changes";
      let updates = Update.findUpdates () in
      Trace.showTimer t;
      updates in
    let reconcile updates =
      let t = Trace.startTimer "Reconciling" in
      Recon.reconcileAll updates in
    let (reconItemList, thereAreEqualUpdates, dangerousPaths) =
      reconcile (findUpdates ()) in
    Trace.showTimer t;
    if reconItemList = [] then
      if thereAreEqualUpdates then
        Trace.status "Replicas have been changed only in identical ways since last sync"
      else
        Trace.status "Everything is up to date"
    else
      Trace.status "Check and/or adjust selected actions; then press Go";
    theState :=
      Array.of_list
         (Safelist.map
            (fun ri -> { ri = ri; bytesTransferred = Uutil.Filesize.zero;
                         whatHappened = None })
            reconItemList);
    current := None;
    displayMain();
    grSet grGo (Array.length !theState > 0);
    grSet grRestart true;
    if dangerousPaths <> [] then begin
      Prefs.set Globals.batch false;
      Util.warn (Uicommon.dangerousPathMsg dangerousPaths)
    end;
in

  (*********************************************************************
    Help menu
   *********************************************************************)
  let addDocSection (shortname, (name, docstr)) =
    if shortname <> "" && name <> "" then
      ignore (helpMenu#add_item
                ~callback:(fun () -> documentation shortname)
                name) in
  Safelist.iter addDocSection Strings.docs;

  (*********************************************************************
    Ignore menu
   *********************************************************************)
  let addRegExpByPath pathfunc =
    match !current with
      Some i ->
        Uicommon.addIgnorePattern (pathfunc !theState.(i).ri.path);
        ignoreAndRedisplay ()
    | None ->
        () in
  grAdd grAction
    (ignoreMenu#add_item ~key:GdkKeysyms._i
       ~callback:(fun () -> getLock (fun () ->
          addRegExpByPath Uicommon.ignorePath))
       "Permanently ignore this path");
  grAdd grAction
    (ignoreMenu#add_item ~key:GdkKeysyms._E
       ~callback:(fun () -> getLock (fun () ->
          addRegExpByPath Uicommon.ignoreExt))
       "Permanently ignore files with this extension");
  grAdd grAction
    (ignoreMenu#add_item ~key:GdkKeysyms._N
       ~callback:(fun () -> getLock (fun () ->
          addRegExpByPath Uicommon.ignoreName))
       "Permanently ignore files with this name (in any dir)");

  (*
  grAdd grRestart
    (ignoreMenu#add_item ~callback:
       (fun () -> getLock ignoreDialog) "Edit ignore patterns");
  *)

  (*********************************************************************
    Sort menu
   *********************************************************************)
  grAdd grAction
    (sortMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Sortri.sortByName();
          sortAndRedisplay()))
       "Sort entries by name");
  grAdd grAction
    (sortMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Sortri.sortBySize();
          sortAndRedisplay()))
       "Sort entries by size");
  grAdd grAction
    (sortMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Sortri.sortNewFirst();
          sortAndRedisplay()))
       "Sort new entries first");
  grAdd grAction
    (sortMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Sortri.restoreDefaultSettings();
          sortAndRedisplay()))
       "Go back to default ordering");

  (*********************************************************************
    Main function : synchronize
   *********************************************************************)
  let synchronize () =
    if Array.length !theState = 0 then
      Trace.status "Nothing to synchronize"
    else begin
      grSet grAction false;
      grSet grDiff false;
      grSet grGo false;
      grSet grRestart false;

      Trace.status "Propagating changes";
      Transport.logStart ();
      let totalLength =
        Array.fold_left
          (fun l si -> Uutil.Filesize.add l (Common.riLength si.ri))
          Uutil.Filesize.zero !theState in
      displayGlobalProgress "     ";
      initGlobalProgress totalLength;
      let t = Trace.startTimer "Propagating changes" in
      let im = Array.length !theState in
      let rec loop i actions pRiThisRound =
        if i < im then begin
          let theSI = !theState.(i) in
          let action =
            match theSI.whatHappened with
              None ->
                if not (pRiThisRound theSI.ri) then
                  return ()
                else
                  catch (fun () ->
                           Transport.transportItem
                             theSI.ri (Uutil.File.ofLine i)
                             (fun title text -> 
			       Trace.status (Printf.sprintf "\n%s\n\n%s\n\n" title text); true)
                           >>= (fun () ->
                           return Util.Succeeded))
                        (fun e ->
                           match e with
                             Util.Transient s ->
                               return (Util.Failed s)
                           | _ ->
                               fail e)
                    >>= (fun res ->
                  theSI.whatHappened <- Some res;
                  redisplay i;
                  makeFirstUnfinishedVisible pRiThisRound;
                  gtk_sync ();
                  return ())
            | Some _ ->
                return () (* Already processed this one (e.g. merged it) *)
          in
          loop (i + 1) (action :: actions) pRiThisRound
        end else
          return actions
      in
      Lwt_unix.run
        (loop 0 [] (fun ri -> not (Common.isDeletion ri)) >>= (fun actions ->
          Lwt_util.join actions));
      Lwt_unix.run
        (loop 0 [] Common.isDeletion >>= (fun actions ->
          Lwt_util.join actions));
      Transport.logFinish ();
      Trace.showTimer t;
      Trace.status "Updating synchronizer state";
      let t = Trace.startTimer "Updating synchronizer state" in
      Update.commitUpdates();
      Trace.showTimer t;

      let failures =
        let count =
          Array.fold_left
            (fun l si ->
               l + (match si.whatHappened with Some(Util.Failed(_)) -> 1 | _ -> 0))
            0 !theState in
        if count = 0 then "" else
          Printf.sprintf "%d failure%s" count (if count=1 then "" else "s") in
      let skipped =
        let count =
          Array.fold_left
            (fun l si ->
               l + (if problematic si.ri then 1 else 0))
            0 !theState in
        if count = 0 then "" else
          Printf.sprintf "%d skipped" count in
      Trace.status
        (Printf.sprintf "Synchronization complete         %s%s%s"
           failures (if failures=""||skipped="" then "" else ", ") skipped);
      displayGlobalProgress "";

      grSet grRestart true
    end in

  (*********************************************************************
    Action bar
   *********************************************************************)
  let actionBar =
    GButton.toolbar
      ~orientation:`HORIZONTAL ~tooltips:true ~space_size:10
      ~packing:(toplevelVBox#pack ~expand:false) () in

  (*********************************************************************
    Quit button
   *********************************************************************)
  actionBar#insert_space ();
  ignore (actionBar#insert_button ~text:"Quit" ~callback:safeExit ());

  (*********************************************************************
    Go button
   *********************************************************************)
  actionBar#insert_space ();
  grAdd grGo
    (actionBar#insert_button ~text:"Go"
       (* tooltip:"Go with displayed actions" *)
       ~callback:(fun () ->
                    getLock synchronize) ());

  (*********************************************************************
    Restart button
   *********************************************************************)
  let detectCmdName = "Restart" in
  let detectCmd () =
    getLock detectUpdatesAndReconcile;
    if Prefs.read Globals.batch then begin
      Prefs.set Globals.batch false; synchronize()
    end
 in
  actionBar#insert_space ();
  grAdd grRestart
    (actionBar#insert_button ~text:detectCmdName ~callback:detectCmd ());

  (*********************************************************************
    Buttons for <--, M, -->, Skip
   *********************************************************************)
  let doAction f =
    match !current with
      Some i ->
        let theSI = !theState.(i) in
        begin match theSI.whatHappened, theSI.ri.replicas with
          None, Different(_, _, dir, _) ->
            f dir;
            redisplay i;
            nextInteresting ()
        | _ ->
            ()
        end
    | None ->
        () in
  let leftAction     _ = doAction (fun dir -> dir := Replica2ToReplica1) in
  let rightAction    _ = doAction (fun dir -> dir := Replica1ToReplica2) in
  let questionAction _ = doAction (fun dir -> dir := Conflict) in
  let mergeAction    _ = doAction (fun dir -> dir := Merge) in

  actionBar#insert_space ();
  grAdd grAction
    (actionBar#insert_button
       ~icon:((GMisc.pixmap leftArrowBlack ())#coerce)
       ~callback:leftAction ());
  actionBar#insert_space ();
  grAdd grAction
    (actionBar#insert_button
       ~icon:((GMisc.pixmap mergeLogoBlack())#coerce)
       ~callback:mergeAction ());
  actionBar#insert_space ();
  grAdd grAction
    (actionBar#insert_button
       ~icon:((GMisc.pixmap rightArrowBlack ())#coerce)
       ~callback:rightAction ());
  actionBar#insert_space ();
  grAdd grAction
    (actionBar#insert_button ~text:"Skip" ~callback:questionAction ());

  (*********************************************************************
    Diff / merge buttons
   *********************************************************************)
  let diffCmd () =
    match !current with
      Some i ->
        getLock (fun () ->
          Uicommon.showDiffs !theState.(i).ri
            (fun title text -> messageBox ~title text)
            Trace.status (Uutil.File.ofLine i))
    | None ->
        () in

  actionBar#insert_space ();
  grAdd grDiff (actionBar#insert_button ~text:"Diff" ~callback:diffCmd ());

(*
  let mergeCmd () =
    match !current with
      Some i ->
        getLock (fun () ->
          toplevelWindow#misc#set_sensitive false;
          begin try
            Uicommon.applyMerge !theState.(i).ri
              (Uutil.File.ofLine i)
              (fun title text ->
                 Trace.status (Printf.sprintf "%s: %s" title text))
              true;
            !theState.(i).whatHappened <- Some(Util.Succeeded);
            toplevelWindow#misc#set_sensitive true
          with
            Util.Transient(s) ->
              toplevelWindow#misc#set_sensitive true;
              oneBox "Merge failed" s "Continue"
          end;
          redisplay i;
          nextInteresting();
          gtk_sync())
    | None ->
        ()
  in

  actionBar#insert_space ();
  grAdd grDiff (actionBar#insert_button ~text:"Merge" ~callback:mergeCmd ());
*)
  (*********************************************************************
    Keyboard commands
   *********************************************************************)
  ignore
    (mainWindow#event#connect#key_press ~callback:
       begin fun ev ->
         let key = GdkEvent.Key.keyval ev in
         if key = GdkKeysyms._Left then begin
           leftAction (); GtkSignal.stop_emit (); true
         end else if key = GdkKeysyms._Right then begin
           rightAction (); GtkSignal.stop_emit (); true
         end else
           false
       end);

  (*********************************************************************
    Action menu
   *********************************************************************)
  let (root1,root2) = Globals.roots () in
  let loc1 = root2hostname root1 in
  let loc2 = root2hostname root2 in
  let descr =
    if loc1 = loc2 then "left to right" else
    Printf.sprintf "from %s to %s" loc1 loc2 in
  let left =
    actionsMenu#add_item ~key:GdkKeysyms._greater ~callback:rightAction
      ("Propagate this path " ^ descr) in
  grAdd grAction left;
  left#add_accelerator ~group:accel_group ~modi:[`SHIFT] GdkKeysyms._greater;

  let merge = actionsMenu#add_item ~key:GdkKeysyms._m ~callback:mergeAction
		"Merge the files" in
  grAdd grAction merge;
  merge#add_accelerator ~group:accel_group GdkKeysyms._m;

  let descl =
    if loc1 = loc2 then "right to left" else
    Printf.sprintf "from %s to %s" loc2 loc1 in
  let right =
    actionsMenu#add_item ~key:GdkKeysyms._less ~callback:leftAction
      ("Propagate this path " ^ descl) in
  grAdd grAction right;
  right#add_accelerator ~group:accel_group ~modi:[`SHIFT] GdkKeysyms._less;

  grAdd grAction
    (actionsMenu#add_item ~key:GdkKeysyms._slash ~callback:questionAction
       "Do not propagate changes to this path");

  (* Override actions *)
  ignore (actionsMenu#add_separator ());
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.setDirection si.ri `Replica1ToReplica2 `Prefer)
            !theState;
          displayMain()))
       "Resolve all conflicts in favor of first root");
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.setDirection si.ri `Replica2ToReplica1 `Prefer)
            !theState;
          displayMain()))
       "Resolve all conflicts in favor of second root");
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.setDirection si.ri `Newer `Prefer)
            !theState;
          displayMain()))
       "Resolve all conflicts in favor of most recently modified");
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.setDirection si.ri `Older `Prefer)
            !theState;
          displayMain()))
       "Resolve all conflicts in favor of least recently modified");
  ignore (actionsMenu#add_separator ());
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.setDirection si.ri `Replica1ToReplica2 `Force)
            !theState;
          displayMain()))
       "Force all changes from first root to second");
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.setDirection si.ri `Replica2ToReplica1 `Force)
            !theState;
          displayMain()))
       "Force all changes from second root to first");
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.setDirection si.ri `Newer `Force)
            !theState;
          displayMain()))
       "Force newer files to replace older ones");
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.setDirection si.ri `Older `Force)
            !theState;
          displayMain()))
       "Force older files to replace newer ones");
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.setDirection si.ri `Merge `Force)
            !theState;
          displayMain()))
       "Revert all paths to the merging default, if avaible");
  ignore (actionsMenu#add_separator ());
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          Array.iter
            (fun si -> Recon.revertToDefaultDirection si.ri)
            !theState;
          displayMain()))
       "Revert all paths to Unison's recommendations");
  grAdd grAction
    (actionsMenu#add_item
       ~callback:(fun () -> getLock (fun () ->
          match !current with
            Some i ->
              let theSI = !theState.(i) in
              Recon.revertToDefaultDirection theSI.ri;
              redisplay i;
              nextInteresting ()
          | None ->
              ()))
       "Revert selected path to Unison's recommendations");

  (* Diff *)
  ignore (actionsMenu#add_separator ());
  grAdd grDiff (actionsMenu#add_item ~key:GdkKeysyms._d ~callback:diffCmd
                  "Show diffs for selected path");
(*  grAdd grDiff (actionsMenu#add_item ~key:GdkKeysyms._m ~callback:mergeCmd
                  "Merge versions of selected path");*)

  (*********************************************************************
    Synchronization menu
   *********************************************************************)

  let loadProfile p =
    debug (fun()-> Util.msg "Loading profile %s..." p);
    Uicommon.initPrefs p displayWaitMessage getFirstRoot getSecondRoot
      (termInteract());
    displayNewProfileLabel p;
    setMainWindowColumnHeaders()
  in

  let reloadProfile () =
    match !Prefs.profileName with
      None -> ()
    | Some(n) -> loadProfile n in

  grAdd grGo
    (fileMenu#add_item ~key:GdkKeysyms._g
       ~callback:(fun () ->
                    getLock synchronize)
       "Go");
  grAdd grRestart
    (fileMenu#add_item ~key:GdkKeysyms._r
       ~callback:(fun () -> reloadProfile(); detectCmd())
       detectCmdName);
  grAdd grRestart
    (fileMenu#add_item ~key:GdkKeysyms._a
       ~callback:(fun () ->
                    reloadProfile();
                    Prefs.set Globals.batch true;
                    detectCmd())
       "Detect updates and proceed (without waiting)");
  grAdd grRestart
    (fileMenu#add_item ~key:GdkKeysyms._f
       ~callback:(
         fun () ->
           let rec loop i acc =
             if i >= Array.length (!theState) then acc else
             let notok =
               (match !theState.(i).whatHappened with
                   None-> true
                 | Some(Util.Failed _) -> true
                 | Some(Util.Succeeded) -> false)
              || match !theState.(i).ri.replicas with
                   Problem _ -> true
                 | Different(rc1,rc2,dir,_) ->
                     (match !dir with
                        Conflict -> true
                      | _ -> false) in
             if notok then loop (i+1) (i::acc)
             else loop (i+1) (acc) in
           let failedindices = loop 0 [] in
           let failedpaths =
             List.map (fun i -> !theState.(i).ri.path) failedindices in
           debug (fun()-> Util.msg "Restarting with paths = %s\n"
                    (String.concat ", " (List.map
                                           (fun p -> "'"^(Path.toString p)^"'")
                                           failedpaths)));
           Prefs.set Globals.paths failedpaths; detectCmd())
       "Recheck unsynchronized items");

  ignore (fileMenu#add_separator ());

  grAdd grRestart
    (fileMenu#add_item ~key:GdkKeysyms._p
       ~callback:(fun _ ->
          match getProfile() with
            None -> ()
          | Some(p) -> loadProfile p; detectCmd())
       "Select a new profile from the profile dialog");

  let fastProf name key =
    grAdd grRestart
      (fileMenu#add_item ~key:key
            ~callback:(fun _ ->
               if Sys.file_exists (Prefs.profilePathname name) then begin
                 Trace.status ("Loading profile " ^ name);
                 loadProfile name; detectCmd()
               end else
                 Trace.status ("Profile " ^ name ^ " not found"))
            ("Select profile " ^ name)) in

  let fastKeysyms =
    [| GdkKeysyms._0; GdkKeysyms._1; GdkKeysyms._2; GdkKeysyms._3;
       GdkKeysyms._4; GdkKeysyms._5; GdkKeysyms._6; GdkKeysyms._7;
       GdkKeysyms._8; GdkKeysyms._9 |] in

  Array.iteri
    (fun i v -> match v with
      None -> ()
    | Some(profile, info) ->
        fastProf profile fastKeysyms.(i))
    profileKeymap;

  if not Util.isCygwin then
    (ignore (fileMenu#add_separator ());
     ignore (fileMenu#add_item
              ~callback:(fun _ -> stat_win#show ()) "Statistics"));

  ignore (fileMenu#add_separator ());
  ignore (fileMenu#add_item ~key:GdkKeysyms._q ~callback:safeExit "Quit");

  (*********************************************************************
    Expert menu
   *********************************************************************)
  if Prefs.read Uicommon.expert then begin
    let expertMenu = add_submenu ~label:"Expert" () in

    let addDebugToggle modname =
      let cm =
        expertMenu#add_check_item ~active:(Trace.enabled modname)
          ~callback:(fun b -> Trace.enable modname b)
          ("Debug '" ^ modname ^ "'") in
      cm#set_show_toggle true in

    addDebugToggle "all";
    addDebugToggle "verbose";
    addDebugToggle "update";

    ignore (expertMenu#add_separator ());
    ignore (expertMenu#add_item
              ~callback:(fun () ->
                           Printf.fprintf stderr "\nGC stats now:\n";
                           Gc.print_stat stderr;
                           Printf.fprintf stderr "\nAfter major collection:\n";
                           Gc.full_major(); Gc.print_stat stderr;
                           flush stderr)
              "Show memory/GC stats")
  end;

  (*********************************************************************
    Finish up
   *********************************************************************)
  grSet grAction false;
  grSet grDiff false;
  grSet grGo false;
  grSet grRestart false;

  ignore (toplevelWindow#event#connect#delete ~callback:
            (fun _ -> safeExit (); true));
  toplevelWindow#show ();
  currentWindow := Some toplevelWindow;
  detectCmd ()


(*********************************************************************
                               STARTUP
 *********************************************************************)

let start _ =
  begin try
    (* Initialize the GTK library *)
    ignore (GMain.Main.init ());

    Util.warnPrinter := Some (warnBox "Warning");

    (* Ask the Remote module to call us back at regular intervals during
       long network operations. *)
    let rec tick () =
      gtk_sync ();
      Lwt_unix.sleep 0.1 >>= tick
    in
    ignore_result (tick ());

    Uicommon.uiInit
      fatalError
      tryAgainOrQuit
      displayWaitMessage
      getProfile
      getFirstRoot
      getSecondRoot
      (termInteract());

    scanProfiles();
    createToplevelWindow();

    (* Display the ui *)
    ignore (GMain.Timeout.add 500 (fun _ -> true));
              (* Hack: this allows signals such as SIGINT to be
                 handled even when Gtk is waiting for events *)
    GMain.Main.main ()
  with
    Util.Transient(s) | Util.Fatal(s) -> fatalError s
  | exn -> fatalError (Uicommon.exn2string exn)
  end

end (* module Private *)


(*********************************************************************
                            UI SELECTION
 *********************************************************************)

module Body : Uicommon.UI = struct

let start = function
    Uicommon.Text -> Uitext.Body.start Uicommon.Text
  | Uicommon.Graphic ->
      let displayAvailable =
        Util.osType = `Win32
          ||
        try Unix.getenv "DISPLAY" <> "" with Not_found -> false
      in
      if displayAvailable then Private.start Uicommon.Graphic
      else Uitext.Body.start Uicommon.Text

let defaultUi = Uicommon.Graphic

end (* module Body *)
