(* Unison file synchronizer: src/osx.ml *)
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


external isMacOSXPred : unit -> bool = "isMacOSX"

let isMacOSX = isMacOSXPred ()

(****)

let rsrcSync =
  Prefs.createString "rsrc" "default"
    "!synchronize resource forks (true/false/default)"
    "When set to {\\tt true}, this flag causes Unison to synchronize \
     resource forks and HFS meta-data.  On filesystems that do not \
     natively support resource forks, this data is stored in \
     Carbon-compatible .\\_ AppleDouble files.  When the flag is set \
     to {\\tt false}, Unison will not synchronize these data.  \
     Ordinarily, the flag is set to {\\tt default}, and these data are
     automatically synchronized if either host is running OSX.  In \
     rare circumstances it is useful to set the flag manually."

(* Defining this variable as a preference ensures that it will be propagated
   to the other host during initialization *)
let rsrc =
  Prefs.createBool "rsrc-aux" false
    "*synchronize resource forks and HFS meta-data" ""

let init b =
  Prefs.set rsrc
    (Prefs.read rsrcSync = "yes" ||
     Prefs.read rsrcSync = "true" ||
     (Prefs.read rsrcSync = "default" && b))

(****)

let appleDoubleFile fspath path =
  let f = Fspath.concatToString fspath path in
  let len = String.length f in
  try
    let i = 1 + String.rindex f '/' in
    let res = String.create (len + 2) in
    String.blit f 0 res 0 i;
    res.[i] <- '.';
    res.[i + 1] <- '_';
    String.blit f i res (i + 2) (len - i);
    res
  with Not_found ->
    assert false

let doubleMagic = "\000\005\022\007"
let doubleVersion = "\000\002\000\000"
let doubleFiller = String.make 16 '\000'
let finfoLength = 32L
let emptyFinderInfo () = String.make 32 '\000'

let getInt2 buf ofs = (Char.code buf.[ofs]) * 256 + Char.code buf.[ofs + 1]

let getInt4 buf ofs =
  let get i = Int64.of_int (Char.code buf.[ofs + i]) in
  let combine x y = Int64.logor (Int64.shift_left x 8) y in
  combine (combine (combine (get 0) (get 1)) (get 2)) (get 3)

let getID buf ofs =
  let get i = Char.code buf.[ofs + i] in
  if get ofs <> 0 || get (ofs + 1) <> 0 || get (ofs + 2) <> 0 then
    `UNKNOWN
  else
    match get (ofs + 3) with
      2 -> `RSRC
    | 9 -> `FINFO
    | _ -> `UNKNOWN

let setInt4 v =
  let s = String.create 4 in
  let set i =
    s.[i] <-
      Char.chr (Int64.to_int (Int64.logand 255L
                               (Int64.shift_right v (24 - 8 * i)))) in
  set 0; set 1; set 2; set 3;
  s

let fail path msg =
  raise (Util.Transient
           (Format.sprintf "Malformed AppleDouble file '%s' (%s)" path msg))

let readDouble path inch len =
  let buf = String.create len in
  begin try
    really_input inch buf 0 len
  with End_of_file ->
    fail path "truncated"
  end;
  buf

let readDoubleFromOffset path inch offset len =
  LargeFile.seek_in inch offset;
  readDouble path inch len

let writeDoubleFromOffset path outch offset str =
  LargeFile.seek_out outch offset;
  output_string outch str

let protect f g =
  try
    f ()
  with Sys_error _ | Unix.Unix_error _ | Util.Transient _ as e ->
    begin try g () with Sys_error _  | Unix.Unix_error _ -> () end;
    raise e

let openDouble fspath path =
  let path = appleDoubleFile fspath path in
  let inch = try open_in_bin path with Sys_error _ -> raise Not_found in
  protect (fun () ->
    Util.convertUnixErrorsToTransient "opening AppleDouble file" (fun () ->
      let header = readDouble path inch 26 in
      if String.sub header 0 4 <> doubleMagic then
        fail path "bad magic number";
      if String.sub header 4 4 <> doubleVersion then
        fail path "bad version";
      if String.sub header 8 16 <> doubleFiller then
        fail path "bad filler";
      let numEntries = getInt2 header 24 in
      let entries = ref [] in
      for i = 1 to numEntries do
        let entry = readDouble path inch 12 in
        let id = getID entry 0 in
        let ofs = getInt4 entry 4 in
        let len = getInt4 entry 8 in
        entries := (id, (ofs, len)) :: !entries
      done;
      (path, inch, !entries)))
    (fun () -> close_in_noerr inch)

(****)

type 'a ressInfo =
    NoRess
  | HfsRess of Uutil.Filesize.t
  | AppleDoubleRess of int * float * float * Uutil.Filesize.t * 'a

type ressStamp = unit ressInfo

let ressStampToString r =
  match r with
    NoRess         ->
      "NoRess"
  | HfsRess len ->
      Format.sprintf "Hfs(%s)" (Uutil.Filesize.toString len)
  | AppleDoubleRess (ino, mtime, ctime, len, _) ->
      Format.sprintf "Hfs(%d,%f,%f,%s)"
        ino mtime ctime (Uutil.Filesize.toString len)

type info =
  { ressInfo : (string * int64) ressInfo;
    finfo : string }

external getFileInfosInternal :
      string -> bool -> string * int64 = "getFileInfos"
external setFileInfosInternal : string -> string -> unit = "setFileInfos"

let defaultInfos typ =
  match typ with
    `FILE      -> { ressInfo = NoRess; finfo = "F" }
  | `DIRECTORY -> { ressInfo = NoRess; finfo = "D" }
  |  _         -> { ressInfo = NoRess; finfo = "" }

let noTypeCreator = String.make 10 '\000'

(* Remove trailing zeroes *)
let trim s =
  let rec trim_rec s pos =
    if s.[pos - 1] = '\000' then
      trim_rec s (pos - 1)
    else
      String.sub s 0 pos
  in
  trim_rec s (String.length s)

let extractInfo typ info =
  let flags = String.sub info 8 2 in
  let xflags = String.sub info 24 2 in
  let typeCreator = String.sub info 0 8 in
  (* Ignore hasBeenInited flag *)
  flags.[0] <- Char.chr (Char.code flags.[0] land 0xfe);
  (* If the extended flags should be ignored, clear them *)
  let xflags =
    if Char.code xflags.[0] land 0x80 <> 0 then "\000\000" else xflags
  in
  let info =
    match typ with
      `FILE       -> "F" ^ typeCreator ^ flags ^ xflags
    | `DIRECTORY  -> "D" ^ flags ^ xflags
  in
  trim info

let getFileInfos fspath path typ =
  if not (Prefs.read rsrc) then defaultInfos typ else
  match typ with
    (`FILE | `DIRECTORY) as typ ->
      Util.convertUnixErrorsToTransient "getting file informations" (fun () ->
        try
          let (fInfo, rsrcLength) =
            getFileInfosInternal
              (Fspath.concatToString fspath path) (typ = `FILE) in
          { ressInfo =
              if rsrcLength = 0L then NoRess
              else HfsRess (Uutil.Filesize.ofInt64 rsrcLength);
            finfo = extractInfo typ fInfo }
        with Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.ENOSYS), _, _) ->
          (* Not a HFS volume.  Look for an AppleDouble file *)
          try
            let (fspath, path) = Fspath.findWorkingDir fspath path in
            let (doublePath, inch, entries) = openDouble fspath path in
            let (rsrcOffset, rsrcLength) =
              try Safelist.assoc `RSRC entries with Not_found ->
                (0L, 0L)
            in
            let finfo =
              protect (fun () ->
                try
                  let (ofs, len) = Safelist.assoc `FINFO entries in
                  if len <> finfoLength then fail doublePath "bad finder info";
                  let res = readDoubleFromOffset doublePath inch ofs 32 in
                  close_in inch;
                  res
                with Not_found ->
                  "")
                (fun () -> close_in_noerr inch)
            in
            let stats = Unix.LargeFile.stat doublePath in
            { ressInfo =
                if rsrcLength = 0L then NoRess else
                AppleDoubleRess
                  (begin match Util.osType with
                     `Win32 -> 0
                   | `Unix  -> (* The inode number is truncated so that
                                  it fits in a 31 bit ocaml integer *)
                               stats.Unix.LargeFile.st_ino land 0x3FFFFFFF
                   end,
                   stats.Unix.LargeFile.st_mtime,
                   begin match Util.osType with
                     `Win32 -> (* Was "stats.Unix.LargeFile.st_ctime", but 
                                  this was bogus: Windows ctimes are
                                  not reliable.  [BCP, Apr 07] *)
                       0.
                   | `Unix  -> 0.
                   end,
                   Uutil.Filesize.ofInt64 rsrcLength,
                   (doublePath, rsrcOffset));
              finfo = extractInfo typ finfo }
          with Not_found ->
            defaultInfos typ)
  | _ ->
      defaultInfos typ

let zeroes = String.make 13 '\000'

let insertInfo fullInfo info =
  let info = info ^ zeroes in
  let isFile = info.[0] = 'F' in
  let offset = if isFile then 9 else 1 in
  (* Type and creator *)
  if isFile then String.blit info 1 fullInfo 0 8;
  (* Finder flags *)
  String.blit info offset fullInfo 8 2;
  (* Extended finder flags *)
  String.blit info (offset + 2) fullInfo 24 2;
  fullInfo

let setFileInfos fspath path finfo =
  assert (finfo <> "");
  Util.convertUnixErrorsToTransient "setting file informations" (fun () ->
    try
      let (fullFinfo, _) =
        getFileInfosInternal (Fspath.concatToString fspath path) false in
      setFileInfosInternal (Fspath.concatToString fspath path)
        (insertInfo fullFinfo finfo)
    with Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.ENOSYS), _, _) ->
      (* Not an HFS volume.  Look for an AppleDouble file *)
      let (fspath, path) = Fspath.findWorkingDir fspath path in
      begin try
        let (doublePath, inch, entries) = openDouble fspath path in
        begin try
          let (ofs, len) = Safelist.assoc `FINFO entries in
          if len <> finfoLength then fail doublePath "bad finder info";
          let fullFinfo =
            protect
              (fun () ->
                let res = readDoubleFromOffset doublePath inch ofs 32 in
                close_in inch;
                res)
              (fun () -> close_in_noerr inch)
          in
          let outch =
            open_out_gen [Open_wronly; Open_binary] 0o600 doublePath in
          protect
            (fun () ->
               writeDoubleFromOffset doublePath outch ofs
                 (insertInfo fullFinfo finfo);
               close_out outch)
            (fun () ->
               close_out_noerr outch);
        with Not_found ->
          close_in_noerr inch;
          raise (Util.Transient
                   (Format.sprintf
                      "Unable to set the file type and creator: \n\
                       The AppleDouble file '%s' has no fileinfo entry."
                      doublePath))
        end
      with Not_found ->
        (* No AppleDouble file, create one if needed. *)
        if finfo <> "F" && finfo <> "D" then begin
          let path = appleDoubleFile fspath path in
          let outch =
            open_out_gen
              [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 path
          in
          protect (fun () ->
            output_string outch doubleMagic;
            output_string outch doubleVersion;
            output_string outch doubleFiller;
            output_string outch "\000\001"; (* One entry *)
            output_string outch "\000\000\000\009"; (* Finder info *)
            output_string outch "\000\000\000\038"; (* offset *)
            output_string outch "\000\000\000\032"; (* length *)
            output_string outch (insertInfo (emptyFinderInfo ()) finfo);
            close_out outch)
            (fun () -> close_out_noerr outch)
        end
      end)

let ressUnchanged info info' t0 dataUnchanged =
  match info, info' with
     NoRess, NoRess ->
       true
   | HfsRess len, HfsRess len' ->
       dataUnchanged && len = len'
   | AppleDoubleRess (ino, mt, ct, _, _),
     AppleDoubleRess (ino', mt', ct', _, _) ->
       ino = ino' && mt = mt' && ct = ct' &&
       if Some mt' <> t0 then
         true
       else begin
         begin try
           Unix.sleep 1
         with Unix.Unix_error _ -> () end;
         false
       end
   |  _ ->
       false

(****)

let name1 = Name.fromString "..namedfork"
let name2 = Name.fromString "rsrc"
let ressPath p = Path.child (Path.child p name1) name2

let stamp info =
  match info.ressInfo with
    NoRess ->
      NoRess
  | (HfsRess len) as s ->
      s
  | AppleDoubleRess (inode, mtime, ctime, len, _) ->
      AppleDoubleRess (inode, mtime, ctime, len, ())

let ressFingerprint fspath path info =
  match info.ressInfo with
    NoRess ->
      Fingerprint.dummy
  | HfsRess _ ->
      Fingerprint.file fspath (ressPath path)
  | AppleDoubleRess (_, _, _, len, (path, offset)) ->
      Fingerprint.subfile path offset len

let ressLength ress =
  match ress with
    NoRess                            -> Uutil.Filesize.zero
  | HfsRess len                       -> len
  | AppleDoubleRess (_, _, _, len, _) -> len

let ressDummy = NoRess

(****)

let openRessIn fspath path =
  Util.convertUnixErrorsToTransient "reading resource fork" (fun () ->
    try
      Unix.in_channel_of_descr
        (Unix.openfile
           (Fspath.concatToString fspath (ressPath path))
           [Unix.O_RDONLY] 0o444)
    with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
      let (doublePath, inch, entries) = openDouble fspath path in
      try
        let (rsrcOffset, rsrcLength) = Safelist.assoc `RSRC entries in
        protect (fun () -> LargeFile.seek_in inch rsrcOffset)
          (fun () -> close_in_noerr inch);
        inch
      with Not_found ->
        close_in_noerr inch;
        raise (Util.Transient "No resource fork found"))

let openRessOut fspath path length =
  Util.convertUnixErrorsToTransient "writing resource fork" (fun () ->
    try
      Unix.out_channel_of_descr
        (Unix.openfile
           (Fspath.concatToString fspath (ressPath path))
           [Unix.O_WRONLY;Unix.O_TRUNC] 0o600)
    with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
      let path = appleDoubleFile fspath path in
      let outch =
        open_out_gen
          [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 path
      in
      protect (fun () ->
        output_string outch doubleMagic;
        output_string outch doubleVersion;
        output_string outch doubleFiller;
        output_string outch "\000\002"; (* Two entries *)
        output_string outch "\000\000\000\009"; (* Finder info *)
        output_string outch "\000\000\000\050"; (* offset *)
        output_string outch "\000\000\000\032"; (* length *)
        output_string outch "\000\000\000\002"; (* Resource fork *)
        output_string outch "\000\000\000\082"; (* offset *)
        output_string outch (setInt4 (Uutil.Filesize.toInt64 length));
                                                (* length *)
        output_string outch (emptyFinderInfo ());
        flush outch)
        (fun () -> close_out_noerr outch);
      outch)
