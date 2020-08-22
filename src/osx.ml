(* Unison file synchronizer: src/osx.ml *)
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

(*
See
http://www.opensource.apple.com/source/copyfile/copyfile-42/copyfile.c
*)

let debug = Trace.debug "osx"

(****)

external isMacOSXPred : unit -> bool = "isMacOSX"

let isMacOSX = isMacOSXPred ()

(****)

let rsrcSync =
  Prefs.createBoolWithDefault "rsrc"
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
    (Prefs.read rsrcSync = `True ||
     (Prefs.read rsrcSync = `Default && b))

(****)

let doubleMagic = "\000\005\022\007"
let doubleVersion = "\000\002\000\000"
let doubleFiller = String.make 16 '\000'
let resource_fork_empty_tag = "This resource fork intentionally left blank   "
let finfoLength = 32L
let emptyFinderInfo () = Bytes.make 32 '\000'
let empty_resource_fork =
  "\000\000\001\000" ^
  "\000\000\001\000" ^
  "\000\000\000\000" ^
  "\000\000\000\030" ^
  resource_fork_empty_tag ^
  String.make (66+128) '\000' ^
  "\000\000\001\000" ^
  "\000\000\001\000" ^
  "\000\000\000\000" ^
  "\000\000\000\030" ^
  "\000\000\000\000" ^
  "\000\000\000\000" ^
  "\000\028\000\030" ^
  "\255\255"
let empty_attribute_chunk () =
  "\000\000" ^ (* pad *)
  "ATTR" ^  (* magic *)
  "\000\000\000\000" ^ (* debug tag *)
  "\000\000\014\226" ^ (* total size *)
  "\000\000\000\156" ^ (* data_start *)
  "\000\000\000\000" ^ (* data_length *)
  "\000\000\000\000" ^ (* reserved *)
  "\000\000\000\000" ^
  "\000\000\000\000" ^
  "\000\000" ^ (* flags *)
  "\000\000" ^ (* num_attrs *)
   String.make 3690 '\000'

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
  let s = Bytes.create 4 in
  let set i =
    Bytes.set s i
      (Char.chr (Int64.to_int (Int64.logand 255L
                               (Int64.shift_right v (24 - 8 * i))))) in
  set 0; set 1; set 2; set 3;
  s

let fail dataFspath dataPath doubleFspath msg =
  debug (fun () -> Util.msg "called 'fail'");
  raise (Util.Transient
           (Format.sprintf
              "The AppleDouble Header file '%s' \
               associated to data file %s is malformed: %s"
              (Fspath.toPrintString doubleFspath)
              (Fspath.toPrintString (Fspath.concat dataFspath dataPath)) msg))

let readDouble dataFspath dataPath doubleFspath inch len =
  let buf = Bytes.create len in
  begin try
    really_input inch buf 0 len
  with End_of_file ->
    fail dataFspath dataPath doubleFspath "truncated"
  end;
  Bytes.to_string buf

let readDoubleFromOffset dataFspath dataPath doubleFspath inch offset len =
  LargeFile.seek_in inch offset;
  readDouble dataFspath dataPath doubleFspath inch len

let writeDoubleFromOffset outch offset str =
  LargeFile.seek_out outch offset;
  output_string outch str

let protect f g =
  try
    f ()
  with Sys_error _ | Unix.Unix_error _ | Util.Transient _ as e ->
    begin try g () with Sys_error _  | Unix.Unix_error _ -> () end;
    raise e

let openDouble dataFspath dataPath =
  let doubleFspath = Fspath.appleDouble (Fspath.concat dataFspath dataPath) in
  let inch =
    try Fs.open_in_bin doubleFspath with Sys_error _ -> raise Not_found in
  protect (fun () ->
    Util.convertUnixErrorsToTransient "opening AppleDouble file" (fun () ->
      let header = readDouble dataFspath dataPath doubleFspath inch 26 in
      if String.sub header 0 4 <> doubleMagic then
        fail dataFspath dataPath doubleFspath "bad magic number";
      if String.sub header 4 4 <> doubleVersion then
        fail dataFspath dataPath doubleFspath "bad version";
      let numEntries = getInt2 header 24 in
      let entries = ref [] in
      for i = 1 to numEntries do
        let entry = readDouble dataFspath dataPath doubleFspath inch 12 in
        let id = getID entry 0 in
        let ofs = getInt4 entry 4 in
        let len = getInt4 entry 8 in
        entries := (id, (ofs, len)) :: !entries
      done;
      (doubleFspath, inch, !entries)))
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
  { ressInfo : (Fspath.t * int64) ressInfo;
    finfo : string }

external getFileInfosInternal :
  System.fspath -> bool -> string * int64 = "getFileInfos"
external setFileInfosInternal :
  System.fspath -> string -> unit = "setFileInfos"

let defaultInfos typ =
  match typ with
    `FILE      -> { ressInfo = NoRess; finfo = "F" }
  | `DIRECTORY -> { ressInfo = NoRess; finfo = "D" }
  |  _         -> { ressInfo = NoRess; finfo = "" }

(* BCP: dead code
   let noTypeCreator = String.make 10 '\000' *)

(* Remove trailing zeroes *)
let trim s =
  let rec trim_rec s pos =
    if pos > 0 && s.[pos - 1] = '\000' then
      trim_rec s (pos - 1)
    else
      String.sub s 0 pos
  in
  trim_rec s (String.length s)

let extractInfo typ info =
  let flags = Bytes.of_string (String.sub info 8 2) in
  let xflags = String.sub info 24 2 in
  let typeCreator = String.sub info 0 8 in
  (* Ignore hasBeenInited flag *)
  Bytes.set flags 0 (Char.chr (Char.code (Bytes.get flags 0) land 0xfe));
  (* If the extended flags should be ignored, clear them *)
  let xflags =
    if Char.code xflags.[0] land 0x80 <> 0 then "\000\000" else xflags
  in
  let info =
    match typ with
      `FILE       -> "F" ^ typeCreator ^ Bytes.to_string flags ^ xflags
    | `DIRECTORY  -> "D" ^ Bytes.to_string flags ^ xflags
  in
  trim info

let getFileInfos dataFspath dataPath typ =
  if not (Prefs.read rsrc) then defaultInfos typ else
  match typ with
    (`FILE | `DIRECTORY) as typ ->
      Util.convertUnixErrorsToTransient "getting file information" (fun () ->
        try
          let (fInfo, rsrcLength) =
            getFileInfosInternal
              (Fspath.toSysPath (Fspath.concat dataFspath dataPath))
              (typ = `FILE)
          in
          { ressInfo =
              if rsrcLength = 0L then NoRess
              else HfsRess (Uutil.Filesize.ofInt64 rsrcLength);
            finfo = extractInfo typ fInfo }
        with Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.ENOSYS), _, _) ->
          (* Not a HFS volume.  Look for an AppleDouble file *)
          try
            let (workingDir, realPath) =
              Fspath.findWorkingDir dataFspath dataPath in
            let (doubleFspath, inch, entries) =
              openDouble workingDir realPath in
            let (rsrcOffset, rsrcLength) =
              try
                let (offset, len) = Safelist.assoc `RSRC entries in
                (* We need to check that the resource fork is not a
                   dummy one included for compatibility reasons *)
                if len = 286L &&
                   protect (fun () ->
                     LargeFile.seek_in inch (Int64.add offset 16L);
                     let len = String.length resource_fork_empty_tag in
                     let buf = Bytes.create len in
                     really_input inch buf 0 len;
                     Bytes.to_string buf = resource_fork_empty_tag)
                     (fun () -> close_in_noerr inch)
                then
                  (0L, 0L)
                else
                  (offset, len)
              with Not_found ->
                (0L, 0L)
            in
            debug (fun () ->
              Util.msg
                "AppleDouble for file %s / %s: resource fork length: %d\n"
                (Fspath.toDebugString dataFspath) (Path.toString dataPath)
                (Int64.to_int rsrcLength));
            let finfo =
              protect (fun () ->
                try
                  let (ofs, len) = Safelist.assoc `FINFO entries in
                  if len < finfoLength then
                    fail dataFspath dataPath doubleFspath "bad finder info";
                  readDoubleFromOffset
                    dataFspath dataPath doubleFspath inch ofs 32
                with Not_found ->
                  String.make 32 '\000')
                (fun () -> close_in_noerr inch)
            in
            close_in inch;
            let stats =
              Util.convertUnixErrorsToTransient "stating AppleDouble file"
                (fun () -> Fs.stat doubleFspath) in
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
                   (doubleFspath, rsrcOffset));
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
  Bytes.to_string fullInfo

let setFileInfos dataFspath dataPath finfo =
  assert (finfo <> "");
  Util.convertUnixErrorsToTransient "setting file information" (fun () ->
    try
      let p = Fspath.toSysPath (Fspath.concat dataFspath dataPath) in
      let (fullFinfo, _) = getFileInfosInternal p false in
      setFileInfosInternal p (insertInfo (Bytes.of_string fullFinfo) finfo)
    with Unix.Unix_error ((Unix.EOPNOTSUPP | Unix.ENOSYS), _, _) ->
      (* Not an HFS volume.  Look for an AppleDouble file *)
      let (workingDir, realPath) = Fspath.findWorkingDir dataFspath dataPath in
      begin try
        let (doubleFspath, inch, entries) = openDouble workingDir realPath in
        begin try
          let (ofs, len) = Safelist.assoc `FINFO entries in
          if len < finfoLength then
            fail dataFspath dataPath doubleFspath "bad finder info";
          let fullFinfo =
            protect
              (fun () ->
                let res =
                  readDoubleFromOffset
                    dataFspath dataPath doubleFspath inch ofs 32 in
                close_in inch;
                res)
              (fun () -> close_in_noerr inch)
            |> Bytes.of_string
          in
          let outch =
            Fs.open_out_gen [Open_wronly; Open_binary] 0o600 doubleFspath in
          protect
            (fun () ->
               writeDoubleFromOffset outch ofs (insertInfo fullFinfo finfo);
               close_out outch)
            (fun () ->
               close_out_noerr outch);
        with Not_found ->
          close_in_noerr inch;
          raise (Util.Transient
                   (Format.sprintf
                      "Unable to set the file type and creator: \n\
                       The AppleDouble file '%s' has no fileinfo entry."
                      (Fspath.toPrintString doubleFspath)))
        end
      with Not_found ->
        (* No AppleDouble file, create one if needed. *)
        if finfo <> "F" && finfo <> "D" then begin
          let doubleFspath =
            Fspath.appleDouble (Fspath.concat workingDir realPath) in
          let outch =
            Fs.open_out_gen
              [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600
              doubleFspath
          in
          (* Apparently, for compatibility with various old versions
             of Mac OS X that did not follow the AppleDouble specification,
             we have to include a dummy resource fork...
             We also put an empty extended attribute section at the
             end of the finder info section, mimicking the Mac OS X
             kernel behavior.  *)
          protect (fun () ->
            output_string outch doubleMagic;
            output_string outch doubleVersion;
            output_string outch doubleFiller;
            output_string outch "\000\002"; (* Two entries *)
            output_string outch "\000\000\000\009"; (* Finder info *)
            output_string outch "\000\000\000\050"; (* offset *)
            output_string outch "\000\000\014\176"; (* length *)
            output_string outch "\000\000\000\002"; (* Resource fork *)
            output_string outch "\000\000\014\226"; (* offset *)
            output_string outch "\000\000\001\030"; (* length *)
            output_string outch (insertInfo (emptyFinderInfo ()) finfo);
            output_string outch (empty_attribute_chunk ());
                                                    (* extended attributes *)
            output_string outch empty_resource_fork;
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
      debug (fun () ->
        Util.msg "resource fork fingerprint: path %s, offset %d, len %d"
        (Fspath.toString path)
        (Int64.to_int offset) (Uutil.Filesize.toInt len));
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
        (Fs.openfile
           (Fspath.concat fspath (ressPath path))
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
      let p = Fspath.concat fspath (ressPath path) in
      debug (fun () -> Util.msg "openRessOut %s\n" (Fspath.toString p));
      Unix.out_channel_of_descr
        (Fs.openfile p [Unix.O_WRONLY;Unix.O_CREAT] 0o600)
    with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
      debug (fun () -> Util.msg "Opening AppleDouble file for resource fork\n");
      let path = Fspath.appleDouble (Fspath.concat fspath path) in
      let outch =
        Fs.open_out_gen
          [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 path
      in
      protect (fun () ->
        output_string outch doubleMagic;
        output_string outch doubleVersion;
        output_string outch doubleFiller;
        output_string outch "\000\002"; (* Two entries *)
        output_string outch "\000\000\000\009"; (* Finder info *)
        output_string outch "\000\000\000\050"; (* offset *)
        output_string outch "\000\000\014\176"; (* length *)
        output_string outch "\000\000\000\002"; (* Resource fork *)
        output_string outch "\000\000\014\226"; (* offset *)
(* FIX: should check for overflow! *)
        output_bytes outch (setInt4 (Uutil.Filesize.toInt64 length));
                                                (* length *)
        output_bytes outch (emptyFinderInfo ());
        output_string outch (empty_attribute_chunk ());
                                                (* extended attributes *)
        flush outch)
        (fun () -> close_out_noerr outch);
      outch)
