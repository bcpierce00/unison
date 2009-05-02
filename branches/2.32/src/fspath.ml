(* Unison file synchronizer: src/fspath.ml *)
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


(* Defines an abstract type of absolute filenames (fspaths).  Keeping the    *)
(* type abstract lets us enforce some invariants which are important for     *)
(* correct behavior of some system calls.                                    *)
(*                                                                         - *)
(* Invariants:                                                               *)
(*     Fspath "" is not allowed                                              *)
(*      All root directories end in /                                        *)
(*      All non-root directories end in some other character                 *)
(*      All separator characters are /, even in Windows                      *)
(*      All fspaths are absolute                                             *)
(*                                                                         - *)

let debug = Util.debug "fspath"
let debugverbose = Util.debug "fspath+"

type t = Fspath of string

let toString (Fspath f) = f

(* Needed to hack around some ocaml/Windows bugs, see comment at stat, below *)
let winRootRx = Rx.rx "(([a-zA-Z]:)?/|//[^/]+/[^/]+/)"
(* FIX I think we could just check the last character of [d]. *)
let isRootDir d =
(* We assume all path separators are slashes in d                            *)
  d="/" ||
  (Util.osType = `Win32 && Rx.match_string winRootRx d)
let winRootFixRx = Rx.rx "//[^/]+/[^/]+"
let winRootFix d =
  if Rx.match_string winRootFixRx d then d^"/" else d

(* [differentSuffix: fspath -> fspath -> (string * string)] returns the      *)
(* least distinguishing suffixes of two fspaths, for displaying in the user  *)
(* interface.                                                                *)
let differentSuffix (Fspath f1) (Fspath f2) =
  if isRootDir f1 or isRootDir f2 then (f1,f2)
  else begin
    (* We use the invariant that neither f1 nor f2 ends in slash             *)
    let len1 = String.length f1 in
    let len2 = String.length f2 in
    let n =
      (* The position of the character from the right where the fspaths      *)
      (* differ                                                              *)
      let rec loop n =
        let i1 = len1-n in
        if i1<0 then n+1 else
        let i2 = len2-n in
        if i2<0 then n+1 else
        if compare (String.get f1 i1) (String.get f2 i2) = 0
        then loop (n+1)
        else n in
      loop 1 in
    let suffix f len =
      try
        let n' = String.rindex_from f (len-n) '/' in
        String.sub f (n'+1) (len-n'-1)
      with _ -> f in
    let s1 = suffix f1 len1 in
    let s2 = suffix f2 len2 in
    (s1,s2)
  end

(* When an HFS file is stored on a non-HFS system it is stored as two
   files, the data fork, and the rest of the file including resource
   fork is stored in the AppleDouble file, which has the same name as
   the data fork file with ._ prepended. *)
let appleDouble (Fspath f) =
  if isRootDir f then raise(Invalid_argument "Fspath.appleDouble") else
  let len = String.length f in
  let i = String.rindex f '/' in
  let before = String.sub f 0 i in
  let after = String.sub f (i+1) (len-i-1) in
  Fspath(before^"/._"^after)

let rsrc (Fspath f) =
  if isRootDir f then raise(Invalid_argument "Fspath.appleDouble") else
  Fspath(f^"/..namedfork/rsrc")

(* WRAPPED SYSTEM CALLS *)

(* CAREFUL!
   Windows porting issue:
     Unix.LargeFile.stat "c:\\windows\\" will fail, you must use
     Unix.LargeFile.stat "c:\\windows" instead.
     The standard file selection dialog, however, will return a directory
     with a trailing backslash.
     Therefore, be careful to remove a trailing slash or backslash before
     calling this in Windows.
     BUT Windows shares are weird!
       //raptor/trevor and //raptor/trevor/mirror are directories
       and //raptor/trevor/.bashrc is a file.  We observe the following:
       Unix.LargeFile.stat "//raptor" will fail.
       Unix.LargeFile.stat "//raptor/" will fail.
       Unix.LargeFile.stat "//raptor/trevor" will fail.
       Unix.LargeFile.stat "//raptor/trevor/" will succeed.
       Unix.LargeFile.stat "//raptor/trevor/mirror" will succeed.
       Unix.LargeFile.stat "//raptor/trevor/mirror/" will fail.
       Unix.LargeFile.stat "//raptor/trevor/.bashrc/" will fail.
       Unix.LargeFile.stat "//raptor/trevor/.bashrc" will succeed.
       Not sure what happens for, e.g.,
         Unix.LargeFile.stat "//raptor/FOO"
       where //raptor/FOO is a file.
       I guess the best we can do is:
         To stat //host/xxx, assume xxx is a directory, and use
         Unix.LargeFile.stat "//host/xxx/". If xxx is not a directory,
         who knows.
         To stat //host/path where path has length >1, don't use
         a trailing slash.
       The way I did this was to assume //host/xxx/ is a root directory.
         Then by the invariants of fspath it should always end in /.

     Unix.LargeFile.stat "c:" will fail.
     Unix.LargeFile.stat "c:/" will succeed.
     Unix.LargeFile.stat "c://" will fail.
   (The Unix version of ocaml handles either a trailing slash or no
   trailing slash.)
*)
(* Invariant on fspath will guarantee that argument is OK for stat           *)
let stat (Fspath f) = Unix.LargeFile.stat f
let lstat (Fspath f) = Unix.LargeFile.lstat f

(* HACK:
   Under Windows 98,
     Unix.opendir "c:/" fails
     Unix.opendir "c:/*" works
     Unix.opendir "/" fails
   Under Windows 2000,
     Unix.opendir "c:/" works
     Unix.opendir "c:/*" fails
     Unix.opendir "/" fails

   Unix.opendir "c:" works as well, but, this refers to the current
   working directory AFAIK.
*)
let opendir (Fspath d) =
  if Util.osType<>`Win32 || not(isRootDir d) then Unix.opendir d else
  try
    Unix.opendir d
  with Unix.Unix_error _ ->
    Unix.opendir (d^"*")

let child (Fspath f) n =
  (* Note, f is not "" by invariants on Fspath *)
  if
    (* We use the invariant that f ends in / iff f is a root filename *)
    isRootDir f
  then
    Fspath(Printf.sprintf "%s%s" f (Name.toString n))
  else
    Fspath (Printf.sprintf "%s%c%s" f '/' (Name.toString n))

let concat fspath path =
  if Path.isEmpty path then
    fspath
  else begin
    let Fspath fspath = fspath in
    if
      (* We use the invariant that f ends in / iff f is a root filename *)
      isRootDir fspath
    then
      Fspath (fspath ^ Path.toString path)
    else
      let p = Path.toString path in
      let l = String.length fspath in
      let l' = String.length p in
      let s = String.create (l + l' + 1) in
      String.blit fspath 0 s 0 l;
      s.[l] <- '/';
      String.blit p 0 s (l + 1) l';
      Fspath s
  end

(* Filename.dirname is screwed up in Windows so we use this function.  It    *)
(* assumes that path separators are slashes.                                 *)
let winBadDirnameArg = Rx.rx "[a-zA-Z]:/[^/]*"
let myDirname s =
  if Util.osType=`Win32 && Rx.match_string winBadDirnameArg s
  then String.sub s 0 3
  else Filename.dirname s

(*****************************************************************************)
(*                         CANONIZING PATHS                                  *)
(*****************************************************************************)

(* Convert a string to an fspath.  HELP ENFORCE INVARIANTS listed above.     *)
let localString2fspath s =
  (* Force path separators to be slashes in Windows, handle weirdness in     *)
  (* Windows network names                                                   *)
  let s =
    if Util.osType = `Win32
    then winRootFix (Fileutil.backslashes2forwardslashes s)
    else s in
  (* Note: s may still contain backslashes under Unix *)
  if isRootDir s then Fspath s
  else if String.length s > 0 then
    let s' = Fileutil.removeTrailingSlashes s in
    if String.length s' = 0 then Fspath "/" (* E.g., s="///" *)
    else Fspath s'
  else
    (* Prevent Fspath "" *)
    raise(Invalid_argument "Os.localString2fspath")

(* Return the canonical fspath of a filename (string), relative to the       *)
(* current host, current directory.                                          *)

(* THIS IS A HACK.  It has to take account of some porting issues between    *)
(* the Unix and Windows versions of ocaml, etc.  In particular, the Unix,    *)
(* Filename, and Sys modules of ocaml have subtle differences under Windows  *)
(* and Unix.  So, be very careful with any changes !!!                       *)
let canonizeFspath p0 =
  let p = match p0 with None -> "." | Some "" -> "." | Some s -> s in
  let p' =
    begin
      let original = Sys.getcwd() in
      try
        let newp =
          (Sys.chdir p; (* This might raise Sys_error *)
           Sys.getcwd()) in
        Sys.chdir original;
        newp
      with
        Sys_error why ->
	  (* We could not chdir to p.  Either                                *)
	  (*                                                               - *)
	  (*              (1) p does not exist                               *)
	  (*              (2) p is a file                                    *)
	  (*              (3) p is a dir but we don't have permission        *)
	  (*                                                               - *)
	  (* In any case, we try to cd to the parent of p, and if that       *)
	  (* fails, we just quit.  This works nicely for most cases of (1),  *)
	  (* it works for (2), and on (3) it may leave a mess for someone    *)
	  (* else to pick up.                                                *)
          let p = if Util.osType = `Win32 then Fileutil.backslashes2forwardslashes p else p in
          if isRootDir p then raise
            (Util.Fatal (Printf.sprintf
               "Cannot find canonical name of root directory %s\n(%s)" p why));
          let parent = myDirname p in
          let parent' = begin
            (try Sys.chdir parent with
               Sys_error why2 -> raise (Util.Fatal (Printf.sprintf
                 "Cannot find canonical name of %s: unable to cd either to it\n
(%s)\nor to its parent %s\n(%s)" p why parent why2)));
            Sys.getcwd() end in
          Sys.chdir original;
          let bn = Filename.basename p in
          if bn="" then parent'
          else toString(child (localString2fspath parent')
                          (Name.fromString bn))
    end in
  localString2fspath p'

(*
(* TJ--I'm disabling this for now.  It is causing directories to be created  *)
(* with the wrong case, e.g., an upper case directory that needs to be       *)
(* propagated will be created with a lower case name.  We'll see if the      *)
(* weird problem with changing case is still happening.                      *)
  if Util.osType<>`Win32 then localString2fspath p'
  else
    (* A strange bug turns up in Windows: sometimes p' has mixed case,       *)
    (* sometimes it is all lower case.  (Sys.getcwd seems to make a random   *)
    (* choice.)  Since file names are not case-sensitive in Windows we just  *)
    (* force everything to lower case.                                       *)

    (* NOTE: WE DON'T ENFORCE THAT FSPATHS CREATED BY CHILDFSPATH ARE ALL    *)
    (* LOWER CASE!!                                                          *)
    let p' = String.lowercase p' in
    localString2fspath p'
*)

let canonize x =
  Util.convertUnixErrorsToFatal "canonizing path" (fun () -> canonizeFspath x)

let concatToString fspath path = toString (concat fspath path)

let maxlinks = 100
let findWorkingDir fspath path =
  let abspath = concatToString fspath path in
  let realpath =
    if not (Path.followLink path) then abspath else
    let rec followlinks n p =
      if n>=maxlinks then
        raise
          (Util.Transient (Printf.sprintf
             "Too many symbolic links from %s" abspath));
      try
        let link = Unix.readlink p in
        let linkabs =
          if Filename.is_relative link then
            Filename.concat (Filename.dirname p) link
          else link in
        followlinks (n+1) linkabs
      with
        Unix.Unix_error _ -> p in
    followlinks 0 abspath in
  if isRootDir realpath then
    raise (Util.Transient(Printf.sprintf
                            "The path %s is a root directory" abspath));
  let realpath = Fileutil.removeTrailingSlashes realpath in
  let p = Filename.basename realpath in
  debug
    (fun() ->
      Util.msg "Os.findWorkingDir(%s,%s) = (%s,%s)\n"
        (toString fspath)
        (Path.toString path)
        (myDirname realpath)
        p);
  (localString2fspath (myDirname realpath), Path.fromString p)
