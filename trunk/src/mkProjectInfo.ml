(* Program for printing project info into a Makefile.  Documentation below. *)

(* FIX: When the time comes for the next alpha-release, remember to
   increment the archive version number first. See update.ml. *)

let projectName = "unison"
let majorVersion = 2
let minorVersion = 40
let pointVersionOrigin = 409 (* Revision that corresponds to point version 0 *)

(* Documentation:
   This is a program to construct a version of the form Major.Minor.Point,
   e.g., 2.10.4.
   The Point release number is calculated from the Subversion revision number,
   so it will be automatically incremented on svn commit.
   The Major and Minor numbers are hard coded, as is the revision number
   corresponding to the 0 point release.

   If you want to increment the Major or Minor number, you will have to do a
   little thinking to get the Point number back to 0.  Suppose the current svn
   revision number is 27, and we have below

        let majorVersion = 2
        let minorVersion = 11
        let pointVersionOrigin = 3

   This means that the current Unison version is 2.11.24, since 27-3 = 24.
   If we want to change the release to 3.0.0 we need to change things to

        let majorVersion = 3
        let minorVersion = 0
        let pointVersionOrigin = 28

   and then do a svn commit.

   The first two lines are obvious.  The last line says that Subversion
   revision 28 corresponds to a 0 point release.  Since we were at revision
   27 and we're going to do a commit before making a release, we
   will be at 28 after the commit and this will be Unison version 3.0.0.
*)

(* ---------------------------------------------------------------------- *)
(* You shouldn't need to edit below. *)

let revisionString = "$Rev: 425$";;

(* BCP (1/10): This bit was added to help with getting Unison via bazaar, but it
   was never used much and I'm not confident it's working.  I'll comment it out
   for now, but if it hasn't been needed or fixed in a few months, the next
   person that edits this file should delete it...

  (* extract a substring using a regular expression *)
  let extract_str re str =
    let _ = Str.search_forward (Str.regexp re) str 0 in
    Str.matched_group 1 str;;
  let extract_int re str = int_of_string (extract_str re str);;

  (* run the bzr tool to get version information for bzr branches *)
  exception BzrException of Unix.process_status;;
  let bzr args =
    let bzr = (try Sys.getenv "BZR" with Not_found -> "bzr") in
    let cmd = bzr ^ " " ^ args in
    let inc = Unix.open_process_in cmd in
    let buf = Buffer.create 16 in
    (try
       while true do
         Buffer.add_channel buf inc 1
       done
     with End_of_file -> ());
    let status = Unix.close_process_in inc in
    match status with
      Unix.WEXITED 0 -> Buffer.contents buf
    | _ -> raise (BzrException status);;

  let pointVersion = if String.length revisionString > 5
  then Scanf.sscanf revisionString "$Rev: %d " (fun x -> x) - pointVersionOrigin
  else (* Determining the pointVersionOrigin in bzr is kind of tricky:
          - The mentioned revision number might not be part of this branch
          - The mentioned revision number might be rhs of some merge
          - The bzr-svn plugin might be outdated or not installed at all

          On the whole, getting this to work seems too much effort for now.
          So we'll simply use the revno as is as the point version,
          and revisit offsetting them if unison should ever move its trunk to bzr.

         let pvo = extract_int "^revno:[ \t]*\\([0-9]+\\)[ \t]*$"
                             (bzr ("log -r svn:" ^
                                   string_of_int pointVersionOrigin)) in
        *)
       extract_int "^\\([0-9]+\\)$" (bzr "revno") (* - pvo *);;
*)

let pointVersion = 
  Scanf.sscanf revisionString "$Rev: %d " (fun x -> x) - pointVersionOrigin;;

Printf.printf "MAJORVERSION=%d.%d\n" majorVersion minorVersion;;
Printf.printf "VERSION=%d.%d.%d\n" majorVersion minorVersion pointVersion;;
Printf.printf "NAME=%s\n" projectName;;










