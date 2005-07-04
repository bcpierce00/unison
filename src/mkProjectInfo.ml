(* Program for printing project info into a Makefile.  Documentation below. *)

let projectName = "unison"
let majorVersion = 2
let minorVersion = 14
let pointVersionOrigin = 71 (* Revision that corresponds to point version 0 *)

(* You shouldn't need to edit below. *)

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

let revisionString = "$Rev$";;
let revision = Scanf.sscanf revisionString "$Rev: %d " (fun x -> x);;
let pointVersion = revision - pointVersionOrigin;;

Printf.printf "MAJORVERSION=%d.%d\n" majorVersion minorVersion;;
Printf.printf "VERSION=%d.%d.%d\n" majorVersion minorVersion pointVersion;;
Printf.printf "NAME=%s\n" projectName;;









