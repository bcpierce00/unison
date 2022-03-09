(lang dune 2.3)

(name unison)

(version dev)

(generate_opam_files false)

(license GPL-3.0-or-later)

(maintainers "maintainers")

(authors "Trevor Jim" "Benjamin C. Pierce" "J\195\169r\195\180me Vouillon")

(source
 (github bcpierce00/unison))

(homepage https://www.cis.upenn.edu/~bcpierce/unison/)

(documentation "https://github.com/bcpierce00/unison/wiki")

(package
 (name unison)
 (synopsis "File-synchronization tool for Unix and Windows")
 (description "\
Text based user interface for Unison.

Unison is a file-synchronization tool for Unix and Windows.
It allows two replicas of a collection of files and directories
to be stored on different hosts (or different disks on the same host),
modified separately, and then brought up to date by propagating
the changes in each replica to the other.
")
 (depends
  (ocaml
   (>= 4.08))
  (dune
   (>= 2.3))))

(package
 (name unison-gui)
 (synopsis "File-synchronization tool for Unix and Windows")
 (description "\
Graphical user interface for Unison.

Unison is a file-synchronization tool for Unix and Windows.
It allows two replicas of a collection of files and directories
to be stored on different hosts (or different disks on the same host),
modified separately, and then brought up to date by propagating
the changes in each replica to the other.
")
 (depends
  (ocaml
   (>= 4.08))
  (dune
   (>= 2.3))
  lablgtk))

(package
 (name unison-fsmonitor)
 (synopsis "File-synchronization tool for Unix and Windows")
 (description "\
This helper application monitors file system changes on all given
(relative to root) paths.

Unison is a file-synchronization tool for Unix and Windows.
It allows two replicas of a collection of files and directories
to be stored on different hosts (or different disks on the same host),
modified separately, and then brought up to date by propagating
the changes in each replica to the other.
")
 (depends
  (ocaml
   (>= 4.08))
  (dune
   (>= 2.3))))
