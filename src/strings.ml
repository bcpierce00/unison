(* DO NOT MODIFY.
   This file has been automatically generated, see docs.ml. *)

let docs =
    ("about", ("About Unison", 
     "Unison File Synchronizer\n\
      Version 2.51.91\n\
      \n\
      "))
::
    ("", ("Overview", 
     "Overview\n\
      \n\
      \032  Unison is a file-synchronization tool for Unix and Windows. It allows\n\
      \032  two replicas of a collection of files and directories to be stored on\n\
      \032  different hosts (or different disks on the same host), modified\n\
      \032  separately, and then brought up to date by propagating the changes in\n\
      \032  each replica to the other.\n\
      \n\
      \032  Unison shares a number of features with tools such as configuration\n\
      \032  management packages (CVS (http://www.cyclic.com/), PRCS\n\
      \032  (http://www.XCF.Berkeley.EDU/~jmacd/prcs.html), etc.), distributed\n\
      \032  filesystems (Coda (http://www.coda.cs.cmu.edu/), etc.), uni-directional\n\
      \032  mirroring utilities (rsync (http://samba.anu.edu.au/rsync/), etc.), and\n\
      \032  other synchronizers (Intellisync (http://www.pumatech.com), Reconcile\n\
      \032  (http://www.merl.com/reports/TR99-14/), etc). However, there are\n\
      \032  several points where it differs:\n\
      \032    * Unison runs on both Windows (95, 98, NT, 2k, and XP) and Unix (OSX,\n\
      \032      Solaris, Linux, etc.) systems. Moreover, Unison works across\n\
      \032      platforms, allowing you to synchronize a Windows laptop with a Unix\n\
      \032      server, for example.\n\
      \032    * Unlike a distributed filesystem, Unison is a user-level program:\n\
      \032      there is no need to modify the kernel or to have superuser\n\
      \032      privileges on either host.\n\
      \032    * Unlike simple mirroring or backup utilities, Unison can deal with\n\
      \032      updates to both replicas of a distributed directory structure.\n\
      \032      Updates that do not conflict are propagated automatically.\n\
      \032      Conflicting updates are detected and displayed.\n\
      \032    * Unison works between any pair of machines connected to the\n\
      \032      internet, communicating over either a direct socket link or\n\
      \032      tunneling over an encrypted ssh connection. It is careful with\n\
      \032      network bandwidth, and runs well over slow links such as PPP\n\
      \032      connections. Transfers of small updates to large files are\n\
      \032      optimized using a compression protocol similar to rsync.\n\
      \032    * Unison has a clear and precise specification, described below.\n\
      \032    * Unison is resilient to failure. It is careful to leave the replicas\n\
      \032      and its own private structures in a sensible state at all times,\n\
      \032      even in case of abnormal termination or communication failures.\n\
      \032    * Unison is free; full source code is available under the GNU Public\n\
      \032      License.\n\
      \n\
      "))
::
    ("", ("Preface", 
     "Preface\n\
      \n\
      "))
::
    ("people", ("People", 
     "People\n\
      \n\
      \032  Benjamin Pierce (http://www.cis.upenn.edu/~bcpierce/) leads the Unison\n\
      \032  project. The current version of Unison was designed and implemented by\n\
      \032  Trevor Jim (http://www.research.att.com/~trevor/), Benjamin Pierce\n\
      \032  (http://www.cis.upenn.edu/~bcpierce/), and J\195\169r\195\180me Vouillon\n\
      \032  (http://www.pps.jussieu.fr/~vouillon/), with Alan Schmitt\n\
      \032  (http://alan.petitepomme.net/), Malo Denielou, Zhe Yang\n\
      \032  (http://www.brics.dk/~zheyang/), Sylvain Gommier, and Matthieu Goulay.\n\
      \032  The Mac user interface was started by Trevor Jim and enormously\n\
      \032  improved by Ben Willmore. Our implementation of the rsync\n\
      \032  (http://samba.org/rsync/) protocol was built by Norman Ramsey\n\
      \032  (http://www.eecs.harvard.edu/~nr/) and Sylvain Gommier. It is based on\n\
      \032  Andrew Tridgell (http://samba.anu.edu.au/~tridge/)\226\128\153s thesis work\n\
      \032  (http://samba.anu.edu.au/~tridge/phd_thesis.pdf) and inspired by his\n\
      \032  rsync (http://samba.org/rsync/) utility. The mirroring and merging\n\
      \032  functionality was implemented by Sylvain Roy, improved by Malo\n\
      \032  Denielou, and improved yet further by St\195\169phane Lescuyer. Jacques\n\
      \032  Garrigue (http://wwwfun.kurims.kyoto-u.ac.jp/~garrigue/) contributed\n\
      \032  the original Gtk version of the user interface; the Gtk2 version was\n\
      \032  built by Stephen Tse. Sundar Balasubramaniam helped build a prototype\n\
      \032  implementation of an earlier synchronizer in Java. Insik Shin\n\
      \032  (http://www.cis.upenn.edu/~ishin/) and Insup Lee\n\
      \032  (http://www.cis.upenn.edu/~lee/) contributed design ideas to this\n\
      \032  implementation. Cedric Fournet\n\
      \032  (http://research.microsoft.com/~fournet/) contributed to an even\n\
      \032  earlier prototype.\n\
      \n\
      "))
::
    ("lists", ("Mailing Lists and Bug Reporting", 
     "Mailing Lists and Bug Reporting\n\
      \n\
      Mailing Lists:\n\
      \n\
      \032  Moderated mailing lists are available for discussions among users and\n\
      \032  discussions among developers. See\n\
      \n\
      \032    https://github.com/bcpierce00/unison/wiki/Mailing-Lists\n\
      \n\
      \032  for descriptions of what content is appropriate on which list, and\n\
      \032  subscripion instructions.\n\
      \n\
      Reporting bugs:\n\
      \n\
      \032  Reports of bugs affecting correctness or safety are of interest to many\n\
      \032  people. If Unison is not working the way you expect, see the\n\
      \032  instructions for debugging, reporting bugs, and asking for help at\n\
      \n\
      \032    https://github.com/bcpierce00/unison/wiki/Reporting-Bugs-and-Feature\n\
      \032    -Requests\n\
      \n\
      Feature Requests:\n\
      \n\
      \032  Requests for features likely to be of interest to others are welcome,\n\
      \032  but will probably just be added to the ever-growing todo list. Please\n\
      \032  see the URL in the previous section for guidance on feature requests.\n\
      \n\
      "))
::
    ("status", ("Development Status", 
     "Development Status\n\
      \n\
      \032  Unison is no longer under active development as a research project.\n\
      \032  (Our research efforts are now focused on a follow-on project called\n\
      \032  Boomerang, described at http://www.cis.upenn.edu/~bcpierce/harmony.) At\n\
      \032  this point, there is no one whose job it is to maintain Unison, fix\n\
      \032  bugs, or answer questions.\n\
      \n\
      \032  However, the original developers are all still using Unison daily. It\n\
      \032  will continue to be maintained and supported for the foreseeable\n\
      \032  future, and we will occasionally release new versions with bug fixes,\n\
      \032  small improvements, and contributed patches.\n\
      \n\
      \032  Proposed changes to unison are welcome. They should be submitted as\n\
      \032  pull requests. (Since safety and robustness are Unison\226\128\153s most important\n\
      \032  properties, patches will be held to high standards of clear design and\n\
      \032  clean coding.) If you want to contribute to Unison, start by\n\
      \032  downloading the developer tarball from the download page. For some\n\
      \032  details on how the code is organized, etc., see the file CONTRIB.\n\
      \n\
      "))
::
    ("copying", ("Copying", 
     "Copying\n\
      \n\
      \032  This file is part of Unison.\n\
      \n\
      \032  Unison is free software: you can redistribute it and/or modify it under\n\
      \032  the terms of the GNU General Public License as published by the Free\n\
      \032  Software Foundation, either version 3 of the License, or (at your\n\
      \032  option) any later version.\n\
      \n\
      \032  Unison is distributed in the hope that it will be useful, but WITHOUT\n\
      \032  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or\n\
      \032  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License\n\
      \032  for more details.\n\
      \n\
      \032  The GNU General Public License can be found at\n\
      \032  http://www.gnu.org/licenses. A copy is also included in the Unison\n\
      \032  source distribution in the file COPYING.\n\
      \n\
      "))
::
    ("ack", ("Acknowledgements", 
     "Acknowledgements\n\
      \n\
      \032  Work on Unison has been supported by the National Science Foundation\n\
      \032  under grants CCR-9701826 and ITR-0113226, Principles and Practice of\n\
      \032  Synchronization, and by University of Pennsylvania\226\128\153s Institute for\n\
      \032  Research in Cognitive Science (IRCS).\n\
      \n\
      "))
::
    ("install", ("Installation", 
     "Installation\n\
      \n\
      \032  Unison is designed to be easy to install. The following sequence of\n\
      \032  steps should get you a fully working installation in a few minutes. If\n\
      \032  you run into trouble, you may find the suggestions on the Frequently\n\
      \032  Asked Questions page\n\
      \032  (http://www.cis.upenn.edu/~bcpierce/unison/faq.html) helpful. Pre-built\n\
      \032  binaries are available for a variety of platforms.\n\
      \n\
      \032  Unison can be used with either of two user interfaces:\n\
      \032   1. a simple textual interface, suitable for dumb terminals (and\n\
      \032      running from scripts), and\n\
      \032   2. a more sophisticated graphical interface, based on Gtk2 (on\n\
      \032      Linux/Windows) or the native UI framework (on OSX).\n\
      \n\
      \032  You will need to install a copy of Unison on every machine that you\n\
      \032  want to synchronize. However, you only need the version with a\n\
      \032  graphical user interface (if you want a GUI at all) on the machine\n\
      \032  where you\226\128\153re actually going to display the interface (the CLIENT\n\
      \032  machine). Other machines that you synchronize with can get along just\n\
      \032  fine with the textual version.\n\
      \n\
      Downloading Unison\n\
      \n\
      \032  See https://github.com/bcpierce00/unison/wiki/Downloading-Unison.\n\
      \n\
      \032  If a pre-built binary of Unison is available for the client machine\226\128\153s\n\
      \032  architecture, just download it and put it somewhere in your search path\n\
      \032  (if you\226\128\153re going to invoke it from the command line) or on your desktop\n\
      \032  (if you\226\128\153ll be click-starting it).\n\
      \n\
      \032  The executable file for the graphical version (with a name including\n\
      \032  gtkui) actually provides both interfaces: the graphical one appears by\n\
      \032  default, while the textual interface can be selected by including -ui\n\
      \032  text on the command line. The textui executable provides just the\n\
      \032  textual interface.\n\
      \n\
      \032  If you don\226\128\153t see a pre-built executable for your architecture, you\226\128\153ll\n\
      \032  need to build it yourself. See the section \226\128\156Building Unison from\n\
      \032  Scratch\226\128\157 .\n\
      \n\
      \032  Check to make sure that what you have downloaded is really executable.\n\
      \032  Either click-start it, or type \"unison -version\" at the command line.\n\
      \n\
      \032  Unison can be used in three different modes: with different directories\n\
      \032  on a single machine, with a local or a remote machine over a direct\n\
      \032  socket connection, or with a remote machine using ssh for\n\
      \032  authentication and secure transfer.\n\
      \n\
      Running Unison\n\
      \n\
      \032  Once you\226\128\153ve got Unison installed on at least one system, read the\n\
      \032  section \226\128\156Tutorial\226\128\157 of the user manual (or type \"unison -doc tutorial\")\n\
      \032  for instructions on how to get started.\n\
      \n\
      Upgrading\n\
      \n\
      \032  Upgrading to a new version of Unison is as simple as throwing away the\n\
      \032  old binary and installing the new one.\n\
      \n\
      \032  Before upgrading, it is a good idea to run the old version one last\n\
      \032  time, to make sure all your replicas are completely synchronized. A new\n\
      \032  version of Unison will sometimes introduce a different format for the\n\
      \032  archive files used to remember information about the previous state of\n\
      \032  the replicas. In this case, the old archive will be ignored (not\n\
      \032  deleted \226\128\148 if you roll back to the previous version of Unison, you will\n\
      \032  find the old archives intact), which means that any differences between\n\
      \032  the replicas will show up as conflicts that need to be resolved\n\
      \032  manually.\n\
      \n\
      \032  As of version 2.52, Unison has a degree of backward and forward\n\
      \032  compatibility. This means three things. First, it is possible for local\n\
      \032  and remote machines to run a different version of Unison. Second, it is\n\
      \032  possible for local and remote machines to run a version (same or\n\
      \032  different) of Unison built with a different version of OCaml compiler\n\
      \032  (this has been problematic historically). Lastly, it is possible to\n\
      \032  upgrade Unison on the local machine (compiled with any OCaml version)\n\
      \032  and keep the existing archive.\n\
      \n\
      \032  If version interoperability requirements are followed then Unison 2.52\n\
      \032  can upgrade the archive created by earlier Unison versions. To avoid\n\
      \032  rebuilding archive files, you must install version 2.52 built with the\n\
      \032  same OCaml version as your previous version of Unison, and then run it\n\
      \032  at least once on each root. Doing so will upgrade the archive file.\n\
      \n\
      \032  After upgrading the archive, you are free to swap the Unison 2.52\n\
      \032  executable to one compiled with a different version of OCaml. The\n\
      \032  archive file is no longer dependent on the compiler version.\n\
      \n\
      Version interoperability\n\
      \n\
      \032  To ensure interoperability with different Unison versions on local and\n\
      \032  remote machines, and to upgrade from an earlier version without\n\
      \032  rebuilding the archive files, you have to remember these guidelines.\n\
      \032  Upgrading from an incompatible version, while possible and normal, will\n\
      \032  require fully scanning both roots, which can be time-consuming with big\n\
      \032  replicas.\n\
      \n\
      \032  Unison 2.52 is compatible with:\n\
      \032    * Unison 2.52 or newer (for as long as backwards compatibility is\n\
      \032      maintained in the newer versions). You do not have to pay any\n\
      \032      attention to OCaml compiler versions.\n\
      \032    * Unison 2.51 if both versions are compiled with same OCaml compiler\n\
      \032      version (you can see which compiler version was used by running\n\
      \032      unison -version).\n\
      \032    * Unison 2.48 if both versions are compiled with same OCaml compiler\n\
      \032      version. See special notes below.\n\
      \n\
      \032  Interoperability matrix for quick reference:\n\
      \n\
      \032  Client versions Server versions\n\
      \032                  2.52 or newer       2.51               2.48\n\
      \032  2.52 or newer   full interop        same OCaml version same OCaml version\n\
      \032  2.51            same OCaml version  full interop       no interop\n\
      \032  2.48            same OCaml version* no interop         full interop\n\
      \n\
      \032  Special notes for Unison 2.48:\n\
      \032    * Unison 2.48 does not show which OCaml compiler was used to compile\n\
      \032      it. If you do not have the option of re-compiling the 2.48 version,\n\
      \032      you have two alternatives. First (and most likely to succeed), see\n\
      \032      what is the version of the OCaml compiler in the same package\n\
      \032      repository where you installed Unison 2.48 from, then use Unison\n\
      \032      2.52 compiled with that version. Second, you can just try Unison\n\
      \032      2.52 executables compiled with different OCaml versions and see\n\
      \032      which one works with your copy of Unison 2.48.\n\
      \032    * When running Unison 2.48 on the client machine with Unison 2.52 or\n\
      \032      newer on the server machine, you have to do some additional\n\
      \032      configuration. The Unison executable name on the server must start\n\
      \032      with unison-2.48 (just unison-2.48 is ok, as is unison-2.48.exe,\n\
      \032      but also unison-2.48+ocaml-4.05). If using TCP socket connection to\n\
      \032      the server then you\226\128\153re all set! If using ssh then you have to add\n\
      \032      one of the following options to your profile or as a command-line\n\
      \032      argument on the client machine: -addversionno; see the section\n\
      \032      \226\128\156Remote Usage\226\128\157 , or -servercmd; see the section \226\128\156Remote Shell\n\
      \032      Method\226\128\157 .\n\
      \n\
      Building Unison from Scratch\n\
      \n\
      \032  If a pre-built image is not available, you will need to compile it from\n\
      \032  scratch; the sources are available from the same place as the binaries.\n\
      \n\
      \032  In principle, Unison should work on any platform to which OCaml has\n\
      \032  been ported and on which the Unix module is fully implemented. It has\n\
      \032  been tested on many flavors of Windows (98, NT, 2000, XP) and Unix (OS\n\
      \032  X, Solaris, Linux, FreeBSD), and on both 32- and 64-bit architectures.\n\
      \n\
      Unix\n\
      \n\
      \032  Unison can be built with or without a graphical user interface (GUI).\n\
      \032  The build system will decide automatically depending on the libraries\n\
      \032  installed on your system, but you can also type make UISTYLE=text to\n\
      \032  build Unison without GUI.\n\
      \n\
      \032  You\226\128\153ll need the Objective Caml compiler, available from\n\
      \032  http://caml.inria.fr. OCaml is available from most package managers\n\
      \032  Building and installing OCaml on Unix systems is very straightforward;\n\
      \032  just follow the instructions in the distribution. You\226\128\153ll probably want\n\
      \032  to build the native-code compiler in addition to the bytecode compiler,\n\
      \032  as Unison runs much faster when compiled to native code, but this is\n\
      \032  not absolutely necessary. (Quick start: on many systems, the following\n\
      \032  sequence of commands will get you a working and installed compiler:\n\
      \032  first do make world opt, then su to root and do make install.)\n\
      \n\
      \032  You\226\128\153ll also need the GNU make utility, which is standard on most Unix\n\
      \032  systems. Unison\226\128\153s build system is not parallelizable, so don\226\128\153t use\n\
      \032  flags that cause it to start processes in parallel (e.g. -j).\n\
      \n\
      \032  Once you\226\128\153ve got OCaml installed, grab a copy of the Unison sources,\n\
      \032  unzip and untar them, change to the new \"unison\" directory, and type\n\
      \032  \226\128\156make UISTYLE=text\226\128\157. The result should be an executable file called\n\
      \032  \"unison\". Type \"./unison\" to make sure the program is executable. You\n\
      \032  should get back a usage message.\n\
      \n\
      \032  If you want to build the graphical user interface, you will need to\n\
      \032  install some additional things:\n\
      \032    * The Gtk2 development libraries (package libgtk2.0-dev on debian\n\
      \032      based systems).\n\
      \032    * OCaml bindings for Gtk2. Install them from your software\n\
      \032      repositories (package liblablgtk2-ocaml on debian based systems).\n\
      \032      Also available from https://github.com/garrigue/lablgtk.\n\
      \032    * Pango, a text rendering library and a part of Gtk2. On some systems\n\
      \032      (e.g. Ubuntu) the bindings between Pango and OCaml need to be\n\
      \032      installed explicitly (package liblablgtk-extras-ocaml-dev on\n\
      \032      Ubuntu).\n\
      \n\
      \032  Type make src to build Unison. If Gtk2 is available on the system,\n\
      \032  Unison with a GUI will be built automatically.\n\
      \n\
      \032  Put the unison executable somewhere in your search path, either by\n\
      \032  adding the Unison directory to your PATH variable or by copying the\n\
      \032  executable to some standard directory where executables are stored. Or\n\
      \032  just type make install to install Unison to $HOME/bin/unison.\n\
      \n\
      Mac OS X\n\
      \n\
      \032  To build the text-only user interface, follow the instructions above\n\
      \032  for building on Unix systems. You should do this first, even if you are\n\
      \032  also planning on building the GUI, just to make sure it works.\n\
      \n\
      \032  To build the basic GUI version, you\226\128\153ll first need to download and\n\
      \032  install the XCode developer tools from Apple. Once this is done, just\n\
      \032  type make in the src directory, and if things go well you should get an\n\
      \032  application that you can move from uimac/build/Default/Unison.app to\n\
      \032  wherever you want it.\n\
      \n\
      Windows\n\
      \n\
      \032  Although the binary distribution should work on any version of Windows,\n\
      \032  some people may want to build Unison from scratch on those systems too.\n\
      \n\
      Bytecode version:\n\
      \n\
      \032  The simpler but slower compilation option to build a Unison executable\n\
      \032  is to build a bytecode version. You need first install Windows version\n\
      \032  of the OCaml compiler (version 3.07 or later, available from\n\
      \032  http://caml.inria.fr). Then grab a copy of Unison sources and type\n\
      \032      make NATIVE=false\n\
      \n\
      \032  to compile the bytecode. The result should be an executable file called\n\
      \032  unison.exe.\n\
      \n\
      Native version:\n\
      \n\
      \032  Building a more efficient, native version of Unison on Windows requires\n\
      \032  a little more work. See the file INSTALL.win32 in the source code\n\
      \032  distribution.\n\
      \n\
      Installation Options\n\
      \n\
      \032  The Makefile in the distribution includes several switches that can be\n\
      \032  used to control how Unison is built. Here are the most useful ones:\n\
      \032    * Building with NATIVE=true uses the native-code OCaml compiler,\n\
      \032      yielding an executable that will run quite a bit faster. We use\n\
      \032      this for building distribution versions.\n\
      \032    * Building with make DEBUGGING=true generates debugging symbols.\n\
      \032    * Building with make STATIC=true generates a (mostly) statically\n\
      \032      linked executable. We use this for building distribution versions,\n\
      \032      for portability.\n\
      \n\
      "))
::
    ("tutorial", ("Tutorial", 
     "Tutorial\n\
      \n\
      Preliminaries\n\
      \n\
      \032  Unison can be used with either of two user interfaces:\n\
      \032   1. a straightforward textual interface and\n\
      \032   2. a more sophisticated graphical interface\n\
      \n\
      \032  The textual interface is more convenient for running from scripts and\n\
      \032  works on dumb terminals; the graphical interface is better for most\n\
      \032  interactive use. For this tutorial, you can use either. If you are\n\
      \032  running Unison from the command line, just typing unison will select\n\
      \032  either the text or the graphical interface, depending on which has been\n\
      \032  selected as default when the executable you are running was built. You\n\
      \032  can force the text interface even if graphical is the default by adding\n\
      \032  -ui text. The other command-line arguments to both versions are\n\
      \032  identical.\n\
      \n\
      \032  The graphical version can also be run directly by clicking on its icon,\n\
      \032  but this may require a little set-up (see the section \226\128\156Click-starting\n\
      \032  Unison\226\128\157 ). For this tutorial, we assume that you\226\128\153re starting it from\n\
      \032  the command line.\n\
      \n\
      \032  Unison can synchronize files and directories on a single machine, or\n\
      \032  between two machines on a network. (The same program runs on both\n\
      \032  machines; the only difference is which one is responsible for\n\
      \032  displaying the user interface.) If you\226\128\153re only interested in a\n\
      \032  single-machine setup, then let\226\128\153s call that machine the CLIENT . If\n\
      \032  you\226\128\153re synchronizing two machines, let\226\128\153s call them CLIENT and SERVER .\n\
      \n\
      Local Usage\n\
      \n\
      \032  Let\226\128\153s get the client machine set up first and see how to synchronize\n\
      \032  two directories on a single machine.\n\
      \n\
      \032  Follow the instructions in the section \226\128\156Installation\226\128\157 to either\n\
      \032  download or build an executable version of Unison, and install it\n\
      \032  somewhere on your search path. (If you just want to use the textual\n\
      \032  user interface, download the appropriate textui binary. If you just\n\
      \032  want to the graphical interface\226\128\148or if you will use both interfaces [the\n\
      \032  gtkui binary actually has both compiled in]\226\128\148then download the gtkui\n\
      \032  binary.)\n\
      \n\
      \032  Create a small test directory a.tmp containing a couple of files and/or\n\
      \032  subdirectories, e.g.,\n\
      \032      mkdir a.tmp\n\
      \032      touch a.tmp/a a.tmp/b\n\
      \032      mkdir a.tmp/d\n\
      \032      touch a.tmp/d/f\n\
      \n\
      \032  Copy this directory to b.tmp:\n\
      \032      cp -r a.tmp b.tmp\n\
      \n\
      \032  Now try synchronizing a.tmp and b.tmp. (Since they are identical,\n\
      \032  synchronizing them won\226\128\153t propagate any changes, but Unison will\n\
      \032  remember the current state of both directories so that it will be able\n\
      \032  to tell next time what has changed.) Type:\n\
      \032      unison a.tmp b.tmp\n\
      \n\
      \032  (You may need to add -ui text, depending how your unison binary was\n\
      \032  built.)\n\
      \n\
      \032  Textual Interface:\n\
      \032    * You should see a message notifying you that all the files are\n\
      \032      actually equal and then get returned to the command line.\n\
      \n\
      \032  Graphical Interface:\n\
      \032    * You should get a big empty window with a message at the bottom\n\
      \032      notifying you that all files are identical. Choose the Exit item\n\
      \032      from the File menu to get back to the command line.\n\
      \n\
      \032  Next, make some changes in a.tmp and/or b.tmp. For example:\n\
      \032       rm a.tmp/a\n\
      \032       echo \"Hello\" > a.tmp/b\n\
      \032       echo \"Hello\" > b.tmp/b\n\
      \032       date > b.tmp/c\n\
      \032       echo \"Hi there\" > a.tmp/d/h\n\
      \032       echo \"Hello there\" > b.tmp/d/h\n\
      \n\
      \032  Run Unison again:\n\
      \032      unison a.tmp b.tmp\n\
      \n\
      \032  This time, the user interface will display only the files that have\n\
      \032  changed. If a file has been modified in just one replica, then it will\n\
      \032  be displayed with an arrow indicating the direction that the change\n\
      \032  needs to be propagated. For example,\n\
      \032                <---  new file   c  [f]\n\
      \n\
      \032  indicates that the file c has been modified only in the second replica,\n\
      \032  and that the default action is therefore to propagate the new version\n\
      \032  to the first replica. To follow Unison\226\128\153s recommendation, press the \226\128\156f\226\128\157\n\
      \032  at the prompt.\n\
      \n\
      \032  If both replicas are modified and their contents are different, then\n\
      \032  the changes are in conflict: <-?-> is displayed to indicate that Unison\n\
      \032  needs guidance on which replica should override the other.\n\
      \032    new file  <-?->  new file   d/h  []\n\
      \n\
      \032  By default, neither version will be propagated and both replicas will\n\
      \032  remain as they are.\n\
      \n\
      \032  If both replicas have been modified but their new contents are the same\n\
      \032  (as with the file b), then no propagation is necessary and nothing is\n\
      \032  shown. Unison simply notes that the file is up to date.\n\
      \n\
      \032  These display conventions are used by both versions of the user\n\
      \032  interface. The only difference lies in the way in which Unison\226\128\153s\n\
      \032  default actions are either accepted or overridden by the user.\n\
      \n\
      \032  Textual Interface:\n\
      \032    * The status of each modified file is displayed, in turn. When the\n\
      \032      copies of a file in the two replicas are not identical, the user\n\
      \032      interface will ask for instructions as to how to propagate the\n\
      \032      change. If some default action is indicated (by an arrow), you can\n\
      \032      simply press Return to go on to the next changed file. If you want\n\
      \032      to do something different with this file, press \226\128\156<\226\128\157 or \226\128\156>\226\128\157 to force\n\
      \032      the change to be propagated from right to left or from left to\n\
      \032      right, or else press \226\128\156/\226\128\157 to skip this file and leave both replicas\n\
      \032      alone. When it reaches the end of the list of modified files,\n\
      \032      Unison will ask you one more time whether it should proceed with\n\
      \032      the updates that have been selected.\n\
      \032      When Unison stops to wait for input from the user, pressing \226\128\156?\226\128\157\n\
      \032      will always give a list of possible responses and their meanings.\n\
      \n\
      \032  Graphical Interface:\n\
      \032    * The main window shows all the files that have been modified in\n\
      \032      either a.tmp or b.tmp. To override a default action (or to select\n\
      \032      an action in the case when there is no default), first select the\n\
      \032      file, either by clicking on its name or by using the up- and\n\
      \032      down-arrow keys. Then press either the left-arrow or \226\128\156<\226\128\157 key (to\n\
      \032      cause the version in b.tmp to propagate to a.tmp) or the\n\
      \032      right-arrow or \226\128\156>\226\128\157 key (which makes the a.tmp version override\n\
      \032      b.tmp).\n\
      \032      Every keyboard command can also be invoked from the menus at the\n\
      \032      top of the user interface. (Conversely, each menu item is annotated\n\
      \032      with its keyboard equivalent, if it has one.)\n\
      \032      When you are satisfied with the directions for the propagation of\n\
      \032      changes as shown in the main window, click the \226\128\156Go\226\128\157 button to set\n\
      \032      them in motion. A check sign will be displayed next to each\n\
      \032      filename when the file has been dealt with.\n\
      \n\
      Remote Usage\n\
      \n\
      \032  Next, we\226\128\153ll get Unison set up to synchronize replicas on two different\n\
      \032  machines.\n\
      \n\
      \032  NB: Unison has not been designed to run with elevated privileges (e.g.\n\
      \032  setuid), and it has not been audited for that environment. Therefore\n\
      \032  Unison should be run with the userid of the owner of the files to be\n\
      \032  synchronized, and should never be run setuid or similar. (Problems\n\
      \032  encountered when running setuid etc. must be reproduced without setuid\n\
      \032  before being reported as bugs.)\n\
      \n\
      \032  Follow the instructions in the Installation section to download or\n\
      \032  build an executable version of Unison on the server machine, and\n\
      \032  install it somewhere on your search path. (It doesn\226\128\153t matter whether\n\
      \032  you install the textual or graphical version, since the copy of Unison\n\
      \032  on the server doesn\226\128\153t need to display any user interface at all.)\n\
      \n\
      \032  It is important that the version of Unison installed on the server\n\
      \032  machine is the same as the version of Unison on the client machine. But\n\
      \032  some flexibility on the version of Unison at the client side can be\n\
      \032  achieved by using the -addversionno option; see the section\n\
      \032  \226\128\156Preferences\226\128\157 .\n\
      \n\
      \032  Now there is a decision to be made. Unison provides two methods for\n\
      \032  communicating between the client and the server:\n\
      \032    * Remote shell method: To use this method, you must have some way of\n\
      \032      invoking remote commands on the server from the client\226\128\153s command\n\
      \032      line, using a facility such as ssh. This method is more convenient\n\
      \032      (since there is no need to manually start a \226\128\156unison server\226\128\157 process\n\
      \032      on the server) and also more secure, assuming you are using ssh).\n\
      \032    * TCP socket method: This method requires only that you can get TCP\n\
      \032      packets from the client to the server and back. It is insecure and\n\
      \032      should not be used.\n\
      \032    * Unix socket method: This method only works within a single machine.\n\
      \032      It is similar to the TCP sockets method, but it is possible to\n\
      \032      configure it securely.\n\
      \n\
      \032  Decide which of these you want to try, and continue with the section\n\
      \032  \226\128\156Remote Shell Method\226\128\157 or the section \226\128\156Socket Method\226\128\157 , as appropriate.\n\
      \n\
      Remote Shell Method\n\
      \n\
      \032  The standard remote shell facility on Unix systems is ssh.\n\
      \n\
      \032  Running ssh requires some coordination between the client and server\n\
      \032  machines to establish that the client is allowed to invoke commands on\n\
      \032  the server; please refer to the ssh documentation for information on\n\
      \032  how to set this up.\n\
      \n\
      \032  First, test that we can invoke Unison on the server from the client.\n\
      \032  Typing\n\
      \032       ssh remotehostname unison -version\n\
      \n\
      \032  should print the same version information as running\n\
      \032       unison -version\n\
      \n\
      \032  locally on the client. If remote execution fails, then either something\n\
      \032  is wrong with your ssh setup (e.g., \226\128\156permission denied\226\128\157) or else the\n\
      \032  search path that\226\128\153s being used when executing commands on the server\n\
      \032  doesn\226\128\153t contain the unison executable (e.g., \226\128\156command not found\226\128\157).\n\
      \n\
      \032  Create a test directory a.tmp in your home directory on the client\n\
      \032  machine.\n\
      \n\
      \032  Test that the local unison client can start and connect to the remote\n\
      \032  server. Type\n\
      \032         unison -testServer a.tmp ssh://remotehostname/a.tmp\n\
      \n\
      \032  Now cd to your home directory and type:\n\
      \032         unison a.tmp ssh://remotehostname/a.tmp\n\
      \n\
      \032  The result should be that the entire directory a.tmp is propagated from\n\
      \032  the client to your home directory on the server.\n\
      \n\
      \032  After finishing the first synchronization, change a few files and try\n\
      \032  synchronizing again. You should see similar results as in the local\n\
      \032  case.\n\
      \n\
      \032  If your user name on the server is not the same as on the client, you\n\
      \032  need to specify it on the command line:\n\
      \032         unison a.tmp ssh://username@remotehostname/a.tmp\n\
      \n\
      \032  Notes:\n\
      \032    * If you want to put a.tmp some place other than your home directory\n\
      \032      on the remote host, you can give an absolute path for it by adding\n\
      \032      an extra slash between remotehostname and the beginning of the\n\
      \032      path:\n\
      \032         unison a.tmp ssh://remotehostname//absolute/path/to/a.tmp\n\
      \n\
      \032    * You can give an explicit path for the unison executable on the\n\
      \032      server by using the command-line option \"-servercmd\n\
      \032      /full/path/name/of/unison\" or adding\n\
      \032      \"servercmd=/full/path/name/of/unison\" to your profile (see the\n\
      \032      section \226\128\156Profiles\226\128\157 ). Similarly, you can specify an explicit path\n\
      \032      for the ssh program using the \"-sshcmd\" option. Extra arguments can\n\
      \032      be passed to ssh by setting the -sshargs preference.\n\
      \032    * By leveraging \"-sshcmd\" and \"-sshargs\", you can effectively use any\n\
      \032      remote shell program, not just ssh; just remember that the roots\n\
      \032      are still specified with ssh as the protocol, that is, they have to\n\
      \032      start with \"ssh://\".\n\
      \n\
      Socket Method\n\
      \n\
      \032  To run Unison over a socket connection, you must start a Unison daemon\n\
      \032  process on the server. This process runs continuously, waiting for\n\
      \032  connections over a given socket from client machines running Unison and\n\
      \032  processing their requests in turn.\n\
      \n\
      \032  Since the socket method is not used by many people, its functionality\n\
      \032  is rather limited. For example, the server can only deal with one\n\
      \032  client at a time.\n\
      \n\
      \032  Note that the Unison daemon process is always started with a\n\
      \032  command-line argument; not from a profile.\n\
      \n\
      TCP Sockets\n\
      \n\
      \032    Warning: The TCP socket method is insecure: not only are the texts\n\
      \032    of your changes transmitted over the network in unprotected form, it\n\
      \032    is also possible for anyone in the world to connect to the server\n\
      \032    process and read out the contents of your filesystem! (Of course, to\n\
      \032    do this they must understand the protocol that Unison uses to\n\
      \032    communicate between client and server, but all they need for this is\n\
      \032    a copy of the Unison sources.) The socket method is provided only\n\
      \032    for expert users with specific needs; everyone else should use the\n\
      \032    ssh method.\n\
      \n\
      \032  To start the daemon for connections over a TCP socket, type\n\
      \032      unison -socket NNNN\n\
      \n\
      \032  on the server machine, where NNNN is the TCP port number that the\n\
      \032  daemon should listen on for connections from clients. (NNNN can be any\n\
      \032  large number that is not being used by some other program; if NNNN is\n\
      \032  already in use, Unison will exit with an error message.)\n\
      \n\
      \032  Create a test directory a.tmp in your home directory on the client\n\
      \032  machine. Now type:\n\
      \032      unison a.tmp socket://remotehostname:NNNN/a.tmp\n\
      \n\
      \032  Note that paths specified by the client will be interpreted relative to\n\
      \032  the directory in which you start the server process; this behavior is\n\
      \032  different from the ssh case, where the path is relative to your home\n\
      \032  directory on the server. The result should be that the entire directory\n\
      \032  a.tmp is propagated from the client to the server (a.tmp will be\n\
      \032  created on the server in the directory that the server was started\n\
      \032  from). After finishing the first synchronization, change a few files\n\
      \032  and try synchronizing again. You should see similar results as in the\n\
      \032  local case.\n\
      \n\
      \032  By default Unison will listen for incoming connections on all\n\
      \032  interfaces. If you want to limit this to certain interfaces or\n\
      \032  addresses then you can use the -listen command-line argument,\n\
      \032  specifying a host name or an IP address to listen on. -listen can be\n\
      \032  given multiple times to listen on several addresses.\n\
      \n\
      Unix Domain Sockets\n\
      \n\
      \032  To start the daemon for connections over a Unix domain socket, type\n\
      \032      unison -socket PPPP\n\
      \n\
      \032  where PPPP is the path to a Unix socket that the daemon should open for\n\
      \032  connections from clients. (PPPP can be any absolute or relative path\n\
      \032  the server process has access to but it must not exist yet; the socket\n\
      \032  is created at that path when the daemon process is started.) You are\n\
      \032  responsible for securing access to the socket path. For example, this\n\
      \032  can be done by controlling the permissions of socket\226\128\153s parent\n\
      \032  directory, or ensuring a restrictive umask value when starting Unison.\n\
      \n\
      \032  Clients can connect to a server over a Unix domain socket by specifying\n\
      \032  the absolute or relative path to the socket, instead of a server\n\
      \032  address and port number:\n\
      \032      unison a.tmp socket://{path/to/unix/socket}/a.tmp\n\
      \n\
      \032  (socket path is enclosed in curly braces).\n\
      \n\
      \032  Note that Unix domain sockets are local sockets (they exist in the\n\
      \032  filesystem namespace). One could use Unixs socket remotely, by\n\
      \032  forwarding access to the socket by other means, for example by using\n\
      \032  spiped secure pipe daemon.\n\
      \n\
      Using Unison for All Your Files\n\
      \n\
      \032  Once you are comfortable with the basic operation of Unison, you may\n\
      \032  find yourself wanting to use it regularly to synchronize your commonly\n\
      \032  used files. There are several possible ways of going about this:\n\
      \032   1. Synchronize your whole home directory, using the Ignore facility\n\
      \032      (see the section \226\128\156Ignoring Paths\226\128\157 ) to avoid synchronizing\n\
      \032      temporary files and things that only belong on one host.\n\
      \032   2. Create a subdirectory called shared (or current, or whatever) in\n\
      \032      your home directory on each host, and put all the files you want to\n\
      \032      synchronize into this directory.\n\
      \032   3. Create a subdirectory called shared (or current, or whatever) in\n\
      \032      your home directory on each host, and put links to all the files\n\
      \032      you want to synchronize into this directory. Use the follow\n\
      \032      preference (see the section \226\128\156Symbolic Links\226\128\157 ) to make Unison treat\n\
      \032      these links as transparent.\n\
      \032   4. Make your home directory the root of the synchronization, but tell\n\
      \032      Unison to synchronize only some of the files and subdirectories\n\
      \032      within it on any given run. This can be accomplished by using the\n\
      \032      -path switch on the command line:\n\
      \032      unison /home/username ssh://remotehost//home/username -path shared\n\
      \n\
      \032      The -path option can be used as many times as needed, to\n\
      \032      synchronize several files or subdirectories:\n\
      \032      unison /home/username ssh://remotehost//home/username \\\n\
      \032         -path shared \\\n\
      \032         -path pub \\\n\
      \032         -path .netscape/bookmarks.html\n\
      \n\
      \032      These -path arguments can also be put in your preference file. See\n\
      \032      the section \226\128\156Preferences\226\128\157 for an example.\n\
      \n\
      \032  Most people find that they only need to maintain a profile (or\n\
      \032  profiles) on one of the hosts that they synchronize, since Unison is\n\
      \032  always initiated from this host. (For example, if you\226\128\153re synchronizing\n\
      \032  a laptop with a fileserver, you\226\128\153ll probably always run Unison on the\n\
      \032  laptop.) This is a bit different from the usual situation with\n\
      \032  asymmetric mirroring programs like rdist, where the mirroring operation\n\
      \032  typically needs to be initiated from the machine with the most recent\n\
      \032  changes. the section \226\128\156Profiles\226\128\157 covers the syntax of Unison profiles,\n\
      \032  together with some sample profiles.\n\
      \n\
      \032  Some tips on improving Unison\226\128\153s performance can be found on the\n\
      \032  Frequently Asked Questions page\n\
      \032  (http://www.cis.upenn.edu/~bcpierce/unison/faq.html).\n\
      \n\
      Using Unison to Synchronize More Than Two Machines\n\
      \n\
      \032  Unison is designed for synchronizing pairs of replicas. However, it is\n\
      \032  possible to use it to keep larger groups of machines in sync by\n\
      \032  performing multiple pairwise synchronizations.\n\
      \n\
      \032  If you need to do this, the most reliable way to set things up is to\n\
      \032  organize the machines into a \226\128\156star topology,\226\128\157 with one machine\n\
      \032  designated as the \226\128\156hub\226\128\157 and the rest as \226\128\156spokes,\226\128\157 and with each spoke\n\
      \032  machine synchronizing only with the hub. The big advantage of the star\n\
      \032  topology is that it eliminates the possibility of confusing \226\128\156spurious\n\
      \032  conflicts\226\128\157 arising from the fact that a separate archive is maintained\n\
      \032  by Unison for every pair of hosts that it synchronizes.\n\
      \n\
      Going Further\n\
      \n\
      \032  On-line documentation for the various features of Unison can be\n\
      \032  obtained either by typing\n\
      \032       unison -doc topics\n\
      \n\
      \032  at the command line, or by selecting the Help menu in the graphical\n\
      \032  user interface. The same information is also available in a typeset\n\
      \032  User\226\128\153s Manual (HTML or PostScript format) through\n\
      \032  http://www.cis.upenn.edu/~bcpierce/unison.\n\
      \n\
      \032  If you use Unison regularly, you should subscribe to one of the mailing\n\
      \032  lists, to receive announcements of new versions. See the section\n\
      \032  \226\128\156Mailing Lists and Bug Reporting\226\128\157 .\n\
      \n\
      "))
::
    ("basics", ("Basic Concepts", 
     "Basic Concepts\n\
      \n\
      \032  To understand how Unison works, it is necessary to discuss a few\n\
      \032  straightforward concepts.\n\
      \n\
      \032  These concepts are developed more rigorously and at more length in a\n\
      \032  number of papers, available at\n\
      \032  http://www.cis.upenn.edu/~bcpierce/papers. But the informal\n\
      \032  presentation here should be enough for most users.\n\
      \n\
      Roots\n\
      \n\
      \032  A replica\226\128\153s root tells Unison where to find a set of files to be\n\
      \032  synchronized, either on the local machine or on a remote host. For\n\
      \032  example,\n\
      \032     relative/path/of/root\n\
      \n\
      \032  specifies a local root relative to the directory where Unison is\n\
      \032  started, while\n\
      \032     /absolute/path/of/root\n\
      \n\
      \032  specifies a root relative to the top of the local filesystem,\n\
      \032  independent of where Unison is running. Remote roots can begin with\n\
      \032  ssh:// to indicate that the remote server should be started with ssh:\n\
      \032     ssh://remotehost//absolute/path/of/root\n\
      \032     ssh://user@remotehost/relative/path/of/root\n\
      \n\
      \032  If the remote server is already running (in the socket mode), then the\n\
      \032  syntax\n\
      \032     socket://remotehost:portnum//absolute/path/of/root\n\
      \032     socket://remotehost:portnum/relative/path/of/root\n\
      \032     socket://[IPv6literal]:portnum/path\n\
      \n\
      \032  is used to specify the hostname and the port that the client Unison\n\
      \032  should use to contact it. Syntax\n\
      \032     socket://{path/of/socket}//absolute/path/of/root\n\
      \032     socket://{path/of/socket}/relative/path/of/root\n\
      \n\
      \032  is used to specify the Unix domain socket the client Unison should use\n\
      \032  to contact the server.\n\
      \n\
      \032  The syntax for roots is based on that of URIs (described in RFC 2396).\n\
      \032  The full grammar is:\n\
      \032 replica ::= [protocol:]//[user@][host][:port][/path]\n\
      \032          |  path\n\
      \n\
      \032 protocol ::= file\n\
      \032           |  socket\n\
      \032           |  ssh\n\
      \n\
      \032 user ::= [-_a-zA-Z0-9]+\n\
      \n\
      \032 host ::= [-_a-zA-Z0-9.]+\n\
      \032       |  \\[ [a-f0-9:.]+ zone? \\]    IPv6 literals (no future format).\n\
      \032       |  { [^}]+ }                   For Unix domain sockets only.\n\
      \n\
      \032 zone ::= %[-_a-zA-Z0-9~%.]+\n\
      \n\
      \032 port ::= [0-9]+\n\
      \n\
      \032  When path is given without any protocol prefix, the protocol is assumed\n\
      \032  to be file:. Under Windows, it is possible to synchronize with a remote\n\
      \032  directory using the file: protocol over the Windows Network\n\
      \032  Neighborhood. For example,\n\
      \032      unison foo //host/drive/bar\n\
      \n\
      \032  synchronizes the local directory foo with the directory drive:\\bar on\n\
      \032  the machine host, provided that host is accessible via Network\n\
      \032  Neighborhood. When the file: protocol is used in this way, there is no\n\
      \032  need for a Unison server to be running on the remote host. However,\n\
      \032  running Unison this way is only a good idea if the remote host is\n\
      \032  reached by a very fast network connection, since the full contents of\n\
      \032  every file in the remote replica will have to be transferred to the\n\
      \032  local machine to detect updates.\n\
      \n\
      \032  The names of roots are canonized by Unison before it uses them to\n\
      \032  compute the names of the corresponding archive files, so\n\
      \032  //saul//home/bcpierce/common and //saul.cis.upenn.edu/common will be\n\
      \032  recognized as the same replica under different names.\n\
      \n\
      Paths\n\
      \n\
      \032  A path refers to a point within a set of files being synchronized; it\n\
      \032  is specified relative to the root of the replica.\n\
      \n\
      \032  Formally, a path is just a sequence of names, separated by /. Note that\n\
      \032  the path separator character is always a forward slash, no matter what\n\
      \032  operating system Unison is running on. Forward slashes are converted to\n\
      \032  backslashes as necessary when paths are converted to filenames in the\n\
      \032  local filesystem on a particular host. (For example, suppose that we\n\
      \032  run Unison on a Windows system, synchronizing the local root c:\\pierce\n\
      \032  with the root ssh://saul.cis.upenn.edu/home/bcpierce on a Unix server.\n\
      \032  Then the path current/todo.txt refers to the file\n\
      \032  c:\\pierce\\current\\todo.txt on the client and\n\
      \032  /home/bcpierce/current/todo.txt on the server.)\n\
      \n\
      \032  The empty path (i.e., the empty sequence of names) denotes the whole\n\
      \032  replica. Unison displays the empty path as \226\128\156[root].\226\128\157\n\
      \n\
      \032  If p is a path and q is a path beginning with p, then q is said to be a\n\
      \032  descendant of p. (Each path is also a descendant of itself.)\n\
      \n\
      What is an Update?\n\
      \n\
      \032  The contents of a path p in a particular replica could be a file, a\n\
      \032  directory, a symbolic link, or absent (if p does not refer to anything\n\
      \032  at all in that replica). More specifically:\n\
      \032    * If p refers to an ordinary file, then the contents of p are the\n\
      \032      actual contents of this file (a string of bytes) plus the current\n\
      \032      permission bits of the file.\n\
      \032    * If p refers to a symbolic link, then the contents of p are just the\n\
      \032      string specifying where the link points.\n\
      \032    * If p refers to a directory, then the contents of p are just the\n\
      \032      token \226\128\156DIRECTORY\226\128\157 plus the current permission bits of the\n\
      \032      directory.\n\
      \032    * If p does not refer to anything in this replica, then the contents\n\
      \032      of p are the token \226\128\156ABSENT.\226\128\157\n\
      \n\
      \032  Unison keeps a record of the contents of each path after each\n\
      \032  successful synchronization of that path (i.e., it remembers the\n\
      \032  contents at the last moment when they were the same in the two\n\
      \032  replicas).\n\
      \n\
      \032  We say that a path is updated (in some replica) if its current contents\n\
      \032  are different from its contents the last time it was successfully\n\
      \032  synchronized. Note that whether a path is updated has nothing to do\n\
      \032  with its last modification time\226\128\148Unison considers only the contents when\n\
      \032  determining whether an update has occurred. This means that touching a\n\
      \032  file without changing its contents will not be recognized as an update.\n\
      \032  A file can even be changed several times and then changed back to its\n\
      \032  original contents; as long as Unison is only run at the end of this\n\
      \032  process, no update will be recognized.\n\
      \n\
      \032  What Unison actually calculates is a close approximation to this\n\
      \032  definition; see the section \226\128\156Caveats and Shortcomings\226\128\157 .\n\
      \n\
      What is a Conflict?\n\
      \n\
      \032  A path is said to be conflicting if the following conditions all hold:\n\
      \032   1. it has been updated in one replica,\n\
      \032   2. it or any of its descendants has been updated in the other replica,\n\
      \032      and\n\
      \032   3. its contents in the two replicas are not identical.\n\
      \n\
      Reconciliation\n\
      \n\
      \032  Unison operates in several distinct stages:\n\
      \032   1. On each host, it compares its archive file (which records the state\n\
      \032      of each path in the replica when it was last synchronized) with the\n\
      \032      current contents of the replica, to determine which paths have been\n\
      \032      updated.\n\
      \032   2. It checks for \226\128\156false conflicts\226\128\157 \226\128\148 paths that have been updated on\n\
      \032      both replicas, but whose current values are identical. These paths\n\
      \032      are silently marked as synchronized in the archive files in both\n\
      \032      replicas.\n\
      \032   3. It displays all the updated paths to the user. For updates that do\n\
      \032      not conflict, it suggests a default action (propagating the new\n\
      \032      contents from the updated replica to the other). Conflicting\n\
      \032      updates are just displayed. The user is given an opportunity to\n\
      \032      examine the current state of affairs, change the default actions\n\
      \032      for nonconflicting updates, and choose actions for conflicting\n\
      \032      updates.\n\
      \032   4. It performs the selected actions, one at a time. Each action is\n\
      \032      performed by first transferring the new contents to a temporary\n\
      \032      file on the receiving host, then atomically moving them into place.\n\
      \032   5. It updates its archive files to reflect the new state of the\n\
      \032      replicas.\n\
      \n\
      "))
::
    ("failures", ("Invariants", 
     "Invariants\n\
      \n\
      \032  Given the importance and delicacy of the job that it performs, it is\n\
      \032  important to understand both what a synchronizer does under normal\n\
      \032  conditions and what can happen under unusual conditions such as system\n\
      \032  crashes and communication failures.\n\
      \n\
      \032  Unison is careful to protect both its internal state and the state of\n\
      \032  the replicas at every point in this process. Specifically, the\n\
      \032  following guarantees are enforced:\n\
      \032    * At every moment, each path in each replica has either (1) its\n\
      \032      original contents (i.e., no change at all has been made to this\n\
      \032      path), or (2) its correct final contents (i.e., the value that the\n\
      \032      user expected to be propagated from the other replica).\n\
      \032    * At every moment, the information stored on disk about Unison\226\128\153s\n\
      \032      private state can be either (1) unchanged, or (2) updated to\n\
      \032      reflect those paths that have been successfully synchronized.\n\
      \n\
      \032  The upshot is that it is safe to interrupt Unison at any time, either\n\
      \032  manually or accidentally. [Caveat: the above is almost true there are\n\
      \032  occasionally brief periods where it is not (and, because of shortcoming\n\
      \032  of the Posix filesystem API, cannot be); in particular, when it is\n\
      \032  copying a file onto a directory or vice versa, it must first move the\n\
      \032  original contents out of the way. If Unison gets interrupted during one\n\
      \032  of these periods, some manual cleanup may be required. In this case, a\n\
      \032  file called DANGER.README will be left in the .unison directory,\n\
      \032  containing information about the operation that was interrupted. The\n\
      \032  next time you try to run Unison, it will notice this file and warn you\n\
      \032  about it.]\n\
      \n\
      \032  If an interruption happens while it is propagating updates, then there\n\
      \032  may be some paths for which an update has been propagated but which\n\
      \032  have not been marked as synchronized in Unison\226\128\153s archives. This is no\n\
      \032  problem: the next time Unison runs, it will detect changes to these\n\
      \032  paths in both replicas, notice that the contents are now equal, and\n\
      \032  mark the paths as successfully updated when it writes back its private\n\
      \032  state at the end of this run.\n\
      \n\
      \032  If Unison is interrupted, it may sometimes leave temporary working\n\
      \032  files (with suffix .tmp) in the replicas. It is safe to delete these\n\
      \032  files. Also, if the backups flag is set, Unison will leave around old\n\
      \032  versions of files that it overwrites, with names like\n\
      \032  file.0.unison.bak. These can be deleted safely when they are no longer\n\
      \032  wanted.\n\
      \n\
      \032  Unison is not bothered by clock skew between the different hosts on\n\
      \032  which it is running. It only performs comparisons between timestamps\n\
      \032  obtained from the same host, and the only assumption it makes about\n\
      \032  them is that the clock on each system always runs forward.\n\
      \n\
      \032  If Unison finds that its archive files have been deleted (or that the\n\
      \032  archive format has changed and they cannot be read, or that they don\226\128\153t\n\
      \032  exist because this is the first run of Unison on these particular\n\
      \032  roots), it takes a conservative approach: it behaves as though the\n\
      \032  replicas had both been completely empty at the point of the last\n\
      \032  synchronization. The effect of this is that, on the first run, files\n\
      \032  that exist in only one replica will be propagated to the other, while\n\
      \032  files that exist in both replicas but are unequal will be marked as\n\
      \032  conflicting.\n\
      \n\
      \032  Touching a file without changing its contents should never affect\n\
      \032  whether or not Unison does an update. (When running with the fastcheck\n\
      \032  preference set to true\226\128\148the default on Unix systems\226\128\148Unison uses file\n\
      \032  modtimes for a quick first pass to tell which files have definitely not\n\
      \032  changed; then, for each file that might have changed, it computes a\n\
      \032  fingerprint of the file\226\128\153s contents and compares it against the\n\
      \032  last-synchronized contents. Also, the -times option allows you to\n\
      \032  synchronize file times, but it does not cause identical files to be\n\
      \032  changed; Unison will only modify the file times.)\n\
      \n\
      \032  It is safe to \226\128\156brainwash\226\128\157 Unison by deleting its archive files on both\n\
      \032  replicas. The next time it runs, it will assume that all the files it\n\
      \032  sees in the replicas are new.\n\
      \n\
      \032  It is safe to modify files while Unison is working. If Unison discovers\n\
      \032  that it has propagated an out-of-date change, or that the file it is\n\
      \032  updating has changed on the target replica, it will signal a failure\n\
      \032  for that file. Run Unison again to propagate the latest change.\n\
      \n\
      \032  Changes to the ignore patterns from the user interface (e.g., using the\n\
      \032  \226\128\152i\226\128\153 key) are immediately reflected in the current profile.\n\
      \n\
      Caveats and Shortcomings\n\
      \n\
      \032  Here are some things to be careful of when using Unison.\n\
      \032    * In the interests of speed, the update detection algorithm may\n\
      \032      (depending on which OS architecture that you run Unison on)\n\
      \032      actually use an approximation to the definition given in the\n\
      \032      section \226\128\156What is an Update?\226\128\157 .\n\
      \032      In particular, the Unix implementation does not compare the actual\n\
      \032      contents of files to their previous contents, but simply looks at\n\
      \032      each file\226\128\153s inode number and modtime; if neither of these have\n\
      \032      changed, then it concludes that the file has not been changed.\n\
      \032      Under normal circumstances, this approximation is safe, in the\n\
      \032      sense that it may sometimes detect \226\128\156false updates\226\128\157 but will never\n\
      \032      miss a real one. However, it is possible to fool it, for example by\n\
      \032      using retouch to change a file\226\128\153s modtime back to a time in the\n\
      \032      past.\n\
      \032    * If you synchronize between a single-user filesystem and a shared\n\
      \032      Unix server, you should pay attention to your permission bits: by\n\
      \032      default, Unison will synchronize permissions verbatim, which may\n\
      \032      leave group-writable files on the server that could be written over\n\
      \032      by a lot of people.\n\
      \032      You can control this by setting your umask on both computers to\n\
      \032      something like 022, masking out the \226\128\156world write\226\128\157 and \226\128\156group write\226\128\157\n\
      \032      permission bits.\n\
      \032      Unison does not synchronize the setuid and setgid bits, for\n\
      \032      security.\n\
      \032    * The graphical user interface is single-threaded. This means that if\n\
      \032      Unison is performing some long-running operation, the display will\n\
      \032      not be repainted until it finishes. We recommend not trying to do\n\
      \032      anything with the user interface while Unison is in the middle of\n\
      \032      detecting changes or propagating files.\n\
      \032    * Unison does not understand hard links.\n\
      \032    * It is important to be a little careful when renaming directories\n\
      \032      containing ignored files.\n\
      \032      For example, suppose Unison is synchronizing directory A between\n\
      \032      the two machines called the \226\128\156local\226\128\157 and the \226\128\156remote\226\128\157 machine;\n\
      \032      suppose directory A contains a subdirectory D; and suppose D on the\n\
      \032      local machine contains a file or subdirectory P that matches an\n\
      \032      ignore directive in the profile used to synchronize. Thus path\n\
      \032      A/D/P exists on the local machine but not on the remote machine.\n\
      \032      If D is renamed to D\226\128\153 on the remote machine, and this change is\n\
      \032      propagated to the local machine, all such files or subdirectories P\n\
      \032      will be deleted. This is because Unison sees the rename as a delete\n\
      \032      and a separate create: it deletes the old directory (including the\n\
      \032      ignored files) and creates a new one (not including the ignored\n\
      \032      files, since they are completely invisible to it).\n\
      \n\
      "))
::
    ("", ("Reference Guide", 
     "Reference Guide\n\
      \n\
      \032  This section covers the features of Unison in detail.\n\
      \n\
      "))
::
    ("running", ("Running Unison", 
     "Running Unison\n\
      \n\
      \032  There are several ways to start Unison.\n\
      \032    * Typing \226\128\156unison profile\226\128\157 on the command line. Unison will look for a\n\
      \032      file profile.prf in the .unison directory. If this file does not\n\
      \032      specify a pair of roots, Unison will prompt for them and add them\n\
      \032      to the information specified by the profile.\n\
      \032    * Typing \226\128\156unison profile root1 root2\226\128\157 on the command line. In this\n\
      \032      case, Unison will use profile, which should not contain any root\n\
      \032      directives.\n\
      \032    * Typing \226\128\156unison root1 root2\226\128\157 on the command line. This has the same\n\
      \032      effect as typing \226\128\156unison default root1 root2.\226\128\157\n\
      \032    * Typing just \226\128\156unison\226\128\157 (or invoking Unison by clicking on a desktop\n\
      \032      icon). In this case, Unison will ask for the profile to use for\n\
      \032      synchronization (or create a new one, if necessary).\n\
      \n\
      The .unison Directory\n\
      \n\
      \032  Unison stores a variety of information in a private directory on each\n\
      \032  host. If the environment variable UNISON is defined, then its value\n\
      \032  will be used as the path/folder name for this directory. This can be\n\
      \032  just a name, or a path.\n\
      \n\
      \032  A name on it\226\128\153s own, for example UNISON=mytestname will place a folder\n\
      \032  in the same directory that the Unison binary was run in, with that\n\
      \032  name. Using a path like UNISON=../mytestname2 will place that folder in\n\
      \032  the folder above where the Unison binary was run from.\n\
      \n\
      \032  If UNISON is not defined, then the directory depends on which operating\n\
      \032  system you are using. In Unix, the default is to use $HOME/.unison. In\n\
      \032  Windows, if the environment variable USERPROFILE is defined, then the\n\
      \032  directory will be $USERPROFILE\\.unison; otherwise if HOME is defined,\n\
      \032  it will be $HOME\\.unison; otherwise, it will be c:\\.unison. On OS X,\n\
      \032  $HOME/.unison will be used if it is present, but\n\
      \032  $HOME/Library/Application Support/Unison will be created and used by\n\
      \032  default.\n\
      \n\
      \032  The archive file for each replica is found in the .unison directory on\n\
      \032  that replica\226\128\153s host. Profiles (described below) are always taken from\n\
      \032  the .unison directory on the client host.\n\
      \n\
      \032  Note that Unison maintains a completely different set of archive files\n\
      \032  for each pair of roots.\n\
      \n\
      \032  We do not recommend synchronizing the whole .unison directory, as this\n\
      \032  will involve frequent propagation of large archive files. It should be\n\
      \032  safe to do it, though, if you really want to. Synchronizing just the\n\
      \032  profile files in the .unison directory is definitely OK.\n\
      \n\
      Archive Files\n\
      \n\
      \032  The name of the archive file on each replica is calculated from\n\
      \032    * the canonical names of all the hosts (short names like saul are\n\
      \032      converted into full addresses like saul.cis.upenn.edu),\n\
      \032    * the paths to the replicas on all the hosts (again, relative\n\
      \032      pathnames, symbolic links, etc. are converted into full, absolute\n\
      \032      paths), and\n\
      \032    * an internal version number that is changed whenever a new Unison\n\
      \032      release changes the format of the information stored in the\n\
      \032      archive.\n\
      \n\
      \032  This method should work well for most users. However, it is\n\
      \032  occasionally useful to change the way archive names are generated.\n\
      \032  Unison provides two ways of doing this.\n\
      \n\
      \032  The function that finds the canonical hostname of the local host (which\n\
      \032  is used, for example, in calculating the name of the archive file used\n\
      \032  to remember which files have been synchronized) normally uses the\n\
      \032  gethostname operating system call. However, if the environment variable\n\
      \032  UNISONLOCALHOSTNAME is set, its value will be used instead. This makes\n\
      \032  it easier to use Unison in situations where a machine\226\128\153s name changes\n\
      \032  frequently (e.g., because it is a laptop and gets moved around a lot).\n\
      \n\
      \032  A more powerful way of changing archive names is provided by the\n\
      \032  rootalias preference. The preference file may contain any number of\n\
      \032  lines of the form:\n\
      \032   rootalias = //hostnameA//path-to-replicaA -> //hostnameB/path-to-replicaB\n\
      \n\
      \032  When calculating the name of the archive files for a given pair of\n\
      \032  roots, Unison replaces any root that matches the left-hand side of any\n\
      \032  rootalias rule by the corresponding right-hand side.\n\
      \n\
      \032  So, if you need to relocate a root on one of the hosts, you can add a\n\
      \032  rule of the form:\n\
      \032   rootalias = //new-hostname//new-path -> //old-hostname/old-path\n\
      \n\
      \032  Note that root aliases are case-sensitive, even on case-insensitive\n\
      \032  file systems.\n\
      \n\
      \032  Warning: The rootalias option is dangerous and should only be used if\n\
      \032  you are sure you know what you\226\128\153re doing. In particular, it should only\n\
      \032  be used if you are positive that either (1) both the original root and\n\
      \032  the new alias refer to the same set of files, or (2) the files have\n\
      \032  been relocated so that the original name is now invalid and will never\n\
      \032  be used again. (If the original root and the alias refer to different\n\
      \032  sets of files, Unison\226\128\153s update detector could get confused.) After\n\
      \032  introducing a new rootalias, it is a good idea to run Unison a few\n\
      \032  times interactively (with the batch flag off, etc.) and carefully check\n\
      \032  that things look reasonable\226\128\148in particular, that update detection is\n\
      \032  working as expected.\n\
      \n\
      Preferences\n\
      \n\
      \032  Many details of Unison\226\128\153s behavior are configurable by user-settable\n\
      \032  \226\128\156preferences.\226\128\157\n\
      \n\
      \032  Some preferences are boolean-valued; these are often called flags.\n\
      \032  Others take numeric or string arguments, indicated in the preferences\n\
      \032  list by n or xxx. Some string arguments take the backslash as an escape\n\
      \032  to include the next character literally; this is mostly useful to\n\
      \032  escape a space or the backslash; a trailing backslash is ignored and is\n\
      \032  useful to protect a trailing whitespace in the string that would\n\
      \032  otherwise be trimmed. Most of the string preferences can be given\n\
      \032  several times; the arguments are accumulated into a list internally.\n\
      \n\
      \032  There are two ways to set the values of preferences: temporarily, by\n\
      \032  providing command-line arguments to a particular run of Unison, or\n\
      \032  permanently, by adding commands to a profile in the .unison directory\n\
      \032  on the client host. The order of preferences (either on the command\n\
      \032  line or in preference files) is not significant. On the command line,\n\
      \032  preferences and other arguments (the profile name and roots) can be\n\
      \032  intermixed in any order.\n\
      \n\
      \032  To set the value of a preference p from the command line, add an\n\
      \032  argument -p (for a boolean flag) or -p n or -p xxx (for a numeric or\n\
      \032  string preference) anywhere on the command line. To set a boolean flag\n\
      \032  to false on the command line, use -p=false.\n\
      \n\
      \032  Here are all the preferences supported by Unison. This list can be\n\
      \032  obtained by typing unison -help.\n\
      \n\
      Usage: unison [options]\n\
      \032   or unison root1 root2 [options]\n\
      \032   or unison profilename [options]\n\
      \n\
      Basic options:\n\
      \n\
      \032 General:\n\
      \032  -doc xxx            show documentation ('-doc topics' lists topics)\n\
      \032  -version            print version and exit\n\
      \n\
      \032 What to sync:\n\
      \032  -group              synchronize group attributes\n\
      \032  -ignore xxx         add a pattern to the ignore list\n\
      \032  -ignorenot xxx      add a pattern to the ignorenot list\n\
      \032  -nocreation xxx     prevent file creations on one replica\n\
      \032  -nodeletion xxx     prevent file deletions on one replica\n\
      \032  -noupdate xxx       prevent file updates and deletions on one replica\n\
      \032  -owner              synchronize owner\n\
      \032  -path xxx           path to synchronize\n\
      \032  -perms n            part of the permissions which is synchronized\n\
      \032  -root xxx           root of a replica (should be used exactly twice)\n\
      \032  -times              synchronize modification times\n\
      \n\
      \032 How to sync:\n\
      \032  -batch              batch mode: ask no questions at all\n\
      \n\
      \032 How to sync (text interface (CLI) only):\n\
      \032  -auto               automatically accept default (nonconflicting) actions\n\
      \032  -silent             print nothing except error messages\n\
      \032  -terse              suppress status messages\n\
      \n\
      \032 Text interface (CLI):\n\
      \032  -i                  interactive profile mode (text UI); command-line only\n\
      \n\
      Advanced options:\n\
      \n\
      \032 Fine-tune sync:\n\
      \032  -atomic xxx         add a pattern to the atomic list\n\
      \032  -follow xxx         add a pattern to the follow list\n\
      \032  -force xxx          force changes from this replica to the other\n\
      \032  -forcepartial xxx   add a pattern to the forcepartial list\n\
      \032  -ignorecase xxx     identify upper/lowercase filenames (true/false/default)\n\
      \032  -immutable xxx      add a pattern to the immutable list\n\
      \032  -immutablenot xxx   add a pattern to the immutablenot list\n\
      \032  -links xxx          allow the synchronization of symbolic links\n\
      \032                      (true/false/default)\n\
      \032  -merge xxx          add a pattern to the merge list\n\
      \032  -nocreationpartial xxx add a pattern to the nocreationpartial list\n\
      \032  -nodeletionpartial xxx add a pattern to the nodeletionpartial list\n\
      \032  -noupdatepartial xxx add a pattern to the noupdatepartial list\n\
      \032  -prefer xxx         choose this replica's version for conflicting changes\n\
      \032  -preferpartial xxx  add a pattern to the preferpartial list\n\
      \032  -rsrc xxx           synchronize resource forks (true/false/default)\n\
      \n\
      \032 How to sync:\n\
      \032  -backup xxx         add a pattern to the backup list\n\
      \032  -backupcurr xxx     add a pattern to the backupcurr list\n\
      \032  -backupcurrnot xxx  add a pattern to the backupcurrnot list\n\
      \032  -backupdir xxx      directory for storing centralized backups\n\
      \032  -backuploc xxx      where backups are stored ('local' or 'central')\n\
      \032  -backupnot xxx      add a pattern to the backupnot list\n\
      \032  -backupprefix xxx   prefix for the names of backup files\n\
      \032  -backups            (deprecated) keep backup copies of all files (see also\n\
      \032                      'backup')\n\
      \032  -backupsuffix xxx   a suffix to be added to names of backup files\n\
      \032  -confirmbigdel      ask about whole-replica (or path) deletes (default true)\n\
      \032  -confirmmerge       ask for confirmation before committing results of a merge\n\
      \032  -copyonconflict     keep copies of conflicting files\n\
      \032  -dontchmod          when set, never use the chmod system call\n\
      \032  -fastcheck xxx      do fast update detection (true/false/default)\n\
      \032  -fat                use appropriate options for FAT filesystems\n\
      \032  -ignoreinodenumbers ignore inode number changes when detecting updates\n\
      \032  -maxbackups n       number of backed up versions of a file\n\
      \032  -numericids         don't map uid/gid values by user/group names\n\
      \032  -sortbysize         list changed files by size, not name\n\
      \032  -sortfirst xxx      add a pattern to the sortfirst list\n\
      \032  -sortlast xxx       add a pattern to the sortlast list\n\
      \032  -sortnewfirst       list new before changed files\n\
      \n\
      \032 How to sync (text interface (CLI) only):\n\
      \032  -repeat xxx         synchronize repeatedly (text interface only)\n\
      \032  -retry n            re-try failed synchronizations N times (text ui only)\n\
      \n\
      \032 Text interface (CLI):\n\
      \032  -color xxx          use color output for text UI (true/false/default)\n\
      \032  -dumbtty            do not change terminal settings in text UI\n\
      \n\
      \032 Graphical interface (GUI):\n\
      \032  -height n           height (in lines) of main window in graphical interface\n\
      \n\
      \032 Remote connections:\n\
      \032  -addversionno       add version number to name of unison on server\n\
      \032  -clientHostName xxx set host name of client\n\
      \032  -halfduplex         (deprecated) force half-duplex communication with the\n\
      \032                      server\n\
      \032  -killserver         kill server when done (even when using sockets)\n\
      \032  -listen xxx         listen on this name or addr in server socket mode (can\n\
      \032                      repeat)\n\
      \032  -rsync              activate the rsync transfer mode (default true)\n\
      \032  -servercmd xxx      name of unison executable on remote server\n\
      \032  -socket xxx         act as a server on a socket\n\
      \032  -sshargs xxx        other arguments (if any) for remote shell command\n\
      \032  -sshcmd xxx         path to the ssh executable\n\
      \032  -stream             (deprecated) use a streaming protocol for transferring\n\
      \032                      file contents (default true)\n\
      \032  -testserver         exit immediately after the connection to the server\n\
      \032  -xferbycopying      optimize transfers using local copies (default true)\n\
      \n\
      \032 Archive management:\n\
      \032  -ignorearchives     ignore existing archive files\n\
      \n\
      \032 Other:\n\
      \032  -addprefsto xxx     file to add new prefs to\n\
      \032  -contactquietly     suppress the 'contacting server' message during startup\n\
      \032  -copymax n          maximum number of simultaneous copyprog transfers\n\
      \032  -copyprog xxx       external program for copying large files\n\
      \032  -copyprogrest xxx   variant of copyprog for resuming partial transfers\n\
      \032  -copyquoterem xxx   add quotes to remote file name for copyprog\n\
      \032                      (true/false/default)\n\
      \032  -copythreshold n    use copyprog on files bigger than this (if >=0, in Kb)\n\
      \032  -diff xxx           set command for showing differences between files\n\
      \032  -ignorelocks        ignore locks left over from previous run (dangerous!)\n\
      \032  -include xxx        include a profile's preferences\n\
      \032  -key xxx            define a keyboard shortcut for this profile (in some UIs)\n\
      \032  -label xxx          provide a descriptive string label for this profile\n\
      \032  -log                record actions in logfile (default true)\n\
      \032  -logfile xxx        logfile name\n\
      \032  -maxerrors n        maximum number of errors before a directory transfer is\n\
      \032                      aborted\n\
      \032  -maxsizethreshold n prevent transfer of files bigger than this (if >=0, in\n\
      \032                      Kb)\n\
      \032  -maxthreads n       maximum number of simultaneous file transfers\n\
      \032  -mountpoint xxx     abort if this path does not exist\n\
      \032  -rootalias xxx      register alias for canonical root names\n\
      \032  -showarchive        show 'true names' (for rootalias) of roots and archive\n\
      \032  -source xxx         include a file's preferences\n\
      \032  -ui xxx             select UI ('text' or 'graphic'); command-line only\n\
      \032  -unicode xxx        assume Unicode encoding in case insensitive mode\n\
      \032  -watch              when set, use a file watcher process to detect changes\n\
      \n\
      Expert options:\n\
      \032  -debug xxx          debug module xxx ('all' -> everything, 'verbose' -> more)\n\
      \032  -dumparchives       dump contents of archives just after loading\n\
      \032  -fastercheckUNSAFE  skip computing fingerprints for new files (experts only!)\n\
      \032  -selftest           run internal tests and exit\n\
      \n\
      \n\
      \032  Here, in more detail, is what they do. Many are discussed in greater\n\
      \032  detail in other sections of the manual.\n\
      \n\
      \032  It should be noted that some command-line arguments are handled\n\
      \032  specially during startup, including -doc, -help, -version, -socket, and\n\
      \032  -ui. They are expected to appear on the command-line only, not in a\n\
      \032  profile. In particular, -version and -doc will print to the standard\n\
      \032  output, so they only make sense if invoked from the command-line (and\n\
      \032  not a click-launched gui that has no standard output). Furthermore, the\n\
      \032  actions associated with these command-line arguments are executed\n\
      \032  without loading a profile or doing the usual command-line parsing.\n\
      \n\
      \032  addprefsto xxx\n\
      \032         By default, new preferences added by Unison (e.g., new ignore\n\
      \032         clauses) will be appended to whatever preference file Unison was\n\
      \032         told to load at the beginning of the run. Setting the preference\n\
      \032         addprefsto filename makes Unison add new preferences to the file\n\
      \032         named filename instead.\n\
      \n\
      \032  addversionno\n\
      \032         When this flag is set to true, Unison will use\n\
      \032         unison-currentmajorversionnumber instead of just unison as the\n\
      \032         remote server command (note that the minor version number is\n\
      \032         dropped \226\128\147 e.g., unison-2.51). This allows multiple binaries for\n\
      \032         different versions of unison to coexist conveniently on the same\n\
      \032         server: whichever version is run on the client, the same version\n\
      \032         will be selected on the server.\n\
      \n\
      \032  atomic xxx\n\
      \032         This preference specifies paths for directories whose contents\n\
      \032         will be considered as a group rather than individually when they\n\
      \032         are both modified. The backups are also made atomically in this\n\
      \032         case. The option backupcurr however has no effect on atomic\n\
      \032         directories.\n\
      \n\
      \032  auto\n\
      \032         When set to true, this flag causes the user interface to skip\n\
      \032         asking for confirmations on non-conflicting changes. (More\n\
      \032         precisely, when the user interface is done setting the\n\
      \032         propagation direction for one entry and is about to move to the\n\
      \032         next, it will skip over all non-conflicting entries and go\n\
      \032         directly to the next conflict.)\n\
      \n\
      \032  backup xxx\n\
      \032         Including the preference -backup pathspec causes Unison to keep\n\
      \032         backup files for each path that matches pathspec; directories\n\
      \032         (nor their permissions or any other metadata) are not backed up.\n\
      \032         These backup files are kept in the directory specified by the\n\
      \032         backuplocation preference. The backups are named according to\n\
      \032         the backupprefix and backupsuffix preferences. The number of\n\
      \032         versions that are kept is determined by the maxbackups\n\
      \032         preference.\n\
      \n\
      \032         The syntax of pathspec is described in the section \226\128\156Path\n\
      \032         Specification\226\128\157 .\n\
      \n\
      \032  backupcurr xxx\n\
      \032         Including the preference -backupcurr pathspec causes Unison to\n\
      \032         keep a backup of the current version of every file matching\n\
      \032         pathspec. This file will be saved as a backup with version\n\
      \032         number 000. Such backups can be used as inputs to external\n\
      \032         merging programs, for instance. See the documentatation for the\n\
      \032         merge preference. For more details, see the section \226\128\156Merging\n\
      \032         Conflicting Versions\226\128\157 .\n\
      \n\
      \032         The syntax of pathspec is described in the section \226\128\156Path\n\
      \032         Specification\226\128\157 .\n\
      \n\
      \032  backupcurrnot xxx\n\
      \032         Exceptions to backupcurr, like the ignorenot preference.\n\
      \n\
      \032  backupdir xxx\n\
      \032         If this preference is set, Unison will use it as the name of the\n\
      \032         directory used to store backup files specified by the backup\n\
      \032         preference, when backuplocation is set to central. It is checked\n\
      \032         after the UNISONBACKUPDIR environment variable.\n\
      \n\
      \032  backuploc xxx\n\
      \032         This preference determines whether backups should be kept\n\
      \032         locally, near the original files, or in a central directory\n\
      \032         specified by the backupdir preference. If set to local, backups\n\
      \032         will be kept in the same directory as the original files, and if\n\
      \032         set to central, backupdir will be used instead.\n\
      \n\
      \032  backupnot xxx\n\
      \032         The values of this preference specify paths or individual files\n\
      \032         or regular expressions that should not be backed up, even if the\n\
      \032         backup preference selects them\226\128\148i.e., it selectively overrides\n\
      \032         backup.\n\
      \n\
      \032  backupprefix xxx\n\
      \032         When a backup for a file NAME is created, it is stored in a\n\
      \032         directory specified by backuplocation, in a file called\n\
      \032         backupprefixNAMEbackupsuffix. backupprefix can include a\n\
      \032         directory name (causing Unison to keep all backup files for a\n\
      \032         given directory in a subdirectory with this name), and both\n\
      \032         backupprefix and backupsuffix can contain the string$VERSION,\n\
      \032         which will be replaced by the age of the backup (1 for the most\n\
      \032         recent, 2 for the second most recent, and so on...). This\n\
      \032         keyword is ignored if it appears in a directory name in the\n\
      \032         prefix; if it does not appear anywhere in the prefix or the\n\
      \032         suffix, it will be automatically placed at the beginning of the\n\
      \032         suffix.\n\
      \n\
      \032         One thing to be careful of: If the backuploc preference is set\n\
      \032         to local, Unison will automatically ignore all files whose\n\
      \032         prefix and suffix match backupprefix and backupsuffix. So be\n\
      \032         careful to choose values for these preferences that are\n\
      \032         sufficiently different from the names of your real files.\n\
      \n\
      \032  backups\n\
      \032         (Deprecated) Setting this flag to true is equivalent to setting\n\
      \032         backuplocation to local and backup to Name *.\n\
      \n\
      \032  backupsuffix xxx\n\
      \032         See backupprefix for full documentation.\n\
      \n\
      \032  batch\n\
      \032         When this is set to true, the user interface will ask no\n\
      \032         questions at all. Non-conflicting changes will be propagated;\n\
      \032         conflicts will be skipped.\n\
      \n\
      \032  clientHostName xxx\n\
      \032         When specified, the host name of the client will not be guessed\n\
      \032         and the provided host name will be used to find the archive.\n\
      \n\
      \032  color xxx\n\
      \032         When set to true, this flag enables color output in text mode\n\
      \032         user interface. When set to false, all color output is disabled.\n\
      \032         Default is to enable color if the NO_COLOR environment variable\n\
      \032         is not set.\n\
      \n\
      \032  confirmbigdel\n\
      \032         When this is set to true, Unison will request an extra\n\
      \032         confirmation if it appears that the entire replica has been\n\
      \032         deleted, before propagating the change. If the batch flag is\n\
      \032         also set, synchronization will be aborted. When the path\n\
      \032         preference is used, the same confirmation will be requested for\n\
      \032         top-level paths. (At the moment, this flag only affects the text\n\
      \032         user interface.) See also the mountpoint preference.\n\
      \n\
      \032  confirmmerge\n\
      \032         Setting this preference causes both the text and graphical\n\
      \032         interfaces to ask the user if the results of a merge command may\n\
      \032         be committed to the replica or not. Since the merge command\n\
      \032         works on temporary files, the user can then cancel all the\n\
      \032         effects of applying the merge if it turns out that the result is\n\
      \032         not satisfactory. In batch-mode, this preference has no effect.\n\
      \032         Default is false.\n\
      \n\
      \032  contactquietly\n\
      \032         If this flag is set, Unison will skip displaying the \226\128\152Contacting\n\
      \032         server\226\128\153 message (which some users find annoying) during startup.\n\
      \n\
      \032  copymax n\n\
      \032         A number indicating how many instances of the external copying\n\
      \032         utility Unison is allowed to run simultaneously (default to 1).\n\
      \n\
      \032  copyonconflict\n\
      \032         When this flag is set, Unison will make a copy of files that\n\
      \032         would otherwise be overwritten or deleted in case of conflicting\n\
      \032         changes, and more generally whenever the default behavior is\n\
      \032         overridden. This makes it possible to automatically resolve\n\
      \032         conflicts in a fairly safe way when synchronizing continuously,\n\
      \032         in combination with the -repeat watch and -prefer newer\n\
      \032         preferences.\n\
      \n\
      \032  copyprog xxx\n\
      \032         A string giving the name of an external program that can be used\n\
      \032         to copy large files efficiently (plus command-line switches\n\
      \032         telling it to copy files in-place). The default setting invokes\n\
      \032         rsync with appropriate options\226\128\148most users should not need to\n\
      \032         change it.\n\
      \n\
      \032  copyprogrest xxx\n\
      \032         A variant of copyprog that names an external program that should\n\
      \032         be used to continue the transfer of a large file that has\n\
      \032         already been partially transferred. Typically, copyprogrest will\n\
      \032         just be copyprog with one extra option (e.g., --partial, for\n\
      \032         rsync). The default setting invokes rsync with appropriate\n\
      \032         options\226\128\148most users should not need to change it.\n\
      \n\
      \032  copyquoterem xxx\n\
      \032         When set to true, this flag causes Unison to add an extra layer\n\
      \032         of quotes to the remote path passed to the external copy\n\
      \032         program. This is needed by rsync, for example, which internally\n\
      \032         uses an ssh connection requiring an extra level of quoting for\n\
      \032         paths containing spaces. When this flag is set to default, extra\n\
      \032         quotes are added if the value of copyprog contains the string\n\
      \032         rsync.\n\
      \n\
      \032  copythreshold n\n\
      \032         A number indicating above what filesize (in kilobytes) Unison\n\
      \032         should use the external copying utility specified by copyprog.\n\
      \032         Specifying 0 will cause all copies to use the external program;\n\
      \032         a negative number will prevent any files from using it. The\n\
      \032         default is -1. See the section \226\128\156Making Unison Faster on Large\n\
      \032         Files\226\128\157 for more information.\n\
      \n\
      \032  debug xxx\n\
      \032         This preference is used to make Unison print various sorts of\n\
      \032         information about what it is doing internally on the standard\n\
      \032         error stream. It can be used many times, each time with the name\n\
      \032         of a module for which debugging information should be printed.\n\
      \032         Possible arguments for debug can be found by looking for calls\n\
      \032         to Util.debug in the sources (using, e.g., grep). Setting -debug\n\
      \032         all causes information from all modules to be printed (this mode\n\
      \032         of usage is the first one to try, if you are trying to\n\
      \032         understand something that Unison seems to be doing wrong);\n\
      \032         -debug verbose turns on some additional debugging output from\n\
      \032         some modules (e.g., it will show exactly what bytes are being\n\
      \032         sent across the network).\n\
      \n\
      \032  diff xxx\n\
      \032         This preference can be used to control the name and command-line\n\
      \032         arguments of the system utility used to generate displays of\n\
      \032         file differences. The default is \226\128\152diff -u OLDER NEWER\226\128\153. If the\n\
      \032         value of this preference contains the substrings CURRENT1 and\n\
      \032         CURRENT2, these will be replaced by the names of the files to be\n\
      \032         diffed. If the value of this preference contains the substrings\n\
      \032         NEWER and OLDER, these will be replaced by the names of files to\n\
      \032         be diffed, NEWER being the most recently modified file of the\n\
      \032         two. Without any of these substrings, the two filenames will be\n\
      \032         appended to the command. In all cases, the filenames are\n\
      \032         suitably quoted.\n\
      \n\
      \032  doc xxx\n\
      \032         The command-line argument -doc secname causes unison to display\n\
      \032         section secname of the manual on the standard output and then\n\
      \032         exit. Use -doc all to display the whole manual, which includes\n\
      \032         exactly the same information as the printed and HTML manuals,\n\
      \032         modulo formatting. Use -doc topics to obtain a list of the names\n\
      \032         of the various sections that can be printed.\n\
      \n\
      \032  dontchmod\n\
      \032         By default, Unison uses the \226\128\153chmod\226\128\153 system call to set the\n\
      \032         permission bits of files after it has copied them. But in some\n\
      \032         circumstances (and under some operating systems), the chmod call\n\
      \032         always fails. Setting this preference completely prevents Unison\n\
      \032         from ever calling chmod.\n\
      \n\
      \032  dumbtty\n\
      \032         When set to true, this flag makes the text mode user interface\n\
      \032         avoid trying to change any of the terminal settings. (Normally,\n\
      \032         Unison puts the terminal in \226\128\152raw mode\226\128\153, so that it can do things\n\
      \032         like overwriting the current line.) This is useful, for example,\n\
      \032         when Unison runs in a shell inside of Emacs.\n\
      \n\
      \032         When dumbtty is set, commands to the user interface need to be\n\
      \032         followed by a carriage return before Unison will execute them.\n\
      \032         (When it is off, Unison recognizes keystrokes as soon as they\n\
      \032         are typed.)\n\
      \n\
      \032         This preference has no effect on the graphical user interface.\n\
      \n\
      \032  dumparchives\n\
      \032         When this preference is set, Unison will create a file\n\
      \032         unison.dump on each host, containing a text summary of the\n\
      \032         archive, immediately after loading it.\n\
      \n\
      \032  fastcheck xxx\n\
      \032         When this preference is set to true, Unison will use the\n\
      \032         modification time and length of a file as a \226\128\152pseudo inode\n\
      \032         number\226\128\153 when scanning replicas for updates, instead of reading\n\
      \032         the full contents of every file. (This does not apply to the\n\
      \032         very first run, when Unison will always scan all files\n\
      \032         regardless of this switch). Under Windows, this may cause Unison\n\
      \032         to miss propagating an update if the modification time and\n\
      \032         length of the file are both unchanged by the update. However,\n\
      \032         Unison will never overwrite such an update with a change from\n\
      \032         the other replica, since it always does a safe check for updates\n\
      \032         just before propagating a change. Thus, it is reasonable to use\n\
      \032         this switch under Windows most of the time and occasionally run\n\
      \032         Unison once with fastcheck set to false, if you are worried that\n\
      \032         Unison may have overlooked an update. For backward\n\
      \032         compatibility, yes, no, and default can be used in place of\n\
      \032         true, false, and auto. See the section \226\128\156Fast Update Detection\226\128\157\n\
      \032         for more information.\n\
      \n\
      \032  fastercheckUNSAFE\n\
      \032         THIS FEATURE IS STILL EXPERIMENTAL AND SHOULD BE USED WITH\n\
      \032         EXTREME CAUTION.\n\
      \n\
      \032         When this flag is set to true, Unison will compute a\n\
      \032         \226\128\153pseudo-fingerprint\226\128\153 the first time it sees a file (either\n\
      \032         because the file is new or because Unison is running for the\n\
      \032         first time). This enormously speeds update detection, but it\n\
      \032         must be used with care, as it can cause Unison to miss\n\
      \032         conflicts: If a given path in the filesystem contains files on\n\
      \032         both sides that Unison has not yet seen, and if those files have\n\
      \032         the same length but different contents, then Unison will not\n\
      \032         notice the presence of a conflict. If, later, one of the files\n\
      \032         is changed, the changed file will be propagated, overwriting the\n\
      \032         other.\n\
      \n\
      \032         Moreover, even when the files are initially identical, setting\n\
      \032         this flag can lead to potentially confusing behavior: if a newly\n\
      \032         created file is later touched without being modified, Unison\n\
      \032         will treat this conservatively as a potential change (since it\n\
      \032         has no record of the earlier contents) and show it as needing to\n\
      \032         be propagated to the other replica.\n\
      \n\
      \032         Most users should leave this flag off \226\128\147 the small time savings\n\
      \032         of not fingerprinting new files is not worth the cost in terms\n\
      \032         of safety. However, it can be very useful for power users with\n\
      \032         huge replicas that are known to be already synchronized (e.g.,\n\
      \032         because one replica is a newly created duplicate of the other,\n\
      \032         or because they have previously been synchronized with Unison\n\
      \032         but Unison\226\128\153s archives need to be rebuilt). In such situations,\n\
      \032         it is recommended that this flag be set only for the initial run\n\
      \032         of Unison, so that new archives can be created quickly, and then\n\
      \032         turned off for normal use.\n\
      \n\
      \032  fat\n\
      \032         When this is set to true, Unison will use appropriate options to\n\
      \032         synchronize efficiently and without error a replica located on a\n\
      \032         FAT filesystem on a non-Windows machine: do not synchronize\n\
      \032         permissions (perms = 0); never use chmod (dontchmod = true);\n\
      \032         treat filenames as case insensitive (ignorecase = true); do not\n\
      \032         attempt to synchronize symbolic links (links = false); ignore\n\
      \032         inode number changes when detecting updates (ignoreinodenumbers\n\
      \032         = true). Any of these change can be overridden by explicitly\n\
      \032         setting the corresponding preference in the profile.\n\
      \n\
      \032  follow xxx\n\
      \032         Including the preference -follow pathspec causes Unison to treat\n\
      \032         symbolic links matching pathspec as \226\128\152invisible\226\128\153 and behave as if\n\
      \032         the object pointed to by the link had appeared literally at this\n\
      \032         position in the replica. See the section \226\128\156Symbolic Links\226\128\157 for\n\
      \032         more details. The syntax of pathspec is described in the section\n\
      \032         \226\128\156Path Specification\226\128\157 .\n\
      \n\
      \032  force xxx\n\
      \032         Including the preference -force root causes Unison to resolve\n\
      \032         all differences (even non-conflicting changes) in favor of root.\n\
      \032         This effectively changes Unison from a synchronizer into a\n\
      \032         mirroring utility.\n\
      \n\
      \032         You can also specify -force newer (or -force older) to force\n\
      \032         Unison to choose the file with the later (earlier) modtime. In\n\
      \032         this case, the -times preference must also be enabled.\n\
      \n\
      \032         This preference is overridden by the forcepartial preference.\n\
      \n\
      \032         This preference should be used only if you are sure you know\n\
      \032         what you are doing!\n\
      \n\
      \032  forcepartial xxx\n\
      \032         Including the preference forcepartial = PATHSPEC -> root causes\n\
      \032         Unison to resolve all differences (even non-conflicting changes)\n\
      \032         in favor of root for the files in PATHSPEC (see the section\n\
      \032         \226\128\156Path Specification\226\128\157 for more information). This effectively\n\
      \032         changes Unison from a synchronizer into a mirroring utility.\n\
      \n\
      \032         You can also specify forcepartial PATHSPEC -> newer (or\n\
      \032         forcepartial PATHSPEC older) to force Unison to choose the file\n\
      \032         with the later (earlier) modtime. In this case, the -times\n\
      \032         preference must also be enabled.\n\
      \n\
      \032         This preference should be used only if you are sure you know\n\
      \032         what you are doing!\n\
      \n\
      \032  group\n\
      \032         When this flag is set to true, the group attributes of the files\n\
      \032         are synchronized. Whether the group names or the group\n\
      \032         identifiers are synchronized depends on the preference numerids.\n\
      \n\
      \032  halfduplex\n\
      \032         (Deprecated) When this flag is set to true, Unison network\n\
      \032         communication is forced to be half duplex (the client and the\n\
      \032         server never simultaneously emit data). If you experience\n\
      \032         unstabilities with your network link, this may help.\n\
      \n\
      \032  height n\n\
      \032         Used to set the height (in lines) of the main window in the\n\
      \032         graphical user interface.\n\
      \n\
      \032  i\n\
      \032         Provide this preference in the command line arguments to enable\n\
      \032         interactive profile manager in the text user interface.\n\
      \032         Currently only profile listing and interactive selection are\n\
      \032         available. Preferences like batch and silent remain applicable\n\
      \032         to synchronization functionality.\n\
      \n\
      \032  ignore xxx\n\
      \032         Including the preference -ignore pathspec causes Unison to\n\
      \032         completely ignore paths that match pathspec (as well as their\n\
      \032         children). This is useful for avoiding synchronizing temporary\n\
      \032         files, object files, etc. The syntax of pathspec is described in\n\
      \032         the section \226\128\156Path Specification\226\128\157 , and further details on\n\
      \032         ignoring paths is found in the section \226\128\156Ignoring Paths\226\128\157 .\n\
      \n\
      \032  ignorearchives\n\
      \032         When this preference is set, Unison will ignore any existing\n\
      \032         archive files and behave as though it were being run for the\n\
      \032         first time on these replicas. It is not a good idea to set this\n\
      \032         option in a profile: it is intended for command-line use.\n\
      \n\
      \032  ignorecase xxx\n\
      \032         When set to true, this flag causes Unison to treat filenames as\n\
      \032         case insensitive\226\128\148i.e., files in the two replicas whose names\n\
      \032         differ in (upper- and lower-case) \226\128\152spelling\226\128\153 are treated as the\n\
      \032         same file. When the flag is set to false, Unison will treat all\n\
      \032         filenames as case sensitive. Ordinarily, when the flag is set to\n\
      \032         default, filenames are automatically taken to be\n\
      \032         case-insensitive if either host is running Windows or OSX. In\n\
      \032         rare circumstances it may be useful to set the flag manually.\n\
      \n\
      \032  ignoreinodenumbers\n\
      \032         When set to true, this preference makes Unison not take\n\
      \032         advantage of inode numbers during fast update detection. This\n\
      \032         switch should be used with care, as it is less safe than the\n\
      \032         standard update detection method, but it can be useful with\n\
      \032         filesystems which do not support inode numbers.\n\
      \n\
      \032  ignorelocks\n\
      \032         When this preference is set, Unison will ignore any lock files\n\
      \032         that may have been left over from a previous run of Unison that\n\
      \032         was interrupted while reading or writing archive files; by\n\
      \032         default, when Unison sees these lock files it will stop and\n\
      \032         request manual intervention. This option should be set only if\n\
      \032         you are positive that no other instance of Unison might be\n\
      \032         concurrently accessing the same archive files (e.g., because\n\
      \032         there was only one instance of unison running and it has just\n\
      \032         crashed or you have just killed it). It is probably not a good\n\
      \032         idea to set this option in a profile: it is intended for\n\
      \032         command-line use.\n\
      \n\
      \032  ignorenot xxx\n\
      \032         This preference overrides the preference ignore. It gives a list\n\
      \032         of patterns (in the same format as ignore) for paths that should\n\
      \032         definitely not be ignored, whether or not they happen to match\n\
      \032         one of the ignore patterns.\n\
      \n\
      \032         Note that the semantics of ignore and ignorenot is a little\n\
      \032         counter-intuitive. When detecting updates, Unison examines paths\n\
      \032         in depth-first order, starting from the roots of the replicas\n\
      \032         and working downwards. Before examining each path, it checks\n\
      \032         whether it matches ignore and does not match ignorenot; in this\n\
      \032         case it skips this path and all its descendants. This means\n\
      \032         that, if some parent of a given path matches an ignore pattern,\n\
      \032         then it will be skipped even if the path itself matches an\n\
      \032         ignorenot pattern. In particular, putting ignore = Path * in\n\
      \032         your profile and then using ignorenot to select particular paths\n\
      \032         to be synchronized will not work. Instead, you should use the\n\
      \032         path preference to choose particular paths to synchronize.\n\
      \n\
      \032  immutable xxx\n\
      \032         This preference specifies paths for directories whose immediate\n\
      \032         children are all immutable files \226\128\148 i.e., once a file has been\n\
      \032         created, its contents never changes. When scanning for updates,\n\
      \032         Unison does not check whether these files have been modified;\n\
      \032         this can speed update detection significantly (in particular,\n\
      \032         for mail directories).\n\
      \n\
      \032  immutablenot xxx\n\
      \032         This preference overrides immutable.\n\
      \n\
      \032  include xxx\n\
      \032         Include preferences from a profile. include name reads the\n\
      \032         profile \"name\" (or file \"name\" in the .unison directory if\n\
      \032         profile \"name\" does not exist) and includes its contents as if\n\
      \032         it was part of a profile or given directly on command line.\n\
      \n\
      \032  key xxx\n\
      \032         Used in a profile to define a numeric key (0-9) that can be used\n\
      \032         in the user interface to switch immediately to this profile.\n\
      \n\
      \032  killserver\n\
      \032         When set to true, this flag causes Unison to kill the remote\n\
      \032         server process when the synchronization is finished. This\n\
      \032         behavior is the default for ssh connections, so this preference\n\
      \032         is not normally needed when running over ssh; it is provided so\n\
      \032         that socket-mode servers can be killed off after a single run of\n\
      \032         Unison, rather than waiting to accept future connections. (Some\n\
      \032         users prefer to start a remote socket server for each run of\n\
      \032         Unison, rather than leaving one running all the time.)\n\
      \n\
      \032  label xxx\n\
      \032         Used in a profile to provide a descriptive string documenting\n\
      \032         its settings. (This is useful for users that switch between\n\
      \032         several profiles, especially using the \226\128\152fast switch\226\128\153 feature of\n\
      \032         the graphical user interface.)\n\
      \n\
      \032  links xxx\n\
      \032         When set to true, this flag causes Unison to synchronize\n\
      \032         symbolic links. When the flag is set to false, symbolic links\n\
      \032         will result in an error during update detection. Ordinarily,\n\
      \032         when the flag is set to default, symbolic links are synchronized\n\
      \032         except when one of the hosts is running Windows. On a Windows\n\
      \032         client, Unison makes an attempt to detect if symbolic links are\n\
      \032         supported and allowed by user privileges. You may have to get\n\
      \032         elevated privileges to create symbolic links.\n\
      \n\
      \032  listen xxx\n\
      \032         When acting as a server on a TCP socket, Unison will by default\n\
      \032         listen on \"any\" address (0.0.0.0 and [::]). This command-line\n\
      \032         argument allows to specify a different listening address and can\n\
      \032         be repeated to listen on multiple addresses. Listening address\n\
      \032         can be specified as a host name or an IP address.\n\
      \n\
      \032  log\n\
      \032         When this flag is set, Unison will log all changes to the\n\
      \032         filesystems on a file.\n\
      \n\
      \032  logfile xxx\n\
      \032         By default, logging messages will be appended to the file\n\
      \032         unison.log in your .unison directory. Set this preference if you\n\
      \032         prefer another file. It can be a path relative to your .unison\n\
      \032         directory. Sending SIGUSR1 will close the logfile; the logfile\n\
      \032         will be re-opened (and created, if needed) automatically, to\n\
      \032         allow for log rotation.\n\
      \n\
      \032  maxbackups n\n\
      \032         This preference specifies the number of backup versions that\n\
      \032         will be kept by unison, for each path that matches the predicate\n\
      \032         backup. The default is 2.\n\
      \n\
      \032  maxerrors n\n\
      \032         This preference controls after how many errors Unison aborts a\n\
      \032         directory transfer. Setting it to a large number allows Unison\n\
      \032         to transfer most of a directory even when some files fail to be\n\
      \032         copied. The default is 1. If the preference is set too high,\n\
      \032         Unison may take a long time to abort in case of repeated\n\
      \032         failures (for instance, when the disk is full).\n\
      \n\
      \032  maxsizethreshold n\n\
      \032         A number indicating above what filesize (in kilobytes) Unison\n\
      \032         should flag a conflict instead of transferring the file. This\n\
      \032         conflict remains even in the presence of force or prefer\n\
      \032         options. A negative number will allow every transfer\n\
      \032         independently of the size. The default is -1.\n\
      \n\
      \032  maxthreads n\n\
      \032         This preference controls how much concurrency is allowed during\n\
      \032         the transport phase. Normally, it should be set reasonably high\n\
      \032         to maximize performance, but when Unison is used over a\n\
      \032         low-bandwidth link it may be helpful to set it lower (e.g. to 1)\n\
      \032         so that Unison doesn\226\128\153t soak up all the available bandwidth. The\n\
      \032         default is the special value 0, which mean 20 threads when file\n\
      \032         content streaming is desactivated and 1000 threads when it is\n\
      \032         activated.\n\
      \n\
      \032  merge xxx\n\
      \032         This preference can be used to run a merge program which will\n\
      \032         create a new version for each of the files and the backup, with\n\
      \032         the last backup and both replicas. The syntax of pathspec -> cmd\n\
      \032         is described in the section \226\128\156Path Specification\226\128\157 , and further\n\
      \032         details on Merging functions are present in the section \226\128\156Merging\n\
      \032         Conflicting Versions\226\128\157 .\n\
      \n\
      \032  mountpoint xxx\n\
      \032         Including the preference -mountpoint PATH causes Unison to\n\
      \032         double-check, at the end of update detection, that PATH exists\n\
      \032         and abort if it does not. This is useful when Unison is used to\n\
      \032         synchronize removable media. This preference can be given more\n\
      \032         than once. See the section \226\128\156Mount Points and Removable Media\226\128\157 .\n\
      \n\
      \032  nocreation xxx\n\
      \032         Including the preference -nocreation root prevents Unison from\n\
      \032         performing any file creation on root root.\n\
      \n\
      \032         This preference can be included twice, once for each root, if\n\
      \032         you want to prevent any creation.\n\
      \n\
      \032  nocreationpartial xxx\n\
      \032         Including the preference nocreationpartial = PATHSPEC -> root\n\
      \032         prevents Unison from performing any file creation in PATHSPEC on\n\
      \032         root root (see the section \226\128\156Path Specification\226\128\157 for more\n\
      \032         information). It is recommended to use BelowPath patterns when\n\
      \032         selecting a directory and all its contents.\n\
      \n\
      \032  nodeletion xxx\n\
      \032         Including the preference -nodeletion root prevents Unison from\n\
      \032         performing any file deletion on root root.\n\
      \n\
      \032         This preference can be included twice, once for each root, if\n\
      \032         you want to prevent any deletion.\n\
      \n\
      \032  nodeletionpartial xxx\n\
      \032         Including the preference nodeletionpartial = PATHSPEC -> root\n\
      \032         prevents Unison from performing any file deletion in PATHSPEC on\n\
      \032         root root (see the section \226\128\156Path Specification\226\128\157 for more\n\
      \032         information). It is recommended to use BelowPath patterns when\n\
      \032         selecting a directory and all its contents.\n\
      \n\
      \032  noupdate xxx\n\
      \032         Including the preference -noupdate root prevents Unison from\n\
      \032         performing any file update or deletion on root root.\n\
      \n\
      \032         This preference can be included twice, once for each root, if\n\
      \032         you want to prevent any update.\n\
      \n\
      \032  noupdatepartial xxx\n\
      \032         Including the preference noupdatepartial = PATHSPEC -> root\n\
      \032         prevents Unison from performing any file update or deletion in\n\
      \032         PATHSPEC on root root (see the section \226\128\156Path Specification\226\128\157 for\n\
      \032         more information). It is recommended to use BelowPath patterns\n\
      \032         when selecting a directory and all its contents.\n\
      \n\
      \032  numericids\n\
      \032         When this flag is set to true, groups and users are synchronized\n\
      \032         numerically, rather than by name.\n\
      \n\
      \032         The special uid 0 and the special group 0 are never mapped via\n\
      \032         user/group names even if this preference is not set.\n\
      \n\
      \032  owner\n\
      \032         When this flag is set to true, the owner attributes of the files\n\
      \032         are synchronized. Whether the owner names or the owner\n\
      \032         identifiers are synchronizeddepends on the preference numerids.\n\
      \n\
      \032  path xxx\n\
      \032         When no path preference is given, Unison will simply synchronize\n\
      \032         the two entire replicas, beginning from the given pair of roots.\n\
      \032         If one or more path preferences are given, then Unison will\n\
      \032         synchronize only these paths and their children. (This is useful\n\
      \032         for doing a fast sync of just one directory, for example.) Note\n\
      \032         that path preferences are interpreted literally\226\128\148they are not\n\
      \032         regular expressions.\n\
      \n\
      \032  perms n\n\
      \032         The integer value of this preference is a mask indicating which\n\
      \032         permission bits should be synchronized. It is set by default to\n\
      \032         0o1777: all bits but the set-uid and set-gid bits are\n\
      \032         synchronised (synchronizing theses latter bits can be a security\n\
      \032         hazard). If you want to synchronize all bits, you can set the\n\
      \032         value of this preference to \226\136\1461. If one of the replica is on a\n\
      \032         FAT [Windows] filesystem, you should consider using the fat\n\
      \032         preference instead of this preference. If you need Unison not to\n\
      \032         set permissions at all, set the value of this preference to 0\n\
      \032         and set the preference dontchmod to true.\n\
      \n\
      \032  prefer xxx\n\
      \032         Including the preference -prefer root causes Unison always to\n\
      \032         resolve conflicts in favor of root, rather than asking for\n\
      \032         guidance from the user, except for paths marked by the\n\
      \032         preference merge. (The syntax of root is the same as for the\n\
      \032         root preference, plus the special values newer and older.)\n\
      \n\
      \032         This preference is overridden by the preferpartial preference.\n\
      \n\
      \032         This preference should be used only if you are sure you know\n\
      \032         what you are doing!\n\
      \n\
      \032  preferpartial xxx\n\
      \032         Including the preference preferpartial = PATHSPEC -> root causes\n\
      \032         Unison always to resolve conflicts in favor of root, rather than\n\
      \032         asking for guidance from the user, for the files in PATHSPEC\n\
      \032         (see the section \226\128\156Path Specification\226\128\157 for more information).\n\
      \032         (The syntax of root is the same as for the root preference, plus\n\
      \032         the special values newer and older.)\n\
      \n\
      \032         This preference should be used only if you are sure you know\n\
      \032         what you are doing!\n\
      \n\
      \032  repeat xxx\n\
      \032         Setting this preference causes the text-mode interface to\n\
      \032         synchronize repeatedly, rather than doing it just once and\n\
      \032         stopping. If the argument is a number, Unison will pause for\n\
      \032         that many seconds before beginning again. When the argument is\n\
      \032         watch, Unison relies on an external file monitoring process to\n\
      \032         synchronize whenever a change happens.\n\
      \n\
      \032  retry n\n\
      \032         Setting this preference causes the text-mode interface to try\n\
      \032         again to synchronize updated paths where synchronization fails.\n\
      \032         Each such path will be tried N times.\n\
      \n\
      \032  root xxx\n\
      \032         Each use of this preference names the root of one of the\n\
      \032         replicas for Unison to synchronize. Exactly two roots are\n\
      \032         needed, so normal modes of usage are either to give two values\n\
      \032         for root in the profile, or to give no values in the profile and\n\
      \032         provide two on the command line. Details of the syntax of roots\n\
      \032         can be found in the section \226\128\156Roots\226\128\157 .\n\
      \n\
      \032         The two roots can be given in either order; Unison will sort\n\
      \032         them into a canonical order before doing anything else. It also\n\
      \032         tries to \226\128\152canonize\226\128\153 the machine names and paths that appear in\n\
      \032         the roots, so that, if Unison is invoked later with a slightly\n\
      \032         different name for the same root, it will be able to locate the\n\
      \032         correct archives.\n\
      \n\
      \032  rootalias xxx\n\
      \032         When calculating the name of the archive files for a given pair\n\
      \032         of roots, Unison replaces any roots matching the left-hand side\n\
      \032         of any rootalias rule by the corresponding right-hand side.\n\
      \n\
      \032  rsrc xxx\n\
      \032         When set to true, this flag causes Unison to synchronize\n\
      \032         resource forks and HFS meta-data. On filesystems that do not\n\
      \032         natively support resource forks, this data is stored in\n\
      \032         Carbon-compatible ._ AppleDouble files. When the flag is set to\n\
      \032         false, Unison will not synchronize these data. Ordinarily, the\n\
      \032         flag is set to default, and these data are automatically\n\
      \032         synchronized if either host is running OSX. In rare\n\
      \032         circumstances it is useful to set the flag manually.\n\
      \n\
      \032  rsync\n\
      \032         Unison uses the \226\128\153rsync algorithm\226\128\153 for \226\128\153diffs-only\226\128\153 transfer of\n\
      \032         updates to large files. Setting this flag to false makes Unison\n\
      \032         use whole-file transfers instead. Under normal circumstances,\n\
      \032         there is no reason to do this, but if you are having trouble\n\
      \032         with repeated \226\128\153rsync failure\226\128\153 errors, setting it to false should\n\
      \032         permit you to synchronize the offending files.\n\
      \n\
      \032  selftest\n\
      \032         Run internal tests and exit. This option is mostly for\n\
      \032         developers and must be used carefully: in particular, it will\n\
      \032         delete the contents of both roots, so that it can install its\n\
      \032         own files for testing. This flag only makes sense on the command\n\
      \032         line. When it is provided, no preference file is read: all\n\
      \032         preferences must be specified on thecommand line. Also, since\n\
      \032         the self-test procedure involves overwriting the roots and\n\
      \032         backup directory, the names of the roots and of the backupdir\n\
      \032         preference must include the string \"test\" or else the tests will\n\
      \032         be aborted. (If these are not given on the command line, dummy\n\
      \032         subdirectories in the current directory will be created\n\
      \032         automatically.)\n\
      \n\
      \032  servercmd xxx\n\
      \032         This preference can be used to explicitly set the name of the\n\
      \032         Unison executable on the remote server (e.g., giving a full path\n\
      \032         name), if necessary.\n\
      \n\
      \032  showarchive\n\
      \032         When this preference is set, Unison will print out the \226\128\153true\n\
      \032         names\226\128\153of the roots, in the same form as is expected by the\n\
      \032         rootalias preference.\n\
      \n\
      \032  silent\n\
      \032         When this preference is set to true, the textual user interface\n\
      \032         will print nothing at all, except in the case of errors. Setting\n\
      \032         silent to true automatically sets the batch preference to true.\n\
      \n\
      \032  socket xxx\n\
      \032         Start unison as a server listening on a TCP socket (with TCP\n\
      \032         port number as argument) or a local socket (aka Unix domain\n\
      \032         socket) (with socket path as argument).\n\
      \n\
      \032  sortbysize\n\
      \032         When this flag is set, the user interface will list changed\n\
      \032         files by size (smallest first) rather than by name. This is\n\
      \032         useful, for example, for synchronizing over slow links, since it\n\
      \032         puts very large files at the end of the list where they will not\n\
      \032         prevent smaller files from being transferred quickly.\n\
      \n\
      \032         This preference (as well as the other sorting flags, but not the\n\
      \032         sorting preferences that require patterns as arguments) can be\n\
      \032         set interactively and temporarily using the \226\128\153Sort\226\128\153 menu in the\n\
      \032         graphical and text user interfaces.\n\
      \n\
      \032  sortfirst xxx\n\
      \032         Each argument to sortfirst is a pattern pathspec, which\n\
      \032         describes a set of paths. Files matching any of these patterns\n\
      \032         will be listed first in the user interface. The syntax of\n\
      \032         pathspec is described in the section \226\128\156Path Specification\226\128\157 .\n\
      \n\
      \032  sortlast xxx\n\
      \032         Similar to sortfirst, except that files matching one of these\n\
      \032         patterns will be listed at the very end.\n\
      \n\
      \032  sortnewfirst\n\
      \032         When this flag is set, the user interface will list newly\n\
      \032         created files before all others. This is useful, for example,\n\
      \032         for checking that newly created files are not \226\128\152junk\226\128\153, i.e., ones\n\
      \032         that should be ignored or deleted rather than synchronized.\n\
      \n\
      \032  source xxx\n\
      \032         Include preferences from a file. source name reads the file\n\
      \032         \"name\" in the .unison directory and includes its contents as if\n\
      \032         it was part of a profile or given directly on command line.\n\
      \n\
      \032  sshargs xxx\n\
      \032         The string value of this preference will be passed as additional\n\
      \032         arguments (besides the host name and the name of the Unison\n\
      \032         executable on the remote system) to the ssh command used to\n\
      \032         invoke the remote server. The backslash is an escape character.\n\
      \n\
      \032  sshcmd xxx\n\
      \032         This preference can be used to explicitly set the name of the\n\
      \032         ssh executable (e.g., giving a full path name), if necessary.\n\
      \n\
      \032  stream\n\
      \032         (Deprecated) When this preference is set, Unison will use an\n\
      \032         experimental streaming protocol for transferring file contents\n\
      \032         more efficiently. The default value is true.\n\
      \n\
      \032  terse\n\
      \032         When this preference is set to true, the user interface will not\n\
      \032         print status messages.\n\
      \n\
      \032  testserver\n\
      \032         Setting this flag on the command line causes Unison to attempt\n\
      \032         to connect to the remote server and, if successful, print a\n\
      \032         message and immediately exit. Useful for debugging installation\n\
      \032         problems. Should not be set in preference files.\n\
      \n\
      \032  times\n\
      \032         When this flag is set to true, file modification times (but not\n\
      \032         directory modtimes) are propagated.\n\
      \n\
      \032  ui xxx\n\
      \032         This preference selects either the graphical or the textual user\n\
      \032         interface. Legal values are graphic or text.\n\
      \n\
      \032         Because this option is processed specially during Unison\226\128\153s\n\
      \032         start-up sequence, it can only be used on the command line. In\n\
      \032         preference files it has no effect.\n\
      \n\
      \032         If the Unison executable was compiled with only a textual\n\
      \032         interface, this option has no effect. (The pre-compiled binaries\n\
      \032         are all compiled with both interfaces available.)\n\
      \n\
      \032  unicode xxx\n\
      \032         When set to true, this flag causes Unison to perform case\n\
      \032         insensitive file comparisons assuming Unicode encoding. This is\n\
      \032         the default. When the flag is set to false, a Latin 1 encoding\n\
      \032         is assumed. When Unison runs in case sensitive mode, this flag\n\
      \032         only makes a difference if one host is running Windows or Mac OS\n\
      \032         X. Under Windows, the flag selects between using the Unicode or\n\
      \032         8bit Windows API for accessing the filesystem. Under Mac OS X,\n\
      \032         it selects whether comparing the filenames up to decomposition,\n\
      \032         or byte-for-byte.\n\
      \n\
      \032  version\n\
      \032         Print the current version number and exit. (This option only\n\
      \032         makes sense on the command line.)\n\
      \n\
      \032  watch\n\
      \032         Unison uses a file watcher process, when available, to detect\n\
      \032         filesystem changes; this is used to speed up update detection.\n\
      \032         Setting this flag to false disables the use of this process.\n\
      \n\
      \032  xferbycopying\n\
      \032         When this preference is set, Unison will try to avoid\n\
      \032         transferring file contents across the network by recognizing\n\
      \032         when a file with the required contents already exists in the\n\
      \032         target replica. This usually allows file moves to be propagated\n\
      \032         very quickly. The default value istrue.\n\
      \n\
      Profiles\n\
      \n\
      \032  A profile is a text file that specifies permanent settings for roots,\n\
      \032  paths, ignore patterns, and other preferences, so that they do not need\n\
      \032  to be typed at the command line every time Unison is run. Profiles\n\
      \032  should reside in the .unison directory on the client machine. If Unison\n\
      \032  is started with just one argument name on the command line, it looks\n\
      \032  for a profile called name.prf in the .unison directory. If it is\n\
      \032  started with no arguments, it scans the .unison directory for files\n\
      \032  whose names end in .prf and offers a menu (provided that the Unison\n\
      \032  executable is compiled with the graphical user interface). If a file\n\
      \032  named default.prf is found, its settings will be offered as the default\n\
      \032  choices.\n\
      \n\
      \032  To set the value of a preference p permanently, add to the appropriate\n\
      \032  profile a line of the form\n\
      \032       p = true\n\
      \n\
      \032  for a boolean flag or\n\
      \032       p = <value>\n\
      \n\
      \032  for a preference of any other type.\n\
      \n\
      \032  Whitespaces around p and xxx are ignored. A profile may also include\n\
      \032  blank lines and lines beginning with #; both are ignored.\n\
      \n\
      \032  When Unison starts, it first reads the profile and then the command\n\
      \032  line, so command-line options will override settings from the profile.\n\
      \n\
      \032  Profiles may also include lines of the form include name, which will\n\
      \032  cause the file name (or name.prf, if name does not exist in the .unison\n\
      \032  directory) to be read at the point, and included as if its contents,\n\
      \032  instead of the include line, was part of the profile. Include lines\n\
      \032  allows settings common to several profiles to be stored in one place. A\n\
      \032  similar line of the form source name does the same except that it does\n\
      \032  not attempt to add a suffix to name. Similar lines of the form include?\n\
      \032  name or source? name do the same as their respective lines without the\n\
      \032  question mark except that it does not constitue an error to specify a\n\
      \032  non-existing file name. In name the backslash is an escape character.\n\
      \n\
      \032  A profile may include a preference \226\128\152label = desc\226\128\153 to provide a\n\
      \032  description of the options selected in this profile. The string desc is\n\
      \032  listed along with the profile name in the profile selection dialog, and\n\
      \032  displayed in the top-right corner of the main Unison window in the\n\
      \032  graphical user interface.\n\
      \n\
      \032  The graphical user-interface also supports one-key shortcuts for\n\
      \032  commonly used profiles. If a profile contains a preference of the form\n\
      \032  \226\128\152key = n\226\128\153, where n is a single digit, then pressing this digit key will\n\
      \032  cause Unison to immediately switch to this profile and begin\n\
      \032  synchronization again from scratch. In this case, all actions that have\n\
      \032  been selected for a set of changes currently being displayed will be\n\
      \032  discarded.\n\
      \n\
      Sample Profiles\n\
      \n\
      A Minimal Profile\n\
      \n\
      \032  Here is a very minimal profile file, such as might be found in\n\
      \032  .unison/default.prf:\n\
      \032   # Roots of the synchronization\n\
      \032   root = /home/bcpierce\n\
      \032   root = ssh://saul//home/bcpierce\n\
      \n\
      \032   # Paths to synchronize\n\
      \032   path = current\n\
      \032   path = common\n\
      \032   path = .netscape/bookmarks.html\n\
      \n\
      A Basic Profile\n\
      \n\
      \032  Here is a more sophisticated profile, illustrating some other useful\n\
      \032  features.\n\
      \032   # Roots of the synchronization\n\
      \032   root = /home/bcpierce\n\
      \032   root = ssh://saul//home/bcpierce\n\
      \n\
      \032   # Paths to synchronize\n\
      \032   path = current\n\
      \032   path = common\n\
      \032   path = .netscape/bookmarks.html\n\
      \n\
      \032   # Some regexps specifying names and paths to ignore\n\
      \032   ignore = Name temp.*\n\
      \032   ignore = Name *~\n\
      \032   ignore = Name .*~\n\
      \032   ignore = Path */pilot/backup/Archive_*\n\
      \032   ignore = Name *.o\n\
      \032   ignore = Name *.tmp\n\
      \n\
      \032   # Window height\n\
      \032   height = 37\n\
      \n\
      \032   # Keep a backup copy of every file in a central location\n\
      \032   backuplocation = central\n\
      \032   backupdir = /home/bcpierce/backups\n\
      \032   backup = Name *\n\
      \032   backupprefix = $VERSION.\n\
      \032   backupsuffix =\n\
      \n\
      \032   # Use this command for displaying diffs\n\
      \032   diff = diff -y -W 79 --suppress-common-lines\n\
      \n\
      \032   # Log actions to the terminal\n\
      \032   log = true\n\
      \n\
      A Power-User Profile\n\
      \n\
      \032  When Unison is used with large replicas, it is often convenient to be\n\
      \032  able to synchronize just a part of the replicas on a given run (this\n\
      \032  saves the time of detecting updates in the other parts). This can be\n\
      \032  accomplished by splitting up the profile into several parts \226\128\148 a common\n\
      \032  part containing most of the preference settings, plus one \226\128\156top-level\226\128\157\n\
      \032  file for each set of paths that need to be synchronized. (The include\n\
      \032  mechanism can also be used to allow the same set of preference settings\n\
      \032  to be used with different roots.)\n\
      \n\
      \032  The collection of profiles implementing this scheme might look as\n\
      \032  follows. The file default.prf is empty except for an include directive:\n\
      \032   # Include the contents of the file common\n\
      \032   include common\n\
      \n\
      \032  Note that the name of the common file is common, not common.prf; this\n\
      \032  prevents Unison from offering common as one of the list of profiles in\n\
      \032  the opening dialog (in the graphical UI).\n\
      \n\
      \032  The file common contains the real preferences:\n\
      \032   # Roots of the synchronization\n\
      \032   root = /home/bcpierce\n\
      \032   root = ssh://saul//home/bcpierce\n\
      \n\
      \032   # (... other preferences ...)\n\
      \n\
      \032   # If any new preferences are added by Unison (e.g. 'ignore'\n\
      \032   # preferences added via the graphical UI), then store them in the\n\
      \032   # file 'common' rather than in the top-level preference file\n\
      \032   addprefsto = common\n\
      \n\
      \032   # Names and paths to ignore:\n\
      \032   ignore = Name temp.*\n\
      \032   ignore = Name *~\n\
      \032   ignore = Name .*~\n\
      \032   ignore = Path */pilot/backup/Archive_*\n\
      \032   ignore = Name *.o\n\
      \032   ignore = Name *.tmp\n\
      \n\
      \032  Note that there are no path preferences in common. This means that,\n\
      \032  when we invoke Unison with the default profile (e.g., by typing \226\128\153unison\n\
      \032  default\226\128\153 or just \226\128\153unison\226\128\153 on the command line), the whole replicas will\n\
      \032  be synchronized. (If we never want to synchronize the whole replicas,\n\
      \032  then default.prf would instead include settings for all the paths that\n\
      \032  are usually synchronized.)\n\
      \n\
      \032  To synchronize just part of the replicas, Unison is invoked with an\n\
      \032  alternate preference file\226\128\148e.g., doing \226\128\153unison workingset\226\128\153, where the\n\
      \032  preference file workingset.prf contains\n\
      \032   path = current/papers\n\
      \032   path = Mail/inbox\n\
      \032   path = Mail/drafts\n\
      \032   include common\n\
      \n\
      \032  causes Unison to synchronize just the listed subdirectories.\n\
      \n\
      \032  The key preference can be used in combination with the graphical UI to\n\
      \032  quickly switch between different sets of paths. For example, if the\n\
      \032  file mail.prf contains\n\
      \032   path = Mail\n\
      \032   batch = true\n\
      \032   key = 2\n\
      \032   include common\n\
      \n\
      \032  then pressing 2 will cause Unison to look for updates in the Mail\n\
      \032  subdirectory and (because the batch flag is set) immediately propagate\n\
      \032  any that it finds.\n\
      \n\
      Keeping Backups\n\
      \n\
      \032  When Unison overwrites (or deletes) a file or directory while\n\
      \032  propagating changes from the other replica, it can keep the old version\n\
      \032  around as a backup. There are several preferences that control\n\
      \032  precisely where these backups are stored and how they are named.\n\
      \n\
      \032  To enable backups, you must give one or more backup preferences. Each\n\
      \032  of these has the form\n\
      \032   backup = <pathspec>\n\
      \n\
      \032  where <pathspec> has the same form as for the ignore preference. For\n\
      \032  example,\n\
      \032   backup = Name *\n\
      \n\
      \032  causes Unison to keep backups of all files and directories. The\n\
      \032  backupnot preference can be used to give a few exceptions: it specifies\n\
      \032  which files and directories should not be backed up, even if they match\n\
      \032  the backup pathspec.\n\
      \n\
      \032  It is important to note that the pathspec is matched against the path\n\
      \032  that is being updated by Unison, not its descendants. For example, if\n\
      \032  you set backup = Name *.txt and then delete a whole directory named foo\n\
      \032  containing some text files, these files will not be backed up because\n\
      \032  Unison will just check that foo does not match *.txt. Similarly, if the\n\
      \032  directory itself happened to be called foo.txt, then the whole\n\
      \032  directory and all the files in it will be backed up, regardless of\n\
      \032  their names.\n\
      \n\
      \032  Backup files can be stored either centrally or locally. This behavior\n\
      \032  is controlled by the preference backuplocation, whose value must be\n\
      \032  either central or local. (The default is central.)\n\
      \n\
      \032  When backups are stored locally, they are kept in the same directory as\n\
      \032  the original.\n\
      \n\
      \032  When backups are stored centrally, the directory used to hold them is\n\
      \032  controlled by the preference backupdir and the environment variable\n\
      \032  UNISONBACKUPDIR. (The environment variable is checked first.) If\n\
      \032  neither of these are set, then the directory .unison/backup in the\n\
      \032  user\226\128\153s home directory is used.\n\
      \n\
      \032  The preference maxbackups controls how many previous versions of each\n\
      \032  file are kept (including the current version).\n\
      \n\
      \032  By default, backup files are named .bak.VERSION.FILENAME, where\n\
      \032  FILENAME is the original filename and VERSION is the backup number (1\n\
      \032  for the most recent, 2 for the next most recent, etc.). This can be\n\
      \032  changed by setting the preferences backupprefix and/or backupsuffix. If\n\
      \032  desired, backupprefix may include a directory prefix; this can be used\n\
      \032  with backuplocation = local to put all backup files for each directory\n\
      \032  into a single subdirectory. For example, setting\n\
      \032   backuplocation = local\n\
      \032   backupprefix = .unison/$VERSION.\n\
      \032   backupsuffix =\n\
      \n\
      \032  will put all backups in a local subdirectory named .unison. Also, note\n\
      \032  that the string $VERSION in either backupprefix or backupsuffix (it\n\
      \032  must appear in one or the other) is replaced by the version number.\n\
      \032  This can be used, for example, to ensure that backup files retain the\n\
      \032  same extension as the originals.\n\
      \n\
      \032  For backward compatibility, the backups preference is also supported.\n\
      \032  It simply means backup = Name * and backuplocation = local.\n\
      \n\
      Merging Conflicting Versions\n\
      \n\
      \032  Unison can invoke external programs to merge conflicting versions of a\n\
      \032  file. The preference merge controls this process.\n\
      \n\
      \032  The merge preference may be given once or several times in a preference\n\
      \032  file (it can also be given on the command line, of course, but this\n\
      \032  tends to be awkward because of the spaces and special characters\n\
      \032  involved). Each instance of the preference looks like this:\n\
      \032   merge = <PATHSPEC> -> <MERGECMD>\n\
      \n\
      \032  The <PATHSPEC> here has exactly the same format as for the ignore\n\
      \032  preference (see the section \226\128\156Path Specification\226\128\157 ). For example, using\n\
      \032  \226\128\156Name *.txt\226\128\157 as the <PATHSPEC> tells Unison that this command should be\n\
      \032  used whenever a file with extension .txt needs to be merged.\n\
      \n\
      \032  Many external merging programs require as inputs not just the two files\n\
      \032  that need to be merged, but also a file containing the last\n\
      \032  synchronized version. You can ask Unison to keep a copy of the last\n\
      \032  synchronized version for some files using the backupcurrent preference.\n\
      \032  This preference is used in exactly the same way as backup and its\n\
      \032  meaning is similar, except that it causes backups to be kept of the\n\
      \032  current contents of each file after it has been synchronized by Unison,\n\
      \032  rather than the previous contents that Unison overwrote. These backups\n\
      \032  are kept on both replicas in the same place as ordinary backup\n\
      \032  files\226\128\148i.e. according to the backuplocation and backupdir preferences.\n\
      \032  They are named like the original files if backupslocation is set to\n\
      \032  \226\128\153central\226\128\153 and otherwise, Unison uses the backupprefix and backupsuffix\n\
      \032  preferences and assumes a version number 000 for these backups.\n\
      \n\
      \032  The <MERGECMD> part of the preference specifies what external command\n\
      \032  should be invoked to merge files at paths matching the <PATHSPEC>.\n\
      \032  Within this string, several special substrings are recognized; these\n\
      \032  will be substituted with appropriate values before invoking a sub-shell\n\
      \032  to execute the command.\n\
      \032    * CURRENT1 is replaced by the name of (a temporary copy of) the local\n\
      \032      variant of the file.\n\
      \032    * CURRENT2 is replaced by the name of a temporary file, into which\n\
      \032      the contents of the remote variant of the file have been\n\
      \032      transferred by Unison prior to performing the merge.\n\
      \032    * CURRENTARCH is replaced by the name of the backed up copy of the\n\
      \032      original version of the file (i.e., the file saved by Unison if the\n\
      \032      current filename matches the path specifications for the\n\
      \032      backupcurrent preference, as explained above), if one exists. If no\n\
      \032      archive exists and CURRENTARCH appears in the merge command, then\n\
      \032      an error is signalled.\n\
      \032    * CURRENTARCHOPT is replaced by the name of the backed up copy of the\n\
      \032      original version of the file (i.e., its state at the end of the\n\
      \032      last successful run of Unison), if one exists, or the empty string\n\
      \032      if no archive exists.\n\
      \032    * NEW is replaced by the name of a temporary file that Unison expects\n\
      \032      to be written by the merge program when it finishes, giving the\n\
      \032      desired new contents of the file.\n\
      \032    * PATH is replaced by the path (relative to the roots of the\n\
      \032      replicas) of the file being merged.\n\
      \032    * NEW1 and NEW2 are replaced by the names of temporary files that\n\
      \032      Unison expects to be written by the merge program when it is only\n\
      \032      able to partially merge the originals; in this case, NEW1 will be\n\
      \032      written back to the local replica and NEW2 to the remote replica;\n\
      \032      NEWARCH, if present, will be used as the \226\128\156last common state\226\128\157 of the\n\
      \032      replicas. (These three options are provided for later compatibility\n\
      \032      with the Harmony data synchronizer.)\n\
      \032    * BATCHMODE is replaced according to the batch mode of Unison; if it\n\
      \032      is in batch mode, then a non empty string (\226\128\156batch\226\128\157) is substituted,\n\
      \032      otherwise the empty string is substituted.\n\
      \n\
      \032  To accommodate the wide variety of programs that users might want to\n\
      \032  use for merging, Unison checks for several possible situations when the\n\
      \032  merge program exits:\n\
      \032    * If the merge program exits with a non-zero status, then merge is\n\
      \032      considered to have failed and the replicas are not changed.\n\
      \032    * If the file NEW has been created, it is written back to both\n\
      \032      replicas (and stored in the backup directory). Similarly, if just\n\
      \032      the file NEW1 has been created, it is written back to both\n\
      \032      replicas.\n\
      \032    * If neither NEW nor NEW1 have been created, then Unison examines the\n\
      \032      temporary files CURRENT1 and CURRENT2 that were given as inputs to\n\
      \032      the merge program. If either has been changed (or both have been\n\
      \032      changed in identical ways), then its new contents are written back\n\
      \032      to both replicas. If either CURRENT1 or CURRENT2 has been deleted,\n\
      \032      then the contents of the other are written back to both replicas.\n\
      \032    * If the files NEW1, NEW2, and NEWARCH have all been created, they\n\
      \032      are written back to the local replica, remote replica, and backup\n\
      \032      directory, respectively. If the files NEW1, NEW2 have been created,\n\
      \032      but NEWARCH has not, then these files are written back to the local\n\
      \032      replica and remote replica, respectively. Also, if NEW1 and NEW2\n\
      \032      have identical contents, then the same contents are stored as a\n\
      \032      backup (if the backupcurrent preference is set for this path) to\n\
      \032      reflect the fact that the path is currently in sync.\n\
      \032    * If NEW1 and NEW2 (resp. CURRENT1 and CURRENT2) are created (resp.\n\
      \032      overwritten) with different contents but the merge command did not\n\
      \032      fail (i.e., it exited with status code 0), then we copy NEW1 (resp.\n\
      \032      CURRENT1) to the other replica and to the archive.\n\
      \032      This behavior is a design choice made to handle the case where a\n\
      \032      merge command only synchronizes some specific contents between two\n\
      \032      files, skipping some irrelevant information (order between entries,\n\
      \032      for instance). We assume that, if the merge command exits normally,\n\
      \032      then the two resulting files are \226\128\156as good as equal.\226\128\157 (The reason we\n\
      \032      copy one on top of the other is to avoid Unison detecting that the\n\
      \032      files are unequal the next time it is run and trying again to merge\n\
      \032      them when, in fact, the merge program has already made them as\n\
      \032      similar as it is able to.)\n\
      \n\
      \032  You can disable a merge by setting a <MERGECMD> that does nothing. For\n\
      \032  example you can override the merging of text files specified in a\n\
      \032  profile by typing on the command line:\n\
      \032   unison profile -merge 'Name *.txt -> echo SKIP'\n\
      \n\
      \032  If the confirmmerge preference is set and Unison is not run in batch\n\
      \032  mode, then Unison will always ask for confirmation before actually\n\
      \032  committing the results of the merge to the replicas.\n\
      \n\
      \032  You can detect batch mode by testing BATCHMODE; for example to avoid a\n\
      \032  merge completely do nothing:\n\
      \032   merge = Name *.txt -> [ -z \"BATCHMODE\" ] && mergecmd CURRENT1 CURRENT2\n\
      \n\
      \032  A large number of external merging programs are available. For example,\n\
      \032  on Unix systems setting the merge preference to\n\
      \032   merge = Name *.txt -> diff3 -m CURRENT1 CURRENTARCH CURRENT2\n\
      \032                           > NEW || echo \"differences detected\"\n\
      \n\
      \032  will tell Unison to use the external diff3 program for merging.\n\
      \032  Alternatively, users of emacs may find the following settings\n\
      \032  convenient:\n\
      \032   merge = Name *.txt -> emacs -q --eval '(ediff-merge-files-with-ancestor\n\
      \032                            \"CURRENT1\" \"CURRENT2\" \"CURRENTARCH\" nil \"NEW\")'\n\
      \n\
      \032  (These commands are displayed here on two lines to avoid running off\n\
      \032  the edge of the page. In your preference file, each command should be\n\
      \032  written on a single line.)\n\
      \n\
      \032  Users running emacs under windows may find something like this useful:\n\
      \032  merge = Name * -> C:\\Progra~1\\Emacs\\emacs\\bin\\emacs.exe -q --eval\n\
      \032                           \"(ediff-files \"\"\"CURRENT1\"\"\" \"\"\"CURRENT2\"\"\")\"\n\
      \n\
      \032  Users running Mac OS X (you may need the Developer Tools installed to\n\
      \032  get the opendiff utility) may prefer\n\
      \032   merge = Name *.txt -> opendiff CURRENT1 CURRENT2 -ancestor CURRENTARCH -merg\n\
      e NEW\n\
      \n\
      \032  Here is a slightly more involved hack. The opendiff program can operate\n\
      \032  either with or without an archive file. A merge command of this form\n\
      \032   merge = Name *.txt ->\n\
      \032             if [ CURRENTARCHOPTx = x ];\n\
      \032             then opendiff CURRENT1 CURRENT2 -merge NEW;\n\
      \032             else opendiff CURRENT1 CURRENT2 -ancestor CURRENTARCHOPT -merge NE\n\
      W;\n\
      \032             fi\n\
      \n\
      \032  (still all on one line in the preference file!) will test whether an\n\
      \032  archive file exists and use the appropriate variant of the arguments to\n\
      \032  opendiff.\n\
      \n\
      \032  Linux users may enjoy this variant:\n\
      \032   merge = Name * -> kdiff3 -o NEW CURRENTARCHOPT CURRENT1 CURRENT2\n\
      \n\
      \032  Ordinarily, external merge programs are only invoked when Unison is not\n\
      \032  running in batch mode. To specify an external merge program that should\n\
      \032  be used no matter the setting of the batch flag, use the mergebatch\n\
      \032  preference instead of merge.\n\
      \n\
      \032    Please post suggestions for other useful values of the merge\n\
      \032    preference to the unison-users mailing list\226\128\148we\226\128\153d like to give\n\
      \032    several examples here.\n\
      \n\
      The User Interface\n\
      \n\
      \032  Both the textual and the graphical user interfaces are intended to be\n\
      \032  mostly self-explanatory. Here are just a few tricks:\n\
      \032    * By default, when running on Unix the textual user interface will\n\
      \032      try to put the terminal into the \226\128\156raw mode\226\128\157 so that it reads the\n\
      \032      input a character at a time rather than a line at a time. (This\n\
      \032      means you can type just the single keystroke \226\128\156>\226\128\157 to tell Unison to\n\
      \032      propagate a file from left to right, rather than \226\128\156> Enter.\226\128\157)\n\
      \032      There are some situations, though, where this will not work \226\128\148 for\n\
      \032      example, when Unison is running in a shell window inside Emacs.\n\
      \032      Setting the dumbtty preference will force Unison to leave the\n\
      \032      terminal alone and process input a line at a time.\n\
      \n\
      Exit Code\n\
      \n\
      \032  When running in the textual mode, Unison returns an exit status, which\n\
      \032  describes whether, and at which level, the synchronization was\n\
      \032  successful. The exit status could be useful when Unison is invoked from\n\
      \032  a script. Currently, there are four possible values for the exit\n\
      \032  status:\n\
      \032    * 0: successful synchronization; everything is up-to-date now.\n\
      \032    * 1: some files were skipped, but all file transfers were successful.\n\
      \032    * 2: non-fatal failures occurred during file transfer.\n\
      \032    * 3: a fatal error occurred, or the execution was interrupted.\n\
      \n\
      \032  The graphical interface does not return any useful information through\n\
      \032  the exit status.\n\
      \n\
      Path Specification\n\
      \n\
      \032  Several Unison preferences (e.g., ignore/ignorenot, follow,\n\
      \032  sortfirst/sortlast, backup, merge, etc.) specify individual paths or\n\
      \032  sets of paths. These preferences share a common syntax based on\n\
      \032  regular-expressions. Each preference is associated with a list of path\n\
      \032  patterns; the paths specified are those that match any one of the path\n\
      \032  pattern.\n\
      \032    * Pattern preferences can be given on the command line, or, more\n\
      \032      often, stored in profiles, using the same syntax as other\n\
      \032      preferences. For example, a profile line of the form\n\
      \032            ignore = pattern\n\
      \n\
      \032      adds pattern to the list of patterns to be ignored.\n\
      \032    * Each pattern can have one of three forms. The most general form is\n\
      \032      a Posix extended regular expression introduced by the keyword\n\
      \032      Regex. (The collating sequences and character classes of full Posix\n\
      \032      regexps are not currently supported).\n\
      \032                Regex regexp\n\
      \n\
      \032      For convenience, three other styles of pattern are also recognized:\n\
      \032                Name name\n\
      \n\
      \032      matches any path in which the last component matches name,\n\
      \032                Path path\n\
      \n\
      \032      matches exactly the path path, and\n\
      \032                BelowPath path\n\
      \n\
      \032      matches the path path and any path below. The name and path\n\
      \032      arguments of the latter forms of patterns are not regular\n\
      \032      expressions. Instead, standard \226\128\156globbing\226\128\157 conventions can be used\n\
      \032      in name and path:\n\
      \032         + a * matches any sequence of characters not including / (and\n\
      \032           not beginning with ., when used at the beginning of a name)\n\
      \032         + a ? matches any single character except / (and leading .)\n\
      \032         + [xyz] matches any character from the set {x, y, z }\n\
      \032         + {a,bb,ccc} matches any one of a, bb, or ccc. (Be careful not\n\
      \032           to put extra spaces after the commas: these will be\n\
      \032           interpreted literally as part of the strings to be matched!)\n\
      \032    * The path separator in path patterns is always the forward-slash\n\
      \032      character \226\128\156/\226\128\157 \226\128\148 even when the client or server is running under\n\
      \032      Windows, where the normal separator character is a backslash. This\n\
      \032      makes it possible to use the same set of path patterns for both\n\
      \032      Unix and Windows file systems.\n\
      \032    * A path specification may be followed by the separator \226\128\156 -> \226\128\157 itself\n\
      \032      followed by a string which will be associated to the matching\n\
      \032      paths:\n\
      \032                Path path -> associated string\n\
      \n\
      \032      Not all pathspec preferences use these associated strings but all\n\
      \032      pathspec preferences are parsed identically and the strings may be\n\
      \032      ignored. Only the last match of the separator string on the line is\n\
      \032      used as a delimiter. Thus to allow a path specification to contain\n\
      \032      the separator string, append an associated string to it, even if it\n\
      \032      is not used. The associated string cannot contain the separator\n\
      \032      string.\n\
      \n\
      \032  Some examples of path patterns appear in the section \226\128\156Ignoring Paths\226\128\157 .\n\
      \032  Associated strings are used by the preference merge.\n\
      \n\
      Ignoring Paths\n\
      \n\
      \032  Most users of Unison will find that their replicas contain lots of\n\
      \032  files that they don\226\128\153t ever want to synchronize \226\128\148 temporary files, very\n\
      \032  large files, old stuff, architecture-specific binaries, etc. They can\n\
      \032  instruct Unison to ignore these paths using patterns introduced in the\n\
      \032  section \226\128\156Path Specification\226\128\157 .\n\
      \n\
      \032  For example, the following pattern will make Unison ignore any path\n\
      \032  containing the name CVS or a name ending in .cmo:\n\
      \032            ignore = Name {CVS,*.cmo}\n\
      \n\
      \032  The next pattern makes Unison ignore the path a/b:\n\
      \032            ignore = Path a/b\n\
      \n\
      \032  Path patterns do not skip filenames beginning with . (as Name patterns\n\
      \032  do). For example,\n\
      \032            ignore = Path */tmp\n\
      \n\
      \032  will include .foo/tmp in the set of ignore directories, as it is a\n\
      \032  path, not a name, that is ignored.\n\
      \n\
      \032  The following pattern makes Unison ignore any path beginning with a/b\n\
      \032  and ending with a name ending by .ml.\n\
      \032            ignore = Regex a/b/.*\\.ml\n\
      \n\
      \032  Note that regular expression patterns are \226\128\156anchored\226\128\157: they must match\n\
      \032  the whole path, not just a substring of the path.\n\
      \n\
      \032  Here are a few extra points regarding the ignore preference.\n\
      \032    * If a directory is ignored, all its descendants will be too.\n\
      \032    * The user interface provides some convenient commands for adding new\n\
      \032      patterns to be ignored. To ignore a particular file, select it and\n\
      \032      press \226\128\156i\226\128\157. To ignore all files with the same extension, select it\n\
      \032      and press \226\128\156E\226\128\157 (with the shift key). To ignore all files with the\n\
      \032      same name, no matter what directory they appear in, select it and\n\
      \032      press \226\128\156N\226\128\157. These new patterns become permanent: they are\n\
      \032      immediately added to the current profile on disk.\n\
      \032    * If you use the include directive to include a common collection of\n\
      \032      preferences in several top-level preference files, you will\n\
      \032      probably also want to set the addprefsto preference to the name of\n\
      \032      this file. This will cause any new ignore patterns that you add\n\
      \032      from inside Unison to be appended to this file, instead of\n\
      \032      whichever top-level preference file you started Unison with.\n\
      \032    * Ignore patterns can also be specified on the command line, if you\n\
      \032      like (this is probably not very useful), using an option like\n\
      \032      -ignore 'Name temp.txt'.\n\
      \032    * Be careful about renaming directories containing ignored files.\n\
      \032      Because Unison understands the rename as a delete plus a create,\n\
      \032      any ignored files in the directory will be lost (since they are\n\
      \032      invisible to Unison and therefore they do not get recreated in the\n\
      \032      new version of the directory).\n\
      \032    * There is also an ignorenot preference, which specifies a set of\n\
      \032      patterns for paths that should not be ignored, even if they match\n\
      \032      an ignore pattern. However, the interaction of these two sets of\n\
      \032      patterns can be a little tricky. Here is exactly how it works:\n\
      \032         + Unison starts detecting updates from the root of the\n\
      \032           replicas\226\128\148i.e., from the empty path. If the empty path matches\n\
      \032           an ignore pattern and does not match an ignorenot pattern,\n\
      \032           then the whole replica will be ignored. (For this reason, it\n\
      \032           is not a good idea to include Name * as an ignore pattern. If\n\
      \032           you want to ignore everything except a certain set of files,\n\
      \032           use Name ?*.)\n\
      \032         + If the root is a directory, Unison continues looking for\n\
      \032           updates in all the immediate children of the root. Again, if\n\
      \032           the name of some child matches an ignore pattern and does not\n\
      \032           match an ignorenot pattern, then this whole path including\n\
      \032           everything below it will be ignored.\n\
      \032         + If any of the non-ignored children are directories, then the\n\
      \032           process continues recursively.\n\
      \n\
      Symbolic Links\n\
      \n\
      \032  Ordinarily, Unison treats symbolic links in Unix replicas as \226\128\156opaque\226\128\157:\n\
      \032  it considers the contents of the link to be just the string specifying\n\
      \032  where the link points, and it will propagate changes in this string to\n\
      \032  the other replica.\n\
      \n\
      \032  It is sometimes useful to treat a symbolic link \226\128\156transparently,\226\128\157 acting\n\
      \032  as though whatever it points to were physically in the replica at the\n\
      \032  point where the symbolic link appears. To tell Unison to treat a link\n\
      \032  in this manner, add a line of the form\n\
      \032            follow = pathspec\n\
      \n\
      \032  to the profile, where pathspec is a path pattern as described in the\n\
      \032  section \226\128\156Path Specification\226\128\157 .\n\
      \n\
      \032  Not all Windows versions and file systems support symbolic links;\n\
      \032  Unison will refuse to propagate an opaque symbolic link from Unix to\n\
      \032  Windows and flag the path as erroneous if the support or privileges are\n\
      \032  lacking on the Windows side. When a Unix replica is to be synchronized\n\
      \032  with such Windows system, all symbolic links should match either an\n\
      \032  ignore pattern or a follow pattern.\n\
      \n\
      \032  You may need to acquire extra privileges to create symbolic links under\n\
      \032  Windows. By default, this is only allowed for administrators. Unison\n\
      \032  may not be able to automatically detect support for symbolic links\n\
      \032  under Windows. In that case, set the preference links to true\n\
      \032  explicitly.\n\
      \n\
      Permissions\n\
      \n\
      \032  Synchronizing the permission bits of files is slightly tricky when two\n\
      \032  different filesystems are involved (e.g., when synchronizing a Windows\n\
      \032  client and a Unix server). In detail, here\226\128\153s how it works:\n\
      \032    * When the permission bits of an existing file or directory are\n\
      \032      changed, the values of those bits that make sense on both operating\n\
      \032      systems will be propagated to the other replica. The other bits\n\
      \032      will not be changed.\n\
      \032    * When a newly created file is propagated to a remote replica, the\n\
      \032      permission bits that make sense in both operating systems are also\n\
      \032      propagated. The values of the other bits are set to default values\n\
      \032      (they are taken from the current umask, if the receiving host is a\n\
      \032      Unix system).\n\
      \032    * For security reasons, the Unix setuid and setgid bits are not\n\
      \032      propagated.\n\
      \032    * The Unix owner and group ids are not propagated. (What would this\n\
      \032      mean, in general?) All files are created with the owner and group\n\
      \032      of the server process.\n\
      \n\
      Cross-Platform Synchronization\n\
      \n\
      \032  If you use Unison to synchronize files between Windows and Unix\n\
      \032  systems, there are a few special issues to be aware of.\n\
      \n\
      \032  Case conflicts. In Unix, filenames are case sensitive: foo and FOO can\n\
      \032  refer to different files. In Windows, on the other hand, filenames are\n\
      \032  not case sensitive: foo and FOO can only refer to the same file. This\n\
      \032  means that a Unix foo and FOO cannot be synchronized onto a Windows\n\
      \032  system \226\128\148 Windows won\226\128\153t allow two different files to have the \226\128\156same\226\128\157\n\
      \032  name. Unison detects this situation for you, and reports that it cannot\n\
      \032  synchronize the files.\n\
      \n\
      \032  You can deal with a case conflict in a couple of ways. If you need to\n\
      \032  have both files on the Windows system, your only choice is to rename\n\
      \032  one of the Unix files to avoid the case conflict, and re-synchronize.\n\
      \032  If you don\226\128\153t need the files on the Windows system, you can simply\n\
      \032  disregard Unison\226\128\153s warning message, and go ahead with the\n\
      \032  synchronization; Unison won\226\128\153t touch those files. If you don\226\128\153t want to\n\
      \032  see the warning on each synchronization, you can tell Unison to ignore\n\
      \032  the files (see the section \226\128\156Ignoring Paths\226\128\157 ).\n\
      \n\
      \032  Illegal filenames. Unix allows some filenames that are illegal in\n\
      \032  Windows. For example, colons (\226\128\152:\226\128\153) are not allowed in Windows\n\
      \032  filenames, but they are legal in Unix filenames. This means that a Unix\n\
      \032  file foo:bar can\226\128\153t be synchronized to a Windows system. As with case\n\
      \032  conflicts, Unison detects this situation for you, and you have the same\n\
      \032  options: you can either rename the Unix file and re-synchronize, or you\n\
      \032  can ignore it.\n\
      \n\
      Slow Links\n\
      \n\
      \032  Unison is built to run well even over relatively slow links such as\n\
      \032  modems and DSL connections.\n\
      \n\
      \032  Unison uses the \226\128\156rsync protocol\226\128\157 designed by Andrew Tridgell and Paul\n\
      \032  Mackerras to greatly speed up transfers of large files in which only\n\
      \032  small changes have been made. More information about the rsync protocol\n\
      \032  can be found at the rsync web site (http://samba.anu.edu.au/rsync/).\n\
      \n\
      \032  If you are using Unison with ssh, you may get some speed improvement by\n\
      \032  enabling ssh\226\128\153s compression feature. Do this by adding the option\n\
      \032  \226\128\156-sshargs -C\226\128\157 to the command line or \226\128\156sshargs = -C\226\128\157 to your profile.\n\
      \n\
      Making Unison Faster on Large Files\n\
      \n\
      \032  Unison\226\128\153s built-in implementation of the rsync algorithm makes\n\
      \032  transferring updates to existing files pretty fast. However, for\n\
      \032  whole-file copies of newly created files, the built-in transfer method\n\
      \032  is not highly optimized. Also, if Unison is interrupted in the middle\n\
      \032  of transferring a large file, it will attempt to retransfer the whole\n\
      \032  thing on the next run.\n\
      \n\
      \032  These shortcomings can be addressed with a little extra work by telling\n\
      \032  Unison to use an external file copying utility for whole-file\n\
      \032  transfers. The recommended one is the standalone rsync tool, which is\n\
      \032  available by default on most Unix systems and can easily be installed\n\
      \032  on Windows systems using Cygwin.\n\
      \n\
      \032  If you have rsync installed on both hosts, you can make Unison use it\n\
      \032  simply by setting the copythreshold flag to something non-negative. If\n\
      \032  you set it to 0, Unison will use the external copy utility for all\n\
      \032  whole-file transfers. (This is probably slower than letting Unison copy\n\
      \032  small files by itself, but can be useful for testing.) If you set it to\n\
      \032  a larger value, Unison will use the external utility for all files\n\
      \032  larger than this size (which is given in kilobytes, so setting it to\n\
      \032  1000 will cause the external tool to be used for all transfers larger\n\
      \032  than a megabyte).\n\
      \n\
      \032  If you want to use a different external copy utility, set both the\n\
      \032  copyprog and copyprogrest preferences\226\128\148the former is used for the first\n\
      \032  transfer of a file, while the latter is used when Unison sees a\n\
      \032  partially transferred temp file on the receiving host. Be careful here:\n\
      \032  Your external tool needs to be instructed to copy files in place\n\
      \032  (otherwise if the transfer is interrupted Unison will not notice that\n\
      \032  some of the data has already been transferred, the next time it tries).\n\
      \032  The default values are:\n\
      \032  copyprog      =   rsync --inplace --compress\n\
      \032  copyprogrest  =   rsync --partial --inplace --compress\n\
      \n\
      \032  You may also need to set the copyquoterem preference. When it is set to\n\
      \032  true, this causes Unison to add an extra layer of quotes to the remote\n\
      \032  path passed to the external copy program. This is is needed by rsync,\n\
      \032  for example, which internally uses an ssh connection, requiring an\n\
      \032  extra level of quoting for paths containing spaces. When this flag is\n\
      \032  set to default, extra quotes are added if the value of copyprog\n\
      \032  contains the string rsync. The default value is default, naturally.\n\
      \n\
      \032  If a directory transfer is interrupted, the next run of Unison will\n\
      \032  automatically skip any files that were completely transferred before\n\
      \032  the interruption. (This behavior is always on: it does not depend on\n\
      \032  the setting of the copythreshold preference.) Note, though, that the\n\
      \032  new directory will not appear in the destination filesystem until\n\
      \032  everything has been transferred\226\128\148partially transferred directories are\n\
      \032  kept in a temporary location (with names like .unison.DIRNAME....)\n\
      \032  until the transfer is complete.\n\
      \n\
      Fast Update Detection\n\
      \n\
      \032  If your replicas are large and at least one of them is on a Windows\n\
      \032  system, you may find that Unison\226\128\153s default method for detecting changes\n\
      \032  (which involves scanning the full contents of every file on every\n\
      \032  sync\226\128\148the only completely safe way to do it under Windows) is too slow.\n\
      \032  Unison provides a preference fastcheck that, when set to true, causes\n\
      \032  it to use file creation times as \226\128\153pseudo inode numbers\226\128\153 when scanning\n\
      \032  replicas for updates, instead of reading the full contents of every\n\
      \032  file.\n\
      \n\
      \032  When fastcheck is set to no, Unison will perform slow\n\
      \032  checking\226\128\148re-scanning the contents of each file on each\n\
      \032  synchronization\226\128\148on all replicas. When fastcheck is set to default\n\
      \032  (which, naturally, is the default), Unison will use fast checks on Unix\n\
      \032  replicas and slow checks on Windows replicas.\n\
      \n\
      \032  This strategy may cause Unison to miss propagating an update if the\n\
      \032  modification time and length of the file are both unchanged by the\n\
      \032  update. However, Unison will never overwrite such an update with a\n\
      \032  change from the other replica, since it always does a safe check for\n\
      \032  updates just before propagating a change. Thus, it is reasonable to use\n\
      \032  this switch most of the time and occasionally run Unison once with\n\
      \032  fastcheck set to no, if you are worried that Unison may have overlooked\n\
      \032  an update.\n\
      \n\
      \032  Fastcheck is (always) automatically disabled for files with extension\n\
      \032  .xls or .mpp, to prevent Unison from being confused by the habits of\n\
      \032  certain programs (Excel, in particular) of updating files without\n\
      \032  changing their modification times.\n\
      \n\
      Mount Points and Removable Media\n\
      \n\
      \032  Using Unison removable media such as USB drives can be dangerous unless\n\
      \032  you are careful. If you synchronize a directory that is stored on\n\
      \032  removable media when the media is not present, it will look to Unison\n\
      \032  as though the whole directory has been deleted, and it will proceed to\n\
      \032  delete the directory from the other replica\226\128\148probably not what you want!\n\
      \n\
      \032  To prevent accidents, Unison provides a preference called mountpoint.\n\
      \032  Including a line like\n\
      \032            mountpoint = foo\n\
      \n\
      \032  in your preference file will cause Unison to check, after it finishes\n\
      \032  detecting updates, that something actually exists at the path foo on\n\
      \032  both replicas; if it does not, the Unison run will abort.\n\
      \n\
      Click-starting Unison\n\
      \n\
      \032  On Windows NT/2k/XP systems, the graphical version of Unison can be\n\
      \032  invoked directly by clicking on its icon. On Windows 95/98 systems,\n\
      \032  click-starting also works, as long as you are not using ssh. Due to an\n\
      \032  incompatibility with OCaml and Windows 95/98 that is not under our\n\
      \032  control, you must start Unison from a DOS window in Windows 95/98 if\n\
      \032  you want to use ssh.\n\
      \n\
      \032  When you click on the Unison icon, two windows will be created:\n\
      \032  Unison\226\128\153s regular window, plus a console window, which is used only for\n\
      \032  giving your password to ssh (if you do not use ssh to connect, you can\n\
      \032  ignore this window). When your password is requested, you\226\128\153ll need to\n\
      \032  activate the console window (e.g., by clicking in it) before typing. If\n\
      \032  you start Unison from a DOS window, Unison\226\128\153s regular window will appear\n\
      \032  and you will type your password in the DOS window you were using.\n\
      \n\
      \032  To use Unison in this mode, you must first create a profile (see the\n\
      \032  section \226\128\156Profiles\226\128\157 ). Use your favorite editor for this.\n\
      \n\
      "))
::
    ("", ("Junk", 
     "Junk\n\
      \032    __________________________________________________________________\n\
      \n\
      \032    This document was translated from L^AT[E]X by [1]H^EV^EA.\n\
      \n\
      References\n\
      \n\
      \032  1. http://hevea.inria.fr/index.html\n\
      "))
::
    [];;

