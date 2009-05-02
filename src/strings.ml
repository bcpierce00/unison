(* DO NOT MODIFY.
   This file has been automatically generated, see docs.ml. *)

let docs =
    ("about", ("About Unison", 
     "Unison File Synchronizer\n\
      Version 2.32.9\n\
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
      \032  filesystems (Coda (http://www.coda.cs.cmu.edu/), etc.),\n\
      \032  uni-directional mirroring utilities (rsync\n\
      \032  (http://samba.anu.edu.au/rsync/), etc.), and other synchronizers\n\
      \032  (Intellisync (http://www.pumatech.com), Reconcile\n\
      \032  (http://www.merl.com/reports/TR99-14/), etc). However, there are\n\
      \032  several points where it differs:\n\
      \032    * Unison runs on both Windows (95, 98, NT, 2k, and XP) and Unix\n\
      \032      (OSX, Solaris, Linux, etc.) systems. Moreover, Unison works across\n\
      \032      platforms, allowing you to synchronize a Windows laptop with a\n\
      \032      Unix server, for example.\n\
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
      \032    * Unison is resilient to failure. It is careful to leave the\n\
      \032      replicas and its own private structures in a sensible state at all\n\
      \032      times, even in case of abnormal termination or communication\n\
      \032      failures.\n\
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
      \032  (http://www.cis.upenn.edu/~bcpierce/), and J\233r\244me Vouillon\n\
      \032  (http://www.pps.jussieu.fr/~vouillon/), with Alan Schmitt\n\
      \032  (http://alan.petitepomme.net/), Malo Denielou, Zhe Yang\n\
      \032  (http://www.brics.dk/~zheyang/), Sylvain Gommier, and Matthieu Goulay.\n\
      \032  The Mac user interface was started by Trevor Jim and enormously\n\
      \032  improved by Ben Willmore. Our implementation of the rsync\n\
      \032  (http://samba.org/rsync/) protocol was built by Norman Ramsey\n\
      \032  (http://www.eecs.harvard.edu/~nr/) and Sylvain Gommier. It is is based\n\
      \032  on Andrew Tridgell (http://samba.anu.edu.au/~tridge/)'s thesis work\n\
      \032  (http://samba.anu.edu.au/~tridge/phd_thesis.pdf) and inspired by his\n\
      \032  rsync (http://samba.org/rsync/) utility. The mirroring and merging\n\
      \032  functionality was implemented by Sylvain Roy, improved by Malo\n\
      \032  Denielou, and improved yet further by St\233phane Lescuyer. Jacques\n\
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
      \032  Moderated mailing lists are available for bug reporting, announcements\n\
      \032  of new versions, discussions among users, and discussions among\n\
      \032  developers. See\n\
      \n\
      \032    http://www.cis.upenn.edu/~bcpierce/unison/lists.html\n\
      \n\
      \032  for more information.\n\
      \n\
      "))
::
    ("status", ("Development Status", 
     "Development Status\n\
      \n\
      \032  Unison is no longer under active development as a research project.\n\
      \032  (Our research efforts are now focused on a follow-on project called\n\
      \032  Harmony, described at http://www.cis.upenn.edu/~bcpierce/harmony.) At\n\
      \032  this point, there is no one whose job it is to maintain Unison, fix\n\
      \032  bugs, or answer questions.\n\
      \n\
      \032  However, the original developers are all still using Unison daily. It\n\
      \032  will continue to be maintained and supported for the foreseeable\n\
      \032  future, and we will occasionally release new versions with bug fixes,\n\
      \032  small improvements, and contributed patches.\n\
      \n\
      \032  Reports of bugs affecting correctness or safety are of interest to\n\
      \032  many people and will generally get high priority. Other bug reports\n\
      \032  will be looked at as time permits. Bugs should be reported to the\n\
      \032  users list at unison-users@yahoogroups.com\n\
      \032  (mailto:unison-users@yahoogroups.com).\n\
      \n\
      \032  Feature requests are welcome, but will probably just be added to the\n\
      \032  ever-growing todo list. They should also be sent to\n\
      \032  unison-users@yahoogroups.com (mailto:unison-users@yahoogroups.com).\n\
      \n\
      \032  Patches are even more welcome. They should be sent to\n\
      \032  unison-hackers@lists.seas.upenn.edu\n\
      \032  (mailto:unison-hackers@lists.seas.upenn.edu). (Since safety and\n\
      \032  robustness are Unison's most important properties, patches will be\n\
      \032  held to high standards of clear design and clean coding.) If you want\n\
      \032  to contribute to Unison, start by downloading the developer tarball\n\
      \032  from the download page. For some details on how the code is organized,\n\
      \032  etc., see the file CONTRIB.\n\
      \n\
      "))
::
    ("copying", ("Copying", 
     "Copying\n\
      \n\
      \032  This file is part of Unison.\n\
      \n\
      \032  Unison is free software: you can redistribute it and/or modify it\n\
      \032  under the terms of the GNU General Public License as published by the\n\
      \032  Free Software Foundation, either version 3 of the License, or (at your\n\
      \032  option) any later version.\n\
      \n\
      \032  Unison is distributed in the hope that it will be useful, but WITHOUT\n\
      \032  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or\n\
      \032  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License\n\
      \032  for more details.\n\
      \n\
      \032  The GNU Public License can be found at http://www.gnu.org/licenses. A\n\
      \032  copy is also included in the Unison source distribution in the file\n\
      \032  COPYING.\n\
      \n\
      "))
::
    ("ack", ("Acknowledgements", 
     "Acknowledgements\n\
      \n\
      \032  Work on Unison has been supported by the National Science Foundation\n\
      \032  under grants CCR-9701826 and ITR-0113226, Principles and Practice of\n\
      \032  Synchronization, and by University of Pennsylvania's Institute for\n\
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
      \032  (http://www.cis.upenn.edu/~bcpierce/unison/faq.html) helpful.\n\
      \032  Pre-built binaries are available for a variety of platforms.\n\
      \n\
      \032  Unison can be used with either of two user interfaces:\n\
      \032   1. a simple textual interface, suitable for dumb terminals (and\n\
      \032      running from scripts), and\n\
      \032   2. a more sophisticated grapical interface, based on Gtk2.\n\
      \n\
      \032  You will need to install a copy of Unison on every machine that you\n\
      \032  want to synchronize. However, you only need the version with a\n\
      \032  graphical user interface (if you want a GUI at all) on the machine\n\
      \032  where you're actually going to display the interface (the CLIENT\n\
      \032  machine). Other machines that you synchronize with can get along just\n\
      \032  fine with the textual version.\n\
      \n\
      Downloading Unison\n\
      \n\
      \032  The Unison download site lives under\n\
      \032  http://www.cis.upenn.edu/~bcpierce/unison.\n\
      \n\
      \032  If a pre-built binary of Unison is available for the client machine's\n\
      \032  architecture, just download it and put it somewhere in your search\n\
      \032  path (if you're going to invoke it from the command line) or on your\n\
      \032  desktop (if you'll be click-starting it).\n\
      \n\
      \032  The executable file for the graphical version (with a name including\n\
      \032  gtkui) actually provides both interfaces: the graphical one appears by\n\
      \032  default, while the textual interface can be selected by including -ui\n\
      \032  text on the command line. The textui executable provides just the\n\
      \032  textual interface.\n\
      \n\
      \032  If you don't see a pre-built executable for your architecture, you'll\n\
      \032  need to build it yourself. See the section \"Building Unison\" . There\n\
      \032  are also a small number of contributed ports to other architectures\n\
      \032  that are not maintained by us. See the Contributed Ports page\n\
      \032  (http://www.cis.upenn.edu/~bcpierce/unison/download.html) to check\n\
      \032  what's available.\n\
      \n\
      \032  Check to make sure that what you have downloaded is really executable.\n\
      \032  Either click-start it, or type \"unison -version\" at the command line.\n\
      \n\
      \032  Unison can be used in three different modes: with different\n\
      \032  directories on a single machine, with a remote machine over a direct\n\
      \032  socket connection, or with a remote machine using ssh for\n\
      \032  authentication and secure transfer. If you intend to use the last\n\
      \032  option, you may need to install ssh; see the section \"Installing Ssh\"\n\
      \032  .\n\
      \n\
      Running Unison\n\
      \n\
      \032  Once you've got Unison installed on at least one system, read the\n\
      \032  section \"Tutorial\" of the user manual (or type \"unison -doc tutorial\")\n\
      \032  for instructions on how to get started.\n\
      \n\
      Upgrading\n\
      \n\
      \032  Upgrading to a new version of Unison is as simple as throwing away the\n\
      \032  old binary and installing the new one.\n\
      \n\
      \032  Before upgrading, it is a good idea to run the old version one last\n\
      \032  time, to make sure all your replicas are completely synchronized. A\n\
      \032  new version of Unison will sometimes introduce a different format for\n\
      \032  the archive files used to remember information about the previous\n\
      \032  state of the replicas. In this case, the old archive will be ignored\n\
      \032  (not deleted -- if you roll back to the previous version of Unison,\n\
      \032  you will find the old archives intact), which means that any\n\
      \032  differences between the replicas will show up as conflicts that need\n\
      \032  to be resolved manually.\n\
      \n\
      Building Unison from Scratch\n\
      \n\
      \032  If a pre-built image is not available, you will need to compile it\n\
      \032  from scratch; the sources are available from the same place as the\n\
      \032  binaries.\n\
      \n\
      \032  In principle, Unison should work on any platform to which OCaml has\n\
      \032  been ported and on which the Unix module is fully implemented. It has\n\
      \032  been tested on many flavors of Windows (98, NT, 2000, XP) and Unix (OS\n\
      \032  X, Solaris, Linux, FreeBSD), and on both 32- and 64-bit architectures.\n\
      \n\
      Unix\n\
      \n\
      \032  You'll need the Objective Caml compiler (version 3.07 or later), which\n\
      \032  is available from http://caml.inria.fr. Building and installing OCaml\n\
      \032  on Unix systems is very straightforward; just follow the instructions\n\
      \032  in the distribution. You'll probably want to build the native-code\n\
      \032  compiler in addition to the bytecode compiler, as Unison runs much\n\
      \032  faster when compiled to native code, but this is not absolutely\n\
      \032  necessary. (Quick start: on many systems, the following sequence of\n\
      \032  commands will get you a working and installed compiler: first do make\n\
      \032  world opt, then su to root and do make install.)\n\
      \n\
      \032  You'll also need the GNU make utility, standard on many Unix systems.\n\
      \032  (Type \"make -version\" to check that you've got the GNU version.)\n\
      \n\
      \032  Once you've got OCaml installed, grab a copy of the Unison sources,\n\
      \032  unzip and untar them, change to the new \"unison\" directory, and type\n\
      \032  \"make UISTYLE=text.\" The result should be an executable file called\n\
      \032  \"unison\". Type \"./unison\" to make sure the program is executable. You\n\
      \032  should get back a usage message.\n\
      \n\
      \032  If you want to build the graphical user interface, you will need to\n\
      \032  install two additional things:\n\
      \032    * The Gtk2 libraries. These areavailable from http://www.gtk.org and\n\
      \032      are standard on many Unix installations.\n\
      \032    * The lablgtk2 OCaml library. Grab the developers' tarball from\n\
      \n\
      \032    http://wwwfun.kurims.kyoto-u.ac.jp/soft/olabl/lablgtk.html,\n\
      \032      untar it, and follow the instructions to build and install it.\n\
      \032      (Quick start: make configure, then make, then make opt, then su\n\
      \032      and make install.)\n\
      \n\
      \032  Now build unison. If your search paths are set up correctly, simply\n\
      \032  typing make again should build a unison executable with a Gtk2\n\
      \032  graphical interface. (In previous releases of Unison, it was necessary\n\
      \032  to add UISTYLE=gtk2 to the 'make' command above. This requirement has\n\
      \032  been removed: the makefile should detect automatically when lablgtk2\n\
      \032  is present and set this flag automatically.)\n\
      \n\
      \032  Put the unison executable somewhere in your search path, either by\n\
      \032  adding the Unison directory to your PATH variable or by copying the\n\
      \032  executable to some standard directory where executables are stored.\n\
      \n\
      Windows\n\
      \n\
      \032  Although the binary distribution should work on any version of\n\
      \032  Windows, some people may want to build Unison from scratch on those\n\
      \032  systems too.\n\
      \n\
      Bytecode version:\n\
      \n\
      \032  The simpler but slower compilation option to build a Unison executable\n\
      \032  is to build a bytecode version. You need first install Windows version\n\
      \032  of the OCaml compiler (version 3.07 or later, available from\n\
      \032  http://caml.inria.fr). Then grab a copy of Unison sources and type\n\
      \032      make NATIVE=false\n\
      \n\
      \032  to compile the bytecode. The result should be an executable file\n\
      \032  called unison.exe.\n\
      \n\
      Native version:\n\
      \n\
      \032  Building a more efficient, native version of Unison on Windows\n\
      \032  requires a little more work. See the file INSTALL.win32 in the source\n\
      \032  code distribution.\n\
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
      \032  either the text or the graphical interface, depending on which has\n\
      \032  been selected as default when the executable you are running was\n\
      \032  built. You can force the text interface even if graphical is the\n\
      \032  default by adding -ui text. The other command-line arguments to both\n\
      \032  versions are identical.\n\
      \n\
      \032  The graphical version can also be run directly by clicking on its\n\
      \032  icon, but this may require a little set-up (see the section\n\
      \032  \"Click-starting Unison\" ). For this tutorial, we assume that you're\n\
      \032  starting it from the command line.\n\
      \n\
      \032  Unison can synchronize files and directories on a single machine, or\n\
      \032  between two machines on a network. (The same program runs on both\n\
      \032  machines; the only difference is which one is responsible for\n\
      \032  displaying the user interface.) If you're only interested in a\n\
      \032  single-machine setup, then let's call that machine the CLIENT . If\n\
      \032  you're synchronizing two machines, let's call them CLIENT and SERVER .\n\
      \n\
      Local Usage\n\
      \n\
      \032  Let's get the client machine set up first and see how to synchronize\n\
      \032  two directories on a single machine.\n\
      \n\
      \032  Follow the instructions in the section \"Installation\" to either\n\
      \032  download or build an executable version of Unison, and install it\n\
      \032  somewhere on your search path. (If you just want to use the textual\n\
      \032  user interface, download the appropriate textui binary. If you just\n\
      \032  want to the graphical interface--or if you will use both interfaces\n\
      \032  [the gtkui binary actually has both compiled in]--then download the\n\
      \032  gtkui binary.)\n\
      \n\
      \032  Create a small test directory a.tmp containing a couple of files\n\
      \032  and/or subdirectories, e.g.,\n\
      \032      mkdir a.tmp\n\
      \032      touch a.tmp/a a.tmp/b\n\
      \032      mkdir a.tmp/d\n\
      \032      touch a.tmp/d/f\n\
      \n\
      \032  Copy this directory to b.tmp:\n\
      \032      cp -r a.tmp b.tmp\n\
      \n\
      \032  Now try synchronizing a.tmp and b.tmp. (Since they are identical,\n\
      \032  synchronizing them won't propagate any changes, but Unison will\n\
      \032  remember the current state of both directories so that it will be able\n\
      \032  to tell next time what has changed.) Type:\n\
      \032      unison a.tmp b.tmp\n\
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
      \032  indicates that the file c has been modified only in the second\n\
      \032  replica, and that the default action is therefore to propagate the new\n\
      \032  version to the first replica. To follw Unison's recommendation, press\n\
      \032  the \"f\" at the prompt.\n\
      \n\
      \032  If both replicas are modified and their contents are different, then\n\
      \032  the changes are in conflict: <-?-> is displayed to indicate that\n\
      \032  Unison needs guidance on which replica should override the other.\n\
      \032    new file  <-?->  new file   d/h  []\n\
      \n\
      \032  By default, neither version will be propagated and both replicas will\n\
      \032  remain as they are.\n\
      \n\
      \032  If both replicas have been modified but their new contents are the\n\
      \032  same (as with the file b), then no propagation is necessary and\n\
      \032  nothing is shown. Unison simply notes that the file is up to date.\n\
      \n\
      \032  These display conventions are used by both versions of the user\n\
      \032  interface. The only difference lies in the way in which Unison's\n\
      \032  default actions are either accepted or overriden by the user.\n\
      \n\
      \032  Textual Interface:\n\
      \032    * The status of each modified file is displayed, in turn. When the\n\
      \032      copies of a file in the two replicas are not identical, the user\n\
      \032      interface will ask for instructions as to how to propagate the\n\
      \032      change. If some default action is indicated (by an arrow), you can\n\
      \032      simply press Return to go on to the next changed file. If you want\n\
      \032      to do something different with this file, press \"<\" or \">\" to\n\
      \032      force the change to be propagated from right to left or from left\n\
      \032      to right, or else press \"/\" to skip this file and leave both\n\
      \032      replicas alone. When it reaches the end of the list of modified\n\
      \032      files, Unison will ask you one more time whether it should proceed\n\
      \032      with the updates that have been selected.\n\
      \032      When Unison stops to wait for input from the user, pressing \"?\"\n\
      \032      will always give a list of possible responses and their meanings.\n\
      \n\
      \032  Graphical Interface:\n\
      \032    * The main window shows all the files that have been modified in\n\
      \032      either a.tmp or b.tmp. To override a default action (or to select\n\
      \032      an action in the case when there is no default), first select the\n\
      \032      file, either by clicking on its name or by using the up- and\n\
      \032      down-arrow keys. Then press either the left-arrow or \"<\" key (to\n\
      \032      cause the version in b.tmp to propagate to a.tmp) or the\n\
      \032      right-arrow or \">\" key (which makes the a.tmp version override\n\
      \032      b.tmp).\n\
      \032      Every keyboard command can also be invoked from the menus at the\n\
      \032      top of the user interface. (Conversely, each menu item is\n\
      \032      annotated with its keyboard equivalent, if it has one.)\n\
      \032      When you are satisfied with the directions for the propagation of\n\
      \032      changes as shown in the main window, click the \"Go\" button to set\n\
      \032      them in motion. A check sign will be displayed next to each\n\
      \032      filename when the file has been dealt with.\n\
      \n\
      Remote Usage\n\
      \n\
      \032  Next, we'll get Unison set up to synchronize replicas on two different\n\
      \032  machines.\n\
      \n\
      \032  Follow the instructions in the Installation section to download or\n\
      \032  build an executable version of Unison on the server machine, and\n\
      \032  install it somewhere on your search path. (It doesn't matter whether\n\
      \032  you install the textual or graphical version, since the copy of Unison\n\
      \032  on the server doesn't need to display any user interface at all.)\n\
      \n\
      \032  It is important that the version of Unison installed on the server\n\
      \032  machine is the same as the version of Unison on the client machine.\n\
      \032  But some flexibility on the version of Unison at the client side can\n\
      \032  be achieved by using the -addversionno option; see the section\n\
      \032  \"Preferences\" .\n\
      \n\
      \032  Now there is a decision to be made. Unison provides two methods for\n\
      \032  communicating between the client and the server:\n\
      \032    * Remote shell method: To use this method, you must have some way of\n\
      \032      invoking remote commands on the server from the client's command\n\
      \032      line, using a facility such as ssh. This method is more convenient\n\
      \032      (since there is no need to manually start a \"unison server\"\n\
      \032      process on the server) and also more secure (especially if you use\n\
      \032      ssh).\n\
      \032    * Socket method: This method requires only that you can get TCP\n\
      \032      packets from the client to the server and back. A draconian\n\
      \032      firewall can prevent this, but otherwise it should work anywhere.\n\
      \n\
      \032  Decide which of these you want to try, and continue with the section\n\
      \032  \"Remote Shell Method\" or the section \"Socket Method\" , as appropriate.\n\
      \n\
      Remote Shell Method\n\
      \n\
      \032  The standard remote shell facility on Unix systems is ssh, which\n\
      \032  provides the same functionality as the older rsh but much better\n\
      \032  security. Ssh is available from ftp://ftp.cs.hut.fi/pub/ssh/;\n\
      \032  up-to-date binaries for some architectures can also be found at\n\
      \032  ftp://ftp.faqs.org/ssh/contrib. See section [1]A.2 for installation\n\
      \032  instructions for the Windows version.\n\
      \n\
      \032  Running ssh requires some coordination between the client and server\n\
      \032  machines to establish that the client is allowed to invoke commands on\n\
      \032  the server; please refer to the or ssh documentation for information\n\
      \032  on how to set this up. The examples in this section use ssh, but you\n\
      \032  can substitute rsh for ssh if you wish.\n\
      \n\
      \032  First, test that we can invoke Unison on the server from the client.\n\
      \032  Typing\n\
      \032       ssh remotehostname unison -version\n\
      \n\
      \032  should print the same version information as running\n\
      \032       unison -version\n\
      \n\
      \032  locally on the client. If remote execution fails, then either\n\
      \032  something is wrong with your ssh setup (e.g., \"permission denied\") or\n\
      \032  else the search path that's being used when executing commands on the\n\
      \032  server doesn't contain the unison executable (e.g., \"command not\n\
      \032  found\").\n\
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
      \032  The result should be that the entire directory a.tmp is propagated\n\
      \032  from the client to your home directory on the server.\n\
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
      \032    * You can give an explicit path for the unison executable on the\n\
      \032      server by using the command-line option \"-servercmd\n\
      \032      /full/path/name/of/unison\" or adding\n\
      \032      \"servercmd=/full/path/name/of/unison\" to your profile (see the\n\
      \032      section \"Profile\" ). Similarly, you can specify a explicit path\n\
      \032      for the ssh program using the \"-sshcmd\" option. Extra arguments\n\
      \032      can be passed to ssh by setting the -sshargs preference.\n\
      \n\
      Socket Method\n\
      \n\
      \032    Warning: The socket method is insecure: not only are the texts of\n\
      \032    your changes transmitted over the network in unprotected form, it\n\
      \032    is also possible for anyone in the world to connect to the server\n\
      \032    process and read out the contents of your filesystem! (Of course,\n\
      \032    to do this they must understand the protocol that Unison uses to\n\
      \032    communicate between client and server, but all they need for this\n\
      \032    is a copy of the Unison sources.) The socket method is provided\n\
      \032    only for expert users with specific needs; everyone else should use\n\
      \032    the ssh method.\n\
      \n\
      \032  To run Unison over a socket connection, you must start a Unison daemon\n\
      \032  process on the server. This process runs continuously, waiting for\n\
      \032  connections over a given socket from client machines running Unison\n\
      \032  and processing their requests in turn.\n\
      \n\
      \032  To start the daemon, type\n\
      \032      unison -socket NNNN\n\
      \n\
      \032  on the server machine, where NNNN is the socket number that the daemon\n\
      \032  should listen on for connections from clients. (NNNN can be any large\n\
      \032  number that is not being used by some other program; if NNNN is\n\
      \032  already in use, Unison will exit with an error message.) Note that\n\
      \032  paths specified by the client will be interpreted relative to the\n\
      \032  directory in which you start the server process; this behavior is\n\
      \032  different from the ssh case, where the path is relative to your home\n\
      \032  directory on the server.\n\
      \n\
      \032  Create a test directory a.tmp in your home directory on the client\n\
      \032  machine. Now type:\n\
      \032      unison a.tmp socket://remotehostname:NNNN/a.tmp\n\
      \n\
      \032  The result should be that the entire directory a.tmp is propagated\n\
      \032  from the client to the server (a.tmp will be created on the server in\n\
      \032  the directory that the server was started from). After finishing the\n\
      \032  first synchronization, change a few files and try synchronizing again.\n\
      \032  You should see similar results as in the local case.\n\
      \n\
      \032  Since the socket method is not used by many people, its functionality\n\
      \032  is rather limited. For example, the server can only deal with one\n\
      \032  client at a time.\n\
      \n\
      Using Unison for All Your Files\n\
      \n\
      \032  Once you are comfortable with the basic operation of Unison, you may\n\
      \032  find yourself wanting to use it regularly to synchronize your commonly\n\
      \032  used files. There are several possible ways of going about this:\n\
      \032   1. Synchronize your whole home directory, using the Ignore facility\n\
      \032      (see the section \"Ignore\" ) to avoid synchronizing temporary files\n\
      \032      and things that only belong on one host.\n\
      \032   2. Create a subdirectory called shared (or current, or whatever) in\n\
      \032      your home directory on each host, and put all the files you want\n\
      \032      to synchronize into this directory.\n\
      \032   3. Create a subdirectory called shared (or current, or whatever) in\n\
      \032      your home directory on each host, and put links to all the files\n\
      \032      you want to synchronize into this directory. Use the follow\n\
      \032      preference (see the section \"Symbolic Links\" ) to make Unison\n\
      \032      treat these links as transparent.\n\
      \032   4. Make your home directory the root of the synchronization, but tell\n\
      \032      Unison to synchronize only some of the files and subdirectories\n\
      \032      within it on any given run. This can be accomplished by using the\n\
      \032      -path switch on the command line:\n\
      \032      unison /home/username ssh://remotehost//home/username -path shared\n\
      \032      The -path option can be used as many times as needed, to\n\
      \032      synchronize several files or subdirectories:\n\
      \032      unison /home/username ssh://remotehost//home/username \\\n\
      \032         -path shared \\\n\
      \032         -path pub \\\n\
      \032         -path .netscape/bookmarks.html\n\
      \032      These -path arguments can also be put in your preference file. See\n\
      \032      the section \"Preferences\" for an example.\n\
      \n\
      \032  Most people find that they only need to maintain a profile (or\n\
      \032  profiles) on one of the hosts that they synchronize, since Unison is\n\
      \032  always initiated from this host. (For example, if you're synchronizing\n\
      \032  a laptop with a fileserver, you'll probably always run Unison on the\n\
      \032  laptop.) This is a bit different from the usual situation with\n\
      \032  asymmetric mirroring programs like rdist, where the mirroring\n\
      \032  operation typically needs to be initiated from the machine with the\n\
      \032  most recent changes. the section \"Profile\" covers the syntax of Unison\n\
      \032  profiles, together with some sample profiles.\n\
      \n\
      \032  Some tips on improving Unison's performance can be found on the\n\
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
      \032  organize the machines into a \"star topology,\" with one machine\n\
      \032  designated as the \"hub\" and the rest as \"spokes,\" and with each spoke\n\
      \032  machine synchronizing only with the hub. The big advantage of the star\n\
      \032  topology is that it eliminates the possibility of confusing \"spurious\n\
      \032  conflicts\" arising from the fact that a separate archive is maintained\n\
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
      \032  User's Manual (HTML or PostScript format) through\n\
      \032  http://www.cis.upenn.edu/~bcpierce/unison.\n\
      \n\
      \032  If you use Unison regularly, you should subscribe to one of the\n\
      \032  mailing lists, to receive announcements of new versions. See the\n\
      \032  section \"Mailing Lists\" .\n\
      \n\
      "))
::
    ("basics", ("Basic Concepts", 
     "Basic Concepts\n\
      \n\
      \032  To understand how Unison works, it is necessary to discuss a few\n\
      \032  straightforward concepts. These concepts are developed more rigorously\n\
      \032  and at more length in a number of papers, available at\n\
      \032  http://www.cis.upenn.edu/~bcpierce/papers. But the informal\n\
      \032  presentation here should be enough for most users.\n\
      \n\
      Roots\n\
      \n\
      \032  A replica's root tells Unison where to find a set of files to be\n\
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
      \032  ssh://, rsh:// to indicate that the remote server should be started\n\
      \032  with rsh or ssh:\n\
      \032     ssh://remotehost//absolute/path/of/root\n\
      \032     rsh://user@remotehost/relative/path/of/root\n\
      \n\
      \032  If the remote server is already running (in the socket mode), then the\n\
      \032  syntax\n\
      \032     socket://remotehost:portnum//absolute/path/of/root\n\
      \032     socket://remotehost:portnum/relative/path/of/root\n\
      \n\
      \032  is used to specify the hostname and the port that the client Unison\n\
      \032  should use to contact it.\n\
      \n\
      \032  The syntax for roots is based on that of URIs (described in RFC 2396).\n\
      \032  The full grammar is:\n\
      \032 replica ::= [protocol:]//[user@][host][:port][/path]\n\
      \032          |  path\n\
      \n\
      \032 protocol ::= file\n\
      \032           |  socket\n\
      \032           |  ssh\n\
      \032           |  rsh\n\
      \n\
      \032 user ::= [-_a-zA-Z0-9]+\n\
      \n\
      \032 host ::= [-_a-zA-Z0-9.]+\n\
      \n\
      \032 port ::= [0-9]+\n\
      \n\
      \032  When path is given without any protocol prefix, the protocol is\n\
      \032  assumed to be file:. Under Windows, it is possible to synchronize with\n\
      \032  a remote directory using the file: protocol over the Windows Network\n\
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
      \032  Formally, a path is just a sequence of names, separated by /. Note\n\
      \032  that the path separator character is always a forward slash, no matter\n\
      \032  what operating system Unison is running on. Forward slashes are\n\
      \032  converted to backslashes as necessary when paths are converted to\n\
      \032  filenames in the local filesystem on a particular host. (For example,\n\
      \032  suppose that we run Unison on a Windows system, synchronizing the\n\
      \032  local root c:\\pierce with the root\n\
      \032  ssh://saul.cis.upenn.edu/home/bcpierce on a Unix server. Then the path\n\
      \032  current/todo.txt refers to the file c:\\pierce\\current\\todo.txt on the\n\
      \032  client and /home/bcpierce/current/todo.txt on the server.)\n\
      \n\
      \032  The empty path (i.e., the empty sequence of names) denotes the whole\n\
      \032  replica. Unison displays the empty path as \"[root].\"\n\
      \n\
      \032  If p is a path and q is a path beginning with p, then q is said to be\n\
      \032  a descendant of p. (Each path is also a descendant of itself.)\n\
      \n\
      What is an Update?\n\
      \n\
      \032  The contents of a path p in a particular replica could be a file, a\n\
      \032  directory, a symbolic link, or absent (if p does not refer to anything\n\
      \032  at all in that replica). More specifically:\n\
      \032    * If p refers to an ordinary file, then the contents of p are the\n\
      \032      actual contents of this file (a string of bytes) plus the current\n\
      \032      permission bits of the file.\n\
      \032    * If p refers to a symbolic link, then the contents of p are just\n\
      \032      the string specifying where the link points.\n\
      \032    * If p refers to a directory, then the contents of p are just the\n\
      \032      token \"DIRECTORY\" plus the current permission bits of the\n\
      \032      directory.\n\
      \032    * If p does not refer to anything in this replica, then the contents\n\
      \032      of p are the token \"ABSENT.\"\n\
      \n\
      \032  Unison keeps a record of the contents of each path after each\n\
      \032  successful synchronization of that path (i.e., it remembers the\n\
      \032  contents at the last moment when they were the same in the two\n\
      \032  replicas).\n\
      \n\
      \032  We say that a path is updated (in some replica) if its current\n\
      \032  contents are different from its contents the last time it was\n\
      \032  successfully synchronized. Note that whether a path is updated has\n\
      \032  nothing to do with its last modification time--Unison considers only\n\
      \032  the contents when determining whether an update has occurred. This\n\
      \032  means that touching a file without changing its contents will not be\n\
      \032  recognized as an update. A file can even be changed several times and\n\
      \032  then changed back to its original contents; as long as Unison is only\n\
      \032  run at the end of this process, no update will be recognized.\n\
      \n\
      \032  What Unison actually calculates is a close approximation to this\n\
      \032  definition; see the section \"Caveats and Shortcomings\" .\n\
      \n\
      What is a Conflict?\n\
      \n\
      \032  A path is said to be conflicting if the following conditions all hold:\n\
      \032   1. it has been updated in one replica,\n\
      \032   2. it or any of its descendants has been updated in the other\n\
      \032      replica, and\n\
      \032   3. its contents in the two replicas are not identical.\n\
      \n\
      Reconciliation\n\
      \n\
      \032  Unison operates in several distinct stages:\n\
      \032   1. On each host, it compares its archive file (which records the\n\
      \032      state of each path in the replica when it was last synchronized)\n\
      \032      with the current contents of the replica, to determine which paths\n\
      \032      have been updated.\n\
      \032   2. It checks for \"false conflicts\" -- paths that have been updated on\n\
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
      \032      file on the receiving host, then atomically moving them into\n\
      \032      place.\n\
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
      \032    * At every moment, the information stored on disk about Unison's\n\
      \032      private state can be either (1) unchanged, or (2) updated to\n\
      \032      reflect those paths that have been successfully synchronized.\n\
      \n\
      \032  The upshot is that it is safe to interrupt Unison at any time, either\n\
      \032  manually or accidentally. [Caveat: the above is almost true there are\n\
      \032  occasionally brief periods where it is not (and, because of\n\
      \032  shortcoming of the Posix filesystem API, cannot be); in particular,\n\
      \032  when it is copying a file onto a directory or vice versa, it must\n\
      \032  first move the original contents out of the way. If Unison gets\n\
      \032  interrupted during one of these periods, some manual cleanup may be\n\
      \032  required. In this case, a file called DANGER.README will be left in\n\
      \032  your home directory, containing information about the operation that\n\
      \032  was interrupted. The next time you try to run Unison, it will notice\n\
      \032  this file and warn you about it.]\n\
      \n\
      \032  If an interruption happens while it is propagating updates, then there\n\
      \032  may be some paths for which an update has been propagated but which\n\
      \032  have not been marked as synchronized in Unison's archives. This is no\n\
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
      \032  archive format has changed and they cannot be read, or that they don't\n\
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
      \032  preference set to true--the default on Unix systems--Unison uses file\n\
      \032  modtimes for a quick first pass to tell which files have definitely\n\
      \032  not changed; then, for each file that might have changed, it computes\n\
      \032  a fingerprint of the file's contents and compares it against the\n\
      \032  last-synchronized contents. Also, the -times option allows you to\n\
      \032  synchronize file times, but it does not cause identical files to be\n\
      \032  changed; Unison will only modify the file times.)\n\
      \n\
      \032  It is safe to \"brainwash\" Unison by deleting its archive files on both\n\
      \032  replicas. The next time it runs, it will assume that all the files it\n\
      \032  sees in the replicas are new.\n\
      \n\
      \032  It is safe to modify files while Unison is working. If Unison\n\
      \032  discovers that it has propagated an out-of-date change, or that the\n\
      \032  file it is updating has changed on the target replica, it will signal\n\
      \032  a failure for that file. Run Unison again to propagate the latest\n\
      \032  change.\n\
      \n\
      \032  Changes to the ignore patterns from the user interface (e.g., using\n\
      \032  the `i' key) are immediately reflected in the current profile.\n\
      \n\
      Caveats and Shortcomings\n\
      \n\
      \032  Here are some things to be careful of when using Unison.\n\
      \032    * In the interests of speed, the update detection algorithm may\n\
      \032      (depending on which OS architecture that you run Unison on)\n\
      \032      actually use an approximation to the definition given in the\n\
      \032      section \"What is an Update?\" .\n\
      \032      In particular, the Unix implementation does not compare the actual\n\
      \032      contents of files to their previous contents, but simply looks at\n\
      \032      each file's inode number and modtime; if neither of these have\n\
      \032      changed, then it concludes that the file has not been changed.\n\
      \032      Under normal circumstances, this approximation is safe, in the\n\
      \032      sense that it may sometimes detect \"false updates\" will never miss\n\
      \032      a real one. However, it is possible to fool it, for example by\n\
      \032      using retouch to change a file's modtime back to a time in the\n\
      \032      past.\n\
      \032    * If you synchronize between a single-user filesystem and a shared\n\
      \032      Unix server, you should pay attention to your permission bits: by\n\
      \032      default, Unison will synchronize permissions verbatim, which may\n\
      \032      leave group-writable files on the server that could be written\n\
      \032      over by a lot of people.\n\
      \032      You can control this by setting your umask on both computers to\n\
      \032      something like 022, masking out the \"world write\" and \"group\n\
      \032      write\" permission bits.\n\
      \032      Unison does not synchronize the setuid and setgid bits, for\n\
      \032      security.\n\
      \032    * The graphical user interface is single-threaded. This means that\n\
      \032      if Unison is performing some long-running operation, the display\n\
      \032      will not be repainted until it finishes. We recommend not trying\n\
      \032      to do anything with the user interface while Unison is in the\n\
      \032      middle of detecting changes or propagating files.\n\
      \032    * Unison does not understand hard links.\n\
      \032    * It is important to be a little careful when renaming directories\n\
      \032      containing \"ignore\"d files.\n\
      \032      For example, suppose Unison is synchronizing directory A between\n\
      \032      the two machines called the \"local\" and the \"remote\" machine;\n\
      \032      suppose directory A contains a subdirectory D; and suppose D on\n\
      \032      the local machine contains a file or subdirectory P that matches\n\
      \032      an ignore directive in the profile used to synchronize. Thus path\n\
      \032      A/D/P exists on the local machine but not on the remote machine.\n\
      \032      If D is renamed to D' on the remote machine, and this change is\n\
      \032      propagated to the local machine, all such files or subdirectories\n\
      \032      P will be deleted. This is because Unison sees the rename as a\n\
      \032      delete and a separate create: it deletes the old directory\n\
      \032      (including the ignored files) and creates a new one (not including\n\
      \032      the ignored files, since they are completely invisible to it).\n\
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
      \032    * Typing \"unison profile\" on the command line. Unison will look for\n\
      \032      a file profile.prf in the .unison directory. If this file does not\n\
      \032      specify a pair of roots, Unison will prompt for them and add them\n\
      \032      to the information specified by the profile.\n\
      \032    * Typing \"unison profile root1 root2\" on the command line. In this\n\
      \032      case, Unison will use profile, which should not contain any root\n\
      \032      directives.\n\
      \032    * Typing \"unison root1 root2\" on the command line. This has the same\n\
      \032      effect as typing \"unison default root1 root2.\"\n\
      \032    * Typing just \"unison\" (or invoking Unison by clicking on a desktop\n\
      \032      icon). In this case, Unison will ask for the profile to use for\n\
      \032      synchronization (or create a new one, if necessary).\n\
      \n\
      The .unison Directory\n\
      \n\
      \032  Unison stores a variety of information in a private directory on each\n\
      \032  host. If the environment variable UNISON is defined, then its value\n\
      \032  will be used as the name of this directory. If UNISON is not defined,\n\
      \032  then the name of the directory depends on which operating system you\n\
      \032  are using. In Unix, the default is to use $HOME/.unison. In Windows,\n\
      \032  if the environment variable USERPROFILE is defined, then the directory\n\
      \032  will be $USERPROFILE\\.unison; otherwise if HOME is defined, it will be\n\
      \032  $HOME\\.unison; otherwise, it will be c:\\.unison.\n\
      \n\
      \032  The archive file for each replica is found in the .unison directory on\n\
      \032  that replica's host. Profiles (described below) are always taken from\n\
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
      \032  The function that finds the canonical hostname of the local host\n\
      \032  (which is used, for example, in calculating the name of the archive\n\
      \032  file used to remember which files have been synchronized) normally\n\
      \032  uses the gethostname operating system call. However, if the\n\
      \032  environment variable UNISONLOCALHOSTNAME is set, its value will be\n\
      \032  used instead. This makes it easier to use Unison in situations where a\n\
      \032  machine's name changes frequently (e.g., because it is a laptop and\n\
      \032  gets moved around a lot).\n\
      \n\
      \032  A more powerful way of changing archive names is provided by the\n\
      \032  rootalias preference. The preference file may contain any number of\n\
      \032  lines of the form:\n\
      \032   rootalias = //hostnameA//path-to-replicaA -> //hostnameB//path-to-replicaB\n\
      \n\
      \032  When calculating the name of the archive files for a given pair of\n\
      \032  roots, Unison replaces any root that matches the left-hand side of any\n\
      \032  rootalias rule by the corresponding right-hand side.\n\
      \n\
      \032  So, if you need to relocate a root on one of the hosts, you can add a\n\
      \032  rule of the form:\n\
      \032   rootalias = //new-hostname//new-path -> //old-hostname//old-path\n\
      \n\
      \032  Warning: The rootalias option is dangerous and should only be used if\n\
      \032  you are sure you know what you're doing. In particular, it should only\n\
      \032  be used if you are positive that either (1) both the original root and\n\
      \032  the new alias refer to the same set of files, or (2) the files have\n\
      \032  been relocated so that the original name is now invalid and will never\n\
      \032  be used again. (If the original root and the alias refer to different\n\
      \032  sets of files, Unison's update detector could get confused.) After\n\
      \032  introducing a new rootalias, it is a good idea to run Unison a few\n\
      \032  times interactively (with the batch flag off, etc.) and carefully\n\
      \032  check that things look reasonable--in particular, that update\n\
      \032  detection is working as expected.\n\
      \n\
      Preferences\n\
      \n\
      \032  Many details of Unison's behavior are configurable by user-settable\n\
      \032  \"preferences.\"\n\
      \n\
      \032  Some preferences are boolean-valued; these are often called flags.\n\
      \032  Others take numeric or string arguments, indicated in the preferences\n\
      \032  list by n or xxx. Most of the string preferences can be given several\n\
      \032  times; the arguments are accumulated into a list internally.\n\
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
      \032-auto              automatically accept default (nonconflicting) actions\n\
      \032-batch             batch mode: ask no questions at all\n\
      \032-doc xxx           show documentation ('-doc topics' lists topics)\n\
      \032-follow xxx        add a pattern to the follow list\n\
      \032-force xxx         force changes from this replica to the other\n\
      \032-group             synchronize group attributes\n\
      \032-ignore xxx        add a pattern to the ignore list\n\
      \032-ignorenot xxx     add a pattern to the ignorenot list\n\
      \032-owner             synchronize owner\n\
      \032-path xxx          path to synchronize\n\
      \032-perms n           part of the permissions which is synchronized\n\
      \032-prefer xxx        choose this replica's version for conflicting changes\n\
      \032-root xxx          root of a replica (should be used exactly twice)\n\
      \032-silent            print nothing except error messages\n\
      \032-terse             suppress status messages\n\
      \032-testserver        exit immediately after the connection to the server\n\
      \032-times             synchronize modification times\n\
      \032-version           print version and exit\n\
      \n\
      Advanced options:\n\
      \032-addprefsto xxx    file to add new prefs to\n\
      \032-addversionno      add version number to name of unison on server\n\
      \032-backup xxx        add a pattern to the backup list\n\
      \032-backupcurr xxx    add a pattern to the backupcurr list\n\
      \032-backupcurrnot xxx add a pattern to the backupcurrnot list\n\
      \032-backupdir xxx     directory for storing centralized backups\n\
      \032-backuploc xxx     where backups are stored ('local' or 'central')\n\
      \032-backupnot xxx     add a pattern to the backupnot list\n\
      \032-backupprefix xxx  prefix for the names of backup files\n\
      \032-backups           keep backup copies of all files (see also 'backup')\n\
      \032-backupsuffix xxx  a suffix to be added to names of backup files\n\
      \032-confirmbigdel     ask about whole-replica (or path) deletes (default true)\n\
      \032-confirmmerge      ask for confirmation before commiting results of a merge\n\
      \032-contactquietly    suppress the 'contacting server' message during startup\n\
      \032-copyprog xxx      external program for copying large files\n\
      \032-copyprogrest xxx  variant of copyprog for resuming partial transfers\n\
      \032-copyquoterem xxx  add quotes to remote file name for copyprog (true/false/def\n\
      ault)\n\
      \032-copythreshold n   use copyprog on files bigger than this (if >=0, in Kb)\n\
      \032-debug xxx         debug module xxx ('all' -> everything, 'verbose' -> more)\n\
      \032-diff xxx          command for showing differences between files\n\
      \032-dontchmod         When set, never use the chmod system call\n\
      \032-dumbtty           do not change terminal settings in text UI\n\
      \032-fastcheck xxx     do fast update detection (true/false/default)\n\
      \032-forcepartial xxx  add a pattern to the forcepartial list\n\
      \032-height n          height (in lines) of main window in graphical interface\n\
      \032-host xxx          bind the socket to this host name in server socket mode\n\
      \032-ignorecase xxx    identify upper/lowercase filenames (true/false/default)\n\
      \032-ignorelocks       ignore locks left over from previous run (dangerous!)\n\
      \032-immutable xxx     add a pattern to the immutable list\n\
      \032-immutablenot xxx  add a pattern to the immutablenot list\n\
      \032-key xxx           define a keyboard shortcut for this profile (in some UIs)\n\
      \032-killserver        kill server when done (even when using sockets)\n\
      \032-label xxx         provide a descriptive string label for this profile\n\
      \032-log               record actions in logfile (default true)\n\
      \032-logfile xxx       logfile name\n\
      \032-maxbackups n      number of backed up versions of a file\n\
      \032-maxthreads n      maximum number of simultaneous file transfers\n\
      \032-merge xxx         add a pattern to the merge list\n\
      \032-mountpoint xxx    abort if this path does not exist\n\
      \032-numericids        don't map uid/gid values by user/group names\n\
      \032-preferpartial xxx add a pattern to the preferpartial list\n\
      \032-pretendwin        Use creation times for detecting updates\n\
      \032-repeat xxx        synchronize repeatedly (text interface only)\n\
      \032-retry n           re-try failed synchronizations N times (text ui only)\n\
      \032-rootalias xxx     register alias for canonical root names\n\
      \032-rsrc xxx          synchronize resource forks (true/false/default)\n\
      \032-rsync             activate the rsync transfer mode (default true)\n\
      \032-selftest          run internal tests and exit\n\
      \032-servercmd xxx     name of unison executable on remote server\n\
      \032-showarchive       show 'true names' (for rootalias) of roots and archive\n\
      \032-socket xxx        act as a server on a socket\n\
      \032-sortbysize        list changed files by size, not name\n\
      \032-sortfirst xxx     add a pattern to the sortfirst list\n\
      \032-sortlast xxx      add a pattern to the sortlast list\n\
      \032-sortnewfirst      list new before changed files\n\
      \032-sshargs xxx       other arguments (if any) for remote shell command\n\
      \032-sshcmd xxx        path to the ssh executable\n\
      \032-ui xxx            select UI ('text' or 'graphic'); command-line only\n\
      \032-xferbycopying     optimize transfers using local copies (default true)\n\
      \n\
      \032  Here, in more detail, is what they do. Many are discussed in greater\n\
      \032  detail in other sections of the manual.\n\
      \032  addprefsto xxx\n\
      \032         By default, new preferences added by Unison (e.g., new ignore\n\
      \032         clauses) will be appended to whatever preference file Unison\n\
      \032         was told to load at the beginning of the run. Setting the\n\
      \032         preference addprefsto filename makes Unison add new preferences\n\
      \032         to the file named filename instead.\n\
      \032  addversionno \n\
      \032         When this flag is set to true, Unison will use\n\
      \032         unison-currentversionnumber instead of just unison as the\n\
      \032         remote server command. This allows multiple binaries for\n\
      \032         different versions of unison to coexist conveniently on the\n\
      \032         same server: whichever version is run on the client, the same\n\
      \032         version will be selected on the server.\n\
      \032  auto \n\
      \032         When set to true, this flag causes the user interface to skip\n\
      \032         asking for confirmations on non-conflicting changes. (More\n\
      \032         precisely, when the user interface is done setting the\n\
      \032         propagation direction for one entry and is about to move to the\n\
      \032         next, it will skip over all non-conflicting entries and go\n\
      \032         directly to the next conflict.)\n\
      \032  backup xxx\n\
      \032         Including the preference -backup pathspec causes Unison to keep\n\
      \032         backup files for each path that matches pathspec. These backup\n\
      \032         files are kept in the directory specified by the backuplocation\n\
      \032         preference. The backups are named according to the backupprefix\n\
      \032         and backupsuffix preferences. The number of versions that are\n\
      \032         kept is determined by the maxbackups preference.\n\
      \032         The syntax of pathspec is described in the section \"Path\n\
      \032         Specification\" .\n\
      \032  backupcurr xxx\n\
      \032         Including the preference -backupcurr pathspec causes Unison to\n\
      \032         keep a backup of the current version of every file matching\n\
      \032         pathspec. This file will be saved as a backup with version\n\
      \032         number 000. Such backups can be used as inputs to external\n\
      \032         merging programs, for instance. See the documentatation for the\n\
      \032         merge preference. For more details, see the section \"Merging\n\
      \032         Conflicting Versions\" .\n\
      \032         The syntax of pathspec is described in the section \"Path\n\
      \032         Specification\" .\n\
      \032  backupcurrnot xxx\n\
      \032         Exceptions to backupcurr, like the ignorenot preference.\n\
      \032  backupdir xxx\n\
      \032         If this preference is set, Unison will use it as the name of\n\
      \032         the directory used to store backup files specified by the\n\
      \032         backup preference, when backuplocation is set to central. It is\n\
      \032         checked after the UNISONBACKUPDIR environment variable.\n\
      \032  backuploc xxx\n\
      \032         This preference determines whether backups should be kept\n\
      \032         locally, near the original files, or in a central directory\n\
      \032         specified by the backupdir preference. If set to local, backups\n\
      \032         will be kept in the same directory as the original files, and\n\
      \032         if set to central, backupdir will be used instead.\n\
      \032  backupnot xxx\n\
      \032         The values of this preference specify paths or individual files\n\
      \032         or regular expressions that should not be backed up, even if\n\
      \032         the backup preference selects them--i.e., it selectively\n\
      \032         overrides backup. The same caveats apply here as with ignore\n\
      \032         and t ignorenot.\n\
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
      \032         One thing to be careful of: If the backuploc preference is set\n\
      \032         to local, Unison will automatically ignore all files whose\n\
      \032         prefix and suffix match backupprefix and backupsuffix. So be\n\
      \032         careful to choose values for these preferences that are\n\
      \032         sufficiently different from the names of your real files.\n\
      \032  backups \n\
      \032         Setting this flag to true is equivalent to setting\n\
      \032         backuplocation to local and backup to Name *.\n\
      \032  backupsuffix xxx\n\
      \032         See backupprefix for full documentation.\n\
      \032  batch \n\
      \032         When this is set to true, the user interface will ask no\n\
      \032         questions at all. Non-conflicting changes will be propagated;\n\
      \032         conflicts will be skipped.\n\
      \032  confirmbigdel \n\
      \032         !When this is set to true, Unison will request an extra\n\
      \032         confirmation if it appears that the entire replica has been\n\
      \032         deleted, before propagating the change. If the batch flag is\n\
      \032         also set, synchronization will be aborted. When the path\n\
      \032         preference is used, the same confirmation will be requested for\n\
      \032         top-level paths. (At the moment, this flag only affects the\n\
      \032         text user interface.) See also the mountpoint preference.\n\
      \032  confirmmerge \n\
      \032         Setting this preference causes both the text and graphical\n\
      \032         interfaces to ask the user if the results of a merge command\n\
      \032         may be commited to the replica or not. Since the merge command\n\
      \032         works on temporary files, the user can then cancel all the\n\
      \032         effects of applying the merge if it turns out that the result\n\
      \032         is not satisfactory. In batch-mode, this preference has no\n\
      \032         effect. Default is false.\n\
      \032  contactquietly \n\
      \032         If this flag is set, Unison will skip displaying the\n\
      \032         `Contacting server' message (which some users find annoying)\n\
      \032         during startup.\n\
      \032  copyprog xxx\n\
      \032         A string giving the name of an external program that can be\n\
      \032         used to copy large files efficiently (plus command-line\n\
      \032         switches telling it to copy files in-place). The default\n\
      \032         setting invokes rsync with appropriate options--most users\n\
      \032         should not need to change it.\n\
      \032  copyprogrest xxx\n\
      \032         A variant of copyprog that names an external program that\n\
      \032         should be used to continue the transfer of a large file that\n\
      \032         has already been partially transferred. Typically, copyprogrest\n\
      \032         will just be copyprog with one extra option (e.g., -partial,\n\
      \032         for rsync). The default setting invokes rsync with appropriate\n\
      \032         options--most users should not need to change it.\n\
      \032  copyquoterem xxx\n\
      \032         When set to true, this flag causes Unison to add an extra layer\n\
      \032         of quotes to the remote path passed to the external copy\n\
      \032         program. This is needed by rsync, for example, which internally\n\
      \032         uses an ssh connection requiring an extra level of quoting for\n\
      \032         paths containing spaces. When this flag is set to default,\n\
      \032         extra quotes are added if the value of copyprog contains the\n\
      \032         string rsync.\n\
      \032  copythreshold n\n\
      \032         A number indicating above what filesize (in kilobytes) Unison\n\
      \032         should use the external copying utility specified by copyprog.\n\
      \032         Specifying 0 will cause all copies to use the external program;\n\
      \032         a negative number will prevent any files from using it. The\n\
      \032         default is -1. See the section \"Making Unison Faster on Large\n\
      \032         Files\" for more information.\n\
      \032  debug xxx\n\
      \032         This preference is used to make Unison print various sorts of\n\
      \032         information about what it is doing internally on the standard\n\
      \032         error stream. It can be used many times, each time with the\n\
      \032         name of a module for which debugging information should be\n\
      \032         printed. Possible arguments for debug can be found by looking\n\
      \032         for calls to Util.debug in the sources (using, e.g., grep).\n\
      \032         Setting -debug all causes information from all modules to be\n\
      \032         printed (this mode of usage is the first one to try, if you are\n\
      \032         trying to understand something that Unison seems to be doing\n\
      \032         wrong); -debug verbose turns on some additional debugging\n\
      \032         output from some modules (e.g., it will show exactly what bytes\n\
      \032         are being sent across the network).\n\
      \032  diff xxx\n\
      \032         This preference can be used to control the name and\n\
      \032         command-line arguments of the system utility used to generate\n\
      \032         displays of file differences. The default is `diff -u CURRENT2\n\
      \032         CURRENT1'. If the value of this preference contains the\n\
      \032         substrings CURRENT1 and CURRENT2, these will be replaced by the\n\
      \032         names of the files to be diffed. If not, the two filenames will\n\
      \032         be appended to the command. In both cases, the filenames are\n\
      \032         suitably quoted.\n\
      \032  doc xxx\n\
      \032         The command-line argument -doc secname causes unison to display\n\
      \032         section secname of the manual on the standard output and then\n\
      \032         exit. Use -doc all to display the whole manual, which includes\n\
      \032         exactly the same information as the printed and HTML manuals,\n\
      \032         modulo formatting. Use -doc topics to obtain a list of the\n\
      \032         names of the various sections that can be printed.\n\
      \032  dontchmod \n\
      \032         By default, Unison uses the 'chmod' system call to set the\n\
      \032         permission bits of files after it has copied them. But in some\n\
      \032         circumstances (and under some operating systems), the chmod\n\
      \032         call always fails. Setting this preference completely prevents\n\
      \032         Unison from ever calling chmod.\n\
      \032  dumbtty \n\
      \032         When set to true, this flag makes the text mode user interface\n\
      \032         avoid trying to change any of the terminal settings. (Normally,\n\
      \032         Unison puts the terminal in `raw mode', so that it can do\n\
      \032         things like overwriting the current line.) This is useful, for\n\
      \032         example, when Unison runs in a shell inside of Emacs.\n\
      \032         When dumbtty is set, commands to the user interface need to be\n\
      \032         followed by a carriage return before Unison will execute them.\n\
      \032         (When it is off, Unison recognizes keystrokes as soon as they\n\
      \032         are typed.)\n\
      \032         This preference has no effect on the graphical user interface.\n\
      \032  dumparchives \n\
      \032         When this preference is set, Unison will create a file\n\
      \032         unison.dump on each host, containing a text summary of the\n\
      \032         archive, immediately after loading it.\n\
      \032  fastcheck xxx\n\
      \032         When this preference is set to true, Unison will use the\n\
      \032         modification time and length of a file as a `pseudo inode\n\
      \032         number' when scanning replicas for updates, instead of reading\n\
      \032         the full contents of every file. Under Windows, this may cause\n\
      \032         Unison to miss propagating an update if the modification time\n\
      \032         and length of the file are both unchanged by the update.\n\
      \032         However, Unison will never overwrite such an update with a\n\
      \032         change from the other replica, since it always does a safe\n\
      \032         check for updates just before propagating a change. Thus, it is\n\
      \032         reasonable to use this switch under Windows most of the time\n\
      \032         and occasionally run Unison once with fastcheck set to false,\n\
      \032         if you are worried that Unison may have overlooked an update.\n\
      \032         The default value of the preference is auto, which causes\n\
      \032         Unison to use fast checking on Unix replicas (where it is safe)\n\
      \032         and slow checking on Windows replicas. For backward\n\
      \032         compatibility, yes, no, and default can be used in place of\n\
      \032         true, false, and auto. See the section \"Fast Checking\" for more\n\
      \032         information.\n\
      \032  follow xxx\n\
      \032         Including the preference -follow pathspec causes Unison to\n\
      \032         treat symbolic links matching pathspec as `invisible' and\n\
      \032         behave as if the object pointed to by the link had appeared\n\
      \032         literally at this position in the replica. See the section\n\
      \032         \"Symbolic Links\" for more details. The syntax of pathspec> is\n\
      \032         described in the section \"Path Specification\" .\n\
      \032  force xxx\n\
      \032         Including the preference -force root causes Unison to resolve\n\
      \032         all differences (even non-conflicting changes) in favor of\n\
      \032         root. This effectively changes Unison from a synchronizer into\n\
      \032         a mirroring utility.\n\
      \032         You can also specify -force newer (or -force older) to force\n\
      \032         Unison to choose the file with the later (earlier) modtime. In\n\
      \032         this case, the -times preference must also be enabled.\n\
      \032         This preference is overridden by the forcepartial preference.\n\
      \032         This preference should be used only if you are sure you know\n\
      \032         what you are doing!\n\
      \032  forcepartial xxx\n\
      \032         Including the preference forcepartial PATHSPEC -> root causes\n\
      \032         Unison to resolve all differences (even non-conflicting\n\
      \032         changes) in favor of root for the files in PATHSPEC (see the\n\
      \032         section \"Path Specification\" for more information). This\n\
      \032         effectively changes Unison from a synchronizer into a mirroring\n\
      \032         utility.\n\
      \032         You can also specify forcepartial PATHSPEC -> newer (or\n\
      \032         forcepartial PATHSPEC older) to force Unison to choose the file\n\
      \032         with the later (earlier) modtime. In this case, the -times\n\
      \032         preference must also be enabled.\n\
      \032         This preference should be used only if you are sure you know\n\
      \032         what you are doing!\n\
      \032  group \n\
      \032         When this flag is set to true, the group attributes of the\n\
      \032         files are synchronized. Whether the group names or the group\n\
      \032         identifiers are synchronizeddepends on the preference numerids.\n\
      \032  height n\n\
      \032         Used to set the height (in lines) of the main window in the\n\
      \032         graphical user interface.\n\
      \032  ignore xxx\n\
      \032         Including the preference -ignore pathspec causes Unison to\n\
      \032         completely ignore paths that match pathspec (as well as their\n\
      \032         children). This is useful for avoiding synchronizing temporary\n\
      \032         files, object files, etc. The syntax of pathspec is described\n\
      \032         in the section \"Path Specification\" , and further details on\n\
      \032         ignoring paths is found in the section \"Ignoring Paths\" .\n\
      \032  ignorecase xxx\n\
      \032         When set to true, this flag causes Unison to treat filenames as\n\
      \032         case insensitive--i.e., files in the two replicas whose names\n\
      \032         differ in (upper- and lower-case) `spelling' are treated as the\n\
      \032         same file. When the flag is set to false, Unison will treat all\n\
      \032         filenames as case sensitive. Ordinarily, when the flag is set\n\
      \032         to default, filenames are automatically taken to be\n\
      \032         case-insensitive if either host is running Windows or OSX. In\n\
      \032         rare circumstances it is useful to set the flag manually (e.g.\n\
      \032         when running Unison on a Unix system with a FAT [Windows]\n\
      \032         volume mounted).\n\
      \032  ignorelocks \n\
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
      \032  ignorenot xxx\n\
      \032         This preference overrides the preference ignore. It gives a\n\
      \032         list of patterns (in the same format as ignore) for paths that\n\
      \032         should definitely not be ignored, whether or not they happen to\n\
      \032         match one of the ignore patterns.\n\
      \032         Note that the semantics of ignore and ignorenot is a little\n\
      \032         counter-intuitive. When detecting updates, Unison examines\n\
      \032         paths in depth-first order, starting from the roots of the\n\
      \032         replicas and working downwards. Before examining each path, it\n\
      \032         checks whether it matches ignore and does not match ignorenot;\n\
      \032         in this case it skips this path and all its descendants. This\n\
      \032         means that, if some parent of a given path matches an ignore\n\
      \032         pattern, then it will be skipped even if the path itself\n\
      \032         matches an ignorenot pattern. In particular, putting ignore =\n\
      \032         Path * in your profile and then using t ignorenot to select\n\
      \032         particular paths to be synchronized will not work. Instead, you\n\
      \032         should use the path preference to choose particular paths to\n\
      \032         synchronize.\n\
      \032  immutable xxx\n\
      \032         This preference specifies paths for directories whose immediate\n\
      \032         children are all immutable files -- i.e., once a file has been\n\
      \032         created, its contents never changes. When scanning for updates,\n\
      \032         Unison does not check whether these files have been modified;\n\
      \032         this can speed update detection significantly (in particular,\n\
      \032         for mail directories).\n\
      \032  immutablenot xxx\n\
      \032         This preference overrides immutable.\n\
      \032  key xxx\n\
      \032         Used in a profile to define a numeric key (0-9) that can be\n\
      \032         used in the graphical user interface to switch immediately to\n\
      \032         this profile.\n\
      \032  killserver \n\
      \032         When set to true, this flag causes Unison to kill the remote\n\
      \032         server process when the synchronization is finished. This\n\
      \032         behavior is the default for ssh connections, so this preference\n\
      \032         is not normally needed when running over ssh; it is provided so\n\
      \032         that socket-mode servers can be killed off after a single run\n\
      \032         of Unison, rather than waiting to accept future connections.\n\
      \032         (Some users prefer to start a remote socket server for each run\n\
      \032         of Unison, rather than leaving one running all the time.)\n\
      \032  label xxx\n\
      \032         Used in a profile to provide a descriptive string documenting\n\
      \032         its settings. (This is useful for users that switch between\n\
      \032         several profiles, especially using the `fast switch' feature of\n\
      \032         the graphical user interface.)\n\
      \032  log \n\
      \032         When this flag is set, Unison will log all changes to the\n\
      \032         filesystems on a file.\n\
      \032  logfile xxx\n\
      \032         By default, logging messages will be appended to the file\n\
      \032         unison.log in your HOME directory. Set this preference if you\n\
      \032         prefer another file.\n\
      \032  maxbackups n\n\
      \032         This preference specifies the number of backup versions that\n\
      \032         will be kept by unison, for each path that matches the\n\
      \032         predicate backup. The default is 2.\n\
      \032  maxthreads n\n\
      \032         This preference controls how much concurrency is allowed during\n\
      \032         the transport phase. Normally, it should be set reasonably high\n\
      \032         (default is 20) to maximize performance, but when Unison is\n\
      \032         used over a low-bandwidth link it may be helpful to set it\n\
      \032         lower (e.g. to 1) so that Unison doesn't soak up all the\n\
      \032         available bandwidth.\n\
      \032  merge xxx\n\
      \032         This preference can be used to run a merge program which will\n\
      \032         create a new version for each of the files and the backup, with\n\
      \032         the last backup and the both replicas. Setting the merge\n\
      \032         preference for a path will also cause this path to be backed\n\
      \032         up, just like t backup. The syntax of pathspec>cmd is described\n\
      \032         in the section \"Path Specification\" , and further details on\n\
      \032         Merging functions are present in the section \"Merging files\" .\n\
      \032  mountpoint xxx\n\
      \032         Including the preference -mountpoint PATH causes Unison to\n\
      \032         double-check, at the end of update detection, that PATH exists\n\
      \032         and abort if it does not. This is useful when Unison is used to\n\
      \032         synchronize removable media. This preference can be given more\n\
      \032         than once. See the section \"Mount Points\" .\n\
      \032  numericids \n\
      \032         When this flag is set to true, groups and users are\n\
      \032         synchronized numerically, rather than by name.\n\
      \032         The special uid 0 and the special group 0 are never mapped via\n\
      \032         user/group names even if this preference is not set.\n\
      \032  owner \n\
      \032         When this flag is set to true, the owner attributes of the\n\
      \032         files are synchronized. Whether the owner names or the owner\n\
      \032         identifiers are synchronizeddepends on the preference\n\
      \032         extttnumerids.\n\
      \032  path xxx\n\
      \032         When no path preference is given, Unison will simply\n\
      \032         synchronize the two entire replicas, beginning from the given\n\
      \032         pair of roots. If one or more path preferences are given, then\n\
      \032         Unison will synchronize only these paths and their children.\n\
      \032         (This is useful for doing a fast sync of just one directory,\n\
      \032         for example.) Note that path preferences are intepreted\n\
      \032         literally--they are not regular expressions.\n\
      \032  perms n\n\
      \032         The integer value of this preference is a mask indicating which\n\
      \032         permission bits should be synchronized. It is set by default to\n\
      \032         0o1777: all bits but the set-uid and set-gid bits are\n\
      \032         synchronised (synchronizing theses latter bits can be a\n\
      \032         security hazard). If you want to synchronize all bits, you can\n\
      \032         set the value of this preference to -1.\n\
      \032  prefer xxx\n\
      \032         Including the preference -prefer root causes Unison always to\n\
      \032         resolve conflicts in favor of root, rather than asking for\n\
      \032         guidance from the user. (The syntax of root is the same as for\n\
      \032         the root preference, plus the special values newer and older.)\n\
      \032         This preference is overridden by the preferpartial preference.\n\
      \032         This preference should be used only if you are sure you know\n\
      \032         what you are doing!\n\
      \032  preferpartial xxx\n\
      \032         Including the preference preferpartial PATHSPEC -> root causes\n\
      \032         Unison always to resolve conflicts in favor of root, rather\n\
      \032         than asking for guidance from the user, for the files in\n\
      \032         PATHSPEC (see the section \"Path Specification\" for more\n\
      \032         information). (The syntax of root is the same as for the root\n\
      \032         preference, plus the special values newer and older.)\n\
      \032         This preference should be used only if you are sure you know\n\
      \032         what you are doing!\n\
      \032  pretendwin \n\
      \032         When set to true, this preference makes Unison use\n\
      \032         Windows-style fast update detection (using file creation times\n\
      \032         as \"pseudo-inode-numbers\"), even when running on a Unix system.\n\
      \032         This switch should be used with care, as it is less safe than\n\
      \032         the standard update detection method, but it can be useful for\n\
      \032         synchronizing VFAT filesystems (which do not support inode\n\
      \032         numbers) mounted on Unix systems. The fastcheck option should\n\
      \032         also be set to true.\n\
      \032  repeat xxx\n\
      \032         Setting this preference causes the text-mode interface to\n\
      \032         synchronize repeatedly, rather than doing it just once and\n\
      \032         stopping. If the argument is a number, Unison will pause for\n\
      \032         that many seconds before beginning again.\n\
      \032  retry n\n\
      \032         Setting this preference causes the text-mode interface to try\n\
      \032         again to synchronize updated paths where synchronization fails.\n\
      \032         Each such path will be tried N times.\n\
      \032  root xxx\n\
      \032         Each use of this preference names the root of one of the\n\
      \032         replicas for Unison to synchronize. Exactly two roots are\n\
      \032         needed, so normal modes of usage are either to give two values\n\
      \032         for root in the profile, or to give no values in the profile\n\
      \032         and provide two on the command line. Details of the syntax of\n\
      \032         roots can be found in the section \"Roots\" .\n\
      \032         The two roots can be given in either order; Unison will sort\n\
      \032         them into a canonical order before doing anything else. It also\n\
      \032         tries to `canonize' the machine names and paths that appear in\n\
      \032         the roots, so that, if Unison is invoked later with a slightly\n\
      \032         different name for the same root, it will be able to locate the\n\
      \032         correct archives.\n\
      \032  rootalias xxx\n\
      \032         When calculating the name of the archive files for a given pair\n\
      \032         of roots, Unison replaces any roots matching the left-hand side\n\
      \032         of any rootalias rule by the corresponding right-hand side.\n\
      \032  rshargs xxx\n\
      \032         The string value of this preference will be passed as\n\
      \032         additional arguments (besides the host name and the name of the\n\
      \032         Unison executable on the remote system) to the rsh command used\n\
      \032         to invoke the remote server.\n\
      \032  rshcmd xxx\n\
      \032         This preference can be used to explicitly set the name of the\n\
      \032         rsh executable (e.g., giving a full path name), if necessary.\n\
      \032  rsrc xxx\n\
      \032         When set to true, this flag causes Unison to synchronize\n\
      \032         resource forks and HFS meta-data. On filesystems that do not\n\
      \032         natively support resource forks, this data is stored in\n\
      \032         Carbon-compatible ._ AppleDouble files. When the flag is set to\n\
      \032         false, Unison will not synchronize these data. Ordinarily, the\n\
      \032         flag is set to default, and these data are automatically\n\
      \032         synchronized if either host is running OSX. In rare\n\
      \032         circumstances it is useful to set the flag manually.\n\
      \032  rsync \n\
      \032         Unison uses the 'rsync algorithm' for 'diffs-only' transfer of\n\
      \032         updates to large files. Setting this flag to false makes Unison\n\
      \032         use whole-file transfers instead. Under normal circumstances,\n\
      \032         there is no reason to do this, but if you are having trouble\n\
      \032         with repeated 'rsync failure' errors, setting it to false\n\
      \032         should permit you to synchronize the offending files.\n\
      \032  selftest \n\
      \032         Run internal tests and exit. This option is mostly for\n\
      \032         developers and must be used carefully: in particular, it will\n\
      \032         delete the contents of both roots, so that it can install its\n\
      \032         own files for testing. This flag only makes sense on the\n\
      \032         command line. When it is provided, no preference file is read:\n\
      \032         all preferences must be specified on thecommand line. Also,\n\
      \032         since the self-test procedure involves overwriting the roots\n\
      \032         and backup directory, the names of the roots and of the\n\
      \032         backupdir preference must include the string \"test\" or else the\n\
      \032         tests will be aborted. (If these are not given on the command\n\
      \032         line, dummy subdirectories in the current directory will be\n\
      \032         created automatically.)\n\
      \032  servercmd xxx\n\
      \032         This preference can be used to explicitly set the name of the\n\
      \032         Unison executable on the remote server (e.g., giving a full\n\
      \032         path name), if necessary.\n\
      \032  showarchive \n\
      \032         When this preference is set, Unison will print out the 'true\n\
      \032         names'of the roots, in the same form as is expected by the\n\
      \032         rootaliaspreference.\n\
      \032  silent \n\
      \032         When this preference is set to true, the textual user interface\n\
      \032         will print nothing at all, except in the case of errors.\n\
      \032         Setting silent to true automatically sets the batch preference\n\
      \032         to true.\n\
      \032  sortbysize \n\
      \032         When this flag is set, the user interface will list changed\n\
      \032         files by size (smallest first) rather than by name. This is\n\
      \032         useful, for example, for synchronizing over slow links, since\n\
      \032         it puts very large files at the end of the list where they will\n\
      \032         not prevent smaller files from being transferred quickly.\n\
      \032         This preference (as well as the other sorting flags, but not\n\
      \032         the sorting preferences that require patterns as arguments) can\n\
      \032         be set interactively and temporarily using the 'Sort' menu in\n\
      \032         the graphical user interface.\n\
      \032  sortfirst xxx\n\
      \032         Each argument to sortfirst is a pattern pathspec, which\n\
      \032         describes a set of paths. Files matching any of these patterns\n\
      \032         will be listed first in the user interface. The syntax of\n\
      \032         pathspec is described in the section \"Path Specification\" .\n\
      \032  sortlast xxx\n\
      \032         Similar to sortfirst, except that files matching one of these\n\
      \032         patterns will be listed at the very end.\n\
      \032  sortnewfirst \n\
      \032         When this flag is set, the user interface will list newly\n\
      \032         created files before all others. This is useful, for example,\n\
      \032         for checking that newly created files are not `junk', i.e.,\n\
      \032         ones that should be ignored or deleted rather than\n\
      \032         synchronized.\n\
      \032  sshargs xxx\n\
      \032         The string value of this preference will be passed as\n\
      \032         additional arguments (besides the host name and the name of the\n\
      \032         Unison executable on the remote system) to the ssh command used\n\
      \032         to invoke the remote server.\n\
      \032  sshcmd xxx\n\
      \032         This preference can be used to explicitly set the name of the\n\
      \032         ssh executable (e.g., giving a full path name), if necessary.\n\
      \032  sshversion xxx\n\
      \032         This preference can be used to control which version of ssh\n\
      \032         should be used to connect to the server. Legal values are 1 and\n\
      \032         2, which will cause unison to try to use ssh1 orssh2 instead of\n\
      \032         just ssh to invoke ssh. The default value is empty, which will\n\
      \032         make unison use whatever version of ssh is installed as the\n\
      \032         default `ssh' command.\n\
      \032  terse \n\
      \032         When this preference is set to true, the user interface will\n\
      \032         not print status messages.\n\
      \032  testserver \n\
      \032         Setting this flag on the command line causes Unison to attempt\n\
      \032         to connect to the remote server and, if successful, print a\n\
      \032         message and immediately exit. Useful for debugging installation\n\
      \032         problems. Should not be set in preference files.\n\
      \032  times \n\
      \032         When this flag is set to true, file modification times (but not\n\
      \032         directory modtimes) are propagated.\n\
      \032  ui xxx\n\
      \032         This preference selects either the graphical or the textual\n\
      \032         user interface. Legal values are graphic or text.\n\
      \032         Because this option is processed specially during Unison's\n\
      \032         start-up sequence, it can only be used on the command line. In\n\
      \032         preference files it has no effect.\n\
      \032         If the Unison executable was compiled with only a textual\n\
      \032         interface, this option has no effect. (The pre-compiled\n\
      \032         binaries are all compiled with both interfaces available.)\n\
      \032  version \n\
      \032         Print the current version number and exit. (This option only\n\
      \032         makes sense on the command line.)\n\
      \032  xferbycopying \n\
      \032         When this preference is set, Unison will try to avoid\n\
      \032         transferring file contents across the network by recognizing\n\
      \032         when a file with the required contents already exists in the\n\
      \032         target replica. This usually allows file moves to be propagated\n\
      \032         very quickly. The default value istrue.\n\
      \n\
      Profiles\n\
      \n\
      \032  A profile is a text file that specifies permanent settings for roots,\n\
      \032  paths, ignore patterns, and other preferences, so that they do not\n\
      \032  need to be typed at the command line every time Unison is run.\n\
      \032  Profiles should reside in the .unison directory on the client machine.\n\
      \032  If Unison is started with just one argument name on the command line,\n\
      \032  it looks for a profile called name.prf in the .unison directory. If it\n\
      \032  is started with no arguments, it scans the .unison directory for files\n\
      \032  whose names end in .prf and offers a menu (provided that the Unison\n\
      \032  executable is compiled with the graphical user interface). If a file\n\
      \032  named default.prf is found, its settings will be offered as the\n\
      \032  default choices.\n\
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
      \032  cause the file name (or name.prf, if name does not exist in the\n\
      \032  .unison directory) to be read at the point, and included as if its\n\
      \032  contents, instead of the include line, was part of the profile.\n\
      \032  Include lines allows settings common to several profiles to be stored\n\
      \032  in one place.\n\
      \n\
      \032  A profile may include a preference `label = desc' to provide a\n\
      \032  description of the options selected in this profile. The string desc\n\
      \032  is listed along with the profile name in the profile selection dialog,\n\
      \032  and displayed in the top-right corner of the main Unison window in the\n\
      \032  graphical user interface.\n\
      \n\
      \032  The graphical user-interface also supports one-key shortcuts for\n\
      \032  commonly used profiles. If a profile contains a preference of the form\n\
      \032  `key = n', where n is a single digit, then pressing this digit key\n\
      \032  will cause Unison to immediately switch to this profile and begin\n\
      \032  synchronization again from scratch. In this case, all actions that\n\
      \032  have been selected for a set of changes currently being displayed will\n\
      \032  be discarded.\n\
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
      \032  accomplished by splitting up the profile into several parts -- a\n\
      \032  common part containing most of the preference settings, plus one\n\
      \032  \"top-level\" file for each set of paths that need to be synchronized.\n\
      \032  (The include mechanism can also be used to allow the same set of\n\
      \032  preference settings to be used with different roots.)\n\
      \n\
      \032  The collection of profiles implementing this scheme might look as\n\
      \032  follows. The file default.prf is empty except for an include\n\
      \032  directive:\n\
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
      \032   # file 'common' rathen than in the top-level preference file\n\
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
      \032  when we invoke Unison with the default profile (e.g., by typing\n\
      \032  'unison default' or just 'unison' on the command line), the whole\n\
      \032  replicas will be synchronized. (If we never want to synchronize the\n\
      \032  whole replicas, then default.prf would instead include settings for\n\
      \032  all the paths that are usually synchronized.)\n\
      \n\
      \032  To synchronize just part of the replicas, Unison is invoked with an\n\
      \032  alternate preference file--e.g., doing 'unison workingset', where the\n\
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
      \032  When Unison overwrites a file or directory by propagating a new\n\
      \032  version from the other replica, it can keep the old version around as\n\
      \032  a backup. There are several preferences that control precisely where\n\
      \032  these backups are stored and how they are named.\n\
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
      \032  backupnot preference can be used to give a few exceptions: it\n\
      \032  specifies which files and directories should not be backed up, even if\n\
      \032  they match the backup pathspec.\n\
      \n\
      \032  It is important to note that the pathspec is matched against the path\n\
      \032  that is being updated by Unison, not its descendants. For example, if\n\
      \032  you set backup = Name *.txt and then delete a whole directory named\n\
      \032  foo containing some text files, these files will not be backed up\n\
      \032  because Unison will just check that foo does not match *.txt.\n\
      \032  Similarly, if the directory itself happened to be called foo.txt, then\n\
      \032  the whole directory and all the files in it will be backed up,\n\
      \032  regardless of their names.\n\
      \n\
      \032  Backup files can be stored either centrally or locally. This behavior\n\
      \032  is controlled by the preference backuplocation, whose value must be\n\
      \032  either central or local. (The default is central.)\n\
      \n\
      \032  When backups are stored locally, they are kept in the same directory\n\
      \032  as the original.\n\
      \n\
      \032  When backups are stored centrally, the directory used to hold them is\n\
      \032  controlled by the preference backupdir and the environment variable\n\
      \032  UNISONBACKUPDIR. (The environment variable is checked first.) If\n\
      \032  neither of these are set, then the directory .unison/backup in the\n\
      \032  user's home directory is used.\n\
      \n\
      \032  The preference maxbackups controls how many previous versions of each\n\
      \032  file are kept (including the current version).\n\
      \n\
      \032  By default, backup files are named .bak.VERSION.FILENAME, where\n\
      \032  FILENAME is the original filename and VERSION is the backup number (1\n\
      \032  for the most recent, 2 for the next most recent, etc.). This can be\n\
      \032  changed by setting the preferences backupprefix and/or backupsuffix.\n\
      \032  If desired, backupprefix may include a directory prefix; this can be\n\
      \032  used with backuplocation = local to put all backup files for each\n\
      \032  directory into a single subdirectory. For example, setting\n\
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
      \032  The merge preference may be given once or several times in a\n\
      \032  preference file (it can also be given on the command line, of course,\n\
      \032  but this tends to be awkward because of the spaces and special\n\
      \032  characters involved). Each instance of the preference looks like this:\n\
      \032   merge = <PATHSPEC> -> <MERGECMD>\n\
      \n\
      \032  The <PATHSPEC> here has exactly the same format as for the ignore\n\
      \032  preference (see the section \"Path specification\" ). For example, using\n\
      \032  \"Name *.txt\" as the <PATHSPEC> tells Unison that this command should\n\
      \032  be used whenever a file with extension .txt needs to be merged.\n\
      \n\
      \032  Many external merging programs require as inputs not just the two\n\
      \032  files that need to be merged, but also a file containing the last\n\
      \032  synchronized version. You can ask Unison to keep a copy of the last\n\
      \032  synchronized version for some files using the backupcurrent\n\
      \032  preference. This preference is used in exactly the same way as backup\n\
      \032  and its meaning is similar, except that it causes backups to be kept\n\
      \032  of the current contents of each file after it has been synchronized by\n\
      \032  Unison, rather than the previous contents that Unison overwrote. These\n\
      \032  backups are kept on both replicas in the same place as ordinary backup\n\
      \032  files--i.e. according to the backuplocation and backupdir preferences.\n\
      \032  They are named like the original files if backupslocation is set to\n\
      \032  'central' and otherwise, Unison uses the backupprefix and backupsuffix\n\
      \032  preferences and assumes a version number 000 for these backups.\n\
      \n\
      \032  The <MERGECMD> part of the preference specifies what external command\n\
      \032  should be invoked to merge files at paths matching the <PATHSPEC>.\n\
      \032  Within this string, several special substrings are recognized; these\n\
      \032  will be substituted with appropriate values before invoking a\n\
      \032  sub-shell to execute the command.\n\
      \032    * CURRENT1 is replaced by the name of (a temporary copy of) the\n\
      \032      local variant of the file.\n\
      \032    * CURRENT2 is replaced by the name of a temporary file, into which\n\
      \032      the contents of the remote variant of the file have been\n\
      \032      transferred by Unison prior to performing the merge.\n\
      \032    * CURRENTARCH is replaced by the name of the backed up copy of the\n\
      \032      original version of the file (i.e., the file saved by Unison if\n\
      \032      the current filename matches the path specifications for the\n\
      \032      backupcurrent preference, as explained above), if one exists. If\n\
      \032      no archive exists and CURRENTARCH appears in the merge command,\n\
      \032      then an error is signalled.\n\
      \032    * CURRENTARCHOPT is replaced by the name of the backed up copy of\n\
      \032      the original version of the file (i.e., its state at the end of\n\
      \032      the last successful run of Unison), if one exists, or the empty\n\
      \032      string if no archive exists.\n\
      \032    * NEW is replaced by the name of a temporary file that Unison\n\
      \032      expects to be written by the merge program when it finishes,\n\
      \032      giving the desired new contents of the file.\n\
      \032    * PATH is replaced by the path (relative to the roots of the\n\
      \032      replicas) of the file being merged.\n\
      \032    * NEW1 and NEW2 are replaced by the names of temporary files that\n\
      \032      Unison expects to be written by the merge program when it is only\n\
      \032      able to partially merge the originals; in this case, NEW1 will be\n\
      \032      written back to the local replica and NEW2 to the remote replica;\n\
      \032      NEWARCH, if present, will be used as the \"last common state\" of\n\
      \032      the replicas. (These three options are provided for later\n\
      \032      compatibility with the Harmony data synchronizer.)\n\
      \n\
      \032  To accomodate the wide variety of programs that users might want to\n\
      \032  use for merging, Unison checks for several possible situations when\n\
      \032  the merge program exits:\n\
      \032    * If the merge program exits with a non-zero status, then merge is\n\
      \032      considered to have failed and the replicas are not changed.\n\
      \032    * If the file NEW has been created, it is written back to both\n\
      \032      replicas (and stored in the backup directory). Similarly, if just\n\
      \032      the file NEW1 has been created, it is written back to both\n\
      \032      replicas.\n\
      \032    * If neither NEW nor NEW1 have been created, then Unison examines\n\
      \032      the temporary files CURRENT1 and CURRENT2 that were given as\n\
      \032      inputs to the merge program. If either has been changed (or both\n\
      \032      have been changed in identical ways), then its new contents are\n\
      \032      written back to both replicas. If either CURRENT1 or CURRENT2 has\n\
      \032      been deleted, then the contents of the other are written back to\n\
      \032      both replicas.\n\
      \032    * If the files NEW1, NEW2, and NEWARCH have all been created, they\n\
      \032      are written back to the local replica, remote replica, and backup\n\
      \032      directory, respectively. If the files NEW1, NEW2 have been\n\
      \032      created, but NEWARCH has not, then these files are written back to\n\
      \032      the local replica and remote replica, respectively. Also, if NEW1\n\
      \032      and NEW2 have identical contents, then the same contents are\n\
      \032      stored as a backup (if the backupcurrent preference is set for\n\
      \032      this path) to reflect the fact that the path is currently in sync.\n\
      \032    * If NEW1 and NEW2 (resp. CURRENT1 and CURRENT2) are created (resp.\n\
      \032      overwritten) with different contents but the merge command did not\n\
      \032      fail (i.e., it exited with status code 0), then we copy NEW1\n\
      \032      (resp. CURRENT1) to the other replica and to the archive.\n\
      \032      This behavior is a design choice made to handle the case where a\n\
      \032      merge command only synchronizes some specific contents between two\n\
      \032      files, skipping some irrelevant information (order between\n\
      \032      entries, for instance). We assume that, if the merge command exits\n\
      \032      normally, then the two resulting files are \"as good as equal.\"\n\
      \032      (The reason we copy one on top of the other is to avoid Unison\n\
      \032      detecting that the files are unequal the next time it is run and\n\
      \032      trying again to merge them when, in fact, the merge program has\n\
      \032      already made them as similar as it is able to.)\n\
      \n\
      \032  If the confirmmerge preference is set and Unison is not run in batch\n\
      \032  mode, then Unison will always ask for confirmation before actually\n\
      \032  committing the results of the merge to the replicas.\n\
      \n\
      \032  A large number of external merging programs are available. For\n\
      \032  example, on Unix systems setting the merge preference to\n\
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
      \032   merge = Name *.txt -> opendiff CURRENT1 CURRENT2 -ancestor CURRENTARCH -mer\n\
      ge NEW\n\
      \n\
      \032  Here is a slightly more involved hack. The opendiff program can\n\
      \032  operate either with or without an archive file. A merge command of\n\
      \032  this form\n\
      \032   merge = Name *.txt ->\n\
      \032             if [ CURRENTARCHOPTx = x ];\n\
      \032             then opendiff CURRENT1 CURRENT2 -merge NEW;\n\
      \032             else opendiff CURRENT1 CURRENT2 -ancestor CURRENTARCHOPT -merge N\n\
      EW;\n\
      \032             fi\n\
      \n\
      \032  (still all on one line in the preference file!) will test whether an\n\
      \032  archive file exists and use the appropriate variant of the arguments\n\
      \032  to opendiff.\n\
      \n\
      \032  Ordinarily, external merge programs are only invoked when Unison is\n\
      \032  not running in batch mode. To specify an external merge program that\n\
      \032  should be used no matter the setting of the batch flag, use the\n\
      \032  mergebatch preference instead of merge.\n\
      \n\
      \032    Please post suggestions for other useful values of the merge\n\
      \032    preference to the unison-users mailing list--we'd like to give\n\
      \032    several examples here. \n\
      \n\
      The User Interface\n\
      \n\
      \032  Both the textual and the graphical user interfaces are intended to be\n\
      \032  mostly self-explanatory. Here are just a few tricks:\n\
      \032    * By default, when running on Unix the textual user interface will\n\
      \032      try to put the terminal into the \"raw mode\" so that it reads the\n\
      \032      input a character at a time rather than a line at a time. (This\n\
      \032      means you can type just the single keystroke \">\" to tell Unison to\n\
      \032      propagate a file from left to right, rather than \"> Enter.\")\n\
      \032      There are some situations, though, where this will not work -- for\n\
      \032      example, when Unison is running in a shell window inside Emacs.\n\
      \032      Setting the dumbtty preference will force Unison to leave the\n\
      \032      terminal alone and process input a line at a time.\n\
      \n\
      Exit code\n\
      \n\
      \032  When running in the textual mode, Unison returns an exit status, which\n\
      \032  describes whether, and at which level, the synchronization was\n\
      \032  successful. The exit status could be useful when Unison is invoked\n\
      \032  from a script. Currently, there are four possible values for the exit\n\
      \032  status:\n\
      \032    * 0: successful synchronization; everything is up-to-date now.\n\
      \032    * 1: some files were skipped, but all file transfers were\n\
      \032      successful.\n\
      \032    * 2: non-fatal failures occurred during file transfer.\n\
      \032    * 3: a fatal error occurred, or the execution was interrupted.\n\
      \n\
      \032  The graphical interface does not return any useful information through\n\
      \032  the exit status.\n\
      \n\
      Path specification\n\
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
      \032      adds pattern to the list of patterns to be ignored.\n\
      \032    * Each pattern can have one of three forms. The most general form is\n\
      \032      a Posix extended regular expression introduced by the keyword\n\
      \032      Regex. (The collating sequences and character classes of full\n\
      \032      Posix regexps are not currently supported).\n\
      \032                Regex regexp\n\
      \032      For convenience, two other styles of pattern are also recognized:\n\
      \032                Name name\n\
      \032      matches any path in which the last component matches name, while\n\
      \032                Path path\n\
      \032      matches exactly the path path. The name and path arguments of the\n\
      \032      latter forms of patterns are not regular expressions. Instead,\n\
      \032      standard \"globbing\" conventions can be used in name and path:\n\
      \032         + a * matches any sequence of characters not including / (and\n\
      \032           not beginning with ., when used at the beginning of a name)\n\
      \032         + a ? matches any single character except / (and leading .)\n\
      \032         + [xyz] matches any character from the set {x, y, z }\n\
      \032         + {a,bb,ccc} matches any one of a, bb, or ccc.\n\
      \032    * The path separator in path patterns is always the forward-slash\n\
      \032      character \"/\" -- even when the client or server is running under\n\
      \032      Windows, where the normal separator character is a backslash. This\n\
      \032      makes it possible to use the same set of path patterns for both\n\
      \032      Unix and Windows file systems.\n\
      \n\
      \032  Some examples of path patterns appear in the section \"Ignoring Paths\"\n\
      \032  .\n\
      \n\
      Ignoring Paths\n\
      \n\
      \032  Most users of Unison will find that their replicas contain lots of\n\
      \032  files that they don't ever want to synchronize -- temporary files,\n\
      \032  very large files, old stuff, architecture-specific binaries, etc. They\n\
      \032  can instruct Unison to ignore these paths using patterns introduced in\n\
      \032  the section \"Path Patterns\" .\n\
      \n\
      \032  For example, the following pattern will make Unison ignore any path\n\
      \032  containing the name CVS or a name ending in .cmo:\n\
      \032            ignore = Name {CVS,*.cmo}\n\
      \n\
      \032  The next pattern makes Unison ignore the path a/b:\n\
      \032            ignore = Path a/b\n\
      \n\
      \032  Path patterns do not skip filesnames beginning with . (as Name\n\
      \032  patterns do). For example,\n\
      \032            ignore = Path */tmp\n\
      \n\
      \032  will include .foo/tmp in the set of ignore directories, as it is a\n\
      \032  path, not a name, that is ignored.\n\
      \n\
      \032  The following pattern makes Unison ignore any path beginning with a/b\n\
      \032  and ending with a name ending by .ml.\n\
      \032            ignore = Regex a/b/.*\\.ml\n\
      \n\
      \032  Note that regular expression patterns are \"anchored\": they must match\n\
      \032  the whole path, not just a substring of the path.\n\
      \n\
      \032  Here are a few extra points regarding the ignore preference.\n\
      \032    * If a directory is ignored, all its descendents will be too.\n\
      \032    * The user interface provides some convenient commands for adding\n\
      \032      new patterns to be ignored. To ignore a particular file, select it\n\
      \032      and press \"i\". To ignore all files with the same extension, select\n\
      \032      it and press \"E\" (with the shift key). To ignore all files with\n\
      \032      the same name, no matter what directory they appear in, select it\n\
      \032      and press \"N\". These new patterns become permanent: they are\n\
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
      \032           replicas--i.e., from the empty path. If the empty path\n\
      \032           matches an ignore pattern and does not match an ignorenot\n\
      \032           pattern, then the whole replica will be ignored. (For this\n\
      \032           reason, it is not a good idea to include Name * as an ignore\n\
      \032           pattern. If you want to ignore everything except a certain\n\
      \032           set of files, use Name ?*.)\n\
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
      \032  Ordinarily, Unison treats symbolic links in Unix replicas as \"opaque\":\n\
      \032  it considers the contents of the link to be just the string specifying\n\
      \032  where the link points, and it will propagate changes in this string to\n\
      \032  the other replica.\n\
      \n\
      \032  It is sometimes useful to treat a symbolic link \"transparently,\"\n\
      \032  acting as though whatever it points to were physically in the replica\n\
      \032  at the point where the symbolic link appears. To tell Unison to treat\n\
      \032  a link in this manner, add a line of the form\n\
      \032            follow = pathspec\n\
      \n\
      \032  to the profile, where pathspec is a path pattern as described in the\n\
      \032  section \"Path Patterns\" .\n\
      \n\
      \032  Windows file systems do not support symbolic links; Unison will refuse\n\
      \032  to propagate an opaque symbolic link from Unix to Windows and flag the\n\
      \032  path as erroneous. When a Unix replica is to be synchronized with a\n\
      \032  Windows system, all symbolic links should match either an ignore\n\
      \032  pattern or a follow pattern.\n\
      \n\
      Permissions\n\
      \n\
      \032  Synchronizing the permission bits of files is slightly tricky when two\n\
      \032  different filesytems are involved (e.g., when synchronizing a Windows\n\
      \032  client and a Unix server). In detail, here's how it works:\n\
      \032    * When the permission bits of an existing file or directory are\n\
      \032      changed, the values of those bits that make sense on both\n\
      \032      operating systems will be propagated to the other replica. The\n\
      \032      other bits will not be changed.\n\
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
      \032  system -- Windows won't allow two different files to have the \"same\"\n\
      \032  name. Unison detects this situation for you, and reports that it\n\
      \032  cannot synchronize the files.\n\
      \n\
      \032  You can deal with a case conflict in a couple of ways. If you need to\n\
      \032  have both files on the Windows system, your only choice is to rename\n\
      \032  one of the Unix files to avoid the case conflict, and re-synchronize.\n\
      \032  If you don't need the files on the Windows system, you can simply\n\
      \032  disregard Unison's warning message, and go ahead with the\n\
      \032  synchronization; Unison won't touch those files. If you don't want to\n\
      \032  see the warning on each synchronization, you can tell Unison to ignore\n\
      \032  the files (see the section \"Ignore\" ).\n\
      \n\
      \032  Illegal filenames. Unix allows some filenames that are illegal in\n\
      \032  Windows. For example, colons (`:') are not allowed in Windows\n\
      \032  filenames, but they are legal in Unix filenames. This means that a\n\
      \032  Unix file foo:bar can't be synchronized to a Windows system. As with\n\
      \032  case conflicts, Unison detects this situation for you, and you have\n\
      \032  the same options: you can either rename the Unix file and\n\
      \032  re-synchronize, or you can ignore it.\n\
      \n\
      Slow Links\n\
      \n\
      \032  Unison is built to run well even over relatively slow links such as\n\
      \032  modems and DSL connections.\n\
      \n\
      \032  Unison uses the \"rsync protocol\" designed by Andrew Tridgell and Paul\n\
      \032  Mackerras to greatly speed up transfers of large files in which only\n\
      \032  small changes have been made. More information about the rsync\n\
      \032  protocol can be found at the rsync web site\n\
      \032  (http://samba.anu.edu.au/rsync/).\n\
      \n\
      \032  If you are using Unison with ssh, you may get some speed improvement\n\
      \032  by enabling ssh's compression feature. Do this by adding the option\n\
      \032  \"-rshargs -C\" to the command line or \"rshargs = -C\" to your profile.\n\
      \n\
      Making Unison Faster on Large Files\n\
      \n\
      \032  Unison's built-in implementation of the rsync algorithm makes\n\
      \032  transferring updates to existing files pretty fast. However, for\n\
      \032  whole-file copies of newly created files, the built-in transfer method\n\
      \032  is not highly optimized. Also, if Unison is interrupted in the middle\n\
      \032  of transferring a large file, it will attempt to retransfer the whole\n\
      \032  thing on the next run.\n\
      \n\
      \032  These shortcomings can be addressed with a little extra work by\n\
      \032  telling Unison to use an external file copying utility for whole-file\n\
      \032  transfers. The recommended one is the standalone rsync tool, which is\n\
      \032  available by default on most Unix systems and can easily be installed\n\
      \032  on Windows systems using Cygwin.\n\
      \n\
      \032  If you have rsync installed on both hosts, you can make Unison use it\n\
      \032  simply by setting the copythreshold flag to something non-negative. If\n\
      \032  you set it to 0, Unison will use the external copy utility for all\n\
      \032  whole-file transfers. (This is probably slower than letting Unison\n\
      \032  copy small files by itself, but can be useful for testing.) If you set\n\
      \032  it to a larger value, Unison will use the external utility for all\n\
      \032  files larger than this size (which is given in kilobytes, so setting\n\
      \032  it to 1000 will cause the external tool to be used for all transfers\n\
      \032  larger than a megabyte).\n\
      \n\
      \032  If you want to use a different external copy utility, set both the\n\
      \032  copyprog and copyprogpartial preferences--the former is used for the\n\
      \032  first transfer of a file, while the latter is used when Unison sees a\n\
      \032  partially transferred temp file on the receiving host. Be careful\n\
      \032  here: Your external tool needs to be instructed to copy files in place\n\
      \032  (otherwise if the transfer is interrupted Unison will not notice that\n\
      \032  some of the data has already been transferred, the next time it\n\
      \032  tries). The default values are:\n\
      \032  copyprog      =   rsync --inplace --compress\n\
      \032  copyprogrest  =   rsync --partial --inplace --compress\n\
      \n\
      \032  You may also need to set the copyquoterem preference. When it is set\n\
      \032  to true, this causes Unison to add an extra layer of quotes to the\n\
      \032  remote path passed to the external copy program. This is is needed by\n\
      \032  rsync, for example, which internally uses an ssh connection, requiring\n\
      \032  an extra level of quoting for paths containing spaces. When this flag\n\
      \032  is set to default, extra quotes are added if the value of copyprog\n\
      \032  contains the string rsync. The default value is default, naturally.\n\
      \n\
      \032  If a directory transfer is interrupted, the next run of Unison will\n\
      \032  automatically skip any files that were completely transferred before\n\
      \032  the interruption. (This behavior is always on: it does not depend on\n\
      \032  the setting of the copythreshold preference.) Note, though, that the\n\
      \032  new directory will not appear in the destination filesystem until\n\
      \032  everything has been transferred--partially transferred directories are\n\
      \032  kept in a temporary location (with names like .unison.DIRNAME....)\n\
      \032  until the transfer is complete.\n\
      \n\
      Fast Update Detection\n\
      \n\
      \032  If your replicas are large and at least one of them is on a Windows\n\
      \032  system, you may find that Unison's default method for detecting\n\
      \032  changes (which involves scanning the full contents of every file on\n\
      \032  every sync--the only completely safe way to do it under Windows) is\n\
      \032  too slow. Unison provides a preference fastcheck that, when set to\n\
      \032  true, causes it to use file creation times as 'pseudo inode numbers'\n\
      \032  when scanning replicas for updates, instead of reading the full\n\
      \032  contents of every file.\n\
      \n\
      \032  When fastcheck is set to no, Unison will perform slow\n\
      \032  checking--re-scanning the contents of each file on each\n\
      \032  synchronization--on all replicas. When fastcheck is set to default\n\
      \032  (which, naturally, is the default), Unison will use fast checks on\n\
      \032  Unix replicas and slow checks on Windows replicas.\n\
      \n\
      \032  This strategy may cause Unison to miss propagating an update if the\n\
      \032  modification time and length of the file are both unchanged by the\n\
      \032  update. However, Unison will never overwrite such an update with a\n\
      \032  change from the other replica, since it always does a safe check for\n\
      \032  updates just before propagating a change. Thus, it is reasonable to\n\
      \032  use this switch most of the time and occasionally run Unison once with\n\
      \032  fastcheck set to no, if you are worried that Unison may have\n\
      \032  overlooked an update.\n\
      \n\
      \032  Fastcheck is (always) automatically disabled for files with extension\n\
      \032  .xls or .mpp, to prevent Unison from being confused by the habits of\n\
      \032  certain programs (Excel, in particular) of updating files without\n\
      \032  changing their modification times.\n\
      \n\
      Mount Points and Removable Media\n\
      \n\
      \032  Using Unison removable media such as USB drives can be dangerous\n\
      \032  unless you are careful. If you synchronize a directory that is stored\n\
      \032  on removable media when the media is not present, it will look to\n\
      \032  Unison as though the whole directory has been deleted, and it will\n\
      \032  proceed to delete the directory from the other replica--probably not\n\
      \032  what you want!\n\
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
      \032  incompatibility with ocaml and Windows 95/98 that is not under our\n\
      \032  control, you must start Unison from a DOS window in Windows 95/98 if\n\
      \032  you want to use ssh.\n\
      \n\
      \032  When you click on the Unison icon, two windows will be created:\n\
      \032  Unison's regular window, plus a console window, which is used only for\n\
      \032  giving your password to ssh (if you do not use ssh to connect, you can\n\
      \032  ignore this window). When your password is requested, you'll need to\n\
      \032  activate the console window (e.g., by clicking in it) before typing.\n\
      \032  If you start Unison from a DOS window, Unison's regular window will\n\
      \032  appear and you will type your password in the DOS window you were\n\
      \032  using.\n\
      \n\
      \032  To use Unison in this mode, you must first create a profile (see the\n\
      \032  section \"Profile\" ). Use your favorite editor for this.\n\
      \n\
      "))
::
    ("ssh", ("Installing Ssh", 
     "Installing Ssh\n\
      \n\
      \032  Warning: These instructions may be out of date. More current\n\
      \032  information can be found the Unison Wiki\n\
      \032  (http://alliance.seas.upenn.edu/ bcpierce/wiki/index.php?n=Main.Unison\n\
      \032  FAQOSSpecific).\n\
      \n\
      \032  Your local host will need just an ssh client; the remote host needs an\n\
      \032  ssh server (or daemon), which is available on Unix systems. Unison is\n\
      \032  known to work with ssh version 1.2.27 (Unix) and version 1.2.14\n\
      \032  (Windows); other versions may or may not work.\n\
      \n\
      Unix\n\
      \n\
      \032  Most modern Unix installations come with ssh pre-installed.\n\
      \n\
      Windows\n\
      \n\
      \032  Many Windows implementations of ssh only provide graphical interfaces,\n\
      \032  but Unison requires an ssh client that it can invoke with a\n\
      \032  command-line interface. A suitable version of ssh can be installed as\n\
      \032  follows.\n\
      \032   1. Download an ssh executable.\n\
      \032      Warning: there are many implementations and ports of ssh for\n\
      \032      Windows, and not all of them will work with Unison. We have gotten\n\
      \032      Unison to work with Cygwin's port of openssh, and we suggest you\n\
      \032      try that one first. Here's how to install it:\n\
      \032        a. First, create a new folder on your desktop to hold temporary\n\
      \032           installation files. It can have any name you like, but in\n\
      \032           these instructions we'll assume that you call it Foo.\n\
      \032        b. Direct your web browser to www.cygwin.com, and click on the\n\
      \032           \"Install now!\" link. This will download a file, setup.exe;\n\
      \032           save it in the directory Foo. The file setup.exe is a small\n\
      \032           program that will download the actual install files from the\n\
      \032           Internet when you run it.\n\
      \032        c. Start setup.exe (by double-clicking). This brings up a series\n\
      \032           of dialogs that you will have to go through. Select \"Install\n\
      \032           from Internet.\" For \"Local Package Directory\" select the\n\
      \032           directory Foo. For \"Select install root directory\" we\n\
      \032           recommend that you use the default, C:\\cygwin. The next\n\
      \032           dialog asks you to select the way that you want to connect to\n\
      \032           the network to download the installation files; we have used\n\
      \032           \"Use IE5 Settings\" successfully, but you may need to make a\n\
      \032           different selection depending on your networking setup. The\n\
      \032           next dialog gives a list of mirrors; select one close to you.\n\
      \032           Next you are asked to select which packages to install. The\n\
      \032           default settings in this dialog download a lot of packages\n\
      \032           that are not strictly necessary to run Unison with ssh. If\n\
      \032           you don't want to install a package, click on it until \"skip\"\n\
      \032           is shown. For a minimum installation, select only the\n\
      \032           packages \"cygwin\" and \"openssh,\" which come to about 1900KB;\n\
      \032           the full installation is much larger.\n\
      \n\
      \032    Note that you are plan to build unison using the free CygWin GNU C\n\
      \032    compiler, you need to install essential development packages such\n\
      \032    as \"gcc\", \"make\", \"fileutil\", etc; we refer to the file\n\
      \032    \"INSTALL.win32-cygwin-gnuc\" in the source distribution for further\n\
      \032    details. \n\
      \032           After the packages are downloaded and installed, the next\n\
      \032           dialog allows you to choose whether to \"Create Desktop Icon\"\n\
      \032           and \"Add to Start Menu.\" You make the call.\n\
      \032        d. You can now delete the directory Foo and its contents.\n\
      \032      Some people have reported problems using Cygwin's ssh with Unison.\n\
      \032      If you have trouble, you might try this one instead:\n\
      \032 http://opensores.thebunker.net/pub/mirrors/ssh/contrib/ssh-1.2.14-win32bin.zi\n\
      p\n\
      \032   2. You must set the environment variables HOME and PATH. Ssh will\n\
      \032      create a directory .ssh in the directory given by HOME, so that it\n\
      \032      has a place to keep data like your public and private keys. PATH\n\
      \032      must be set to include the Cygwin bin directory, so that Unison\n\
      \032      can find the ssh executable.\n\
      \032         + On Windows 95/98, add the lines\n\
      \032   set PATH=%PATH%;<SSHDIR>\n\
      \032   set HOME=<HOMEDIR>\n\
      \032           to the file C:\\AUTOEXEC.BAT, where <HOMEDIR> is the directory\n\
      \032           where you want ssh to create its .ssh directory, and <SSHDIR>\n\
      \032           is the directory where the executable ssh.exe is stored; if\n\
      \032           you've installed Cygwin in the default location, this is\n\
      \032           C:\\cygwin\\bin. You will have to reboot your computer to take\n\
      \032           the changes into account.\n\
      \032         + On Windows NT/2k/XP, open the environment variables dialog\n\
      \032           box:\n\
      \032              o Windows NT: My Computer/Properties/Environment\n\
      \032              o Windows 2k: My Computer/Properties/Advanced/Environment\n\
      \032                variables\n\
      \032           then select Path and edit its value by appending ;<SSHDIR> to\n\
      \032           it, where <SSHDIR> is the full name of the directory that\n\
      \032           includes the ssh executable; if you've installed Cygwin in\n\
      \032           the default location, this is C:\\cygwin\\bin.\n\
      \032   3. Test ssh from a DOS shell by typing\n\
      \032     ssh <remote host> -l <login name>\n\
      \032      You should get a prompt for your password on <remote host>,\n\
      \032      followed by a working connection.\n\
      \032   4. Note that ssh-keygen may not work (fails with \"gethostname: no\n\
      \032      such file or directory\") on some systems. This is OK: you can use\n\
      \032      ssh with your regular password for the remote system.\n\
      \032   5. You should now be able to use Unison with an ssh connection. If\n\
      \032      you are logged in with a different user name on the local and\n\
      \032      remote hosts, provide your remote user name when providing the\n\
      \032      remote root (i.e., //username@host/path...).\n\
      \n\
      "))
::
    ("news", ("Changes in Version 2.32.9", 
     "Changes in Version 2.32.9\n\
      \n\
      \032  Changes since 2.31:\n\
      \032    * Small user interface changes\n\
      \032         + Small change to text UI \"scanning...\" messages, to print just\n\
      \032           directories (hopefully making it clearer that individual\n\
      \032           files are not necessarily being fingerprinted).\n\
      \032    * Minor fixes and improvements:\n\
      \032         + Ignore one hour differences when deciding whether a file may\n\
      \032           have been updated. This avoids slow update detection after\n\
      \032           daylight saving time changes under Windows. This makes Unison\n\
      \032           slightly more likely to miss an update, but it should be safe\n\
      \032           enough.\n\
      \032         + Fix a small bug that was affecting mainly windows users. We\n\
      \032           need to commit the archives at the end of the sync even if\n\
      \032           there are no updates to propagate because some files (in\n\
      \032           fact, if we've just switched to DST on windows, a LOT of\n\
      \032           files) might have new modtimes in the archive. (Changed the\n\
      \032           text UI only. It's less clear where to change the GUI.)\n\
      \032         + Don't delete the temp file when a transfer fails due to a\n\
      \032           fingerprint mismatch (so that we can have a look and see\n\
      \032           why!) We've also added more debugging code togive more\n\
      \032           informative error messages when we encounter the dreaded and\n\
      \032           longstanding \"assert failed during file transfer\" bug\n\
      \n\
      \032  Changes since 2.27:\n\
      \032    * If Unison is interrupted during a directory transfer, it will now\n\
      \032      leave the partially transferred directory intact in a temporary\n\
      \032      location. (This maintains the invariant that new files/directories\n\
      \032      are transferred either completely or not at all.) The next time\n\
      \032      Unison is run, it will continue filling in this temporary\n\
      \032      directory, skipping transferring files that it finds are already\n\
      \032      there.\n\
      \032    * We've added experimental support for invoking an external file\n\
      \032      transfer tool for whole-file copies instead of Unison's built-in\n\
      \032      transfer protocol. Three new preferences have been added:\n\
      \032         + copyprog is a string giving the name (and command-line\n\
      \032           switches, if needed) of an external program that can be used\n\
      \032           to copy large files efficiently. By default, rsync is\n\
      \032           invoked, but other tools such as scp can be used instead by\n\
      \032           changing the value of this preference. (Although this is not\n\
      \032           its primary purpose, rsync is actually a pretty fast way of\n\
      \032           copying files that don't already exist on the receiving\n\
      \032           host.) For files that do already exist on (but that have been\n\
      \032           changed in one replica), Unison will always use its built-in\n\
      \032           implementation of the rsync algorithm.\n\
      \032         + Added a \"copyprogrest\" preference, so that we can give\n\
      \032           different command lines for invoking the external copy\n\
      \032           utility depending on whether a partially transferred file\n\
      \032           already exists or not. (Rsync doesn't seem to care about\n\
      \032           this, but other utilities may.)\n\
      \032         + copythreshold is an integer (-1 by default), indicating above\n\
      \032           what filesize (in megabytes) Unison should use the external\n\
      \032           copying utility specified by copyprog. Specifying 0 will\n\
      \032           cause ALL copies to use the external program; a negative\n\
      \032           number will prevent any files from using it. (Default is -1.)\n\
      \032      Thanks to Alan Schmitt for a huge amount of hacking and to an\n\
      \032      anonymous sponsor for suggesting and underwriting this extension.\n\
      \032    * Small improvements:\n\
      \032         + Added a new preference, dontchmod. By default, Unison uses\n\
      \032           the chmod system call to set the permission bits of files\n\
      \032           after it has copied them. But in some circumstances (and\n\
      \032           under some operating systems), the chmod call always fails.\n\
      \032           Setting this preference completely prevents Unison from ever\n\
      \032           calling chmod.\n\
      \032         + Don't ignore files that look like backup files if the\n\
      \032           backuplocation preference is set to central\n\
      \032         + Shortened the names of several preferences. The old names are\n\
      \032           also still supported, for backwards compatibility, but they\n\
      \032           do not appear in the documentation.\n\
      \032         + Lots of little documentation tidying. (In particular,\n\
      \032           preferences are separated into Basic and Advanced! This\n\
      \032           should hopefully make Unison a little more approachable for\n\
      \032           new users.\n\
      \032         + Unison can sometimes fail to transfer a file, giving the\n\
      \032           unhelpful message \"Destination updated during\n\
      \032           synchronization\" even though the file has not been changed.\n\
      \032           This can be caused by programs that change either the file's\n\
      \032           contents or the file's extended attributes without changing\n\
      \032           its modification time. It's not clear what is the best fix\n\
      \032           for this - it is not Unison's fault, but it makes Unison's\n\
      \032           behavior puzzling - but at least Unison can be more helpful\n\
      \032           about suggesting a workaround (running once with fastcheck\n\
      \032           set to false). The failure message has been changed to give\n\
      \032           this advice.\n\
      \032         + Further improvements to the OS X GUI (thanks to Alan Schmitt\n\
      \032           and Craig Federighi).\n\
      \032    * Very preliminary support for triggering Unison from an external\n\
      \032      filesystem-watching utility. The current implementation is very\n\
      \032      simple, not efficient, and almost completely untested--not ready\n\
      \032      for real users. But if someone wants to help improve it (e.g., by\n\
      \032      writing a filesystem watcher for your favorite OS), please make\n\
      \032      yourself known!\n\
      \032      On the Unison side, the new behavior is very simple:\n\
      \032         + use the text UI\n\
      \032         + start Unison with the command-line flag \"-repeat FOO\", where\n\
      \032           FOO is name of a file where Unison should look for\n\
      \032           notifications of changes\n\
      \032         + when it starts up, Unison will read the whole contents of\n\
      \032           this file (on both hosts), which should be a\n\
      \032           newline-separated list of paths (relative to the root of the\n\
      \032           synchronization) and synchronize just these paths, as if it\n\
      \032           had been started with the \"-path=xxx\" option for each one of\n\
      \032           them\n\
      \032         + when it finishes, it will sleep for a few seconds and then\n\
      \032           examine the watchfile again; if anything has been added, it\n\
      \032           will read the new paths, synchronize them, and go back to\n\
      \032           sleep\n\
      \032         + that's it!\n\
      \032      To use this to drive Unison \"incrementally,\" just start it in this\n\
      \032      mode and start up a tool (on each host) to watch for new changes\n\
      \032      to the filesystem and append the appropriate paths to the\n\
      \032      watchfile. Hopefully such tools should not be too hard to write.\n\
      \032    * Bug fixes:\n\
      \032         + Fixed a bug that was causing new files to be created with\n\
      \032           permissions 0x600 instead of using a reasonable default (like\n\
      \032           0x644), if the 'perms' flag was set to 0. (Bug reported by\n\
      \032           Ben Crowell.)\n\
      \032         + Follow maxthreads preference when transferring directories.\n\
      \n\
      \032  Changes since 2.17:\n\
      \032    * Major rewrite and cleanup of the whole Mac OS X graphical user\n\
      \032      interface by Craig Federighi. Thanks, Craig!!!\n\
      \032    * Small fix to ctime (non-)handling in update detection under\n\
      \032      windows with fastcheck.\n\
      \032    * Several small fixes to the GTK2 UI to make it work better under\n\
      \032      Windows [thanks to Karl M for these].\n\
      \032    * The backup functionality has been completely rewritten. The\n\
      \032      external interface has not changed, but numerous bugs, irregular\n\
      \032      behaviors, and cross-platform inconsistencies have been corrected.\n\
      \032    * The Unison project now accepts donations via PayPal. If you'd like\n\
      \032      to donate, you can find a link to the donation page on the Unison\n\
      \032      home page (http://www.cis.upenn.edu/ bcpierce/unison/lists.html).\n\
      \032    * Some important safety improvements:\n\
      \032         + Added a new mountpoint preference, which can be used to\n\
      \032           specify a path that must exist in both replicas at the end of\n\
      \032           update detection (otherwise Unison aborts). This can be used\n\
      \032           to avoid potentially dangerous situations when Unison is used\n\
      \032           with removable media such as external hard drives and compact\n\
      \032           flash cards.\n\
      \032         + The confirmation of \"big deletes\" is now controlled by a\n\
      \032           boolean preference confirmbigdeletes. Default is true, which\n\
      \032           gives the same behavior as previously. (This functionality is\n\
      \032           at least partly superceded by the mountpoint preference, but\n\
      \032           it has been left in place in case it is useful to some\n\
      \032           people.)\n\
      \032         + If Unison is asked to \"follow\" a symbolic link but there is\n\
      \032           nothing at the other end of the link, it will now flag this\n\
      \032           path as an error, rather than treating the symlink itself as\n\
      \032           missing or deleted. This avoids a potentially dangerous\n\
      \032           situation where a followed symlink points to an external\n\
      \032           filesystem that might be offline when Unison is run\n\
      \032           (whereupon Unison would cheerfully delete the corresponding\n\
      \032           files in the other replica!).\n\
      \032    * Smaller changes:\n\
      \032         + Added forcepartial and preferpartial preferences, which\n\
      \032           behave like force and prefer but can be specified on a\n\
      \032           per-path basis. [Thanks to Alan Schmitt for this.]\n\
      \032         + A bare-bones self test feature was added, which runs unison\n\
      \032           through some of its paces and checks that the results are as\n\
      \032           expected. The coverage of the tests is still very limited,\n\
      \032           but the facility has already been very useful in debugging\n\
      \032           the new backup functionality (especially in exposing some\n\
      \032           subtle cross-platform issues).\n\
      \032         + Refined debugging code so that the verbosity of individual\n\
      \032           modules can be controlled separately. Instead of just putting\n\
      \032           '-debug verbose' on the command line, you can put '-debug\n\
      \032           update+', which causes all the extra messages in the Update\n\
      \032           module, but not other modules, to be printed. Putting '-debug\n\
      \032           verbose' causes all modules to print with maximum verbosity.\n\
      \032         + Removed mergebatch preference. (It never seemed very useful,\n\
      \032           and its semantics were confusing.)\n\
      \032         + Rewrote some of the merging functionality, for better\n\
      \032           cooperation with external Harmony instances.\n\
      \032         + Changed the temp file prefix from .# to .unison.\n\
      \032         + Compressed the output from the text user interface\n\
      \032           (particularly when run with the -terse flag) to make it\n\
      \032           easier to interpret the results when Unison is run several\n\
      \032           times in succession from a script.\n\
      \032         + Diff and merge functions now work under Windows.\n\
      \032         + Changed the order of arguments to the default diff command\n\
      \032           (so that the + and - annotations in diff's output are\n\
      \032           reversed).\n\
      \032         + Added .mpp files to the \"never fastcheck\" list (like .xls\n\
      \032           files).\n\
      \032    * Many small bugfixes, including:\n\
      \032         + Fixed a longstanding bug regarding fastcheck and daylight\n\
      \032           saving time under Windows when Unison is set up to\n\
      \032           synchronize modification times. (Modification times cannot be\n\
      \032           updated in the archive in this case, so we have to ignore one\n\
      \032           hour differences.)\n\
      \032         + Fixed a bug that would occasionally cause the archives to be\n\
      \032           left in non-identical states on the two hosts after\n\
      \032           synchronization.\n\
      \032         + Fixed a bug that prevented Unison from communicating\n\
      \032           correctly between 32- and 64-bit architectures.\n\
      \032         + On windows, file creation times are no longer used as a proxy\n\
      \032           for inode numbers. (This is unfortunate, as it makes\n\
      \032           fastcheck a little less safe. But it turns out that file\n\
      \032           creation times are not reliable under Windows: if a file is\n\
      \032           removed and a new file is created in its place, the new one\n\
      \032           will sometimes be given the same creation date as the old\n\
      \032           one!)\n\
      \032         + Set read-only file to R/W on OSX before attempting to change\n\
      \032           other attributes.\n\
      \032         + Fixed bug resulting in spurious \"Aborted\" errors during\n\
      \032           transport (thanks to Jerome Vouillon)\n\
      \032         + Enable diff if file contents have changed in one replica, but\n\
      \032           only properties in the other.\n\
      \032         + Removed misleading documentation for 'repeat' preference.\n\
      \032         + Fixed a bug in merging code where Unison could sometimes\n\
      \032           deadlock with the external merge program, if the latter\n\
      \032           produced large amounts of output.\n\
      \032         + Workaround for a bug compiling gtk2 user interface against\n\
      \032           current versions of gtk2+ libraries.\n\
      \032         + Added a better error message for \"ambiguous paths\".\n\
      \032         + Squashed a longstanding bug that would cause file transfer to\n\
      \032           fail with the message \"Failed: Error in readWrite: Is a\n\
      \032           directory.\"\n\
      \032         + Replaced symlinks with copies of their targets in the Growl\n\
      \032           framework in src/uimac. This should make the sources easier\n\
      \032           to check out from the svn repository on WinXP systems.\n\
      \032         + Added a workaround (suggested by Karl M.) for the problem\n\
      \032           discussed on the unison users mailing list where, on the\n\
      \032           Windows platform, the server would hang when transferring\n\
      \032           files. I conjecture that the problem has to do with the RPC\n\
      \032           mechanism, which was used to make a call back from the server\n\
      \032           to the client (inside the Trace.log function) so that the log\n\
      \032           message would be appended to the log file on the client. The\n\
      \032           workaround is to dump these messages (about when\n\
      \032           xferbycopying shortcuts are applied and whether they succeed)\n\
      \032           just to the standard output of the Unison process, not to the\n\
      \032           log file.\n\
      \n\
      \032  Changes since 2.13.0:\n\
      \032    * The features for performing backups and for invoking external\n\
      \032      merge programs have been completely rewritten by Stephane Lescuyer\n\
      \032      (thanks, Stephane!). The user-visible functionality should not\n\
      \032      change, but the internals have been rationalized and there are a\n\
      \032      number of new features. See the manual (in particular, the\n\
      \032      description of the backupXXX preferences) for details.\n\
      \032    * Incorporated patches for ipv6 support, contributed by Samuel\n\
      \032      Thibault. (Note that, due to a bug in the released OCaml 3.08.3\n\
      \032      compiler, this code will not actually work with ipv6 unless\n\
      \032      compiled with the CVS version of the OCaml compiler, where the bug\n\
      \032      has been fixed; however, ipv4 should continue to work normally.)\n\
      \032    * OSX interface:\n\
      \032         + Incorporated Ben Willmore's cool new icon for the Mac UI.\n\
      \032    * Small fixes:\n\
      \032         + Fixed off by one error in month numbers (in printed dates)\n\
      \032           reported by Bob Burger\n\
      \n\
      \032  Changes since 2.12.0:\n\
      \032    * New convention for release numbering: Releases will continue to be\n\
      \032      given numbers of the form X.Y.Z, but, from now on, just the major\n\
      \032      version number (X.Y) will be considered significant when checking\n\
      \032      compatibility between client and server versions. The third\n\
      \032      component of the version number will be used only to identify\n\
      \032      \"patch levels\" of releases.\n\
      \032      This change goes hand in hand with a change to the procedure for\n\
      \032      making new releases. Candidate releases will initially be given\n\
      \032      \"beta release\" status when they are announced for public\n\
      \032      consumption. Any bugs that are discovered will be fixed in a\n\
      \032      separate branch of the source repository (without changing the\n\
      \032      major version number) and new tarballs re-released as needed. When\n\
      \032      this process converges, the patched beta version will be dubbed\n\
      \032      stable.\n\
      \032    * Warning (failure in batch mode) when one path is completely\n\
      \032      emptied. This prevents Unison from deleting everything on one\n\
      \032      replica when the other disappear.\n\
      \032    * Fix diff bug (where no difference is shown the first time the diff\n\
      \032      command is given).\n\
      \032    * User interface changes:\n\
      \032         + Improved workaround for button focus problem (GTK2 UI)\n\
      \032         + Put leading zeroes in date fields\n\
      \032         + More robust handling of character encodings in GTK2 UI\n\
      \032         + Changed format of modification time displays, from modified\n\
      \032           at hh:mm:ss on dd MMM, yyyy to modified on yyyy-mm-dd\n\
      \032           hh:mm:ss\n\
      \032         + Changed time display to include seconds (so that people on\n\
      \032           FAT filesystems will not be confused when Unison tries to\n\
      \032           update a file time to an odd number of seconds and the\n\
      \032           filesystem truncates it to an even number!)\n\
      \032         + Use the diff \"-u\" option by default when showing differences\n\
      \032           between files (the output is more readable)\n\
      \032         + In text mode, pipe the diff output to a pager if the\n\
      \032           environment variable PAGER is set\n\
      \032         + Bug fixes and cleanups in ssh password prompting. Now works\n\
      \032           with the GTK2 UI under Linux. (Hopefully the Mac OS X one is\n\
      \032           not broken!)\n\
      \032         + Include profile name in the GTK2 window name\n\
      \032         + Added bindings ',' (same as '<') and '.' (same as '>') in the\n\
      \032           GTK2 UI\n\
      \032    * Mac GUI:\n\
      \032         + actions like < and > scroll to the next item as necessary.\n\
      \032         + Restart has a menu item and keyboard shortcut (command-R).\n\
      \032         + Added a command-line tool for Mac OS X. It can be installed\n\
      \032           from the Unison menu.\n\
      \032         + New icon.\n\
      \032         + Handle the \"help\" command-line argument properly.\n\
      \032         + Handle profiles given on the command line properly.\n\
      \032         + When a profile has been selected, the profile dialog is\n\
      \032           replaced by a \"connecting\" message while the connection is\n\
      \032           being made. This gives better feedback.\n\
      \032         + Size of left and right columns is now large enough so that\n\
      \032           \"PropsChanged\" is not cut off.\n\
      \032    * Minor changes:\n\
      \032         + Disable multi-threading when both roots are local\n\
      \032         + Improved error handling code. In particular, make sure all\n\
      \032           files are closed in case of a transient failure\n\
      \032         + Under Windows, use $UNISON for home directory as a last\n\
      \032           resort (it was wrongly moved before $HOME and $USERPROFILE in\n\
      \032           Unison 2.12.0)\n\
      \032         + Reopen the logfile if its name changes (profile change)\n\
      \032         + Double-check that permissions and modification times have\n\
      \032           been properly set: there are some combination of OS and\n\
      \032           filesystem on which setting them can fail in a silent way.\n\
      \032         + Check for bad Windows filenames for pure Windows\n\
      \032           synchronization also (not just cross architecture\n\
      \032           synchronization). This way, filenames containing backslashes,\n\
      \032           which are not correctly handled by unison, are rejected right\n\
      \032           away.\n\
      \032         + Attempt to resolve issues with synchronizing modification\n\
      \032           times of read-only files under Windows\n\
      \032         + Ignore chmod failures when deleting files\n\
      \032         + Ignore trailing dots in filenames in case insensitive mode\n\
      \032         + Proper quoting of paths, files and extensions ignored using\n\
      \032           the UI\n\
      \032         + The strings CURRENT1 and CURRENT2 are now correctly\n\
      \032           substitued when they occur in the diff preference\n\
      \032         + Improvements to syncing resource forks between Macs via a\n\
      \032           non-Mac system.\n\
      \n\
      \032  Changes since 2.10.2:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed.\n\
      \032    * Source code availability: The Unison sources are now managed using\n\
      \032      Subversion. One nice side-effect is that anonymous checkout is now\n\
      \032      possible, like this:\n\
      \032       svn co https://cvs.cis.upenn.edu:3690/svnroot/unison/\n\
      \032      We will also continue to export a \"developer tarball\" of the\n\
      \032      current (modulo one day) sources in the web export directory. To\n\
      \032      receive commit logs for changes to the sources, subscribe to the\n\
      \032      unison-hackers list\n\
      \032      (http://www.cis.upenn.edu/ bcpierce/unison/lists.html).\n\
      \032    * Text user interface:\n\
      \032         + Substantial reworking of the internal logic of the text UI to\n\
      \032           make it a bit easier to modify.\n\
      \032         + The dumbtty flag in the text UI is automatically set to true\n\
      \032           if the client is running on a Unix system and the EMACS\n\
      \032           environment variable is set to anything other than the empty\n\
      \032           string.\n\
      \032    * Native OS X gui:\n\
      \032         + Added a synchronize menu item with keyboard shortcut\n\
      \032         + Added a merge menu item, still needs to be debugged\n\
      \032         + Fixes to compile for Panther\n\
      \032         + Miscellaneous improvements and bugfixes\n\
      \032    * Small changes:\n\
      \032         + Changed the filename checking code to apply to Windows only,\n\
      \032           instead of OS X as well.\n\
      \032         + Finder flags now synchronized\n\
      \032         + Fallback in copy.ml for filesystem that do not support O_EXCL\n\
      \032         + Changed buffer size for local file copy (was highly\n\
      \032           inefficient with synchronous writes)\n\
      \032         + Ignore chmod failure when deleting a directory\n\
      \032         + Fixed assertion failure when resolving a conflict content\n\
      \032           change / permission changes in favor of the content change.\n\
      \032         + Workaround for transferring large files using rsync.\n\
      \032         + Use buffered I/O for files (this is the only way to open\n\
      \032           files in binary mode under Cygwin).\n\
      \032         + On non-Cygwin Windows systems, the UNISON environment\n\
      \032           variable is now checked first to determine where to look for\n\
      \032           Unison's archive and preference files, followed by HOME and\n\
      \032           USERPROFILE in that order. On Unix and Cygwin systems, HOME\n\
      \032           is used.\n\
      \032         + Generalized diff preference so that it can be given either as\n\
      \032           just the command name to be used for calculating diffs or\n\
      \032           else a whole command line, containing the strings CURRENT1\n\
      \032           and CURRENT2, which will be replaced by the names of the\n\
      \032           files to be diff'ed before the command is called.\n\
      \032         + Recognize password prompts in some newer versions of ssh.\n\
      \n\
      \032  Changes since 2.9.20:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed.\n\
      \032    * Major functionality changes:\n\
      \032         + Major tidying and enhancement of 'merge' functionality. The\n\
      \032           main user-visible change is that the external merge program\n\
      \032           may either write the merged output to a single new file, as\n\
      \032           before, or it may modify one or both of its input files, or\n\
      \032           it may write two new files. In the latter cases, its\n\
      \032           modifications will be copied back into place on both the\n\
      \032           local and the remote host, and (if the two files are now\n\
      \032           equal) the archive will be updated appropriately. More\n\
      \032           information can be found in the user manual. Thanks to Malo\n\
      \032           Denielou and Alan Schmitt for these improvements.\n\
      \032           Warning: the new merging functionality is not completely\n\
      \032           compatible with old versions! Check the manual for details.\n\
      \032         + Files larger than 2Gb are now supported.\n\
      \032         + Added preliminary (and still somewhat experimental) support\n\
      \032           for the Apple OS X operating system.\n\
      \032              o Resource forks should be transferred correctly. (See the\n\
      \032                manual for details of how this works when synchronizing\n\
      \032                HFS with non-HFS volumes.) Synchronization of file type\n\
      \032                and creator information is also supported.\n\
      \032              o On OSX systems, the name of the directory for storing\n\
      \032                Unison's archives, preference files, etc., is now\n\
      \032                determined as follows:\n\
      \032                   # if ~/.unison exists, use it\n\
      \032                   # otherwise, use ~/Library/Application\n\
      \032                     Support/Unison, creating it if necessary.\n\
      \032              o A preliminary native-Cocoa user interface is under\n\
      \032                construction. This still needs some work, and some users\n\
      \032                experience unpredictable crashes, so it is only for\n\
      \032                hackers for now. Run make with UISTYLE=mac to build this\n\
      \032                interface.\n\
      \032    * Minor functionality changes:\n\
      \032         + Added an ignorelocks preference, which forces Unison to\n\
      \032           override left-over archive locks. (Setting this preference is\n\
      \032           dangerous! Use it only if you are positive you know what you\n\
      \032           are doing.)\n\
      \032         + Added a new preference assumeContentsAreImmutable. If a\n\
      \032           directory matches one of the patterns set in this preference,\n\
      \032           then update detection is skipped for files in this directory.\n\
      \032           (The purpose is to speed update detection for cases like Mail\n\
      \032           folders, which contain lots and lots of immutable files.)\n\
      \032           Also a preference assumeContentsAreImmutableNot, which\n\
      \032           overrides the first, similarly to ignorenot. (Later\n\
      \032           amendment: these preferences are now called immutable and\n\
      \032           immutablenot.)\n\
      \032         + The ignorecase flag has been changed from a boolean to a\n\
      \032           three-valued preference. The default setting, called default,\n\
      \032           checks the operating systems running on the client and server\n\
      \032           and ignores filename case if either of them is OSX or\n\
      \032           Windows. Setting ignorecase to true or false overrides this\n\
      \032           behavior. If you have been setting ignorecase on the command\n\
      \032           line using -ignorecase=true or -ignorecase=false, you will\n\
      \032           need to change to -ignorecase true or -ignorecase false.\n\
      \032         + a new preference, 'repeat', for the text user interface\n\
      \032           (only). If 'repeat' is set to a number, then, after it\n\
      \032           finishes synchronizing, Unison will wait for that many\n\
      \032           seconds and then start over, continuing this way until it is\n\
      \032           killed from outside. Setting repeat to true will\n\
      \032           automatically set the batch preference to true.\n\
      \032         + Excel files are now handled specially, so that the fastcheck\n\
      \032           optimization is skipped even if the fastcheck flag is set.\n\
      \032           (Excel does some naughty things with modtimes, making this\n\
      \032           optimization unreliable and leading to failures during change\n\
      \032           propagation.)\n\
      \032         + The ignorecase flag has been changed from a boolean to a\n\
      \032           three-valued preference. The default setting, called\n\
      \032           'default', checks the operating systems running on the client\n\
      \032           and server and ignores filename case if either of them is OSX\n\
      \032           or Windows. Setting ignorecase to 'true' or 'false' overrides\n\
      \032           this behavior.\n\
      \032         + Added a new preference, 'repeat', for the text user interface\n\
      \032           (only, at the moment). If 'repeat' is set to a number, then,\n\
      \032           after it finishes synchronizing, Unison will wait for that\n\
      \032           many seconds and then start over, continuing this way until\n\
      \032           it is killed from outside. Setting repeat to true will\n\
      \032           automatically set the batch preference to true.\n\
      \032         + The 'rshargs' preference has been split into 'rshargs' and\n\
      \032           'sshargs' (mainly to make the documentation clearer). In\n\
      \032           fact, 'rshargs' is no longer mentioned in the documentation\n\
      \032           at all, since pretty much everybody uses ssh now anyway.\n\
      \032    * Documentation\n\
      \032         + The web pages have been completely redesigned and\n\
      \032           reorganized. (Thanks to Alan Schmitt for help with this.)\n\
      \032    * User interface improvements\n\
      \032         + Added a GTK2 user interface, capable (among other things) of\n\
      \032           displaying filenames in any locale encoding. Kudos to Stephen\n\
      \032           Tse for contributing this code!\n\
      \032         + The text UI now prints a list of failed and skipped transfers\n\
      \032           at the end of synchronization.\n\
      \032         + Restarting update detection from the graphical UI will reload\n\
      \032           the current profile (which in particular will reset the -path\n\
      \032           preference, in case it has been narrowed by using the\n\
      \032           \"Recheck unsynchronized items\" command).\n\
      \032         + Several small improvements to the text user interface,\n\
      \032           including a progress display.\n\
      \032    * Bug fixes (too numerous to count, actually, but here are some):\n\
      \032         + The maxthreads preference works now.\n\
      \032         + Fixed bug where warning message about uname returning an\n\
      \032           unrecognized result was preventing connection to server. (The\n\
      \032           warning is no longer printed, and all systems where 'uname'\n\
      \032           returns anything other than 'Darwin' are assumed not to be\n\
      \032           running OS X.)\n\
      \032         + Fixed a problem on OS X that caused some valid file names\n\
      \032           (e.g., those including colons) to be considered invalid.\n\
      \032         + Patched Path.followLink to follow links under cygwin in\n\
      \032           addition to Unix (suggested by Matt Swift).\n\
      \032         + Small change to the storeRootsName function, suggested by\n\
      \032           bliviero at ichips.intel.com, to fix a problem in unison with\n\
      \032           the `rootalias' option, which allows you to tell unison that\n\
      \032           two roots contain the same files. Rootalias was being applied\n\
      \032           after the hosts were sorted, so it wouldn't work properly in\n\
      \032           all cases.\n\
      \032         + Incorporated a fix by Dmitry Bely for setting utimes of\n\
      \032           read-only files on Win32 systems.\n\
      \032    * Installation / portability:\n\
      \032         + Unison now compiles with OCaml version 3.07 and later out of\n\
      \032           the box.\n\
      \032         + Makefile.OCaml fixed to compile out of the box under OpenBSD.\n\
      \032         + a few additional ports (e.g. OpenBSD, Zaurus/IPAQ) are now\n\
      \032           mentioned in the documentation\n\
      \032         + Unison can now be installed easily on OSX systems using the\n\
      \032           Fink package manager\n\
      \n\
      \032  Changes since 2.9.1:\n\
      \032    * Added a preference maxthreads that can be used to limit the number\n\
      \032      of simultaneous file transfers.\n\
      \032    * Added a backupdir preference, which controls where backup files\n\
      \032      are stored.\n\
      \032    * Basic support added for OSX. In particular, Unison now recognizes\n\
      \032      when one of the hosts being synchronized is running OSX and\n\
      \032      switches to a case-insensitive treatment of filenames (i.e., 'foo'\n\
      \032      and 'FOO' are considered to be the same file). (OSX is not yet\n\
      \032      fully working, however: in particular, files with resource forks\n\
      \032      will not be synchronized correctly.)\n\
      \032    * The same hash used to form the archive name is now also added to\n\
      \032      the names of the temp files created during file transfer. The\n\
      \032      reason for this is that, during update detection, we are going to\n\
      \032      silently delete any old temp files that we find along the way, and\n\
      \032      we want to prevent ourselves from deleting temp files belonging to\n\
      \032      other instances of Unison that may be running in parallel, e.g.\n\
      \032      synchronizing with a different host. Thanks to Ruslan Ermilov for\n\
      \032      this suggestion.\n\
      \032    * Several small user interface improvements\n\
      \032    * Documentation\n\
      \032         + FAQ and bug reporting instructions have been split out as\n\
      \032           separate HTML pages, accessible directly from the unison web\n\
      \032           page.\n\
      \032         + Additions to FAQ, in particular suggestions about performance\n\
      \032           tuning.\n\
      \032    * Makefile\n\
      \032         + Makefile.OCaml now sets UISTYLE=text or UISTYLE=gtk\n\
      \032           automatically, depending on whether it finds lablgtk\n\
      \032           installed\n\
      \032         + Unison should now compile \"out of the box\" under OSX\n\
      \n\
      \032  Changes since 2.8.1:\n\
      \032    * Changing profile works again under Windows\n\
      \032    * File movement optimization: Unison now tries to use local copy\n\
      \032      instead of transfer for moved or copied files. It is controled by\n\
      \032      a boolean option \"xferbycopying\".\n\
      \032    * Network statistics window (transfer rate, amount of data\n\
      \032      transferred). [NB: not available in Windows-Cygwin version.]\n\
      \032    * symlinks work under the cygwin version (which is dynamically\n\
      \032      linked).\n\
      \032    * Fixed potential deadlock when synchronizing between Windows and\n\
      \032      Unix\n\
      \032    * Small improvements:\n\
      \032         + If neither the USERPROFILE nor the HOME environment variables\n\
      \032           are set, then Unison will put its temporary commit log\n\
      \032           (called DANGER.README) into the directory named by the UNISON\n\
      \032           environment variable, if any; otherwise it will use C:.\n\
      \032         + alternative set of values for fastcheck: yes = true; no =\n\
      \032           false; default = auto.\n\
      \032         + -silent implies -contactquietly\n\
      \032    * Source code:\n\
      \032         + Code reorganization and tidying. (Started breaking up some of\n\
      \032           the basic utility modules so that the non-unison-specific\n\
      \032           stuff can be made available for other projects.)\n\
      \032         + several Makefile and docs changes (for release);\n\
      \032         + further comments in \"update.ml\";\n\
      \032         + connection information is not stored in global variables\n\
      \032           anymore.\n\
      \n\
      \032  Changes since 2.7.78:\n\
      \032    * Small bugfix to textual user interface under Unix (to avoid\n\
      \032      leaving the terminal in a bad state where it would not echo inputs\n\
      \032      after Unison exited).\n\
      \n\
      \032  Changes since 2.7.39:\n\
      \032    * Improvements to the main web page (stable and beta version docs\n\
      \032      are now both accessible).\n\
      \032    * User manual revised.\n\
      \032    * Added some new preferences:\n\
      \032         + \"sshcmd\" and \"rshcmd\" for specifying paths to ssh and rsh\n\
      \032           programs.\n\
      \032         + \"contactquietly\" for suppressing the \"contacting server\"\n\
      \032           message during Unison startup (under the graphical UI).\n\
      \032    * Bug fixes:\n\
      \032         + Fixed small bug in UI that neglected to change the displayed\n\
      \032           column headers if loading a new profile caused the roots to\n\
      \032           change.\n\
      \032         + Fixed a bug that would put the text UI into an infinite loop\n\
      \032           if it encountered a conflict when run in batch mode.\n\
      \032         + Added some code to try to fix the display of non-Ascii\n\
      \032           characters in filenames on Windows systems in the GTK UI.\n\
      \032           (This code is currently untested--if you're one of the people\n\
      \032           that had reported problems with display of non-ascii\n\
      \032           filenames, we'd appreciate knowing if this actually fixes\n\
      \032           things.)\n\
      \032         + `-prefer/-force newer' works properly now. (The bug was\n\
      \032           reported by Sebastian Urbaniak and Sean Fulton.)\n\
      \032    * User interface and Unison behavior:\n\
      \032         + Renamed `Proceed' to `Go' in the graphical UI.\n\
      \032         + Added exit status for the textual user interface.\n\
      \032         + Paths that are not synchronized because of conflicts or\n\
      \032           errors during update detection are now noted in the log file.\n\
      \032         + [END] messages in log now use a briefer format\n\
      \032         + Changed the text UI startup sequence so that ./unison -ui\n\
      \032           text will use the default profile instead of failing.\n\
      \032         + Made some improvements to the error messages.\n\
      \032         + Added some debugging messages to remote.ml.\n\
      \n\
      \032  Changes since 2.7.7:\n\
      \032    * Incorporated, once again, a multi-threaded transport sub-system.\n\
      \032      It transfers several files at the same time, thereby making much\n\
      \032      more effective use of available network bandwidth. Unlike the\n\
      \032      earlier attempt, this time we do not rely on the native thread\n\
      \032      library of OCaml. Instead, we implement a light-weight,\n\
      \032      non-preemptive multi-thread library in OCaml directly. This\n\
      \032      version appears stable.\n\
      \032      Some adjustments to unison are made to accommodate the\n\
      \032      multi-threaded version. These include, in particular, changes to\n\
      \032      the user interface and logging, for example:\n\
      \032         + Two log entries for each transferring task, one for the\n\
      \032           beginning, one for the end.\n\
      \032         + Suppressed warning messages against removing temp files left\n\
      \032           by a previous unison run, because warning does not work\n\
      \032           nicely under multi-threading. The temp file names are made\n\
      \032           less likely to coincide with the name of a file created by\n\
      \032           the user. They take the form\n\
      \032           .#<filename>.<serial>.unison.tmp. [N.b. This was later\n\
      \032           changed to .unison.<filename>.<serial>.unison.tmp.]\n\
      \032    * Added a new command to the GTK user interface: pressing 'f' causes\n\
      \032      Unison to start a new update detection phase, using as paths just\n\
      \032      those paths that have been detected as changed and not yet marked\n\
      \032      as successfully completed. Use this command to quickly restart\n\
      \032      Unison on just the set of paths still needing attention after a\n\
      \032      previous run.\n\
      \032    * Made the ignorecase preference user-visible, and changed the\n\
      \032      initialization code so that it can be manually set to true, even\n\
      \032      if neither host is running Windows. (This may be useful, e.g.,\n\
      \032      when using Unison running on a Unix system with a FAT volume\n\
      \032      mounted.)\n\
      \032    * Small improvements and bug fixes:\n\
      \032         + Errors in preference files now generate fatal errors rather\n\
      \032           than warnings at startup time. (I.e., you can't go on from\n\
      \032           them.) Also, we fixed a bug that was preventing these\n\
      \032           warnings from appearing in the text UI, so some users who\n\
      \032           have been running (unsuspectingly) with garbage in their\n\
      \032           prefs files may now get error reports.\n\
      \032         + Error reporting for preference files now provides file name\n\
      \032           and line number.\n\
      \032         + More intelligible message in the case of identical change to\n\
      \032           the same files: \"Nothing to do: replicas have been changed\n\
      \032           only in identical ways since last sync.\"\n\
      \032         + Files with prefix '.#' excluded when scanning for preference\n\
      \032           files.\n\
      \032         + Rsync instructions are send directly instead of first\n\
      \032           marshaled.\n\
      \032         + Won't try forever to get the fingerprint of a continuously\n\
      \032           changing file: unison will give up after certain number of\n\
      \032           retries.\n\
      \032         + Other bug fixes, including the one reported by Peter Selinger\n\
      \032           (force=older preference not working).\n\
      \032    * Compilation:\n\
      \032         + Upgraded to the new OCaml 3.04 compiler, with the LablGtk\n\
      \032           1.2.3 library (patched version used for compiling under\n\
      \032           Windows).\n\
      \032         + Added the option to compile unison on the Windows platform\n\
      \032           with Cygwin GNU C compiler. This option only supports\n\
      \032           building dynamically linked unison executables.\n\
      \n\
      \032  Changes since 2.7.4:\n\
      \032    * Fixed a silly (but debilitating) bug in the client startup\n\
      \032      sequence.\n\
      \n\
      \032  Changes since 2.7.1:\n\
      \032    * Added addprefsto preference, which (when set) controls which\n\
      \032      preference file new preferences (e.g. new ignore patterns) are\n\
      \032      added to.\n\
      \032    * Bug fix: read the initial connection header one byte at a time, so\n\
      \032      that we don't block if the header is shorter than expected. (This\n\
      \032      bug did not affect normal operation -- it just made it hard to\n\
      \032      tell when you were trying to use Unison incorrectly with an old\n\
      \032      version of the server, since it would hang instead of giving an\n\
      \032      error message.)\n\
      \n\
      \032  Changes since 2.6.59:\n\
      \032    * Changed fastcheck from a boolean to a string preference. Its legal\n\
      \032      values are yes (for a fast check), no (for a safe check), or\n\
      \032      default (for a fast check--which also happens to be safe--when\n\
      \032      running on Unix and a safe check when on Windows). The default is\n\
      \032      default.\n\
      \032    * Several preferences have been renamed for consistency. All\n\
      \032      preference names are now spelled out in lowercase. For backward\n\
      \032      compatibility, the old names still work, but they are not\n\
      \032      mentioned in the manual any more.\n\
      \032    * The temp files created by the 'diff' and 'merge' commands are now\n\
      \032      named by prepending a new prefix to the file name, rather than\n\
      \032      appending a suffix. This should avoid confusing diff/merge\n\
      \032      programs that depend on the suffix to guess the type of the file\n\
      \032      contents.\n\
      \032    * We now set the keepalive option on the server socket, to make sure\n\
      \032      that the server times out if the communication link is\n\
      \032      unexpectedly broken.\n\
      \032    * Bug fixes:\n\
      \032         + When updating small files, Unison now closes the destination\n\
      \032           file.\n\
      \032         + File permissions are properly updated when the file is behind\n\
      \032           a followed link.\n\
      \032         + Several other small fixes.\n\
      \n\
      \032  Changes since 2.6.38:\n\
      \032    * Major Windows performance improvement!\n\
      \032      We've added a preference fastcheck that makes Unison look only at\n\
      \032      a file's creation time and last-modified time to check whether it\n\
      \032      has changed. This should result in a huge speedup when checking\n\
      \032      for updates in large replicas.\n\
      \032      When this switch is set, Unison will use file creation times as\n\
      \032      'pseudo inode numbers' when scanning Windows replicas for updates,\n\
      \032      instead of reading the full contents of every file. This may cause\n\
      \032      Unison to miss propagating an update if the create time,\n\
      \032      modification time, and length of the file are all unchanged by the\n\
      \032      update (this is not easy to achieve, but it can be done). However,\n\
      \032      Unison will never overwrite such an update with a change from the\n\
      \032      other replica, since it always does a safe check for updates just\n\
      \032      before propagating a change. Thus, it is reasonable to use this\n\
      \032      switch most of the time and occasionally run Unison once with\n\
      \032      fastcheck set to false, if you are worried that Unison may have\n\
      \032      overlooked an update.\n\
      \032      Warning: This change is has not yet been thoroughly field-tested.\n\
      \032      If you set the fastcheck preference, pay careful attention to what\n\
      \032      Unison is doing.\n\
      \032    * New functionality: centralized backups and merging\n\
      \032         + This version incorporates two pieces of major new\n\
      \032           functionality, implemented by Sylvain Roy during a summer\n\
      \032           internship at Penn: a centralized backup facility that keeps\n\
      \032           a full backup of (selected files in) each replica, and a\n\
      \032           merging feature that allows Unison to invoke an external\n\
      \032           file-merging tool to resolve conflicting changes to\n\
      \032           individual files.\n\
      \032         + Centralized backups:\n\
      \032              o Unison now maintains full backups of the\n\
      \032                last-synchronized versions of (some of) the files in\n\
      \032                each replica; these function both as backups in the\n\
      \032                usual sense and as the \"common version\" when invoking\n\
      \032                external merge programs.\n\
      \032              o The backed up files are stored in a directory\n\
      \032                /.unison/backup on each host. (The name of this\n\
      \032                directory can be changed by setting the environment\n\
      \032                variable UNISONBACKUPDIR.)\n\
      \032              o The predicate backup controls which files are actually\n\
      \032                backed up: giving the preference 'backup = Path *'\n\
      \032                causes backing up of all files.\n\
      \032              o Files are added to the backup directory whenever unison\n\
      \032                updates its archive. This means that\n\
      \032                   # When unison reconstructs its archive from scratch\n\
      \032                     (e.g., because of an upgrade, or because the\n\
      \032                     archive files have been manually deleted), all\n\
      \032                     files will be backed up.\n\
      \032                   # Otherwise, each file will be backed up the first\n\
      \032                     time unison propagates an update for it.\n\
      \032              o The preference backupversions controls how many previous\n\
      \032                versions of each file are kept. The default is 2 (i.e.,\n\
      \032                the last synchronized version plus one backup).\n\
      \032              o For backward compatibility, the backups preference is\n\
      \032                also still supported, but backup is now preferred.\n\
      \032              o It is OK to manually delete files from the backup\n\
      \032                directory (or to throw away the directory itself).\n\
      \032                Before unison uses any of these files for anything\n\
      \032                important, it checks that its fingerprint matches the\n\
      \032                one that it expects.\n\
      \032         + Merging:\n\
      \032              o Both user interfaces offer a new 'merge' command,\n\
      \032                invoked by pressing 'm' (with a changed file selected).\n\
      \032              o The actual merging is performed by an external program.\n\
      \032                The preferences merge and merge2 control how this\n\
      \032                program is invoked. If a backup exists for this file\n\
      \032                (see the backup preference), then the merge preference\n\
      \032                is used for this purpose; otherwise merge2 is used. In\n\
      \032                both cases, the value of the preference should be a\n\
      \032                string representing the command that should be passed to\n\
      \032                a shell to invoke the merge program. Within this string,\n\
      \032                the special substrings CURRENT1, CURRENT2, NEW, and OLD\n\
      \032                may appear at any point. Unison will substitute these as\n\
      \032                follows before invoking the command:\n\
      \032                   # CURRENT1 is replaced by the name of the local copy\n\
      \032                     of the file;\n\
      \032                   # CURRENT2 is replaced by the name of a temporary\n\
      \032                     file, into which the contents of the remote copy of\n\
      \032                     the file have been transferred by Unison prior to\n\
      \032                     performing the merge;\n\
      \032                   # NEW is replaced by the name of a temporary file\n\
      \032                     that Unison expects to be written by the merge\n\
      \032                     program when it finishes, giving the desired new\n\
      \032                     contents of the file; and\n\
      \032                   # OLD is replaced by the name of the backed up copy\n\
      \032                     of the original version of the file (i.e., its\n\
      \032                     state at the end of the last successful run of\n\
      \032                     Unison), if one exists (applies only to merge, not\n\
      \032                     merge2).\n\
      \032                For example, on Unix systems setting the merge\n\
      \032                preference to\n\
      \032  merge = diff3 -m CURRENT1 OLD CURRENT2 > NEW\n\
      \032                will tell Unison to use the external diff3 program for\n\
      \032                merging.\n\
      \032                A large number of external merging programs are\n\
      \032                available. For example, emacs users may find the\n\
      \032                following convenient:\n\
      \032   merge2 = emacs -q --eval '(ediff-merge-files \"CURRENT1\" \"CURRENT2\"\n\
      \032              nil \"NEW\")'\n\
      \032   merge = emacs -q --eval '(ediff-merge-files-with-ancestor\n\
      \032              \"CURRENT1\" \"CURRENT2\" \"OLD\" nil \"NEW\")'\n\
      \032                (These commands are displayed here on two lines to avoid\n\
      \032                running off the edge of the page. In your preference\n\
      \032                file, each should be written on a single line.)\n\
      \032              o If the external program exits without leaving any file\n\
      \032                at the path NEW, Unison considers the merge to have\n\
      \032                failed. If the merge program writes a file called NEW\n\
      \032                but exits with a non-zero status code, then Unison\n\
      \032                considers the merge to have succeeded but to have\n\
      \032                generated conflicts. In this case, it attempts to invoke\n\
      \032                an external editor so that the user can resolve the\n\
      \032                conflicts. The value of the editor preference controls\n\
      \032                what editor is invoked by Unison. The default is emacs.\n\
      \032              o Please send us suggestions for other useful values of\n\
      \032                the merge2 and merge preferences - we'd like to give\n\
      \032                several examples in the manual.\n\
      \032    * Smaller changes:\n\
      \032         + When one preference file includes another, unison no longer\n\
      \032           adds the suffix '.prf' to the included file by default. If a\n\
      \032           file with precisely the given name exists in the .unison\n\
      \032           directory, it will be used; otherwise Unison will add .prf,\n\
      \032           as it did before. (This change means that included preference\n\
      \032           files can be named blah.include instead of blah.prf, so that\n\
      \032           unison will not offer them in its 'choose a preference file'\n\
      \032           dialog.)\n\
      \032         + For Linux systems, we now offer both a statically linked and\n\
      \032           a dynamically linked executable. The static one is larger,\n\
      \032           but will probably run on more systems, since it doesn't\n\
      \032           depend on the same versions of dynamically linked library\n\
      \032           modules being available.\n\
      \032         + Fixed the force and prefer preferences, which were getting\n\
      \032           the propagation direction exactly backwards.\n\
      \032         + Fixed a bug in the startup code that would cause unison to\n\
      \032           crash when the default profile (~/.unison/default.prf) does\n\
      \032           not exist.\n\
      \032         + Fixed a bug where, on the run when a profile is first\n\
      \032           created, Unison would confusingly display the roots in\n\
      \032           reverse order in the user interface.\n\
      \032    * For developers:\n\
      \032         + We've added a module dependency diagram to the source\n\
      \032           distribution, in src/DEPENDENCIES.ps, to help new prospective\n\
      \032           developers with navigating the code.\n\
      \n\
      \032  Changes since 2.6.11:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed.\n\
      \032    * INCOMPATIBLE CHANGE: The startup sequence has been completely\n\
      \032      rewritten and greatly simplified. The main user-visible change is\n\
      \032      that the defaultpath preference has been removed. Its effect can\n\
      \032      be approximated by using multiple profiles, with include\n\
      \032      directives to incorporate common settings. All uses of defaultpath\n\
      \032      in existing profiles should be changed to path.\n\
      \032      Another change in startup behavior that will affect some users is\n\
      \032      that it is no longer possible to specify roots both in the profile\n\
      \032      and on the command line.\n\
      \032      You can achieve a similar effect, though, by breaking your profile\n\
      \032      into two:\n\
      \n\
      \032 default.prf =\n\
      \032     root = blah\n\
      \032     root = foo\n\
      \032     include common\n\
      \n\
      \032 common.prf =\n\
      \032     <everything else>\n\
      \032      Now do\n\
      \032 unison common root1 root2\n\
      \032      when you want to specify roots explicitly.\n\
      \032    * The -prefer and -force options have been extended to allow users\n\
      \032      to specify that files with more recent modtimes should be\n\
      \032      propagated, writing either -prefer newer or -force newer. (For\n\
      \032      symmetry, Unison will also accept -prefer older or -force older.)\n\
      \032      The -force older/newer options can only be used when -times is\n\
      \032      also set.\n\
      \032      The graphical user interface provides access to these facilities\n\
      \032      on a one-off basis via the Actions menu.\n\
      \032    * Names of roots can now be \"aliased\" to allow replicas to be\n\
      \032      relocated without changing the name of the archive file where\n\
      \032      Unison stores information between runs. (This feature is for\n\
      \032      experts only. See the \"Archive Files\" section of the manual for\n\
      \032      more information.)\n\
      \032    * Graphical user-interface:\n\
      \032         + A new command is provided in the Synchronization menu for\n\
      \032           switching to a new profile without restarting Unison from\n\
      \032           scratch.\n\
      \032         + The GUI also supports one-key shortcuts for commonly used\n\
      \032           profiles. If a profile contains a preference of the form 'key\n\
      \032           = n', where n is a single digit, then pressing this key will\n\
      \032           cause Unison to immediately switch to this profile and begin\n\
      \032           synchronization again from scratch. (Any actions that may\n\
      \032           have been selected for a set of changes currently being\n\
      \032           displayed will be discarded.)\n\
      \032         + Each profile may include a preference 'label = <string>'\n\
      \032           giving a descriptive string that described the options\n\
      \032           selected in this profile. The string is listed along with the\n\
      \032           profile name in the profile selection dialog, and displayed\n\
      \032           in the top-right corner of the main Unison window.\n\
      \032    * Minor:\n\
      \032         + Fixed a bug that would sometimes cause the 'diff' display to\n\
      \032           order the files backwards relative to the main user\n\
      \032           interface. (Thanks to Pascal Brisset for this fix.)\n\
      \032         + On Unix systems, the graphical version of Unison will check\n\
      \032           the DISPLAY variable and, if it is not set, automatically\n\
      \032           fall back to the textual user interface.\n\
      \032         + Synchronization paths (path preferences) are now matched\n\
      \032           against the ignore preferences. So if a path is both\n\
      \032           specified in a path preference and ignored, it will be\n\
      \032           skipped.\n\
      \032         + Numerous other bugfixes and small improvements.\n\
      \n\
      \032  Changes since 2.6.1:\n\
      \032    * The synchronization of modification times has been disabled for\n\
      \032      directories.\n\
      \032    * Preference files may now include lines of the form include <name>,\n\
      \032      which will cause name.prf to be read at that point.\n\
      \032    * The synchronization of permission between Windows and Unix now\n\
      \032      works properly.\n\
      \032    * A binding CYGWIN=binmode in now added to the environment so that\n\
      \032      the Cygwin port of OpenSSH works properly in a non-Cygwin context.\n\
      \032    * The servercmd and addversionno preferences can now be used\n\
      \032      together: -addversionno appends an appropriate -NNN to the server\n\
      \032      command, which is found by using the value of the -servercmd\n\
      \032      preference if there is one, or else just unison.\n\
      \032    * Both '-pref=val' and '-pref val' are now allowed for boolean\n\
      \032      values. (The former can be used to set a preference to false.)\n\
      \032    * Lot of small bugs fixed.\n\
      \n\
      \032  Changes since 2.5.31:\n\
      \032    * The log preference is now set to true by default, since the log\n\
      \032      file seems useful for most users.\n\
      \032    * Several miscellaneous bugfixes (most involving symlinks).\n\
      \n\
      \032  Changes since 2.5.25:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed (again).\n\
      \032    * Several significant bugs introduced in 2.5.25 have been fixed.\n\
      \n\
      \032  Changes since 2.5.1:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed. Make sure you\n\
      \032      synchronize your replicas before upgrading, to avoid spurious\n\
      \032      conflicts. The first sync after upgrading will be slow.\n\
      \032    * New functionality:\n\
      \032         + Unison now synchronizes file modtimes, user-ids, and\n\
      \032           group-ids.\n\
      \032           These new features are controlled by a set of new\n\
      \032           preferences, all of which are currently false by default.\n\
      \032              o When the times preference is set to true, file\n\
      \032                modification times are propaged. (Because the\n\
      \032                representations of time may not have the same\n\
      \032                granularity on both replicas, Unison may not always be\n\
      \032                able to make the modtimes precisely equal, but it will\n\
      \032                get them as close as the operating systems involved\n\
      \032                allow.)\n\
      \032              o When the owner preference is set to true, file ownership\n\
      \032                information is synchronized.\n\
      \032              o When the group preference is set to true, group\n\
      \032                information is synchronized.\n\
      \032              o When the numericIds preference is set to true, owner and\n\
      \032                group information is synchronized numerically. By\n\
      \032                default, owner and group numbers are converted to names\n\
      \032                on each replica and these names are synchronized. (The\n\
      \032                special user id 0 and the special group 0 are never\n\
      \032                mapped via user/group names even if this preference is\n\
      \032                not set.)\n\
      \032         + Added an integer-valued preference perms that can be used to\n\
      \032           control the propagation of permission bits. The value of this\n\
      \032           preference is a mask indicating which permission bits should\n\
      \032           be synchronized. It is set by default to 0o1777: all bits but\n\
      \032           the set-uid and set-gid bits are synchronised (synchronizing\n\
      \032           theses latter bits can be a security hazard). If you want to\n\
      \032           synchronize all bits, you can set the value of this\n\
      \032           preference to -1.\n\
      \032         + Added a log preference (default false), which makes Unison\n\
      \032           keep a complete record of the changes it makes to the\n\
      \032           replicas. By default, this record is written to a file called\n\
      \032           unison.log in the user's home directory (the value of the\n\
      \032           HOME environment variable). If you want it someplace else,\n\
      \032           set the logfile preference to the full pathname you want\n\
      \032           Unison to use.\n\
      \032         + Added an ignorenot preference that maintains a set of\n\
      \032           patterns for paths that should definitely not be ignored,\n\
      \032           whether or not they match an ignore pattern. (That is, a path\n\
      \032           will now be ignored iff it matches an ignore pattern and does\n\
      \032           not match any ignorenot patterns.)\n\
      \032    * User-interface improvements:\n\
      \032         + Roots are now displayed in the user interface in the same\n\
      \032           order as they were given on the command line or in the\n\
      \032           preferences file.\n\
      \032         + When the batch preference is set, the graphical user\n\
      \032           interface no longer waits for user confirmation when it\n\
      \032           displays a warning message: it simply pops up an advisory\n\
      \032           window with a Dismiss button at the bottom and keeps on\n\
      \032           going.\n\
      \032         + Added a new preference for controlling how many status\n\
      \032           messages are printed during update detection: statusdepth\n\
      \032           controls the maximum depth for paths on the local machine\n\
      \032           (longer paths are not displayed, nor are non-directory\n\
      \032           paths). The value should be an integer; default is 1.\n\
      \032         + Removed the trace and silent preferences. They did not seem\n\
      \032           very useful, and there were too many preferences for\n\
      \032           controlling output in various ways.\n\
      \032         + The text UI now displays just the default command (the one\n\
      \032           that will be used if the user just types <return>) instead of\n\
      \032           all available commands. Typing ? will print the full list of\n\
      \032           possibilities.\n\
      \032         + The function that finds the canonical hostname of the local\n\
      \032           host (which is used, for example, in calculating the name of\n\
      \032           the archive file used to remember which files have been\n\
      \032           synchronized) normally uses the gethostname operating system\n\
      \032           call. However, if the environment variable\n\
      \032           UNISONLOCALHOSTNAME is set, its value will now be used\n\
      \032           instead. This makes it easier to use Unison in situations\n\
      \032           where a machine's name changes frequently (e.g., because it\n\
      \032           is a laptop and gets moved around a lot).\n\
      \032         + File owner and group are now displayed in the \"detail window\"\n\
      \032           at the bottom of the screen, when unison is configured to\n\
      \032           synchronize them.\n\
      \032    * For hackers:\n\
      \032         + Updated to Jacques Garrigue's new version of lablgtk, which\n\
      \032           means we can throw away our local patched version.\n\
      \032           If you're compiling the GTK version of unison from sources,\n\
      \032           you'll need to update your copy of lablgtk to the developers\n\
      \032           release. (Warning: installing lablgtk under Windows is\n\
      \032           currently a bit challenging.)\n\
      \032         + The TODO.txt file (in the source distribution) has been\n\
      \032           cleaned up and reorganized. The list of pending tasks should\n\
      \032           be much easier to make sense of, for people that may want to\n\
      \032           contribute their programming energies. There is also a\n\
      \032           separate file BUGS.txt for open bugs.\n\
      \032         + The Tk user interface has been removed (it was not being\n\
      \032           maintained and no longer compiles).\n\
      \032         + The debug preference now prints quite a bit of additional\n\
      \032           information that should be useful for identifying sources of\n\
      \032           problems.\n\
      \032         + The version number of the remote server is now checked right\n\
      \032           away during the connection setup handshake, rather than\n\
      \032           later. (Somebody sent a bug report of a server crash that\n\
      \032           turned out to come from using inconsistent versions: better\n\
      \032           to check this earlier and in a way that can't crash either\n\
      \032           client or server.)\n\
      \032         + Unison now runs correctly on 64-bit architectures (e.g. Alpha\n\
      \032           linux). We will not be distributing binaries for these\n\
      \032           architectures ourselves (at least for a while) but if someone\n\
      \032           would like to make them available, we'll be glad to provide a\n\
      \032           link to them.\n\
      \032    * Bug fixes:\n\
      \032         + Pattern matching (e.g. for ignore) is now case-insensitive\n\
      \032           when Unison is in case-insensitive mode (i.e., when one of\n\
      \032           the replicas is on a windows machine).\n\
      \032         + Some people had trouble with mysterious failures during\n\
      \032           propagation of updates, where files would be falsely reported\n\
      \032           as having changed during synchronization. This should be\n\
      \032           fixed.\n\
      \032         + Numerous smaller fixes.\n\
      \n\
      \032  Changes since 2.4.1:\n\
      \032    * Added a number of 'sorting modes' for the user interface. By\n\
      \032      default, conflicting changes are displayed at the top, and the\n\
      \032      rest of the entries are sorted in alphabetical order. This\n\
      \032      behavior can be changed in the following ways:\n\
      \032         + Setting the sortnewfirst preference to true causes newly\n\
      \032           created files to be displayed before changed files.\n\
      \032         + Setting sortbysize causes files to be displayed in increasing\n\
      \032           order of size.\n\
      \032         + Giving the preference sortfirst=<pattern> (where <pattern> is\n\
      \032           a path descriptor in the same format as 'ignore' and 'follow'\n\
      \032           patterns, causes paths matching this pattern to be displayed\n\
      \032           first.\n\
      \032         + Similarly, giving the preference sortlast=<pattern> causes\n\
      \032           paths matching this pattern to be displayed last.\n\
      \032      The sorting preferences are described in more detail in the user\n\
      \032      manual. The sortnewfirst and sortbysize flags can also be accessed\n\
      \032      from the 'Sort' menu in the grpahical user interface.\n\
      \032    * Added two new preferences that can be used to change unison's\n\
      \032      fundamental behavior to make it more like a mirroring tool instead\n\
      \032      of a synchronizer.\n\
      \032         + Giving the preference prefer with argument <root> (by adding\n\
      \032           -prefer <root> to the command line or prefer=<root>) to your\n\
      \032           profile) means that, if there is a conflict, the contents of\n\
      \032           <root> should be propagated to the other replica (with no\n\
      \032           questions asked). Non-conflicting changes are treated as\n\
      \032           usual.\n\
      \032         + Giving the preference force with argument <root> will make\n\
      \032           unison resolve all differences in favor of the given root,\n\
      \032           even if it was the other replica that was changed.\n\
      \032      These options should be used with care! (More information is\n\
      \032      available in the manual.)\n\
      \032    * Small changes:\n\
      \032         + Changed default answer to 'Yes' in all two-button dialogs in\n\
      \032           the graphical interface (this seems more intuitive).\n\
      \032         + The rsync preference has been removed (it was used to\n\
      \032           activate rsync compression for file transfers, but rsync\n\
      \032           compression is now enabled by default).\n\
      \032         + In the text user interface, the arrows indicating which\n\
      \032           direction changes are being propagated are printed\n\
      \032           differently when the user has overridded Unison's default\n\
      \032           recommendation (====> instead of ---->). This matches the\n\
      \032           behavior of the graphical interface, which displays such\n\
      \032           arrows in a different color.\n\
      \032         + Carriage returns (Control-M's) are ignored at the ends of\n\
      \032           lines in profiles, for Windows compatibility.\n\
      \032         + All preferences are now fully documented in the user manual.\n\
      \n\
      \032  Changes since 2.3.12:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed. Make sure you\n\
      \032      synchronize your replicas before upgrading, to avoid spurious\n\
      \032      conflicts. The first sync after upgrading will be slow.\n\
      \032    * New/improved functionality:\n\
      \032         + A new preference -sortbysize controls the order in which\n\
      \032           changes are displayed to the user: when it is set to true,\n\
      \032           the smallest changed files are displayed first. (The default\n\
      \032           setting is false.)\n\
      \032         + A new preference -sortnewfirst causes newly created files to\n\
      \032           be listed before other updates in the user interface.\n\
      \032         + We now allow the ssh protocol to specify a port.\n\
      \032         + Incompatible change: The unison: protocol is deprecated, and\n\
      \032           we added file: and socket:. You may have to modify your\n\
      \032           profiles in the .unison directory. If a replica is specified\n\
      \032           without an explicit protocol, we now assume it refers to a\n\
      \032           file. (Previously \"//saul/foo\" meant to use SSH to connect to\n\
      \032           saul, then access the foo directory. Now it means to access\n\
      \032           saul via a remote file mechanism such as samba; the old\n\
      \032           effect is now achieved by writing ssh://saul/foo.)\n\
      \032         + Changed the startup sequence for the case where roots are\n\
      \032           given but no profile is given on the command line. The new\n\
      \032           behavior is to use the default profile (creating it if it\n\
      \032           does not exist), and temporarily override its roots. The\n\
      \032           manual claimed that this case would work by reading no\n\
      \032           profile at all, but AFAIK this was never true.\n\
      \032         + In all user interfaces, files with conflicts are always\n\
      \032           listed first\n\
      \032         + A new preference 'sshversion' can be used to control which\n\
      \032           version of ssh should be used to connect to the server. Legal\n\
      \032           values are 1 and 2. (Default is empty, which will make unison\n\
      \032           use whatever version of ssh is installed as the default 'ssh'\n\
      \032           command.)\n\
      \032         + The situation when the permissions of a file was updated the\n\
      \032           same on both side is now handled correctly (we used to report\n\
      \032           a spurious conflict)\n\
      \032    * Improvements for the Windows version:\n\
      \032         + The fact that filenames are treated case-insensitively under\n\
      \032           Windows should now be handled correctly. The exact behavior\n\
      \032           is described in the cross-platform section of the manual.\n\
      \032         + It should be possible to synchronize with Windows shares,\n\
      \032           e.g., //host/drive/path.\n\
      \032         + Workarounds to the bug in syncing root directories in\n\
      \032           Windows. The most difficult thing to fix is an ocaml bug:\n\
      \032           Unix.opendir fails on c: in some versions of Windows.\n\
      \032    * Improvements to the GTK user interface (the Tk interface is no\n\
      \032      longer being maintained):\n\
      \032         + The UI now displays actions differently (in blue) when they\n\
      \032           have been explicitly changed by the user from Unison's\n\
      \032           default recommendation.\n\
      \032         + More colorful appearance.\n\
      \032         + The initial profile selection window works better.\n\
      \032         + If any transfers failed, a message to this effect is\n\
      \032           displayed along with 'Synchronization complete' at the end of\n\
      \032           the transfer phase (in case they may have scrolled off the\n\
      \032           top).\n\
      \032         + Added a global progress meter, displaying the percentage of\n\
      \032           total bytes that have been transferred so far.\n\
      \032    * Improvements to the text user interface:\n\
      \032         + The file details will be displayed automatically when a\n\
      \032           conflict is been detected.\n\
      \032         + when a warning is generated (e.g. for a temporary file left\n\
      \032           over from a previous run of unison) Unison will no longer\n\
      \032           wait for a response if it is running in -batch mode.\n\
      \032         + The UI now displays a short list of possible inputs each time\n\
      \032           it waits for user interaction.\n\
      \032         + The UI now quits immediately (rather than looping back and\n\
      \032           starting the interaction again) if the user presses 'q' when\n\
      \032           asked whether to propagate changes.\n\
      \032         + Pressing 'g' in the text user interface will proceed\n\
      \032           immediately with propagating updates, without asking any more\n\
      \032           questions.\n\
      \032    * Documentation and installation changes:\n\
      \032         + The manual now includes a FAQ, plus sections on common\n\
      \032           problems and on tricks contributed by users.\n\
      \032         + Both the download page and the download directory explicitly\n\
      \032           say what are the current stable and beta-test version\n\
      \032           numbers.\n\
      \032         + The OCaml sources for the up-to-the-minute developers'\n\
      \032           version (not guaranteed to be stable, or even to compile, at\n\
      \032           any given time!) are now available from the download page.\n\
      \032         + Added a subsection to the manual describing cross-platform\n\
      \032           issues (case conflicts, illegal filenames)\n\
      \032    * Many small bug fixes and random improvements.\n\
      \n\
      \032  Changes since 2.3.1:\n\
      \032    * Several bug fixes. The most important is a bug in the rsync module\n\
      \032      that would occasionally cause change propagation to fail with a\n\
      \032      'rename' error.\n\
      \n\
      \032  Changes since 2.2:\n\
      \032    * The multi-threaded transport system is now disabled by default.\n\
      \032      (It is not stable enough yet.)\n\
      \032    * Various bug fixes.\n\
      \032    * A new experimental feature:\n\
      \032      The final component of a -path argument may now be the wildcard\n\
      \032      specifier *. When Unison sees such a path, it expands this path on\n\
      \032      the client into into the corresponding list of paths by listing\n\
      \032      the contents of that directory.\n\
      \032      Note that if you use wildcard paths from the command line, you\n\
      \032      will probably need to use quotes or a backslash to prevent the *\n\
      \032      from being interpreted by your shell.\n\
      \032      If both roots are local, the contents of the first one will be\n\
      \032      used for expanding wildcard paths. (Nb: this is the first one\n\
      \032      after the canonization step - i.e., the one that is listed first\n\
      \032      in the user interface - not the one listed first on the command\n\
      \032      line or in the preferences file.)\n\
      \n\
      \032  Changes since 2.1:\n\
      \032    * The transport subsystem now includes an implementation by Sylvain\n\
      \032      Gommier and Norman Ramsey of Tridgell and Mackerras's rsync\n\
      \032      protocol. This protocol achieves much faster transfers when only a\n\
      \032      small part of a large file has been changed by sending just diffs.\n\
      \032      This feature is mainly helpful for transfers over slow links--on\n\
      \032      fast local area networks it can actually degrade performance--so\n\
      \032      we have left it off by default. Start unison with the -rsync\n\
      \032      option (or put rsync=true in your preferences file) to turn it on.\n\
      \032    * \"Progress bars\" are now diplayed during remote file transfers,\n\
      \032      showing what percentage of each file has been transferred so far.\n\
      \032    * The version numbering scheme has changed. New releases will now be\n\
      \032      have numbers like 2.2.30, where the second component is\n\
      \032      incremented on every significant public release and the third\n\
      \032      component is the \"patch level.\"\n\
      \032    * Miscellaneous improvements to the GTK-based user interface.\n\
      \032    * The manual is now available in PDF format.\n\
      \032    * We are experimenting with using a multi-threaded transport\n\
      \032      subsystem to transfer several files at the same time, making much\n\
      \032      more effective use of available network bandwidth. This feature is\n\
      \032      not completely stable yet, so by default it is disabled in the\n\
      \032      release version of Unison.\n\
      \032      If you want to play with the multi-threaded version, you'll need\n\
      \032      to recompile Unison from sources (as described in the\n\
      \032      documentation), setting the THREADS flag in Makefile.OCaml to\n\
      \032      true. Make sure that your OCaml compiler has been installed with\n\
      \032      the -with-pthreads configuration option. (You can verify this by\n\
      \032      checking whether the file threads/threads.cma in the OCaml\n\
      \032      standard library directory contains the string -lpthread near the\n\
      \032      end.)\n\
      \n\
      \032  Changes since 1.292:\n\
      \032    * Reduced memory footprint (this is especially important during the\n\
      \032      first run of unison, where it has to gather information about all\n\
      \032      the files in both repositories).\n\
      \032    * Fixed a bug that would cause the socket server under NT to fail\n\
      \032      after the client exits.\n\
      \032    * Added a SHIFT modifier to the Ignore menu shortcut keys in GTK\n\
      \032      interface (to avoid hitting them accidentally).\n\
      \n\
      \032  Changes since 1.231:\n\
      \032    * Tunneling over ssh is now supported in the Windows version. See\n\
      \032      the installation section of the manual for detailed instructions.\n\
      \032    * The transport subsystem now includes an implementation of the\n\
      \032      rsync protocol, built by Sylvain Gommier and Norman Ramsey. This\n\
      \032      protocol achieves much faster transfers when only a small part of\n\
      \032      a large file has been changed by sending just diffs. The rsync\n\
      \032      feature is off by default in the current version. Use the -rsync\n\
      \032      switch to turn it on. (Nb. We still have a lot of tuning to do:\n\
      \032      you may not notice much speedup yet.)\n\
      \032    * We're experimenting with a multi-threaded transport subsystem,\n\
      \032      written by Jerome Vouillon. The downloadable binaries are still\n\
      \032      single-threaded: if you want to try the multi-threaded version,\n\
      \032      you'll need to recompile from sources. (Say make THREADS=true.)\n\
      \032      Native thread support from the compiler is required. Use the\n\
      \032      option -threads N to select the maximal number of concurrent\n\
      \032      threads (default is 5). Multi-threaded and single-threaded\n\
      \032      clients/servers can interoperate.\n\
      \032    * A new GTK-based user interface is now available, thanks to Jacques\n\
      \032      Garrigue. The Tk user interface still works, but we'll be shifting\n\
      \032      development effort to the GTK interface from now on.\n\
      \032    * OCaml 3.00 is now required for compiling Unison from sources. The\n\
      \032      modules uitk and myfileselect have been changed to use labltk\n\
      \032      instead of camltk. To compile the Tk interface in Windows, you\n\
      \032      must have ocaml-3.00 and tk8.3. When installing tk8.3, put it in\n\
      \032      c:\\Tcl rather than the suggested c:\\Program Files\\Tcl, and be sure\n\
      \032      to install the headers and libraries (which are not installed by\n\
      \032      default).\n\
      \032    * Added a new -addversionno switch, which causes unison to use\n\
      \032      unison-<currentversionnumber> instead of just unison as the remote\n\
      \032      server command. This allows multiple versions of unison to coexist\n\
      \032      conveniently on the same server: whichever version is run on the\n\
      \032      client, the same version will be selected on the server.\n\
      \n\
      \032  Changes since 1.219:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed. Make sure you\n\
      \032      synchronize your replicas before upgrading, to avoid spurious\n\
      \032      conflicts. The first sync after upgrading will be slow.\n\
      \032    * This version fixes several annoying bugs, including:\n\
      \032         + Some cases where propagation of file permissions was not\n\
      \032           working.\n\
      \032         + umask is now ignored when creating directories\n\
      \032         + directories are create writable, so that a read-only\n\
      \032           directory and its contents can be propagated.\n\
      \032         + Handling of warnings generated by the server.\n\
      \032         + Synchronizing a path whose parent is not a directory on both\n\
      \032           sides is now flagged as erroneous.\n\
      \032         + Fixed some bugs related to symnbolic links and nonexistant\n\
      \032           roots.\n\
      \032              o When a change (deletion or new contents) is propagated\n\
      \032                onto a 'follow'ed symlink, the file pointed to by the\n\
      \032                link is now changed. (We used to change the link itself,\n\
      \032                which doesn't fit our assertion that 'follow' means the\n\
      \032                link is completely invisible)\n\
      \032              o When one root did not exist, propagating the other root\n\
      \032                on top of it used to fail, becuase unison could not\n\
      \032                calculate the working directory into which to write\n\
      \032                changes. This should be fixed.\n\
      \032    * A human-readable timestamp has been added to Unison's archive\n\
      \032      files.\n\
      \032    * The semantics of Path and Name regular expressions now correspond\n\
      \032      better.\n\
      \032    * Some minor improvements to the text UI (e.g. a command for going\n\
      \032      back to previous items)\n\
      \032    * The organization of the export directory has changed -- should be\n\
      \032      easier to find / download things now.\n\
      \n\
      \032  Changes since 1.200:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed. Make sure you\n\
      \032      synchronize your replicas before upgrading, to avoid spurious\n\
      \032      conflicts. The first sync after upgrading will be slow.\n\
      \032    * This version has not been tested extensively on Windows.\n\
      \032    * Major internal changes designed to make unison safer to run at the\n\
      \032      same time as the replicas are being changed by the user.\n\
      \032    * Internal performance improvements.\n\
      \n\
      \032  Changes since 1.190:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed. Make sure you\n\
      \032      synchronize your replicas before upgrading, to avoid spurious\n\
      \032      conflicts. The first sync after upgrading will be slow.\n\
      \032    * A number of internal functions have been changed to reduce the\n\
      \032      amount of memory allocation, especially during the first\n\
      \032      synchronization. This should help power users with very big\n\
      \032      replicas.\n\
      \032    * Reimplementation of low-level remote procedure call stuff, in\n\
      \032      preparation for adding rsync-like smart file transfer in a later\n\
      \032      release.\n\
      \032    * Miscellaneous bug fixes.\n\
      \n\
      \032  Changes since 1.180:\n\
      \032    * INCOMPATIBLE CHANGE: Archive format has changed. Make sure you\n\
      \032      synchronize your replicas before upgrading, to avoid spurious\n\
      \032      conflicts. The first sync after upgrading will be slow.\n\
      \032    * Fixed some small bugs in the interpretation of ignore patterns.\n\
      \032    * Fixed some problems that were preventing the Windows version from\n\
      \032      working correctly when click-started.\n\
      \032    * Fixes to treatment of file permissions under Windows, which were\n\
      \032      causing spurious reports of different permissions when\n\
      \032      synchronizing between windows and unix systems.\n\
      \032    * Fixed one more non-tail-recursive list processing function, which\n\
      \032      was causing stack overflows when synchronizing very large\n\
      \032      replicas.\n\
      \n\
      \032  Changes since 1.169:\n\
      \032    * The text user interface now provides commands for ignoring files.\n\
      \032    * We found and fixed some more non-tail-recursive list processing\n\
      \032      functions. Some power users have reported success with very large\n\
      \032      replicas.\n\
      \032    * INCOMPATIBLE CHANGE: Files ending in .tmp are no longer ignored\n\
      \032      automatically. If you want to ignore such files, put an\n\
      \032      appropriate ignore pattern in your profile.\n\
      \032    * INCOMPATIBLE CHANGE: The syntax of ignore and follow patterns has\n\
      \032      changed. Instead of putting a line of the form\n\
      \032                ignore = <regexp>\n\
      \032      in your profile (.unison/default.prf), you should put:\n\
      \032                ignore = Regexp <regexp>\n\
      \032      Moreover, two other styles of pattern are also recognized:\n\
      \032                ignore = Name <name>\n\
      \032      matches any path in which one component matches <name>, while\n\
      \032                ignore = Path <path>\n\
      \032      matches exactly the path <path>.\n\
      \032      Standard \"globbing\" conventions can be used in <name> and <path>:\n\
      \032         + a ? matches any single character except /\n\
      \032         + a * matches any sequence of characters not including /\n\
      \032         + [xyz] matches any character from the set {x, y, z }\n\
      \032         + {a,bb,ccc} matches any one of a, bb, or ccc.\n\
      \032      See the user manual for some examples.\n\
      \n\
      \032  Changes since 1.146:\n\
      \032    * Some users were reporting stack overflows when synchronizing huge\n\
      \032      directories. We found and fixed some non-tail-recursive list\n\
      \032      processing functions, which we hope will solve the problem. Please\n\
      \032      give it a try and let us know.\n\
      \032    * Major additions to the documentation.\n\
      \n\
      \032  Changes since 1.142:\n\
      \032    * Major internal tidying and many small bugfixes.\n\
      \032    * Major additions to the user manual.\n\
      \032    * Unison can now be started with no arguments - it will prompt\n\
      \032      automatically for the name of a profile file containing the roots\n\
      \032      to be synchronized. This makes it possible to start the graphical\n\
      \032      UI from a desktop icon.\n\
      \032    * Fixed a small bug where the text UI on NT was raising a 'no such\n\
      \032      signal' exception.\n\
      \n\
      \032  Changes since 1.139:\n\
      \032    * The precompiled windows binary in the last release was compiled\n\
      \032      with an old OCaml compiler, causing propagation of permissions not\n\
      \032      to work (and perhaps leading to some other strange behaviors we've\n\
      \032      heard reports about). This has been corrected. If you're using\n\
      \032      precompiled binaries on Windows, please upgrade.\n\
      \032    * Added a -debug command line flag, which controls debugging of\n\
      \032      various modules. Say -debug XXX to enable debug tracing for module\n\
      \032      XXX, or -debug all to turn on absolutely everything.\n\
      \032    * Fixed a small bug where the text UI on NT was raising a 'no such\n\
      \032      signal' exception.\n\
      \n\
      \032  Changes since 1.111:\n\
      \032    * INCOMPATIBLE CHANGE: The names and formats of the preference files\n\
      \032      in the .unison directory have changed. In particular:\n\
      \032         + the file \"prefs\" should be renamed to default.prf\n\
      \032         + the contents of the file \"ignore\" should be merged into\n\
      \032           default.prf. Each line of the form REGEXP in ignore should\n\
      \032           become a line of the form ignore = REGEXP in default.prf.\n\
      \032    * Unison now handles permission bits and symbolic links. See the\n\
      \032      manual for details.\n\
      \032    * You can now have different preference files in your .unison\n\
      \032      directory. If you start unison like this\n\
      \032            unison profilename\n\
      \032      (i.e. with just one \"anonymous\" command-line argument), then the\n\
      \032      file ~/.unison/profilename.prf will be loaded instead of\n\
      \032      default.prf.\n\
      \032    * Some improvements to terminal handling in the text user interface\n\
      \032    * Added a switch -killServer that terminates the remote server\n\
      \032      process when the unison client is shutting down, even when using\n\
      \032      sockets for communication. (By default, a remote server created\n\
      \032      using ssh/rsh is terminated automatically, while a socket server\n\
      \032      is left running.)\n\
      \032    * When started in 'socket server' mode, unison prints 'server\n\
      \032      started' on stderr when it is ready to accept connections. (This\n\
      \032      may be useful for scripts that want to tell when a socket-mode\n\
      \032      server has finished initalization.)\n\
      \032    * We now make a nightly mirror of our current internal development\n\
      \032      tree, in case anyone wants an up-to-the-minute version to hack\n\
      \032      around with.\n\
      \032    * Added a file CONTRIB with some suggestions for how to help us make\n\
      \032      Unison better.\n\
      \n\
      "))
::
    ("", ("Junk", 
     "Junk\n\
      \032    _________________________________________________________________\n\
      \n\
      \032    This document was translated from L^AT[E]X by [2]H^EV^EA.\n\
      \n\
      References\n\
      \n\
      \032  1. file://localhost/Users/bcpierce/current/unison/branches/2.32/doc/temp.html#ssh-win\n\
      \032  2. http://pauillac.inria.fr/~maranget/hevea/index.html\n\
      "))
::
    [];;

