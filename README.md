# The Unison File Synchronizer

[![Build Status](https://travis-ci.org/bcpierce00/unison.svg?branch=master)](https://travis-ci.org/bcpierce00/unison)
[![CICD](https://github.com/bcpierce00/unison/workflows/CICD/badge.svg)](https://github.com/bcpierce00/unison/actions?query=workflow%3ACICD)

`unison` is a file-synchronization tool for OSX, Unix, and Windows. It allows two
replicas of a collection of files and directories to be stored on different
hosts (or different disks on the same host), modified separately, and then
brought up to date by propagating the changes in each replica to the other.

Unison shares a number of features with tools such as configuration management packages (CVS, PRCS, Subversion, BitKeeper, etc.), distributed filesystems (Coda, etc.), uni-directional mirroring utilities (rsync, etc.), and other synchronizers (Intellisync, Reconcile, etc). However, there are several points where it differs:

Unison runs on both Windows and many flavors of Unix (Solaris, Linux, OS X, etc.) systems. Moreover, Unison works across platforms, allowing you to synchronize a Windows laptop with a Unix server, for example.

Unlike simple mirroring or backup utilities, Unison can deal with updates to both replicas of a distributed directory structure. Updates that do not conflict are propagated automatically. Conflicting updates are detected and displayed.

Unlike a distributed filesystem, Unison is a user-level program: there is no need to modify the kernel or to have superuser privileges on either host.

Unison works between any pair of machines connected to the internet, communicating over either a direct socket link or tunneling over an encrypted ssh connection. It is careful with network bandwidth, and runs well over slow links such as PPP connections. Transfers of small updates to large files are optimized using a compression protocol similar to rsync.

Unison is resilient to failure. It is careful to leave the replicas and its own private structures in a sensible state at all times, even in case of abnormal termination or communication failures.

Unison has a clear and precise specification.

Unison is free; full source code is available under the GNU Public License.

If you just want to use `unison`, you can probably find a pre-built binary for
your architecture either on your favorite package manager or in the
[RELEASES section](https://github.com/bcpierce00/unison/releases) of the [GitHub
`unison` repository](https://github.com/bcpierce00/unison).

Full documentation can be found on the [Unison home page](http://www.cis.upenn.edu/~bcpierce/unison).

Installation instructions are in the file *INSTALL.md* and the [INSTALLATION section](https://www.cis.upenn.edu/~bcpierce/unison/download/releases/stable/unison-manual.html#install) of the [user manual](https://www.cis.upenn.edu/~bcpierce/unison/download/releases/stable/unison-manual.html).

License and copying information can be found in the file *COPYING*.

If you want to play with the internals, have a look at the file *ROADMAP.txt* for some basic orientation.

Here's how to choose which version to use:

- The most recent public release (2.51, as of Summer 2019) is the default
  recommendation and should work well for most circumstances.  Data-loss
  bugs, if any should come up (they have been rare, historically) will be
  fixed in this version, and it will be updated as necessary so that it
  always compiles with the latest OCaml release.
- Previous public releases are still stable and useable, but they will not
  be updated to new OCaml versions or have bugs fixed.
- The "master" branch of the github repo is generally extremely stable and
  safe to use, for those that don't mind compiling from source.

## Contacts

- Bug reports should be sent to unison-help@cis.upenn.edu
- General questions and discussion should be sent to
    unison-users@groups.yahoo.com
- You can subscribe to this list using Yahoo's web interface
    http://groups.yahoo.com/group/unison-users

## Credits

- OS X Unison Icon taken from [Mac4Lin](http://sourceforge.net/projects/mac4lin) (LGPL)
- Some icons in the OSX GUI are directly taken from Matt Ball's [developer icons](http://www.mattballdesign.com/blog/2009/11/23/developer-icons-are-back-online) (Creative Commons Attribution 3.0)
- Others are based on Matt Ball's [developer icons](http://www.mattballdesign.com/blog/2009/11/23/developer-icons-are-back-online) (Creative Commons Attribution 3.0)
- OSX GUI elements from [BWToolkit](http://www.brandonwalkin.com/bwtoolkit) (three-clause BSD license)
