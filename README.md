![Unison](icons/unison.png)

# Unison File Synchronizer

## Meta

***Please read this entire README and
https://github.com/bcpierce00/unison/wiki/Reporting-Bugs-and-Feature-Requests
before creating or commenting on a github issue.***

***TL;DR: Do not ask questions or ask for help in issues.  Upgrade to the latest release.***

For compatibility information with version 2.52, see
https://github.com/bcpierce00/unison/wiki/2.52-Migration-Guide

## About

Unison is a file-synchronization tool for POSIX-compliant systems
(e.g. *BSD and GNU/Linux), macOS and Windows, with the caveat that the
platform must be supported by OCaml.  It allows two replicas of a
collection of files and directories to be stored on different hosts
(or different disks on the same host), modified separately, and then
brought up to date by propagating the changes in each replica to the
other.

Unison has been in use for over 20 years and many people use it to
synchronize data they care about.

Unison shares a number of features with tools such as configuration
management packages (CVS, Subversion, git, Mercurial, etc.),
distributed filesystems (Coda, etc.), uni-directional mirroring
utilities (rsync, etc.), and other synchronizers.  However, there are
several points where it differs:

 * Unison runs on almost any system with an OCaml compiler. Moreover,
   Unison works across platforms, allowing you to synchronize a
   Windows laptop with a Unix server, for example.

 * Unlike simple mirroring or backup utilities, Unison can deal with
   updates to both replicas of a distributed directory
   structure. Updates that do not conflict are propagated
   automatically. Conflicting updates are detected and displayed.

 * Unlike many network filesystems, Unison copies data so that
   already-synchronized data can be read and written while offline.

 * Unlike most distributed filesystems, Unison is a user-level program
   that simply uses normal systems calls: there is no need to modify
   the kernel, to have superuser privileges on either host, or to have
   a FUSE implementation.

 * Unison works between any pair of machines connected to the
   internet, typically communicating over ssh, but also directly over
   TCP.  It is careful with network bandwidth, and runs well over slow
   links such as PPP connections. Transfers of small updates to large
   files are optimized using a compression protocol similar to rsync.

 * Unison is resilient to failure. It is careful to leave the replicas
   and its own private structures in a sensible state at all times,
   even in case of abnormal termination or communication failures.

 * Unison has a clear and precise specification.

 * Unison is Free; full source code is available under the GNU Public
   License, Version 3.

Note that only a very small number of people are actively working on
maintaining unison.  An estimate is 2.5 people and 0.1 Full-Time
Equivalents.  This has a substantial impact on the handling of bug
reports and enhancement reports; see the wiki page linked at the top.
Help in terms of high-quality bug reports, fixes, and proposed changes
is very welcome.  Help in answering mailinglist questions is also
welcome.  Please do not answer questions asked in the bug tracker,
which is contrary to bug tracker usage guidance.

Unison activity is now centered on the two [Unison
mailinglists](https://github.com/bcpierce00/unison/wiki/Mailing-Lists)
for discussion and [Unison's github
page](https://github.com/bcpierce00/unison/) for code, issues and a
wiki.
A no-longer-maintained FAQ can be found at: the [old UPenn
site](http://www.cis.upenn.edu/~bcpierce/unison).

## Getting Unison

The Unison project provides Unison as source code.  Many packaging
systems (including GNU/Linux distributions) provide binary packages of
Unison.  Results from Continuous Integration builds, while performed
for the purposes of testing, are available for use on a limited set of
platforms.

See the [building instructions](INSTALL.md), or read the CI
recipes.  (Currently, this is probably less well explained than it
should be.)

You may be able to find a pre-built binary for your operating system,
version, and CPU type.  For a list of sources, See
https://github.com/bcpierce00/unison/wiki/Downloading-Unison

You should use the most recent formal release, or a newer version from
git.  Earlier versions are no longer maintained, and bug reports are
not accepted about these versions.  This is true even though many
packaging systems (including GNU/Linux distributions) continue to have
2.51 or even 2.48.  The master branch in git historically has been
quite stable.

### Version compatibility

For Unison versions 2.52 and newer, see
https://github.com/bcpierce00/unison/wiki/2.52-Migration-Guide

The information below is true for Unison versions older than 2.52.

Beware that Unison uses OCaml's built-in data marshalling, and that
this facility is unstable across versions of "ocaml" (the standard
implementation of the OCaml language).  Additionally, Unison has
incompatible changes across minor releases (e.g. 2.48 vs 2.51, but
2.51.2 and 2.51.3 are compatible).  Therefore, you must use the same
Unison minor version built with the same ocaml version on all systems.

## Mailinglists

There are two mailinglists: unison-users and unison-hackers.
Descriptions and instructions are at
https://github.com/bcpierce00/unison/wiki/Mailing-Lists

## Development and Submitting Proposed Changes

If you want to play with the internals, have a look at the file
src/ROADMAP.txt for some basic orientation.  Discussion of the source
code, proposed changes, etc. is most appropriate on the unison-hackers
mailinglist.

Proposed code changes are also welcome (as pull requests).  For
significant changes, an enhancement request or bug report is likely in
order to provide the proposed semantics ahead of time.  For changes
that are likely to be widely viewed as clearly desired, that might be
enough.  Others should be discussed on unison-hackers.

Proposed changes should change documentation in concert with code, and
should pass CI.

Unison operates under the widely-used "inbound=outbound" contribution
license process.  Therefore, all contributions to Unison must be
licensed under the project's license, currently GPLv3 (unless a file
under a different license is being modified).  New files of
significance must have a copyright statement and grant permission to
copy under the project's license.  Significant changes should include
copyright statements and/or add authors.  Submitting a pull request or
posting a contribution on a mailinglist is an assertion that the
submitter has the authority to license their changes under the
project's license.  (This paragraph is intended to summarize the
normal conventions, and is not intended to create any new norms.  See
https://sfconservancy.org/blog/2014/jun/09/do-not-need-cla/ for a
longer discussion.)
