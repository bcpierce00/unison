Unison File Synchronizer
========================

[![Build Status](https://travis-ci.org/bcpierce00/unison.svg?branch=master)](https://travis-ci.org/bcpierce00/unison)
![Main workflow](https://github.com/bcpierce00/unison/workflows/Main%20workflow/badge.svg)

Unison is a file-synchronization tool for OSX, Unix, and Windows. It allows two
replicas of a collection of files and directories to be stored on different
hosts (or different disks on the same host), modified separately, and then
brought up to date by propagating the changes in each replica to the other.

If you just want to use Unison, you can probably find a pre-built binary for
your architecture either on your favorite package manager or here:

   http://www.cis.upenn.edu/~bcpierce/unison

If you want to play with the internals, have a look at the file
src/ROADMAP.txt for some basic orientation.

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
