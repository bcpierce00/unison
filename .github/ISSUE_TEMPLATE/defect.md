---
name: Defect Report
about: Create an issue that argues that there is a bug in unison
title: ''
labels: defect
assignees: ''
---

# Instructions

The issue tracker is not a help system.  Before using the issue
tracker, please read the following page thoroughly and follow the
guidelines it expresses:

https://github.com/bcpierce00/unison/wiki/Reporting-Bugs-and-Feature-Requests

Pleaes delete the following dashed line and the text above it, leaving the
statement that you have read the wiki page.
----------------------------------------
I have thoroughly read the [Reporting Bugs and Feature Requests](https://github.com/bcpierce00/unison/wiki/Reporting-Bugs-and-Feature-Requests) wiki page.

# Meta

Bug reports must be about a minimized configuration, as described in the wiki page.

People filing bug reports will be expected to follow up and perhaps
test proposed fixes.

# Environment

  - Unison version
  - OS, version and CPU
  - ocaml version
  - TUI or GUI
  - local, ssh, socket, or ?
  - use of fsmonitor?

# Reproduction Recipe

[Describe how to reproduce the bug.  This must be the smallest
configuration you can find that exhibits the bug, and you should
include the largest working configuration also.  Generally this means
local sync, TUI, no watcher, unless e.g. the bug is phrased as
"Without the watcher, behavior is correct, but on adding
fsmonitor-foo, ....".]

# Expected behavior vs actual behavior

[Describe the actual behavior.  Argue that the behavior is incorrect.]
