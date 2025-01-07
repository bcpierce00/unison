# dev

This directory exists to hold scripts etc. that are useful only to
people who are making changes to unison's source code.  Nothing in
here may be used as part of building programs or documentation, or CI.

#  ktrace-*

Scripts with names like ktrace-* are for particular operating systems,
and aim to enumerate the programs used by a build, for checking
INSTALL.md.

# check-memory

This script can run a unison workload with small amounts of memory,
and is intended to make it easier to understand memory usage.  It is
not a fully-baked test.  It's here rather than in tests because it's
an exploratory tool, not a regression test.

