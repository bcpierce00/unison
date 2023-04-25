# Introduction

This file describes many aspects of contributing to Unison beyond
testing and bug reporting.

## Maintenance status

Unison has no paid contributors.  There are currently about 0.1 FTE
across 2 people regularly contributing.  There are no plans for
significant feature development.

Help is welcome, and the details are below.

## unison-hackers

github is for proposed changes and bugs, but discussion about what to
do should be on `unison-hackers@` (see
https://github.com/bcpierce00/unison/wiki/Mailing-Lists).

# Proposed changes

Currently, proposed changes are via pull requests on github.  Fork the
repo and submit a PR, much like any other project.

Expect comments and requests for adjustments.  Unison typically
expects contributors to use fixup commits for tweaks and rebase so
that there is a clean set of commits from a recent point on master.

Unison's licensing model is inbound=outbound.  By submitting a PR you
are representing that you have the authority to license your changes
and you grant permission under Unison's license (GPL2).

## CI

CI should run on your PR after a delay of perhaps up to a week, often
less; github requires maintainer approval for new contributors to be
able to run workflows.

## Build systems

Unison has both make and dune.  Changes should adjust both, and CI
will check that.

# Format breaks

Since 2.52, we attempt to have back compat for archive format and wire
format.  There is feature negotiation for extensions; see
src/FEATURES.md and `src/features.mli` for more information.

# Portability

Unison runs on *BSD, GNU/Linux, macOS, Solaris/illumos and Windows.
Generally Unison aims to use interfaces specified by POSIX, and views
non-POSIX systems as deficient and in need of workarounds.  In
practice this means Windows :-)

Sometimes it is necessary to use beyond-POSIX interfaces, but this
should be done with care.  An example might be optional support of
features on some systems, where other systems do not implement the
necessary interfaces.  However, if there are semantically similar
interfaces, all such systems should be supported, and the code should
be structured to admit additional implementations.  Generally these
adaptations will be in C code, hiding the portability issues from our
ocaml code base.  Discussion on `unison-hackers@` is almost certainly
warranted.

Unison has a TUI (text/CLI) which requires very little beyond ocaml.
There are two GUIs, one with GTK3 and one for mac.  Generally we try
to be evenhanded with features, but leaving the mac proprietary GUI
behind is not prohibited.

# Accommodation of old systems

Keeping Unison running on old systems can cause accumulation of crufty
workarounds.  Unison is very old, and it likely has accommodations for
90s problems.

Generally, we aim to keep it working on even very old systems as long
as there is no cost in technical debt.  However, we are also trying to
reduce the code size by removing workarounds for systems that are no
longer in use.

Unison's working definition of a system that is new enough to justify
a workaround is one of:
  - system is currently receiving maintenance by its upstream
  - system is Free Software, not LTS, and is no more than 3 years
    beyond the end of maintenance

Even for systems that are not new enough, we will likely apply a
cost/benefit analysis to pruning.  For example, workarounds for
Windows 7 with minor carrying cost will likely not be removed in 2023,
but workarounds for Windows NT are unlikely to be kept.  
