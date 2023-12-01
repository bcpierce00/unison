# Introduction

This file describes many aspects of contributing to Unison beyond
testing and bug reporting.

## Maintenance status

Unison has no paid contributors.  There are currently about 0.1 FTE
across 2 people regularly contributing.  There are no plans for
significant feature development.

Help is welcome, and the details are below.

## unison-hackers

Discussion of the source code, proposed changes, etc. is most
appropriate on the `unison-hackers@` mailinglist.  (See
https://github.com/bcpierce00/unison/wiki/Mailing-Lists).

## Development and Submitting Proposed Changes

The file `src/ROADMAP.txt` has a very basic orientation.

Proposed code changes are also welcome (as pull requests).  For
significant changes, an enhancement request or bug report is likely in
order to provide the proposed semantics ahead of time.  For changes
that are likely to be nearly-universally viewed as clearly desired,
that might be enough.  Others should be discussed on unison-hackers.

Proposed changes should change documentation in concert with code, and
should pass CI.

Expect comments and requests for adjustments.  Unison typically
expects contributors to use fixup commits for tweaks and rebase so
that there is a clean set of commits from a recent point on master.

## Licensing

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

## CI

Github requires maintainer approval for new contributors to be able to
run workflows, and this is routinely granted.  Thus, CI should run on
your PR after a maintainer has had a chance to take an initial look.

## Strings

Running `make docs` will regenerate `src/strings.ml`.  This change
should be in a commit by itself with commit message "Regen
strings.ml".

## Build systems

Unison has both make and dune.  Changes should adjust both, and CI
will check that.

## Format breaks

Since 2.52, we attempt to have back compat for archive format and wire
format.  There is feature negotiation for extensions; see
src/FEATURES.md and `src/features.mli` for more information.

## Portability

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

## Accommodation of old systems

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
