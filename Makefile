# Unison file synchronizer: Makefile
# See LICENSE for terms.

# IMPORTANT!
#
# This file is portable and compatible with GNU Make, BSD make, Solaris
# (d)make and NMAKE. Do not make any changes in this file unless you are
# certain that the changes do not break this portability.

default: all
.PHONY: default

# Sub-makefiles are perfectly fine for parallel builds.
# This makefile is not, due to recursive invocations of make.
.NOTPARALLEL:

.PHONY: all
all: src manpage

.PHONY: src
src: FRC
	cd src && $(MAKE)
FRC: ;
# Not all make seem to work without FRC, even with .PHONY

.PHONY: tui gui macui fsmonitor depend
tui gui macui fsmonitor depend:
	cd src && $(MAKE) $@

# It's a wart that docs need "unison" built (vs some docs utility).
.PHONY: docs
docs: src manpage
	cd doc && $(MAKE)

# "src" is a prerequisite to prevent parallel build errors.
# manpage builds currently require a pre-built "unison" binary.
.PHONY: manpage
manpage: src
	cd man && $(MAKE)

.PHONY: test
test:
	ocaml src/make_tools.ml run ./src/unison -ui text -selftest
# Note: unison binary is not built automatically for the test target,
# so as to avoid building it with unwanted configuration.

# This construct is here to remain compatible with make implementations
# that do not have the -C option, work with different (also non-POSIX)
# shells, and not rely on single shell per line execution.
.PHONY: clean
clean: clean_doc clean_man clean_src
.PHONY: clean_doc
clean_doc:
	cd doc && $(MAKE) clean
.PHONY: clean_man
clean_man:
	cd man && $(MAKE) clean
.PHONY: clean_src
clean_src:
	cd src && $(MAKE) clean

prefix = /usr/local

.PHONY: install
install:
	ocaml src/make_tools.ml install
