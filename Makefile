# Unison file synchronizer: Makefile
# See LICENSE for terms.

# Sub-makefiles are perfectly fine for parallel builds.
# This makefile is not, due to recursive invocations of make.
.NOTPARALLEL:

.PHONY: all
all: src manpage

.PHONY: src
src:
	$(MAKE) -C src

# It's a wart that docs need "unison" built (vs some docs utility).
.PHONY: docs
docs: src manpage
	$(MAKE) -C doc

# "src" is a prerequisite to prevent parallel build errors.
# manpage builds currently require a pre-built "unison" binary.
.PHONY: manpage
manpage: src
	$(MAKE) -C man

.PHONY: test
test: ./src/unison
	./src/unison -ui text -selftest

.PHONY: depend
depend:
	$(MAKE) -C src depend

.PHONY: clean
clean:
	$(MAKE) -C doc clean
	$(MAKE) -C man clean
	$(MAKE) -C src clean

INSTALL ?= install
INSTALL_PROGRAM ?= $(INSTALL)
INSTALL_DATA ?= $(INSTALL) -m 644

prefix = /usr/local
PREFIX ?= $(prefix)
exec_prefix = $(PREFIX)
EXEC_PREFIX ?= $(exec_prefix)
bindir = $(EXEC_PREFIX)/bin
BINDIR ?= $(bindir)
datarootdir = $(PREFIX)/share
DATAROOTDIR ?= $(datarootdir)
mandir = $(DATAROOTDIR)/man
MANDIR ?= $(mandir)
man1dir = $(MANDIR)/man1
MAN1DIR ?= $(man1dir)
manext = .1
MANEXT ?= $(manext)

.PHONY: install
install: all
	$(INSTALL) -d "$(DESTDIR)$(BINDIR)"
ifneq ($(wildcard src/unison),)
	$(INSTALL_PROGRAM) src/unison "$(DESTDIR)$(BINDIR)/unison"
endif
ifneq ($(wildcard src/unison-gui),)
	$(INSTALL_PROGRAM) src/unison-gui "$(DESTDIR)$(BINDIR)/unison-gui"
endif
ifneq ($(wildcard src/unison-fsmonitor),)
	$(INSTALL_PROGRAM) src/unison-fsmonitor "$(DESTDIR)$(BINDIR)/unison-fsmonitor"
else
  ifneq ($(wildcard src/fsmonitor.py),)
	$(INSTALL_PROGRAM) src/fsmonitor.py "$(DESTDIR)$(BINDIR)/fsmonitor.py"
  endif
endif
ifneq ($(wildcard man/unison.1),)
	$(INSTALL) -d "$(DESTDIR)$(MAN1DIR)"
	$(INSTALL_DATA) man/unison.1 "$(DESTDIR)$(MAN1DIR)/unison$(MANEXT)"
endif
ifneq ($(wildcard src/uimac/build/Default/Unison.app),)
	$(info !!! The GUI for macOS has been built but will NOT be installed automatically. \
    You can find the built GUI package at $(abspath src/uimac/build/Default/Unison.app))
endif

# Delegate other targets to the sub-makefile
.PHONY: Makefile

./src/%:
	$(MAKE) -C src $*

%: FORCE
	$(MAKE) -C src $@
.PHONY: FORCE
FORCE: ;
