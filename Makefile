# Unison file synchronizer: Makefile
# See LICENSE for terms.

.PHONY: all
all: src manpage

.PHONY: src
src:
	$(MAKE) -C src

# It's a wart that docs need "unison" built (vs some docs utility).
# Having docs build src/unison points out that UISTYLE is a bug; either
# docs might build the GUI (not wanted as too heavy for docs use), or
# whatever is built might not be rebuilt later.
.PHONY: docs
docs:
	$(MAKE) -C src UISTYLE=text
	$(MAKE) -C doc
	$(MAKE) -C man

# "src" is a prerequisite to prevent parallel build errors.
# manpage builds currently require a pre-built "unison" binary.
.PHONY: manpage
manpage: src
	$(MAKE) -C man

.PHONY: test
test:
	./src/unison -ui text -selftest

.PHONY: depend
depend:
	$(MAKE) -C src depend

.PHONY: clean
clean:
	$(MAKE) -C doc clean
	$(MAKE) -C man clean
	$(MAKE) -C src clean

.PHONY: install
install:
	@printf "\n\n=========================================\n\
To install, copy the files src/unison, src/unison-gui (optional),\n\
src/unison-fsmonitor (optional) and src/fsmonitor.py (optional,\n\
if unison-fsmonitor does not exist) to a freely chosen location.\n\n\
Manual page is at man/unison.1 and user manual is at\n\
doc/unison-manual.pdf, doc/unison-manual.html and doc/unison-manual.txt\n\
=========================================\n\n\n"
	@exit 1
