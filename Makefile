# Unison file synchronizer: Makefile
# See LICENSE for terms.

.PHONY: all src

default: text

text:
	$(MAKE) -C src UISTYLE=text

test:
	./src/unison -ui text -selftest

all: src manpage

src:
	$(MAKE) -C src

-include src/Makefile.ProjectInfo

docs:
	$(MAKE) -C src UISTYLE=text
	$(MAKE) -C doc
	$(MAKE) -C man

manpage:
	$(MAKE) -C man

include src/Makefile.OCaml

depend::
	$(MAKE) -C src depend

clean::
	$(RM) -r *.tmp \
	   *.o *.obj *.cmo *.cmx *.cmi core TAGS *~ *.log \
	   *.aux *.log *.dvi *.out *.backup[0-9] *.bak $(STABLEFLAG)
	$(MAKE) -C doc clean
	$(MAKE) -C man clean
	$(MAKE) -C src clean

install:
	(cd src; $(MAKE) install)

installtext:
	(cd src; $(MAKE) install UISTYLE=text)

src/$(NAME):
	$(MAKE) -C src
