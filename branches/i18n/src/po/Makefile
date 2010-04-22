#######################################################################
# $I1: Unison file synchronizer: src/po/Makefile $
# $I2: Last modified by bcpierce on Sun, 22 Aug 2004 22:29:04 -0400 $
# $I3: Copyright 1999-2004 (see COPYING for details) $
#######################################################################

OCAML_GETTEXT_PACKAGE = unison
LINGUAS=$(shell cat LINGUAS)
SOURCES=POTFILES

OCAML_GETTEXT=ocaml-gettext
OCAML_GETTEXT_EXTRACT_OPTIONS=
OCAML_GETTEXT_COMPILE_OPTIONS=
OCAML_GETTEXT_INSTALL_OPTIONS=
OCAML_GETTEXT_MERGE_OPTIONS=

BUILDPO=../_build/share/locale/

POFILES=$(addsuffix .po,$(LINGUAS))
MOFILES=$(addsuffix .mo,$(LINGUAS))
POTFILE=$(OCAML_GETTEXT_PACKAGE).pot

all: $(POTFILE) install-buildpo

install: install-po

uninstall: uninstall-po

clean:: clean-po

%.mo: %.po
	$(OCAML_GETTEXT) --action compile $(OCAML_GETTEXT_COMPILE_OPTIONS)    \
  	  --compile-output $@ $^

%.pot: $(SOURCES)
	$(OCAML_GETTEXT) --action extract $(OCAML_GETTEXT_EXTRACT_OPTIONS)    \
	  --extract-pot $@ $^

%.po: $(POTFILE)
	$(OCAML_GETTEXT) --action merge   $(OCAML_GETTEXT_MERGE_OPTIONS)      \
	  --merge-pot $(POTFILE) $@

$(BUILDPO): 
	mkdir -p $(BUILDPO)

.PRECIOUS: $(POTFILE) 

install-buildpo: $(MOFILES) $(BUILDPO)
	$(OCAML_GETTEXT) --action install $(OCAML_GETTEXT_INSTALL_OPTIONS)    \
	  --install-textdomain $(OCAML_GETTEXT_PACKAGE)                       \
	  --install-destdir $(BUILDPO) $(MOFILES)

install-po: $(MOFILES) 
	$(OCAML_GETTEXT) --action install $(OCAML_GETTEXT_INSTALL_OPTIONS)    \
	  --install-textdomain $(OCAML_GETTEXT_PACKAGE)                       \
	  --install-destdir $(PODIR) $(MOFILES)

uninstall-po:
	$(OCAML_GETTEXT) --action uninstall $(OCAML_GETTEXT_INSTALL_OPTIONS)  \
	  --uninstall-textdomain $(OCAML_GETTEXT_PACKAGE)                     \
	  --uninstall-orgdir $(PODIR) $(MOFILES)

clean-po:
	-$(OCAML_GETTEXT) --action uninstall $(OCAML_GETTEXT_INSTALL_OPTIONS) \
	  --uninstall-textdomain $(OCAML_GETTEXT_PACKAGE)                     \
	  --uninstall-orgdir $(BUILDPO) $(MOFILES)
	-$(RM) $(MOFILES) 

