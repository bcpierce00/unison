##########################################################################
#  ocaml-gettext: a library to translate messages                        #
#                                                                        #
#  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         #
#  Modified by Krzysztof Królczyk                                        #
#                                                                        #
#  This library is free software; you can redistribute it and/or         #
#  modify it under the terms of the GNU Lesser General Public            #
#  License as published by the Free Software Foundation; either          #
#  version 2.1 of the License, or (at your option) any later version;    #
#  with the OCaml static compilation exception.                          #
#                                                                        #
#  This library is distributed in the hope that it will be useful,       #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     #
#  Lesser General Public License for more details.                       #
#                                                                        #
#  You should have received a copy of the GNU Lesser General Public      #
#  License along with this library; if not, write to the Free Software   #
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   #
#  USA                                                                   #
##########################################################################

OCAML_GETTEXT_PACKAGE=unison
LINGUAS=$(shell cat LINGUAS)
SOURCES=$(shell cat POTFILES)

OCAML_GETTEXT=ocaml-gettext
OCAML_XGETTEXT=ocaml-xgettext
OCAML_GETTEXT_EXTRACT_OPTIONS=--extract-command $(OCAML_XGETTEXT)
OCAML_GETTEXT_COMPILE_OPTIONS=
OCAML_GETTEXT_INSTALL_OPTIONS=
OCAML_GETTEXT_MERGE_OPTIONS=

# This dir exists for Dune builds, not for Makefile ones
BUILDPO=$(if $(shell test -d ../../_build/), ../../_build/share/locale, ./_tmp/share/locale)

POFILES=$(addsuffix .po,$(LINGUAS))
MOFILES=$(addsuffix .mo,$(LINGUAS))
POTFILE=$(OCAML_GETTEXT_PACKAGE).pot

# keep this first, in order to be user-friendly
help:
	@printf "Usage: \n"
	@printf "make [add-lang | update-lang | edit-lang | finish-lang | help | update-sources]\n"
	@printf "make [install-buildpo | install | uninstall | clean]\n"
	@-echo "todo: convert to normal make rules"

add-lang:
	@read -p "Language code (2 letters):" newlang; \
	mkdir -p locales/$$newlang/LC_MESSAGES/; \
	msginit --input=$(POTFILE) --locale=$$newlang --output=locales/$$newlang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).po; \
	echo sed -zi "/$$newlang$$/!s/$$/$$newlang/" LINGUAS; \
	sed 's/charset=ASCII/charset=UTF-8/g' -i locales/$$newlang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).po

edit-lang:
	@echo "todo, respect ~/.selected-editor"
	@read -p "Language code (2 letters):" newlang; \
	vim locales/$$newlang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).po

update-lang:
	@read -p "Language code (2 letters):" updlang; \
	msgmerge --update locales/$$updlang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).po $(POTFILE)

finish-lang:
	@read -p "Language code (2 letters):" preplang; \
	mkdir -p $(BUILDPO)/$$preplang/LC_MESSAGES/; \
	msgfmt locales/$$preplang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).po -o $(BUILDPO)/$$preplang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).mo

# TODO: this would poison CIs env, should cleanup after packaging, if vanilla CI is important. Notice:
#       - copying to /usr/local/share/locale and creating directories needs elevated permissions
#       - we don't want to fail the build when file cannot be copied (probably it should display warning, but ignores for now)
.PHONY: all_langs
all_langs:
	@for lang in $(LINGUAS); do \
		mkdir -p locales/$$lang/LC_MESSAGES/; \
		mkdir -p $(BUILDPO)/$$lang/LC_MESSAGES; \
		msgmerge --update locales/$$lang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).po $(POTFILE); \
		msgfmt locales/$$lang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).po -o $(BUILDPO)/$$lang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).mo; \
		cp $(BUILDPO)/$$lang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).mo /usr/local/share/locale/$$lang/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).mo || true; \
	done

install: install-po

uninstall: uninstall-po

clean:: clean-po

%.mo: %.po
	$(OCAML_GETTEXT) --action compile $(OCAML_GETTEXT_COMPILE_OPTIONS)    \
	--compile-output $@ locales/$(basename $^)/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).po

%.pot: $(SOURCES)
	$(OCAML_GETTEXT) --action extract $(OCAML_GETTEXT_EXTRACT_OPTIONS)    \
	--extract-pot $@ $^

%.po: $(POTFILE)
	$(OCAML_GETTEXT) --action merge   $(OCAML_GETTEXT_MERGE_OPTIONS)      \
	--merge-pot $(POTFILE) locales/$(basename $@)/LC_MESSAGES/$(OCAML_GETTEXT_PACKAGE).po


update-sources: $(POTFILE)
	cd ..; ls *.ml -1 > languages/POTFILES
	echo ".make $(OCAML_GETTEXT_PACKAGE).pot"


$(BUILDPO):
	mkdir -p $(BUILDPO)

.PRECIOUS: $(POTFILE)

install-buildpo: $(MOFILES) $(BUILDPO)
	$(OCAML_GETTEXT) --action install $(OCAML_GETTEXT_INSTALL_OPTIONS)    \
	--install-textdomain $(OCAML_GETTEXT_PACKAGE)                         \
	--install-destdir $(BUILDPO) $(MOFILES)

install-po: $(MOFILES)
	$(OCAML_GETTEXT) --action install $(OCAML_GETTEXT_INSTALL_OPTIONS)    \
	--install-textdomain $(OCAML_GETTEXT_PACKAGE)                         \
	--install-destdir $(BUILDPO) $(MOFILES)

uninstall-po:
	$(OCAML_GETTEXT) --action uninstall $(OCAML_GETTEXT_INSTALL_OPTIONS)  \
	--uninstall-textdomain $(OCAML_GETTEXT_PACKAGE)                       \
	--uninstall-orgdir $(BUILDPO) $(MOFILES)

clean-po:
	-$(OCAML_GETTEXT) --action uninstall $(OCAML_GETTEXT_INSTALL_OPTIONS) \
	--uninstall-textdomain $(OCAML_GETTEXT_PACKAGE)                       \
	--uninstall-orgdir $(BUILDPO) $(MOFILES)
	-$(RM) $(MOFILES)
