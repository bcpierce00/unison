# Unison file synchronizer: src/Makefile
# Copyright 1999-2022 (see ../LICENSE for terms).

## User Settings

# Set NATIVE=false if you are not using the native code compiler (ocamlopt)
# This is not advised, though: Unison runs much slower when byte-compiled.
NATIVE=true

# User interface style.  For legal values, see Makefile.OCaml.
# You probably don't need to set this yourself -- it will be set to
# an appropriate value automatically, depending on whether the lablgtk
# library is available.
#
# UISTYLE=text

########################################################################
########################################################################
#     (There should be no need to change anything from here on)       ##
########################################################################

######################################################################
# Building installation instructions

all:: strings.ml buildexecutable

.PHONY: all clean install doinstall installtext text \
	runtest repeattest \
	selftest selftestdebug selftestremote testmerge \
	checkin installremote

########################################################################
## Miscellaneous developer-only switches
DEBUGGING=true
PROFILING=false
STATIC=false

# NAME, VERSION, and MAJORVERSION, automatically generated
-include Makefile.ProjectInfo

########################################################################
### Compilation rules

include Makefile.OCaml

######################################################################
# Installation

INSTALLDIR = $(HOME)/bin/

# This has two names because on OSX the file INSTALL shadows the target 'install'!
install: doinstall

installtext:
	$(MAKE) -C .. installtext

text:
	$(MAKE) -C .. text

# Note that this is not needed (and doesn't work) for the OSX GUI version.
# Just copy $(UIMACDIR)/build/Default/Unison.app to where you want it.
doinstall: buildexecutable
	@if [ ! $(NAME) ]; then \
	    echo "makefile variable NAME not bound"; \
	    exit 1 \
	  ; fi
	-mv $(INSTALLDIR)/$(NAME)$(EXEC_EXT) /tmp/$(NAME)-$(shell echo $$$$)
	cp $(NAME)$(EXEC_EXT) $(INSTALLDIR)
	cp $(NAME)$(EXEC_EXT) $(INSTALLDIR)$(NAME)-$(MAJORVERSION)$(EXEC_EXT)

# For developers
runtest:
	$(MAKE) NATIVE=false DEBUG=true text
	./unison test

repeattest:
	$(MAKE) all NATIVE=false DEBUG=true UISTYLE=text
	./unison noprofile a.tmp b.tmp -repeat foo.tmp -debug ui

selftest:
	$(MAKE) all NATIVE=false DEBUG=true UISTYLE=text
	./unison -selftest -ui text -batch

selftestdebug:
	$(MAKE) all NATIVE=false DEBUG=true UISTYLE=text
	./unison -selftest -ui text -batch -debug all

selftestremote:
	$(MAKE) all NATIVE=false DEBUG=true UISTYLE=text
	./unison -selftest -ui text -batch test.tmp ssh://eniac.seas.upenn.edu/test.tmp

testmerge:
	$(MAKE) all NATIVE=false UISTYLE=text
	-rm -rf a.tmp b.tmp
	-rm -rf $(HOME)/.unison/backup/file.txt*
	mkdir a.tmp b.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	./unison testmerge -ui text -batch
	echo 1OO >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3oo >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 7oo >> a.tmp/file.txt
	echo 8oo >> a.tmp/file.txt
	echo 9oo >> a.tmp/file.txt
	echo 0oo >> a.tmp/file.txt
	echo 1oo >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3oo >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 7oo >> a.tmp/file.txt
	echo 8oo >> a.tmp/file.txt
	echo 9oo >> a.tmp/file.txt
	echo 0oo >> a.tmp/file.txt
	echo 1oo >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3OO >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	./unison testmerge -ui text -batch
	rm a.tmp/file.txt b.tmp/file.txt
	echo 1OO >> a.tmp/file.txt
	echo second >> a.tmp/file.txt
	echo 3oo >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 7oo >> a.tmp/file.txt
	echo 8oo >> a.tmp/file.txt
	echo 9oo >> a.tmp/file.txt
	echo 0oo >> a.tmp/file.txt
	echo 1oo >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3oo >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 7oo >> a.tmp/file.txt
	echo 8oo >> a.tmp/file.txt
	echo 9oo >> a.tmp/file.txt
	echo 0oo >> a.tmp/file.txt
	echo 1oo >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3OO >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo ---
	echo 1OO >> b.tmp/file.txt
	echo 2oo >> b.tmp/file.txt
	echo 3oo >> b.tmp/file.txt
	echo 4oo >> b.tmp/file.txt
	echo 5oo >> b.tmp/file.txt
	echo 6oo >> b.tmp/file.txt
	echo 7oo >> b.tmp/file.txt
	echo 8oo >> b.tmp/file.txt
	echo 9oo >> b.tmp/file.txt
	echo 0oo >> b.tmp/file.txt
	echo 1oo >> b.tmp/file.txt
	echo 2oo >> b.tmp/file.txt
	echo 3oo >> b.tmp/file.txt
	echo 4oo >> b.tmp/file.txt
	echo 5oo >> b.tmp/file.txt
	echo 6oo >> b.tmp/file.txt
	echo 5oo >> b.tmp/file.txt
	echo 6oo >> b.tmp/file.txt
	echo 7oo >> b.tmp/file.txt
	echo 8oo >> b.tmp/file.txt
	echo 9oo >> b.tmp/file.txt
	echo 0oo >> b.tmp/file.txt
	echo 1oo >> b.tmp/file.txt
	echo 2oo >> b.tmp/file.txt
	echo 3OO >> b.tmp/file.txt
	echo end >> b.tmp/file.txt
	@echo
	@echo -----------------------------------------------------------
	@echo
	./unison testmerge -ui text -batch -debug files -debug update -debug backup
	@echo
	@echo -----------------------------------------------------------
	@echo
	./unison testmerge -ui text -batch
	@echo
	@echo -----------------------------------------------------------
	@echo
	cat a.tmp/file.txt
	cat b.tmp/file.txt
	cat $(HOME)/.unison/backup/file.txt


######################################################################
# Tags

ETAGS=etags

# In Windows, tags and TAGS are the same, so make tags stops working
# after the first invocation.  The .PHONY declaration makes it work
# again.
.PHONY: tags

tags:
	@-if command -v $(ETAGS) > /dev/null ; then \
	    $(ETAGS) *.mli */*.mli *.ml */*.ml */*.m *.c */*.c *.txt \
		     *Makefile* \
	  ; fi

all:: TAGS

TAGS:
	$(MAKE) tags

######################################################################
# Misc

clean::
	-$(RM) *.log *.aux *.log *.dvi *.out *.bak
	-$(RM) -r obsolete
	-$(RM) $(NAME) $(NAME).exe
	-$(RM) $(NAME)-blob.o
	$(MAKE) -C fsmonitor/windows clean

ifneq ($(strip $(UIMACDIR)),)
clean::
	-(cd $(UIMACDIR); xcodebuild clean)
	-(cd $(UIMACDIR); $(RM) -r build ExternalSettings.xcconfig)
endif

####################################################################
# Documentation strings

# Cons up a fake strings.ml if necessary (the real one is generated when
# we build the documentation, but we need to be able to compile the
# executable here to do that!)
strings.ml:
	echo "(* Dummy strings.ml *)" > strings.ml
	echo "let docs = []" >> strings.ml

