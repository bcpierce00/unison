#######################################################################
# $I1: Unison file synchronizer: src/Makefile $
# $I2: Last modified by bcpierce on Sun, 22 Aug 2004 22:29:04 -0400 $
# $I3: Copyright 1999-2004 (see COPYING for details) $
#######################################################################
## User Settings

# Set NATIVE=false if you are not using the native code compiler (ocamlopt)
# This is not advised, though: Unison runs much slower when byte-compiled.
#
# If you set NATIVE=false, then make sure that the THREADS option below is
# also set to false unless your OCaml installation has true posix-compliant
# threads (i.e., -with-pthreads was given as an option to the config script).
NATIVE=true

# Use THREADS=false if your OCaml installation is not configured with the
# -with-pthreads option.  (Unison will crash when compiled with THREADS=true
# if the -with-pthreads configuration option was not used.)
THREADS=false

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

all:: INSTALL

INSTALL: $(NAME)$(EXEC_EXT)
# file isn't made for OS X, so check that it's there first
	(if [ -f $(NAME) ]; then ./$(NAME) -doc install > INSTALLATION; fi)

########################################################################
## Miscellaneous developer-only switches
DEBUGGING=true
PROFILING=false
STATIC=false

# NAME and VERSION, automatically generated
-include Makefile.ProjectInfo

Makefile.ProjectInfo: mkProjectInfo
	./mkProjectInfo > $@

mkProjectInfo: mkProjectInfo.ml
	ocamlc -o $@ $^

clean::
	$(RM) mkProjectInfo
	$(RM) Makefile.ProjectInfo

########################################################################
### Compilation rules

include Makefile.OCaml

######################################################################
# Installation

INSTALLDIR = $(HOME)/bin/

# This has two names because on OSX the file INSTALL shadows the target 'install'!
install: doinstall

doinstall: $(NAME)$(EXEC_EXT)
	-mv $(INSTALLDIR)/$(NAME)$(EXEC_EXT) /tmp/$(NAME)-$(shell echo $$$$)
	cp $(NAME)$(EXEC_EXT) $(INSTALLDIR)
	cp $(NAME)$(EXEC_EXT) $(INSTALLDIR)$(NAME)-$(VERSION)$(EXEC_EXT)
	@# If we're running at Penn, install a public version too
	if [ -d /plclub/bin ]; then cp $(NAME)$(EXEC_EXT) /plclub/bin/$(NAME)-$(VERSION)$(EXEC_EXT); fi


######################################################################
# Demo

setupdemo-old: all
	-mkdir alice.tmp bob.tmp
	-touch alice.tmp/letter alice.tmp/curriculum
	-mkdir bob.tmp/curriculum
	-touch bob.tmp/curriculum/french
	-touch bob.tmp/curriculum/german
	-mkdir bob.tmp/good_friends
	-mkdir bob.tmp/good_friends/addresses
	-mkdir alice.tmp/good_friends
	-touch alice.tmp/good_friends/addresses
	-touch bob.tmp/good_friends/addresses/alice
	-mkdir alice.tmp/book
	-mkdir bob.tmp/book
	echo "first name:alice \n 2234 Chesnut Street \n Philadelphia" \
	   > bob.tmp/good_friends/addresses/alice
	echo "ADDRESS 1 : BOB \n firstName : bob \n 2233 Walnut Street" \
	   > alice.tmp/good_friends/addresses
	echo "Born in Paris in 1976 ..." > alice.tmp/curriculum
	echo "Ne a Paris en 1976 ..." > bob.tmp/curriculum/french
	echo "Geboren in Paris im jahre 1976 ..." > bob.tmp/curriculum/german
	echo "Dear friend, I received your letter ..."    > alice.tmp/letter
	echo "And then the big bad wolf" > bob.tmp/book/page3
	echo "Title : three little pigs" > alice.tmp/book/page1
	echo "there was upon a time ..." > alice.tmp/book/page2

setupdemo:
	rm -rf a.tmp b.tmp
	mkdir a.tmp
	touch a.tmp/a a.tmp/b a.tmp/c
	mkdir a.tmp/d
	touch a.tmp/d/f
	touch a.tmp/d/g
	cp -r a.tmp b.tmp

modifydemo:
	-rm a.tmp/a
	echo "Hello" > a.tmp/b
	echo "Hello" > b.tmp/b
	date > b.tmp/c
	echo "Hi there" > a.tmp/d/h
	echo "Hello there" > b.tmp/d/h

demo: all setupdemo
	@$(MAKE) run
	@$(MAKE) modifydemo
	@$(MAKE) run

run: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) default a.tmp b.tmp

runbatch: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) default a.tmp b.tmp -batch

runt: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) default a.tmp b.tmp -timers

rundebug: all
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) a.tmp b.tmp -debug all  -ui text

runp: all
	-echo cat > a.tmp/cat
	-echo cat > b.tmp/cat
	-chmod 765 a.tmp/cat
	-chmod 700 b.tmp/cat
	./$(NAME) a.tmp b.tmp

runtext: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) -ui text a.tmp b.tmp

runsort: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/b
	-date > b.tmp/m
	-date > b.tmp/z
	-date > b.tmp/f
	-date >> b.tmp/f
	-date > b.tmp/c.$(shell echo $$$$)
	-date > b.tmp/y.$(shell echo $$$$)
	./$(NAME) default a.tmp b.tmp -debug sort

runprefer: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/b
	-date > b.tmp/m
	-date > b.tmp/z
	-echo Hello > a.tmp/z
	-date > b.tmp/f
	-date >> b.tmp/f
	-date > b.tmp/c.$(shell echo $$$$)
	-date > b.tmp/y.$(shell echo $$$$)
	./$(NAME) default a.tmp b.tmp -force b.tmp

prefsdocs: all
	./$(NAME) -prefsdocs 2> prefsdocsjunk.tmp
	mv -f prefsdocsjunk.tmp prefsdocs.tmp

# For developers at Penn
runsaul: all
	-date > a.tmp/x
	./$(NAME) -servercmd current/unison/src/unison \
	          -debug all -debugtimes \
                  a.tmp ssh://saul/$(HOME)/current/unison/src/b.tmp

runlocal: all
	-date > a.tmp/x
	./$(NAME) -servercmd current/unison/src/unison \
                  test1 a.tmp ssh://localhost/$(HOME)/current/unison/src/b.tmp \
	          -debug gc

rshsaul: all
	-date > a.tmp/x
	./$(NAME) a.tmp rsh://saul/$(HOME)/current/unison/src/b.tmp

byte:
	$(MAKE) all NATIVE=false

runtest:
	$(MAKE) all NATIVE=false DEBUG=true
	./unison test

runbare:
	$(MAKE) all NATIVE=false DEBUG=true
	./unison

runtesttext:
	$(MAKE) all NATIVE=false DEBUG=true UISTYLE=text
	./unison test -ui text -batch

runjunk: byte
	./unison current -debug all

temp:
	$(MAKE) UISTYLE=text
	echo "A " > a.tmp/xxx
	date >> a.tmp/xxx
	echo "B " > b.tmp/xxx
	date >> b.tmp/xxx
	md5 a.tmp/xxx
	md5 b.tmp/xxx
	echo m | ./unison test -debug all

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

# In Windows, tags and TAGS are the same, so make tags stops working
# after the first invocation.  The .PHONY declaration makes it work
# again.
.PHONY: tags

tags:
	-$(ETAGS) *.mli */*.mli *.ml */*.ml *.m */*.m *.c */*.c *.txt

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

clean::
	$(MAKE) -C ubase clean
	$(MAKE) -C lwt clean

ifeq (${OSARCH},osx)
clean::
	-(cd uimac; xcodebuild clean)
endif

checkin:
	$(MAKE) -C .. checkin

####################################################################
# Documentation strings

# Cons up a fake strings.ml if necessary (the real one is generated when
# we build the documentation, but we need to be able to compile the
# executable here to do that!)
strings.ml:
	echo "(* Dummy strings.ml *)" > strings.ml
	echo "let docs = []" >> strings.ml

