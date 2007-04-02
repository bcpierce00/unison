#######################################################################
# $I1: Unison file synchronizer: Makefile $
# $I2: Last modified by bcpierce on Mon, 06 Sep 2004 19:43:55 -0400 $
# $I3: Copyright 1999-2004 (see COPYING for details) $
#######################################################################
.PHONY: all src

all: src

src:
	$(MAKE) -C src

-include src/Makefile.ProjectInfo

src/Makefile.ProjectInfo: src/mkProjectInfo
	src/mkProjectInfo > $@

src/mkProjectInfo: src/mkProjectInfo.ml
	ocamlc -o $@ $^

include src/Makefile.OCaml

######################################################################
# Version control

SUBMISSIONADDR = bcpierce@cis.upenn.edu

checkin: logmsg remembernews
	echo >> src/mkProjectInfo.ml # so the Rev keyword gets updated
	svn commit --file logmsg
	$(RM) logmsg

remembernews: logmsg
	echo "CHANGES FROM VERSION" $(VERSION) > rc.tmp
	echo >> rc.tmp
	cat logmsg >> rc.tmp
	echo  >> rc.tmp
	echo    ------------------------------- >> rc.tmp
	-cat src/RECENTNEWS >> rc.tmp
	mv -f rc.tmp src/RECENTNEWS

######################################################################
# Export

ifeq ($(OSARCH),win32)
  BCPHOME=/cygdrive/s
  # BCPHOME=/home/exporting
  EXPORTNATIVE=true
  EXPORTSTATIC=true
else
ifeq ($(OSARCH),win32gnuc)
  BCPHOME=/home/exporting
  EXPORTNATIVE=true
  EXPORTSTATIC=false
else
ifeq ($(OSARCH),linux)
  EXPORTNATIVE=true
  EXPORTSTATIC=false
else
ifeq ($(OSARCH),osx)
  EXPORTNATIVE=true
  EXPORTSTATIC=false
else # Solaris
  EXPORTNATIVE=true
  EXPORTSTATIC=true
endif
endif
  BCPHOME=/home/bcpierce
endif
endif

EXPORTDIR=$(BCPHOME)/pub/$(NAME)
DOWNLOADPARENT=$(EXPORTDIR)/download/$(DOWNLOADAREA)
DOWNLOADDIR=$(DOWNLOADPARENT)/$(NAME)-$(VERSION)
BRANCH=$(MAJORVERSION)
EXPORTNAME=$(NAME)-$(VERSION)
DOWNLOADAREA=releases
TMP=/tmp

# Do this when it's time to create a new beta-release from the development trunk
beta: tools/ask
	@tools/ask tools/exportmsg.txt
	(cd ..; svn copy trunk branches/$(BRANCH))
	(cd ../branches/$(BRANCH); svn commit -m "New release branch")
	@echo
	@echo -n "Press RETURN to export it... "
	@read JUNK
	$(MAKE) -C ../branches/$(BRANCH) export

# Do this in a release branch to export a new tarball (e.g., after fixing a bug)
export:
	$(MAKE) $(DOWNLOADDIR)
	$(MAKE) exportdocs
	$(MAKE) exportsources
	@echo
	@echo -n "OK to commit?  Press RETURN if yes (Crtl-C and tidy web dir if no)... "
	@read JUNK
	$(MAKE) commitexport

commitexport:
	$(MAKE) realcommit
	$(MAKE) mailchanges

realcommit:
	@echo
	@echo Committing new export directory
	-chmod -R a+r $(EXPORTDIR)
	-chmod -R g+wr $(EXPORTDIR)
	-chmod -R o-w $(EXPORTDIR)
	-$(RM) $(DOWNLOADPARENT)/beta
	-ln -s $(EXPORTNAME) $(DOWNLOADPARENT)/beta

$(DOWNLOADDIR):
	@echo Creating DOWNLOADDIR = $(DOWNLOADDIR)
	@echo
	-mkdir -p $(DOWNLOADDIR)
	-touch $(DOWNLOADDIR)/THIS-IS-UNISON-$(VERSION)

exportsources:
	$(RM) -r $(TMP)/$(EXPORTNAME)
	(svn export src /tmp/$(EXPORTNAME))
	-$(RM) $(TMP)/$(EXPORTNAME)/RECENTNEWS
	(cd $(TMP); tar cvf - $(EXPORTNAME) \
           | gzip --force --best > $(EXPORTNAME).tar.gz)
	mv $(TMP)/$(EXPORTNAME).tar.gz $(DOWNLOADDIR)

exportdocs:
	-rm -f src/unison
	$(MAKE) -C src UISTYLE=text DEBUGGING=false \
                       NATIVE=$(EXPORTNATIVE) STATIC=$(EXPORTSTATIC)
	-$(RM) src/strings.ml
	$(MAKE) -C doc TEXDIRECTIVES+="\\\\draftfalse" real
	$(MAKE) -C src UISTYLE=text DEBUGGING=false \
                       NATIVE=$(EXPORTNATIVE) STATIC=$(EXPORTSTATIC)
	src/unison -doc news > src/NEWS
	cp doc/unison-manual.ps $(DOWNLOADDIR)/$(EXPORTNAME)-manual.ps
	-cp doc/unison-manual.pdf $(DOWNLOADDIR)/$(EXPORTNAME)-manual.pdf
	cp doc/unison-manual.html $(DOWNLOADDIR)/$(EXPORTNAME)-manual.html
	cp doc/unison-manual.html $(DOWNLOADDIR)/$(NAME)-manual.html

MAILTMP = $(HOME)/mail.tmp

mailchanges: tools/ask src/$(NAME)
	@echo To: $(NAME)-announce@yahoogroups.com,$(NAME)-users@yahoogroups.com \
            > $(MAILTMP)
	@echo Subject: $(NAME) $(VERSION) now available >> $(MAILTMP)
	@echo >> $(MAILTMP)
	@echo Download address: >> $(MAILTMP)
	@echo "  " http://www.cis.upenn.edu/~bcpierce/unison/download.html \
           >> $(MAILTMP)
	@echo >> $(MAILTMP)
	@cat src/NEWS >> $(MAILTMP)
	@src/unison -doc news >> $(MAILTMP)
	tools/ask tools/mailmsg.txt

######################################################################
# Export binary for the current architecture 
# (this stuff is all probably dead)

EXPORTTMP=$(TMP)/export-$(OSARCH)x.tmp

exportnative:
	-$(RM) -r $(EXPORTTMP)
	cp -r src $(EXPORTTMP)
	make realexportnative
ifeq ($(OSARCH),linux)
	make realexportnative EXPORTSTATIC=true KIND=-static
endif
	$(RM) -r $(EXPORTTMP)

realexportnative:
	-$(MAKE) -C $(EXPORTTMP) clean
	$(MAKE) -C $(EXPORTTMP) UISTYLE=text DEBUGGING=false \
                       NATIVE=$(EXPORTNATIVE) STATIC=$(EXPORTSTATIC)
	-mkdir -p $(DOWNLOADDIR)
	cp $(EXPORTTMP)/$(NAME)$(EXEC_EXT) \
               $(DOWNLOADDIR)/$(EXPORTNAME).$(OSARCH)$(KIND)-textui$(EXEC_EXT)
	gzip --best --force -c \
            $(DOWNLOADDIR)/$(EXPORTNAME).$(OSARCH)$(KIND)-textui$(EXEC_EXT) \
          > $(DOWNLOADDIR)/$(EXPORTNAME).$(OSARCH)$(KIND)-textui$(EXEC_EXT).gz
	$(MAKE) -C $(EXPORTTMP) UISTYLE=gtk DEBUGGING=false \
                       NATIVE=$(EXPORTNATIVE) STATIC=$(EXPORTSTATIC)
	cp $(EXPORTTMP)/$(NAME)$(EXEC_EXT) \
               $(DOWNLOADDIR)/$(EXPORTNAME).$(OSARCH)$(KIND)-gtkui$(EXEC_EXT)
	gzip --best --force -c \
            $(DOWNLOADDIR)/$(EXPORTNAME).$(OSARCH)$(KIND)-gtkui$(EXEC_EXT) \
          > $(DOWNLOADDIR)/$(EXPORTNAME).$(OSARCH)$(KIND)-gtkui$(EXEC_EXT).gz

######################################################################
# Export developer sources  (normally run every night by a cron job on
# saul.cis.upenn.edu; also as a last step of 'make checkin', when performed
# by bcp.  Can also be run manually if needed.
# NOTE: the svn checkout assumes this is being run on saul.

DEVELDIR=$(EXPORTDIR)/download/resources/developers-only

nightly:
	($(RM) -r $(HOME)/tmp/unison; \
         cd $(HOME)/tmp; \
	 svn co https://cvs.cis.upenn.edu:3690/svnroot/unison/trunk unison; \
         cd $(HOME)/tmp/unison; \
         $(MAKE) exportdevel)

exportdevel: tareverything
	-$(RM) $(DEVELDIR)/*
	mv $(TMP)/$(EXPORTNAME).tar.gz $(DEVELDIR)

######################################################################
# Submitting changes

CP = cp

submit: tareverything sendsubmission

tareverything:
	$(RM) -r $(TMP)/$(EXPORTNAME)
	$(CP) -r . $(TMP)/$(EXPORTNAME)
	$(RM) -r $(TMP)/$(EXPORTNAME)/private
	$(MAKE) -C $(TMP)/$(EXPORTNAME) clean
	(cd $(TMP); tar cf - $(EXPORTNAME) \
           | gzip --force --best > $(EXPORTNAME).tar.gz)

sendsubmission:
	echo Subject: $(NAME) submission "(based on version $(VERSION))" \
            > /tmp/submail
	echo >> /tmp/submail
	uuencode $(EXPORTNAME).tar.gz \
             < $(TMP)/$(EXPORTNAME).tar.gz \
	     >> /tmp/submail
	/bin/mail $(SUBMISSIONADDR) < /tmp/submail
	$(RM) /tmp/submail

######################################################################
# Tools

tools/%: tools/%.mll
	$(MAKE) -C tools $*

tools/%: tools/%.ml
	ocamlc -o tools/$* -I $(OCAMLLIBDIR)/labltk labltk.cma tools/$*.ml

######################################################################
# Misc

depend::
	$(MAKE) -C src depend

clean::
	$(RM) -r *.tmp \
	   *.o *.obj *.cmo *.cmx *.cmi core TAGS *~ *.log \
	   *.aux *.log *.dvi *.out *.backup[0-9] obsolete *.bak $(STABLEFLAG)
	$(MAKE) -C doc clean
	$(MAKE) -C tools clean
	$(MAKE) -C src clean
	-find . -name obsolete -exec $(RM) -r {} \;

install:
	$(MAKE) -C src install

installtext:
	$(MAKE) -C src install UISTYLE=text

tools/ask: tools/ask.ml
	$(MAKE) -C tools

src/$(NAME):
	$(MAKE) -C src
