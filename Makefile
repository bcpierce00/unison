#######################################################################
# $I1: Unison file synchronizer: Makefile $
# $I2: Last modified by bcpierce on Mon, 06 Sep 2004 19:43:55 -0400 $
# $I3: Copyright 1999-2004 (see COPYING for details) $
#######################################################################
.PHONY: all src doc

all: src 

src:
	$(MAKE) -C src

include src/Makefile.OCaml

######################################################################
# Version control

DEVELOPERS = unison@cis.upenn.edu
SUBMISSIONADDR = bcpierce@cis.upenn.edu

PRCS = /home/bcpierce/PRCS

merge:
	prcs merge -r@ -R $(PRCS) --force
	prcs rekey -R $(PRCS) --force

checkin: remembernews merge populate diff 
	prcs checkin -r@ -R $(PRCS) --long-format > .checkin.tmp
	prcs rekey -R $(PRCS) --force
	@$(MAKE) summaryheader
	@cat .checkin.tmp >> .summary.tmp
	@cat .summary.tmp > mail.tmp
	/bin/mail $(DEVELOPERS) < mail.tmp
	-$(RM) .checkin.tmp mail.tmp .summary.tmp .diff.tmp changes.tmp
ifeq ($(shell whoami),bcpierce)
	$(MAKE) nightly
endif

oldcheckin: remembernews merge populate diff 
	prcs checkin -r@ -R $(PRCS) --long-format > .checkin.tmp
	prcs rekey -R $(PRCS) --force
	@$(MAKE) summaryheader
	@cat .checkin.tmp >> .summary.tmp
	@echo >> .summary.tmp
	@echo ------------------------------------------ >> .summary.tmp
	@echo Change details: >> .summary.tmp
	@echo >> .summary.tmp
	@cat .summary.tmp .diff.tmp > mail.tmp
	/bin/mail $(DEVELOPERS) < mail.tmp
	-$(RM) .checkin.tmp mail.tmp .summary.tmp .diff.tmp changes.tmp

summaryheader: 
	@echo Subject: $(NAME) $(VERSION) checkin > .summary.tmp
	@echo >> .summary.tmp
	@cat news.tmp >> .summary.tmp
	@echo >> .summary.tmp
	@echo >> .summary.tmp
	@echo Summary of changes: >> .summary.tmp
	@echo >> .summary.tmp

populate:
	prcs populate -R $(PRCS) --delete --force

diff:
	-prcs diff --revision=@ -R $(PRCS) \
                -P --quiet --new -- --expand-tabs --context=5  -b \
          > .diff.tmp

remembernews: tools/grabnews
	tools/grabnews < unison.prj > news.tmp
	echo "CHANGES FROM VERSION" $(VERSION) > changes.tmp
	echo >> changes.tmp
	cat news.tmp >> changes.tmp
	cp changes.tmp rc.tmp
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

STABLEFLAG=stable.flag
ifeq ($(wildcard $(STABLEFLAG)),$(STABLEFLAG))
  DOWNLOADAREA=stable
  TEXDIRECTIVES+="\\\\stabletrue"
else
  DOWNLOADAREA=beta-test
  TEXDIRECTIVES+="\\\\stablefalse"
endif

export: 
	-$(RM) $(STABLEFLAG)
	$(MAKE) exportcommon

exportstable:
	touch $(STABLEFLAG)
	$(MAKE) exportcommon

exportcommon: tools/ask
	tools/ask tools/exportmsg.txt
	$(MAKE) realexport

EXPORTDIR=$(BCPHOME)/pub/$(NAME)
DOWNLOADPARENT=$(EXPORTDIR)/download/$(DOWNLOADAREA)
FINALDOWNLOADDIR=$(DOWNLOADPARENT)/$(NAME)-$(VERSION)
DOWNLOADDIR=$(FINALDOWNLOADDIR)-underconstruction
EXPORTNAME=$(NAME)-$(VERSION)

# Export separate top-level html pages for some important bits of the docs, 
# e.g. the FAQ.   This is run automatically as part of 'make commitexport',
# but can also be run manually at any time.
exporthtml:

realexport: 
	$(MAKE) $(DOWNLOADDIR)
	$(MAKE) exportdocs 
	$(MAKE) exportsources
	@echo
	cat tools/afterexportmsg.txt

$(DOWNLOADDIR):
	@echo Creating DOWNLOADDIR = $(DOWNLOADDIR)
	@echo
	-mkdir -p $(DOWNLOADDIR)
	-touch $(DOWNLOADDIR)/THIS-IS-UNISON-$(VERSION)

commitexport:
	$(MAKE) realcommit
	$(MAKE) mailchanges

realcommit:
	@echo
	@echo Committing new export directory
	$(MAKE) exporthtml
	-chmod -R a+r $(EXPORTDIR)
	-chmod -R g+wr $(EXPORTDIR)
	-chmod -R o-w $(EXPORTDIR)
	-mkdir $(FINALDOWNLOADDIR)
	mv $(DOWNLOADDIR)/* $(FINALDOWNLOADDIR)/
	-rm -rf $(DOWNLOADDIR)
	-$(RM) $(DOWNLOADPARENT)/latest
	-ln -s $(NAME)-$(VERSION) $(DOWNLOADPARENT)/latest
	$(MAKE) nightly
ifeq ($(wildcard $(STABLEFLAG)),$(STABLEFLAG))
	-$(RM) -f $(EXPORTDIR)/download/beta-test/latest
	ln -s ../stable/latest $(EXPORTDIR)/download/beta-test/latest
endif

TMP=/tmp
exportsources:
	$(RM) -rf $(TMP)/$(EXPORTNAME)
	cp -r src $(TMP)/$(EXPORTNAME)
	-$(RM) $(TMP)/$(EXPORTNAME)/RECENTNEWS
	$(MAKE) -C $(TMP)/$(EXPORTNAME) clean
	(cd $(TMP); tar cvf - $(EXPORTNAME) \
           | gzip --force --best > $(EXPORTNAME).tar.gz)
	mv $(TMP)/$(EXPORTNAME).tar.gz $(DOWNLOADDIR)

exportdocs: 
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

EXPORTTMP=$(TMP)/export-$(OSARCH)x.tmp

exportnative:
	-$(RM) -rf $(EXPORTTMP)
	cp -r src $(EXPORTTMP)
	make realexportnative
ifeq ($(OSARCH),linux)
	make realexportnative EXPORTSTATIC=true KIND=-static
endif
	rm -rf $(EXPORTTMP)

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

mailchanges: tools/ask src/$(NAME)
	@echo To: $(NAME)-announce@egroups.com,$(NAME)-users@egroups.com \
            > mail.tmp
	@echo Subject: $(NAME) $(VERSION) now available >> mail.tmp
	@echo >> mail.tmp
	@echo Download address: >> mail.tmp
	@echo "  " http://www.cis.upenn.edu/~bcpierce/unison/download.html \
           >> mail.tmp
	@echo >> mail.tmp
	@cat src/NEWS >> mail.tmp
	@src/unison -doc news >> mail.tmp
	tools/ask tools/mailmsg.txt 
	@send ./mail.tmp

######################################################################
# Export developer sources  (normally run every night by a cron job on
# saul.cis.upenn.edu; also as a last step of 'make checkin', when performed
# by bcp.  Can also be run manually if needed.

DEVELDIR=$(EXPORTDIR)/download/resources/developers-only

nightly:
	($(RM) -rf $(HOME)/tmp/unison; \
         mkdir $(HOME)/tmp/unison; \
         cd $(HOME)/tmp/unison; \
         prcs checkout -R $(PRCS) -f unison; \
         cp src/RECENTNEWS $(TMP); \
         $(MAKE) exportdevel)

exportdevel: tareverything
	-$(RM) -f $(DEVELDIR)/*
	-mv -f $(TMP)/RECENTNEWS $(DEVELDIR)
	mv $(TMP)/$(EXPORTNAME).tar.gz $(DEVELDIR)

######################################################################
# Submitting changes

submit: tareverything sendsubmission

tareverything:
	$(RM) -rf $(TMP)/$(EXPORTNAME)
	cp -r . $(TMP)/$(EXPORTNAME)
	rm -rf $(TMP)/$(EXPORTNAME)/private
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
	$(RM) -rf *.tmp \
	   *.o *.obj *.cmo *.cmx *.cmi core TAGS *~ *.log \
	   *.aux *.log *.dvi *.out *.backup[0-9] obsolete *.bak $(STABLEFLAG)
	$(MAKE) -C src clean
	$(MAKE) -C doc clean
	$(MAKE) -C tools clean
	-find . -name obsolete -exec $(RM) -rf {} \;

install:
	$(MAKE) -C src install

tools/ask: tools/ask.ml
	$(MAKE) -C tools

src/$(NAME):
	$(MAKE) -C src
