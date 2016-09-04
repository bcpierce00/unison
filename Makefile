.PHONY: all src

default: src

text:
	$(MAKE) -C src UISTYLE=text

test:
	./src/unison -selftest

all: src

src:
	$(MAKE) -C src

-include src/Makefile.ProjectInfo

docs:
	$(MAKE) -C src UISTYLE=text
	$(MAKE) -C doc

include src/Makefile.OCaml

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
  BCPHOME=$(HOME)
endif
endif

EXPORTDIR=$(BCPHOME)/pub/$(NAME)
DOWNLOADAREA=releases
DOWNLOADPARENT=$(EXPORTDIR)/download/$(DOWNLOADAREA)
REALDOWNLOADDIR=$(DOWNLOADPARENT)/$(NAME)-$(VERSION)
BRANCH=$(MAJORVERSION)
EXPORTNAME=$(NAME)-$(VERSION)
# OSX/linux portability
ifeq ($(OSARCH),osx)
	TMP=$(shell mktemp -d -t unison)
else
	TMP=$(shell mktemp -d)
endif
DOWNLOADDIR=/tmp/$(NAME)-$(VERSION)
# DOWNLOADDIR=$(REALDOWNLOADDIR)

# Do this when it's time to create a new beta-release from the development trunk
beta: 
	@echo "Makefile needs fixing"
	@exit 1
	@tools/ask tools/exportmsg.txt
	(cd ..; svn copy trunk branches/$(BRANCH))
	(cd ../branches/$(BRANCH); svn commit -m "New release branch")
	@echo
	@echo "Press RETURN to export it... "
	@read JUNK
	$(MAKE) -C ../branches/$(BRANCH) export

# Do this in a release branch to export a new tarball (e.g., after fixing a bug)
export:
	@echo
	@echo "CHECKLIST:"
	@echo "  - Bump version number in src/Makefile.ProjectInfo"
	@echo "  - Move everything interesting from src/RECENTNEWS to doc/changes.tex"
	@echo "  - Do 'make checkin'"
	@echo ""
	@echo "If all this is done, hit RETURN (otherwise Ctrl-C and do it)"
	@read JUNK
	$(MAKE) $(DOWNLOADDIR)
	$(MAKE) exportdocs
	$(MAKE) exportsources
	(cd $(DOWNLOADDIR); genindexhtml)
	@echo
	@echo "OK to commit?  Press RETURN if yes (Crtl-C if no)..."
	@read JUNK
	$(MAKE) commitexport

commitexport:
	$(MAKE) realcommit
	$(MAKE) mailchanges

realcommit:
	@echo
	@echo Committing new export directory
	mv $(DOWNLOADDIR) $(REALDOWNLOADDIR)
	-chmod -R a+r $(EXPORTDIR)
	-chmod -R g+wr $(EXPORTDIR)
	-chmod -R o-w $(EXPORTDIR)
	-$(RM) $(DOWNLOADPARENT)/beta
	-ln -s $(EXPORTNAME) $(DOWNLOADPARENT)/beta
	(cd $(DOWNLOADPARENT); genindexhtml)

$(DOWNLOADDIR):
	@echo Creating DOWNLOADDIR = $(DOWNLOADDIR)
	@echo
	-mkdir -p $(DOWNLOADDIR)

exportsources:
	git archive --output $(DOWNLOADDIR)/$(EXPORTNAME).tar.gz -- HEAD src

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

mailchanges: 
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
	@echo "Announcement draft can be found in $(MAILTMP)"

######################################################################
# Export binary for the current architecture
# (this stuff is all probably dead)

EXPORTTMP=$(TMP)/export-$(OSARCH)x.tmp

exportnative:
	-$(RM) -r $(EXPORTTMP)
	cp -r src $(EXPORTTMP)
	$(MAKE) realexportnative
ifeq ($(OSARCH),linux)
	$(MAKE) realexportnative EXPORTSTATIC=true KIND=-static
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
	$(MAKE) -C $(EXPORTTMP) UISTYLE=gtk2 DEBUGGING=false \
                       NATIVE=$(EXPORTNATIVE) STATIC=$(EXPORTSTATIC)
	cp $(EXPORTTMP)/$(NAME)$(EXEC_EXT) \
               $(DOWNLOADDIR)/$(EXPORTNAME).$(OSARCH)$(KIND)-gtkui$(EXEC_EXT)
	gzip --best --force -c \
            $(DOWNLOADDIR)/$(EXPORTNAME).$(OSARCH)$(KIND)-gtkui$(EXEC_EXT) \
          > $(DOWNLOADDIR)/$(EXPORTNAME).$(OSARCH)$(KIND)-gtkui$(EXEC_EXT).gz


######################################################################
# Version control

checkin: logmsg remembernews
	git commit -a --file=logmsg
	$(RM) logmsg
	@echo 
	@echo "Remember to do:"
	@echo "   git pull && git push"

remembernews: logmsg
	echo "CHANGES FROM VERSION" $(VERSION) > rc.tmp
	echo >> rc.tmp
	cat logmsg >> rc.tmp
	echo  >> rc.tmp
	echo    ------------------------------- >> rc.tmp
	-cat src/RECENTNEWS >> rc.tmp
	mv -f rc.tmp src/RECENTNEWS

######################################################################
# Misc

depend::
	$(MAKE) -C src depend

clean::
	$(RM) -r *.tmp \
	   *.o *.obj *.cmo *.cmx *.cmi core TAGS *~ *.log \
	   *.aux *.log *.dvi *.out *.backup[0-9] *.bak $(STABLEFLAG)
	$(MAKE) -C doc clean
	$(MAKE) -C src clean

install:
	$(MAKE) -C src install

installtext:
	$(MAKE) -C src install UISTYLE=text

src/$(NAME):
	$(MAKE) -C src

