 # Manual

all:: unison-manual.html

-include ../src/Makefile.ProjectInfo
include ../src/Makefile.Ocaml

STRINGS = ../icons/Unison.gif
SOURCES = unison-manual.tex changes.tex \
          local.tex short.tex postproc$(EXEC_EXT) Makefile

# ifeq ($(shell ls /usr/local/bin/hevea), /usr/local/bin/hevea)
#   HEVEAPATH = /usr/local/bin/hevea
# endif
ifeq ($(shell echo type -t hevea | bash), file)
  HEVEAPATH = $(shell echo type -p hevea | bash)
endif
ifneq ($(HEVEAPATH),)
  HEVEA=true
endif

unison-manual.html: $(SOURCES)
	$(MAKE) real

# If the .aux file is missing, do 'make once' an extra time to make
# sure it's present (with section numbers, etc.) before we build the
# manual for real.
real:
	@(if [ ! -f unison-manual.aux ]; then $(MAKE) once; fi)
	$(MAKE) once

once: postproc$(EXEC_EXT) $(STRINGS) docs$(EXEC_EXT)
	@echo HEVEAPATH = $(HEVEAPATH)
	@(if [ ! -f prefs.tmp ]; then $(MAKE) prefs.tmp; fi)
	@(if [ ! -f prefsdocs.tmp ]; then $(MAKE) prefsdocs.tmp; fi)
ifdef HEVEA
	printf '$(TEXDIRECTIVES)\\textversiontrue\\draftfalse' \
           > texdirectives.tex
	latex unison-manual.tex
	hevea unison-manual.tex
	./postproc < unison-manual.html > temp.html
	(TERM=vt100; export TERM; lynx -dump temp.html > unison-manual.dtxt)
	./docs$(EXEC_EXT)
endif
	printf '$(TEXDIRECTIVES)\\textversionfalse' > texdirectives.tex
	latex unison-manual.tex
	latex unison-manual.tex
	cp unison-manual.dvi temp.dvi
	dvips -t letter -o unison-manual.ps unison-manual.dvi
	dvips -t letter -z -Ppdf -G0 -D600 unison-manual.dvi -o junk.ps
	ps2pdf junk.ps unison-manual.pdf
ifdef HEVEA
	hevea unison-manual.tex
endif

# Index file for web dir
index.html: index.tex local.tex short.tex Makefile
	printf '$(TEXDIRECTIVES)\\draftfalse' > texdirectives.tex
	hevea index.tex

# Contacts file
contact.html: contact.tex local.tex Makefile contactsbody.tex
	printf '$(TEXDIRECTIVES)\\draftfalse' > texdirectives.tex
	hevea contact.tex

# FAQ
faq.html: faq.tex local.tex Makefile faqbody.tex
	printf '$(TEXDIRECTIVES)\\draftfalse' > texdirectives.tex
	hevea faq.tex

# Listing of preferences
prefs.tmp:
	-(if [ -f ../src/$(NAME)$(EXEC_EXT) ]; then \
           ../src/unison -help > prefs.tmp; \
           fi)
prefsdocs.tmp:
	-(if [ -f ../src/$(NAME)$(EXEC_EXT) ]; then \
           ../src/unison -prefsdocs 2> prefsdocs.tmp; \
           fi)

# Tools
docs$(EXEC_EXT): docs.ml
	$(OCAMLC) -o $@ $^

postproc$(EXEC_EXT) : postproc.ml
	ocamlopt -o $@ $^

%.ml : %.mll
	-$(RM) $@
	ocamllex $<

clean::
	$(RM) -r \
	   *.dtxt *.aux *.log *.out \
	   texdirectives.tex \
	   junk.* *.htoc *.toc *.bak \
	   docs docs.exe temp.dvi temp.html unison-manual.html \
	   postproc postproc.exe postproc.ml obsolete \
	   unison-manual.dvi unison-manual.ps unison-manual.pdf

../src/Makefile.ProjectInfo: ../src/mkProjectInfo
	../src/mkProjectInfo > $@

../src/mkProjectInfo: ../src/mkProjectInfo.ml
	ocamlc -o $@ $^
