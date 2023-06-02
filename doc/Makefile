 # Manual

.PHONY: all
all: unison-manual.html

DRAFT = false

-include ../src/Makefile.ProjectInfo

STRINGS = ../icons/Unison.gif
SOURCES = unison-manual.tex \
          local.tex short.tex

ifneq ($(strip $(shell hevea -version)),)
  HEVEA=true
endif

unison-manual.html: $(SOURCES)
	$(MAKE) real

# If the .aux file is missing, do 'make once' an extra time to make
# sure it's present (with section numbers, etc.) before we build the
# manual for real.
.PHONY: real
real:
	@(if [ ! -f unison-manual.aux ]; then $(MAKE) once; fi)
	$(MAKE) once

.PHONY: once
once: $(STRINGS) docs.ml prefs.tmp prefsdocs.tmp
	echo '\def\unisonversion{$(VERSION)}' > unisonversion.tex
ifeq ($(HEVEA),true)
	printf '$(TEXDIRECTIVES)\\textversiontrue\\draft$(DRAFT)' \
           > texdirectives.tex
	hevea unison-manual.tex
	(TERM=vt100; export TERM; lynx -display_charset=utf8 -dump unison-manual.html > unison-manual.dtxt)
	sed -e "/^----SNIP----/,+2 d" -e "/^Junk/,$$ d" unison-manual.dtxt > unison-manual.txt
	ocaml docs.ml < unison-manual.dtxt > ../src/strings.ml
endif
	printf '$(TEXDIRECTIVES)\\textversionfalse\\draft$(DRAFT)' > texdirectives.tex
	pdflatex -draftmode unison-manual.tex
	pdflatex -draftmode unison-manual.tex
	pdflatex unison-manual.tex
	pdf2ps unison-manual.pdf
ifeq ($(HEVEA),true)
	hevea unison-manual.tex
endif

# Listing of preferences
prefs.tmp: ../src/$(NAME)$(EXEC_EXT)
	-../src/$(NAME)$(EXEC_EXT) -help > prefs.tmp
prefsdocs.tmp: ../src/$(NAME)$(EXEC_EXT)
	-../src/$(NAME)$(EXEC_EXT) -prefsdocs 2> prefsdocs.tmp

../src/$(NAME)$(EXEC_EXT):
	$(MAKE) -C ../src tui

.PHONY: clean
clean:
	$(RM) -r \
	   *.dtxt *.aux *.haux *.log *.out \
	   texdirectives.tex \
	   junk.* *.htoc *.toc *.bak \
	   prefs.tmp prefsdocs.tmp \
	   docs docs.exe temp.dvi temp.html unison-manual.html \
	   unison-manual.dvi unison-manual.ps unison-manual.pdf \
	   unison-manual.txt unison-manual.info* unisonversion.tex \
	   contact.html faq.html faq.haux index.html

