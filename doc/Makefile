 # Manual

.PHONY: all
all: unison-manual.pdf unison-manual.html unison-manual.txt ../src/strings.ml

DRAFT = false

-include ../src/Makefile.ProjectInfo

SOURCES = unison-manual.tex \
          local.tex short.tex \
          unisonversion.tex prefs.tmp prefsdocs.tmp
FORMATS = html pdf txt dvi ps info
FORMATS_AUX = dtxt aux htoc toc

$(addprefix unison-manual., $(FORMATS) $(FORMATS_AUX)): $(SOURCES)

unison-manual-directives.tex unison-manual-text-directives.tex:

ifneq ($(strip $(shell hevea -version)),)
  HEVEA=true
endif

unisonversion.tex: ../src/Makefile.ProjectInfo
	echo '\def\unisonversion{$(VERSION)}' > $@

# Text versions: when using different directives then _must_ use different
# jobname (or, output name for hevea) so that files for non-text versions are
# not overwritten. Here, suffix -text is used.

%-text-directives.tex:
	printf '$(TEXDIRECTIVES)\\textversiontrue\\draft$(DRAFT)' > $@

%.dtxt: %.tex %-text-directives.tex
ifeq ($(HEVEA),true)
	hevea -o $*-text.html $<
	(TERM=vt100; export TERM; lynx -display_charset=utf8 -dump $*-text.html > $@)
endif

%.txt: %.dtxt
ifeq ($(HEVEA),true)
	sed -e "/^----SNIP----/,+2 d" -e "/^Junk/,$$ d" $< > $@
endif

../src/strings.ml: unison-manual.dtxt docs.ml
ifeq ($(HEVEA),true)
	ocaml docs.ml < $< > $@
endif

%-directives.tex:
	printf '$(TEXDIRECTIVES)\\textversionfalse\\draft$(DRAFT)' > $@

# (pdf)latex must be run multiple times to generate toc and correct references

%.aux %.htoc: %.tex %-directives.tex
	pdflatex -draftmode $<
	pdflatex -draftmode $<

%.pdf: %.tex %-directives.tex %.aux
	pdflatex $<

%.ps: %.pdf
	pdf2ps $<

%.html: %.tex %-directives.tex %.htoc
ifeq ($(HEVEA),true)
	hevea $<
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
	   *-directives.tex \
	   junk.* *.htoc *.toc *.bak \
	   prefs.tmp prefsdocs.tmp \
	   docs docs.exe temp.dvi temp.html unison-manual.html \
	   unison-manual.dvi unison-manual.ps unison-manual.pdf \
	   unison-manual.txt unison-manual.info* unisonversion.tex \
	   unison-manual-text.html \
	   contact.html faq.html faq.haux index.html

