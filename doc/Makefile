 # Manual

# IMPORTANT!
#
# This file is portable and compatible with GNU Make, BSD make and
# Solaris (d)make. Do not make any changes in this file unless you
# are certain that the changes do not break this portability.


# a shorthand just for brevity
m = unison-manual

.PHONY: all
all: $(m).pdf $(m).html $(m).txt ../src/strings.ml

DRAFT = false

include ../src/Makefile.ProjectInfo

SOURCES = unison-manual.tex \
          local.tex short.tex \
          unisonversion.tex prefs.tmp prefsdocs.tmp

$(m).pdf $(m).html $(m).txt $(m).ps: $(SOURCES)
$(m).dtxt $(m).aux $(m).htoc $(m).toc: $(SOURCES)

HEVEA_FOUND = test -n "$$(command -v hevea > /dev/null 2>&1 && echo true)"

unisonversion.tex: ../src/Makefile.ProjectInfo
	echo '\def\unisonversion{$(VERSION)}' > $@

# Text versions: when using different directives then _must_ use different
# jobname (or, output name for hevea) so that files for non-text versions are
# not overwritten. Here, suffix -text is used.

$(m)-text-directives.tex:
	printf '$(TEXDIRECTIVES)\\textversiontrue\\draft$(DRAFT)' > $@

.SUFFIXES:
.SUFFIXES: .tex .pdf .ps .html .dtxt .txt

$(m).dtxt: $(m)-text-directives.tex

.tex.dtxt:
	if $(HEVEA_FOUND) ; then \
	  hevea -o $*-text.html $< ; \
	  (TERM=vt100; export TERM; lynx -display_charset=utf8 -dump $*-text.html > $@) \
	; fi

.dtxt.txt:
	if $(HEVEA_FOUND) ; then \
	  sed -e "/^----SNIP----/,+2 d" -e "/^Junk/,$$ d" $< > $@ \
	; fi

../src/strings.ml: $(m).dtxt docs.ml
	if $(HEVEA_FOUND) ; then \
	  ocaml docs.ml < $(m).dtxt > $@ \
	; fi

$(m)-directives.tex:
	printf '$(TEXDIRECTIVES)\\textversionfalse\\draft$(DRAFT)' > $@

# (pdf)latex must be run multiple times to generate toc and correct references

$(m).aux $(m).htoc &: $(m).tex $(m)-directives.tex
	pdflatex -draftmode $(m).tex && \
	pdflatex -draftmode $(m).tex
# &: is the GNU Make group target separator. Other make implementations do not
# support it but accept it nevertheless because it's treated as a target named &
# Without group targets, the same outcome is achieved by the following:
.ORDER: $(m).aux $(m).htoc                     # BSD make
.NO_PARALLEL$(.MAKE.PID): $(m).aux $(m).htoc   # Solaris make
    # BSD make interprets .NO_PARALLEL as the entire makefile not parallel.
    # .MAKE.PID (which is only defined by BSD make) is in there just to prevent
    # BSD make from interpreting this rule

$(m).pdf: $(m).tex $(m)-directives.tex $(m).aux

.tex.pdf:
	pdflatex $<

.pdf.ps:
	pdf2ps $<

$(m).html: $(m).tex $(m)-directives.tex $(m).htoc

.tex.html:
	if $(HEVEA_FOUND) ; then \
	  hevea $< \
	; fi

# Listing of preferences
prefs.tmp: ../src/$(NAME)
	-../src/$(NAME) -help > prefs.tmp
prefsdocs.tmp: ../src/$(NAME)
	../src/$(NAME) -prefsdocs 2> prefsdocs.tmp

../src/$(NAME):
	$(MAKE) -C ../src tui

RM = rm -f

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

