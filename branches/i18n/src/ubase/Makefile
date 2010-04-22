NAME = ubase

OBJECTS  = \
  ugettext.cmo \
  safelist.cmo uprintf.cmo util.cmo uarg.cmo prefs.cmo trace.cmo rx.cmo \
  myMap.cmo

OCAMLC = ocamlfind ocamlc -g 
OCAMLOPT = ocamlfind ocamlopt 
OCAMLDEP = ocamlfind ocamldep

XOBJECTS = $(OBJECTS:cmo=cmx)

ARCHIVE  = $(NAME).cma
XARCHIVE = $(NAME).cmxa

REQUIRES = gettext-stub camlp4.macro
PREDICATES =
SYNTAX = -syntax camlp4o

all: $(ARCHIVE) 
opt: $(XARCHIVE)

$(ARCHIVE): $(OBJECTS)
	$(OCAMLC) -a -o $(ARCHIVE) -package "$(REQUIRES)" -linkpkg \
	          -predicates "$(PREDICATES)" $(OBJECTS)
$(XARCHIVE): $(XOBJECTS)
	$(OCAMLOPT) -a -o $(XARCHIVE) -package "$(REQUIRES)" -linkpkg \
	          -predicates "$(PREDICATES)" $(XOBJECTS)

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	          $(SYNTAX) \
	          -c $<
.mli.cmi:
	$(OCAMLC) -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	          $(SYNTAX) \
	          -c $<
.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	          $(SYNTAX) \
	          -c $<

depend: *.ml *.mli
	$(OCAMLDEP) -package "$(REQUIRES)" \
	          $(SYNTAX) \
		  *.ml *.mli > depend
include depend

install: all
	{ test ! -f $(XARCHIVE) || extra="$(XARCHIVE) "`basename $(XARCHIVE) .cmxa`.a; }; \
	ocamlfind install $(NAME) *.mli *.cmi $(ARCHIVE) META $$extra

uninstall:
	ocamlfind remove $(NAME)

clean::
	rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o *~ *.bak

# Used by BCP to update Harmony's copy of these files from Unison's
update:
	cp $(HOME)/current/unison/trunk/src/ubase/{*.ml,*.mli,Makefile} .
