# Manual

all: doit

UISTYLE=tk
include ../src/Makefile.OCaml

doit: grabnews$(EXEC_EXT) ask$(EXEC_EXT)

grabnews$(EXEC_EXT): grabnews.cmo
	$(OCAMLC) -o $@ $^

ask$(EXEC_EXT): ask.cmo
	$(OCAMLC) $(COMPFLAGS) -custom -o $@ $(GUILIB) $(LINK) $^

clean::
	$(RM) grabnews grabnews.exe grabnews.ml \
              ask ask.exe 

test: ask$(EXEC_EXT)
	date | ./ask 
