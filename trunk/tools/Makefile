# Manual

all: doit

UISTYLE=tk
-include ../src/Makefile.ProjectInfo
include ../src/Makefile.OCaml

doit: ask$(EXEC_EXT)

ask$(EXEC_EXT): ask.cmo
	$(OCAMLC) $(COMPFLAGS) -custom -o $@ $(GUILIB) $(LINK) $^

clean::
	$(RM) ask ask.exe

test: ask$(EXEC_EXT)
	date | ./ask

../src/Makefile.ProjectInfo: ../src/mkProjectInfo
	../src/mkProjectInfo > $@

../src/mkProjectInfo: ../src/mkProjectInfo.ml
	ocamlc -o $@ $^
