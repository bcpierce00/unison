all: relay start_editor

OCAMLC = ocamlfind ocamlc

relay: relay.ml
	$(OCAMLC) -o relay -linkpkg -package lwt relay.ml -cclib -s

start_editor : editor.ml
	$(OCAMLC) -o start_editor -linkpkg -package lwt editor.ml -cclib -s

clean:
	rm -f *.cmi *.cmo *~ relay start_editor
