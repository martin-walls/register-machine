machine.native: machine.ml parse.ml instructions.ml
	ocamlbuild machine.native -lib str

.PHONY: clean
clean:
	ocamlbuild -clean
