machine.byte: machine.ml parse.ml instructions.ml
	ocamlbuild machine.byte -lib str

.PHONY: clean
clean:
	ocamlbuild -clean
