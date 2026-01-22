all:
	rm -rf _build
	ocamlbuild -use-ocamlfind -r \
		-Is .,frontend,back_end \
		-package unix \
		parser_asml.cmo main.byte
	mv main.byte mincamlc

clean:
	rm -rf _build

cleanest: clean
	rm -f mincamlc

id_test:
	ocamlbuild -use-ocamlfind -r -Is test id_test.byte
	mv id_test.byte id_test
