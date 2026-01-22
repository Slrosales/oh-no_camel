

# all: 
# 	ocamlbuild -use-ocamlfind -r -Is frontend -Is back_end main.byte 
# 	mv main.byte mincamlc



all:
	rm -rf _build
	ocamlbuild -use-ocamlfind -r \
		-Is .,frontend,back_end \
		parser_asml.cmo main.byte
	mv main.byte mincamlc

clean:
	rm -rf _build

cleanest: clean
	rm -f mincamlc

id_test:
	ocamlbuild -use-ocamlfind -r -Is test id_test.byte
	mv id_test.byte id_test	


