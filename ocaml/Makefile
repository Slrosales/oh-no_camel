
all: 
	ocamlbuild -use-ocamlfind -r -Is frontend -Is back_end main.byte 
	mv main.byte mincamlc


id_test:
	ocamlbuild -use-ocamlfind -r -Is test id_test.byte
	mv id_test.byte id_test	

clean:
	rm -rf _build

cleanest: clean
	rm -f mincamlc
