
all: 
<<<<<<< HEAD
	ocamlbuild -r -Is back_end main.byte 
=======
	ocamlbuild  -use-ocamlfind -r -Is frontend -Is back_end main.byte 
>>>>>>> 935b25cbf70d15a28b0bc0bb90bacfcb23e33491
	mv main.byte mincamlc

id_test:
	ocamlbuild -use-ocamlfind -r -Is test id_test.byte
	mv id_test.byte id_test	

clean:
	rm -rf _build

cleanest: clean
	rm -f min-ml


