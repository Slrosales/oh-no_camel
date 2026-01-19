
all: 
	ocamlbuild -use-ocamlfind -r -Is frontend -Is back_end main.byte 
	mv main.byte mincamlc

clean:
	rm -rf _build

cleanest: clean
	rm -f mincamlc
