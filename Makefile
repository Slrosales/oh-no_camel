
.PHONY: all build launch_test help cleanest
all: help 

build:
	cd ./ocaml && make

launch_test:
	cd scripts && ./mincaml-test-parser.sh

cleanest:
	cd ./ocaml && make cleanest

help:
	@echo "Help:"
	@echo "  -  make build: compiling the compilater"
	@echo "  -  make launch_test: launch ths script test store int he scripts folder"
	@echo "  -  make cleanest: clean the compilater"