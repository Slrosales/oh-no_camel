
.PHONY: all build launch_test help
all: help

build:
	cd ./ocaml && make

launch_test:
	cd scripts && ./mincaml-test-parser.sh


help:
	@echo "TODO legend"