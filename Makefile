# http://mmottl.github.io/ocaml-makefile/

SOURCES := gfx.ml planar.ml
RESULT  := planar

PACKS := tsdl

all: native-code

-include OCamlMakefile

run: planar
	./planar
