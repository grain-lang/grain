DUNE := $(shell command -v dune 2> /dev/null)

default: check-libs
	dune build

ifndef DUNE
	$(error "dune not found on your PATH. Please install dune before building: opam install dune")
endif

tests:
	dune runtest

install:
	dune install

check-libs:
	./tools/get-deps.sh

clean:
	dune clean

EXAMPLEFILES=adder.gr lambda.gr domSimple.gr dom.gr
EXAMPLES=$(patsubst %.gr,script/public/examples/%.wasm,$(EXAMPLEFILES))
script/public/examples/%.wasm: test/input/%.gr default
	_build/install/default/bin/grainc -g $< -o $@

.PHONY: examples
examples: $(EXAMPLES)

