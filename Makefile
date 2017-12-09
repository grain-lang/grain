JBUILDER := $(shell command -v jbuilder &> /dev/null)

ifndef JBUILDER
	$(error "jbuilder not found on your PATH. Please install jbuilder before building: opam install jbuilder")
endif

default: check-libs
	jbuilder build

tests:
	jbuilder runtest

install:
	jbuilder install

check-libs:
	jbuilder external-lib-deps --missing @install

clean:
	jbuilder clean

EXAMPLEFILES=adder.gr lambda.gr domSimple.gr dom.gr
EXAMPLES=$(patsubst %.gr,script/public/examples/%.wasm,$(EXAMPLEFILES))
script/public/examples/%.wasm: test/input/%.gr default
	_build/install/default/bin/grainc -g $< -o $@

.PHONY: examples
examples: $(EXAMPLES)

