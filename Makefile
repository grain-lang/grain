JBUILDER := $(shell command -v jbuilder 2> /dev/null)

default: check-libs
	jbuilder build

ifndef JBUILDER
	$(error "jbuilder not found on your PATH. Please install jbuilder before building: opam install jbuilder")
endif

tests:
	jbuilder runtest

install:
	jbuilder install

check-libs:
	./tools/get-deps.sh

clean:
	jbuilder clean

EXAMPLEFILES=adder.gr lambda.gr domSimple.gr dom.gr
EXAMPLES=$(patsubst %.gr,script/public/examples/%.wasm,$(EXAMPLEFILES))
script/public/examples/%.wasm: test/input/%.gr default
	_build/install/default/bin/grainc -g $< -o $@

.PHONY: examples
examples: $(EXAMPLES)

