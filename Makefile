UNAME := $(shell uname)
DEBUG=yes

ifeq ($(UNAME), Linux)
ifeq ($(DEBUG),)
	FORMAT=aout
else
  FORMAT=elf
endif
  PIE=
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
  PIE=
endif
endif

OCAMLFIND_PKGS=oUnit,extlib,batteries,cmdliner,ocamlgraph,wasm,stdint
PKGS=unix,$(OCAMLFIND_PKGS)
OPAM_PKGS=ounit,extlib,batteries,cmdliner,ocamlgraph,wasm,stdint
BUILD=ocaml setup.ml -build -r -use-ocamlfind


grainc: src/*.ml src/parser.mly src/lexer.mll
	make setup.data
	$(BUILD) -no-hygiene -package $(PKGS) src/grainc.native
	rm grainc.byte
	rm test.byte
	mv grainc.native grainc

test: src/*.ml src/parser.mly src/lexer.mll grainc
	make setup.data
	$(BUILD) -no-hygiene -package $(PKGS) src/test.native
	rm grainc.byte
	rm test.byte
	mv test.native test


setup.ml: check-libs
	oasis setup

setup.data: setup.ml
	ocaml $< -configure

.PHONY: check-libs
check-libs:
	@echo Checking that Oasis is installed...
	command -v oasis >/dev/null 2>&1 || opam install oasis
	./check-installed.sh $(OCAMLFIND_PKGS) $(OPAM_PKGS)


# cutest-1.5/CuTest.o: cutest-1.5/CuTest.c cutest-1.5/CuTest.h
# 	gcc -m32 cutest-1.5/CuTest.c -c -g -o cutest-1.5/CuTest.o

# gctest: gctest.o gc.o cutest-1.5/CuTest.o cutest-1.5/CuTest.h
# 	gcc -m32 cutest-1.5/AllTests.c cutest-1.5/CuTest.o gctest.o gc.o -o gctest


clean:
	ocamlbuild -clean
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log *.o *.byte
	rm -rf _build/
	rm -f grainc test .installed-pkgs
	rm -f setup.ml setup.data myocamlbuild.ml

EXAMPLEFILES=adder.gr lambda.gr domSimple.gr dom.gr
EXAMPLES=$(patsubst %,script/public/examples/%,$(EXAMPLEFILES))
script/public/examples/%.wasm: input/%.gr grainc
	./grainc -g $< -o $@

.PHONY: examples
examples: $(EXAMPLES)

.PHONY: install
install: grainc
	cp $< /usr/bin
