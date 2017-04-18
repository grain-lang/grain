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


main: src/*.ml src/parser.mly src/lexer.mll
	make setup.data
	$(BUILD) -no-hygiene -package $(PKGS) src/main.native
	rm main.byte
	rm test.byte
	mv main.native main

test: src/*.ml src/parser.mly src/lexer.mll main
	make setup.data
	$(BUILD) -no-hygiene -package $(PKGS) src/test.native
	rm main.byte
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
	rm -f main test .installed-pkgs
	rm -f setup.ml setup.data myocamlbuild.ml

submission-indigo.zip: *.ml *.mll *.mly check-installed.sh Makefile *.c *.h lib/* input/*.indigo _tags
	zip $@ $^
	(cd .. && ./test-dist.sh starter-indigo/$@)

.PHONY: dist
dist: submission-indigo.zip
