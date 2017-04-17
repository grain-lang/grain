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
BUILD=ocamlbuild -r -use-ocamlfind

main: *.ml parser.mly lexer.mll
	make check-libs
	$(BUILD) -no-hygiene -package $(PKGS) main.native
	mv main.native main

test: *.ml parser.mly lexer.mll main
	make check-libs
	$(BUILD) -no-hygiene -package $(PKGS) test.native
	mv test.native test

.PHONY: check-libs
check-libs:
	./check-installed.sh $(OCAMLFIND_PKGS) $(OPAM_PKGS)

output/%.run: output/%.o main.c 
	clang $(PIE) -mstackrealign -g -m32 -o $@ gc.o main.c $<

output/%.o: output/%.s
ifeq ($(DEBUG),)
	nasm -f $(FORMAT) -o $@ $<
else
	nasm -f $(FORMAT) -g -F dwarf -o $@ $<
endif

.PRECIOUS: output/%.s
output/%.s: input/%.indigo main
	./main $< -o $@



# cutest-1.5/CuTest.o: cutest-1.5/CuTest.c cutest-1.5/CuTest.h
# 	gcc -m32 cutest-1.5/CuTest.c -c -g -o cutest-1.5/CuTest.o

# gctest: gctest.o gc.o cutest-1.5/CuTest.o cutest-1.5/CuTest.h
# 	gcc -m32 cutest-1.5/AllTests.c cutest-1.5/CuTest.o gctest.o gc.o -o gctest


clean:
	ocamlbuild -clean
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log *.o
	rm -rf _build/
	rm -f main test .installed-pkgs

submission-indigo.zip: *.ml *.mll *.mly check-installed.sh Makefile *.c *.h lib/* input/*.indigo _tags
	zip $@ $^
	(cd .. && ./test-dist.sh starter-indigo/$@)

.PHONY: dist
dist: submission-indigo.zip
