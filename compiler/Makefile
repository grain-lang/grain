DUNE := $(shell command -v dune 2> /dev/null)

default: check-libs
	dune build

ifndef DUNE
	$(error "dune not found on your PATH. Please install dune before building: opam install dune")
endif

tests:
	cp -r ../stdlib _build/default/stdlib
	# dune runtest

install:
	dune install

check-libs:
	./tools/get-deps.sh

clean:
	dune clean
