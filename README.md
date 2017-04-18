# Grain 🌾

Grain is an experimental language developed by [Philip Blair][philip] and
[Oscar Spencer][oscar] for our Compilers final project. The language is an ML-like
functional language which targets [WebAssembly][wasm].

Highlights include:
- First-class (higher-order) functions
- JavaScript FFI
- (limited) DOM interaction

The language contains types for numbers, booleans, tuples, lambdas, strings, and DOM elements.

The primary limitation of the language is the lack of garbage collection. This
is because WebAssembly currently does not support GC natively, and it provides
no means of performing any type of stack introspection.

## Building

To build, have `ocaml` (version >= 4.02.3), `opam`, and `wast2wasm` (from [`wabt`][wabt]) installed
and on your `PATH`.
Then, running `make` will install any needed OCaml dependencies and build the
compiler.

## Running

To create a `.wasm` file, run the following:

```
$ ./grainc /path/to/file.gr -o /path/to/output.wasm
```

For an example of how to run the file in JavaScript, look at the
files under `script`.

[philip]: https://github.com/belph
[oscar]: http://github.com/ospencer
[wasm]: http://webassembly.org/
[wabt]: https://github.com/WebAssembly/wabt
