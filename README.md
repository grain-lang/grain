<div align="center">
    <img src="https://github.com/grain-lang/grain/raw/master/grain-header.jpg" alt="Grain" height="350" />
</div>

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

### Compiler

To build the compiler, have `ocaml` (version >= 4.02.3), `opam`, and `jbuilder` installed
and on your `PATH`.
Then, running `make` will install any needed OCaml dependencies and build the
compiler.

### Runtime

To build the runtime, have `node` (version >= 9) and `npm` (version >= 5) installed. Then,

```sh
cd runtime
npm install
npm run build
```

## Running

To create a `.wasm` file, run the following:

```sh
$ ./grainc /path/to/file.gr -o /path/to/output.wasm
```

For an example of how to run the file in JavaScript, look at the
files under `script`.
You'll need to create a symlink from `script/public/javascripts/grain-runtime.js` to the built file in `runtime/dist/grain-runtime.js`, or you'll have to just copy it over to run the example.

[philip]: https://github.com/belph
[oscar]: http://github.com/ospencer
[wasm]: http://webassembly.org/
