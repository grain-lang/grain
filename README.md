<div align="center">
    <a href="https://grain-lang.org/">
        <img src="https://raw.githubusercontent.com/grain-lang/grain/master/grain-logo-large.png" alt="Grain" height="200" />
    </a>
</div>

<div align="center">
    <br>The compiler for the modern web staple.
</div>

# The Grain Compiler

Grain is a strongly-typed functional programming language built for the modern web by leveraging the brilliant work done by the [WebAssembly project](http://webassembly.org/).

For more information about the language, visit [grain-lang.org](https://grain-lang.org/).

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
