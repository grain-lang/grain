<div align="center">
    <a href="https://grain-lang.org/">
        <img src="https://raw.githubusercontent.com/grain-lang/grain/master/grain_shorthand_color.png" alt="Grain" height="200" />
    </a>
</div>

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![GitHub version](https://badge.fury.io/gh/grain-lang%2Fgrain.svg)](https://badge.fury.io/gh/grain-lang%2Fgrain)

# The Grain Compiler

Grain is a strongly-typed functional programming language built for the modern web by leveraging the brilliant work done by the [WebAssembly project](http://webassembly.org/).

This language is still a work in progress, but be sure to stay tuned, or even contribute!

For more information about the language, visit [grain-lang.org](https://grain-lang.org/).

## Building

To build Grain, you'll need `yarn` and Node.js version 10 or higher. To get everything set up, run:

```bash
yarn
yarn setup
yarn compiler:build
```

This will set up the Grain runtime, standard library, and CLI.

To put the Grain compiler binaries on your path, run:

```bash
yarn compiler:install
```

## Running Grain Programs

You can run programs using the Grain CLI:

```bash
echo "print('Hello world')" > hello.gr
grain hello.gr
```

Alternatively, you can invoke the compiler directly:

```bash
grainc hello.gr
```

Copyright ©️ 2017-2020 Philip Blair and Oscar Spencer.

[philip]: https://github.com/belph
[oscar]: http://github.com/ospencer
[wasm]: http://webassembly.org/
