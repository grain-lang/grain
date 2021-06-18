<p align="center">
    <a href="https://grain-lang.org/">
        <img src="https://raw.githubusercontent.com/grain-lang/grain/master/grain_shorthand_color.png" alt="Grain" height="200" />
    </a>
</p>

<p align="center">
    <a href="https://github.com/grain-lang/grain/workflows/Grain%20CI%20Workflow/badge.svg">
        <img src="https://img.shields.io/github/workflow/status/grain-lang/grain/Grain%20CI%20Workflow?style=flat-square" alt="Grain CI Workflow">
    </a>
    <a href="https://choosealicense.com/licenses/lgpl-3.0/">
        <img src="https://img.shields.io/badge/License-LGPL%20v3-blue.svg?style=flat-square" alt="License: LGPL v3">
    </a>
    <a href="https://github.com/grain-lang/grain/releases">
        <img alt="Grain latest release version" src="https://img.shields.io/github/package-json/v/grain-lang/grain?color=rgb%28255%2C%20133%2C%2014%29&style=flat-square">
    </a>
</p>

---

Grain is a new programming language that compiles to [WebAssembly](http://webassembly.org/) via [Binaryen](https://github.com/WebAssembly/binaryen). For more information about the language, check out [grain-lang.org](https://grain-lang.org/).

If it's your first time here, we recommended that you follow [the Grain guide](https://grain-lang.org/docs) to get up and running.

## Contributing

There are tons of ways to contribute to Grain. Check out our [contributing guide](https://github.com/grain-lang/grain/blob/master/CONTRIBUTING.md) for more info and come [chat with us on Discord](https://discord.gg/grain-lang)! All contributors are held to our [contributor code of conduct](https://github.com/grain-lang/grain/blob/master/CODE_OF_CONDUCT.md).

## Building

Grain currently only works on Mac and Linux. We are hard at work trying to integrate Binaryen and OCaml on Windows, but this is uncharted territory! Please let us know if you have this low-level expertise. :bow:

To build Grain, you'll need [Node.js](https://nodejs.org/en/download/current/) v14 and [Yarn](https://yarnpkg.com/getting-started/install). To get everything set up, run:

```bash
yarn
yarn compiler build
```

This will set up the Grain runtime, standard library, and CLI.

If running tests is your kind of thing, run

```bash
yarn compiler test
```

If you are using `vscode` as your editor, we recommend you run:

```bash
yarn vscode
```

This will create local settings that point OCaml LSP to our `compiler` directory. Then, you should be able to use the [OCaml Platform](https://github.com/ocamllabs/vscode-ocaml-platform) extension!

### Other Commands

To build the standard library:

```bash
yarn stdlib build
```

To build the runtime:

```bash
yarn runtime build
```

To link the CLI:

```bash
yarn cli link
```

To reset your compiler build:

```bash
yarn compiler clean
```

To navigate tasks available in the system:

```bash
yarn run
```

This will display an interactive session where you can select the project and command you want.

## Running Grain Programs

You can run programs using the Grain CLI:

```bash
echo "print('Hello world')" > hello.gr
grain hello.gr
```

Alternatively, you can invoke the compiler directly:

```bash
grain compile hello.gr
```

and then to run the compiled program:

```bash
grain run hello.gr.wasm
```

Copyright ©️ 2017-2020 Philip Blair, Oscar Spencer, & contributors.
