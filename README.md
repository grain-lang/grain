<p align="center">
    <a href="https://grain-lang.org/">
        <img src="https://raw.githubusercontent.com/grain-lang/grain/master/grain_shorthand_color.png" alt="Grain" height="200" />
    </a>
</p>

<p align="center">
    <a target="_blank" rel="noopener noreferrer" href="https://github.com/grain-lang/grain/workflows/Grain%20CI%20Workflow/badge.svg"><img src="https://github.com/grain-lang/grain/workflows/Grain%20CI%20Workflow/badge.svg" alt="Grain CI Workflow" style="max-width:100%;"></a>
    <a href="https://www.gnu.org/licenses/lgpl-3.0" rel="nofollow"><img src="https://camo.githubusercontent.com/4e8beb53bf7fc54e0addd2106a833503fc81a083/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f4c6963656e73652d4c47504c25323076332d626c75652e737667" alt="License: LGPL v3" data-canonical-src="https://img.shields.io/badge/License-LGPL%20v3-blue.svg" style="max-width:100%;"></a>
    <a href="https://badge.fury.io/gh/grain-lang%2Fgrain" rel="nofollow"><img src="https://camo.githubusercontent.com/909d4481f00303c64c5b47ae8d62b791726960d3/68747470733a2f2f62616467652e667572792e696f2f67682f677261696e2d6c616e67253246677261696e2e737667" alt="GitHub version" data-canonical-src="https://badge.fury.io/gh/grain-lang%2Fgrain.svg" style="max-width:100%;"></a>
</p>

---

Grain is a new programming language that compiles to [WebAssembly](http://webassembly.org/) via [Binaryen](https://github.com/WebAssembly/binaryen). For more information about the language, check out [grain-lang.org](https://grain-lang.org/).

If it's your first time here, we recommended that you follow [the Grain guide](https://grain-lang.org/docs) to get up and running.

## Contributing

There are tons of ways to contribute to Grain. Check out our [contributing guide](https://github.com/grain-lang/grain/blob/master/CONTRIBUTING.md) for more info and come [chat with us on Discord](https://discord.gg/7U3newJ)! All contributors are held to our [contributor code of conduct](https://github.com/grain-lang/grain/blob/master/CODE_OF_CONDUCT.md).

## Building

To build Grain, you'll need [Node.js](https://nodejs.org/en/download/current/) v14, [Yarn](https://yarnpkg.com/getting-started/install), and [CMake](https://cgold.readthedocs.io/en/latest/first-step/installation.html). To get everything set up, run:

```bash
yarn
yarn setup
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
grainc hello.gr
```

Copyright ©️ 2017-2020 Philip Blair, Oscar Spencer, & contributors.
