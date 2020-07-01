<div align="center">
    <a href="https://grain-lang.org/">
        <img src="https://raw.githubusercontent.com/grain-lang/grain/master/grain_shorthand_color.png" alt="Grain" height="200" />
    </a>
</div>

![Grain CI Workflow](https://github.com/grain-lang/grain/workflows/Grain%20CI%20Workflow/badge.svg)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![GitHub version](https://badge.fury.io/gh/grain-lang%2Fgrain.svg)](https://badge.fury.io/gh/grain-lang%2Fgrain)

# The Grain Compiler

Grain is a new programming language that compiles to [WebAssembly](http://webassembly.org/). For more information about the language, check out [grain-lang.org](https://grain-lang.org/).

If it's your first time here, it's highly recommended that you follow [the Grain guide](https://grain-lang.org/docs) to get up and running!

## Contributing

There are tons of ways to contribute to Grain. Check out our [contributing guide](https://github.com/grain-lang/grain/blob/master/CONTRIBUTING.md) for more info and come [chat with us on Discord](https://discord.gg/7U3newJ)! All contributors are held to our [contributor code of conduct](https://github.com/grain-lang/grain/blob/master/CODE_OF_CONDUCT.md).

## Building

To build Grain, you'll need `yarn` and Node.js version 14 or higher. To get everything set up, run:

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

Copyright ©️ 2017-2020 Philip Blair and Oscar Spencer.
