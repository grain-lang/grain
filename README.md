<p align="center">
    <a href="https://grain-lang.org/">
        <img src="https://raw.githubusercontent.com/grain-lang/grain/master/grain_shorthand_color.png" alt="Grain" height="200" />
    </a>
</p>

<p align="center">
    <a href="https://github.com/grain-lang/grain/actions/workflows/ci.yml">
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

For instructions on how to build Grain from source, please consult the [official documentation](https://grain-lang.org/docs/getting_grain#Building-Grain-from-Source).

### Other Commands

To build the standard library:

```bash
yarn stdlib build
```

To build the JS runner:

```bash
yarn js-runner build
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

Copyright ©️ 2017-2021 Philip Blair, Oscar Spencer, & contributors.
