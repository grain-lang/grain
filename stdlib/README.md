# @grain/stdlib

The standard library for the Grain language.

## Why

This package provides a distributable copy of the Grain standard library. Currently, it is available through npm.

## Installation

Usually, you won't need to install this package. However, if you need it, install it with npm:

```sh
npm install @grain/stdlib
```

## Usage

Grain programs will search for an imported module in the `stdlib` if not available in your other search paths.

For example, you can import the `number` stdlib:

```grain
module Main
from "number" include Number
assert Number.abs(-1) == 1
```

## API

This package includes generated markdown documentation, but we recommend using our API docs on [grain-lang.org](https://grain-lang.org/docs/stdlib/pervasives).

## License

MIT

## Contributing

If you want to contribute to the `stdlib`, please consider the guidelines [here](https://github.com/grain-lang/grain/blob/main/docs/contributor/standard_library.md), if you want something to work on you can find open issues [here](https://github.com/grain-lang/grain/issues?q=is%3Aopen+is%3Aissue+label%3Astdlib).

To regenerate the `stdlib`` documentation you can run:

```sh
grain doc stdlib -o stdlib --current-version=$(grain -v)
```

To format the `stdlib` you can run:

```sh
grain format ./stdlib/ -o ./stdlib/
```

To run the `stdlib` tests you can run:

```sh
npm run compiler test -- --filter stdlib
```
