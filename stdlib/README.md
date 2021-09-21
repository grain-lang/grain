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
import Number from "number"
Number.abs(-1)
```

## API

This package includes generated markdown documentation, but we recommend using our API docs on [grain-lang.org](https://grain-lang.org/docs/stdlib/pervasives).

## License

MIT
