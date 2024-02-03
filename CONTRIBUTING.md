# Contributing to Grain

Thanks for your interest in contributing to Grain! There are all sorts of ways to contribute—there's core compiler work, implementing standard libraries, writing documentation, or even maintaining the website. No matter your interests or skill level, we're happy to have your help. Logistically, the best way to get involved is to chat with us on Discord. From there, we'll be able to direct you to various areas of the project.

## Code of Conduct

Please note that this project is released with a [contributor code of conduct](https://github.com/grain-lang/grain/blob/main/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Joining the Discord

You can use this link to chat with us on Discord: [grain-lang discord server](https://discord.com/invite/grain-lang)

## Getting up and running

If it's your first time here, we suggest that you follow [the Grain setup guide](https://grain-lang.org/docs/getting_grain) to get up and running. We also have build instructions listed in the README. The instructions should be fairly straightforward, but if you run into any issues please reach out to us on Discord.

## Contributor docs

There are a set of contributor docs in [docs/contributor](https://github.com/grain-lang/grain/tree/main/docs/contributor). These documents go into technical details on how things work in Grain.

## Typical development workflows

### Compiler

When working with the compiler, you'll want to install [esy](https://esy.sh/docs/en/getting-started.html#install-esy) globally and make it available in your `$PATH`. This will allow you to have LSP services while working on the compiler code.

Once you have the compiler building, the typical flow for development is to make changes in the `compiler` directory, then run:

```bash
npm run compiler build
npm run compiler test
```

It can sometimes be helpful to run small Grain programs directly to test some functionality without running the full test suite.

### Standard library

It's usually easiest to create a small Grain program that imports your library to try it out, like so:

```grain
import Array from "array"

let appended = Array.append([> 1, 2, 3], [> 4, 5, 6])
print(appended)
```

The tests for the standard library are located in `compiler/test/stdlib`. Since the standard library tests are written in Grain, rather than running the whole test suite, you can just run them directly:

```bash
grain compiler/test/stdlib/array.test.gr
```

If there's no error, you're all set.
