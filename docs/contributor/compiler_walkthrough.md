# A Walkthrough of the Grain Compiler

This guide will take you through all of the phases of the compiler to give you a general sense of how we go from a `.gr` file to a `.wasm` file.

We'll largely be following the `next_state` function in [compile.re](https://github.com/grain-lang/grain/blob/main/compiler/src/compile.re).

## An overview of the compiler

The Grain compiler is a [multi-stage](https://en.wikipedia.org/wiki/Multi-pass_compiler) compiler, which means instead of converting directly from Grain syntax into `wasm` code, we send the input program through multiple phases, transforming from one intermediate representation to the next until we get to the final output. This approach allows us to have a more maintainable compiler and perform deeper analysis of the source code, which lets us provide better errors and better code output.

## File Structure
All files directly related to the compiler can be found in `compiler/src` with a map of the sub-folders found below:
* `src/parsing` - all code related to parsing and lexing
* `src/typed` - all code related to typechecking and the typed phases of the compiler
* `src/codegen` - all code related to generating both the mashtree and final wasm output which is the last two compilation steps before linking
* `src/linking` - the grain linker and code responsible for linking the intermediate wasm modules into the final wasm output
* `src/diagnostics` - all code related to parsing and handling comments for `graindoc`
* `src/formatting` - all the relevant code to the grain formatter
* `src/language_server` - all relevant code to the language server
* `src/utils` - all of our common helpers used in various places throughout the compiler

## Lexing

The first stage of the compiler is [Lexing](https://en.wikipedia.org/wiki/Lexical_analysis), which is the process of breaking up an input string into tokens that are easier for us to later parse into an abstract syntax tree. A Grain program string is tokenized into things like:

- keywords (`let`, `include`, `type`, `assert`, etc.)
- constants (`17`, `"foobar"`, `1uL`, `'a'`, etc.)
- delimiters (`{`, `}`, `[`, `]`, `,`, etc.)
- operators (`*`, `+`, `==`, `&&`, etc.)
- identifiers (`myVar`, `List`, etc.)
- comments (`# this is a comment`, etc.)

The grain compiler uses [sedlex](https://github.com/ocaml-community/sedlex) to build tokenization rules from easy to maintain patterns that we've defined in [parsing/lexer.re](https://github.com/grain-lang/grain/blob/main/compiler/src/parsing/lexer.re)

## Parsing

After the program has been tokenized, we move to the [parsing](https://en.wikipedia.org/wiki/Parsing) stage. The goal of parsing is to convert our lexed tokens to an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree), or AST. An AST is a more contextual representation of the given program structure in a format that is easier to analyze and understand compared to individual tokens. For example, operator precedence isn't very clear in a tokenized program. For the list of tokens `` `1`, `+`, `2`, `*`, `3` ``, it's unclear that the multiplication should happen first. After parsing, we end up with a tree that looks like this:

```plaintext
     times
    /     \
  add      3
 /   \
1     2
```

Writing a parser by hand is great when you've got a stable language grammar, but Grain is in rapid development. To allow us to quickly make changes to the language, we use a parser generator. [Menhir](http://gallium.inria.fr/~fpottier/menhir/) is an excellent production-grade parser generator that produces OCaml code for a parser based on parser rules we've defined. We call these rules a "grammar" and you can find the grammar for the Grain language in [parsing/parser.mly](https://github.com/grain-lang/grain/blob/main/compiler/src/parsing/parser.mly). If you'd like to learn more about BNF grammars, check out [this resource](http://people.cs.ksu.edu/~schmidt/300s05/Lectures/GrammarNotes/bnf.html). Menhir offers great support for specific errors which can be configured through the [parsing/parser.messages](https://github.com/grain-lang/grain/blob/main/compiler/src/parsing/parser.messages) file.

The definition for the Grain AST (which we often refer to as the parsetree) can be found in [parsing/parsetree.re](https://github.com/grain-lang/grain/blob/main/compiler/src/parsing/parsetree.re).

## Well-formedness

This is just a fancy term for asking the question "does this program—for the most part—make sense?" In Grain, type identifers must always start with a capital letter, so there's a well-formedness check that enforces this. In general, we like to be as lenient as possible while parsing and provide helpful error messages from well-formedness checks. If a user writes a program like `type foo = ...`, it's much better to say `Error: 'foo' should be capitalized` rather than `Syntax error`.

You can find the Grain well-formedness checks in [parsing/well_formedness.re](https://github.com/grain-lang/grain/blob/main/compiler/src/parsing/well_formedness.re).

## Typechecking

Grain implements a [Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system). This is by far the most academically challenging step of the compilation process. As such, the Grain typechecker is largely borrowed from the [OCaml compiler](https://github.com/ocaml/ocaml) (yay open source 🎉). [Typechecking](https://en.wikipedia.org/wiki/Type_system#Type_checking) is the process of verifying that the program consistently applies the right kinds of functions to the right kinds of data. The Grain typechecker is also responsible for inferring the type of all Grain expressions. For example, the typechecker will infer the type of `add` in `let add = (x, y) => x + y` to be `(a: Number, b: Number) => Number`. If the user calls `add` with an invalid type such as `add(1, "test")` it will throw an error. Typechecking drastically reduces the number of bugs encountered at runtime.

The internals pretty much never need to be touched 🙏, though it's sometimes necessary to make changes to how we make calls to the typechecker in [typed/typemod.re](https://github.com/grain-lang/grain/blob/main/compiler/src/typed/typemod.re) or [typed/typecore.re](https://github.com/grain-lang/grain/blob/main/compiler/src/typed/typecore.re).

After typechecking a module, we're left with a typedtree. You can find the definition in [typed/typedtree.re](https://github.com/grain-lang/grain/blob/main/compiler/src/typed/typedtree.re).

## Typed Well-formedness

After typechecking, we have more information about the program. We do a second well-formedness pass to further weed out invalid programs. This takes place in [types/typed_well_formedness.re](https://github.com/grain-lang/grain/blob/main/compiler/src/typed/typed_well_formedness.re)

## Linearization

In this step, we convert the typedtree into [A-normal Form](https://en.wikipedia.org/wiki/A-normal_form), or ANF. This purpose of this step is to create a linear set of expressions that could be performed in order from start to finish. For example, given the expression `foo(3 * 4, bar(5))`, we'd want to produce:

```plaintext
$arg1 := 3 * 4
$arg2 := bar(5)
foo($arg1, $arg2)
```

This gets us a step closer to actually starting to emit wasm instructions. The linearization step produces an anftree, and you can find the definition in [middle_end/anftree.re](https://github.com/grain-lang/grain/blob/main/compiler/src/middle_end/anftree.re).

## Optimization

Who doesn't love fast programs? This step analyzes the program and alters it to run faster or use less memory. For example, three optimizations we do are constant propagation, constant folding, and dead assignment elimination. Constant propagation rewrites identifiers for constants into just the constant value, like so:

```plaintext
Before:

let x = 5
let y = x + 2

After:

let x = 5
let y = 5 + 2
```

Constant folding precomputes operations on constants:

```plaintext
Before:

let y = 5 + 2

After:

let y = 7
```

Dead assignment elimination gets rid of unused assignments:

```plaintext
Before:

let x = 5
let y = 5 + 2

After:

let y = 5 + 2
```

By default, we do a number of optimization passes which allows all of the optimizations to work together. Simple programs will optimize down to just their result:

```plaintext
Before:

let a = 4 + 12
let b = a * 2
let c = (b / 4)
c - 2

After:

6
```

You can find the entrypoints into optimization in [middle_end/optimize.re](https://github.com/grain-lang/grain/blob/main/compiler/src/middle_end/optimize.re).

## Mashing

We couldn't think of a better name for this stage, but it's (mostly) the last representation before outputting the actual WebAssembly instructions. It's here that we decide what actually gets allocated in memory and how we retrieve and change things in memory. You can find the mashtree [here](https://github.com/grain-lang/grain/blob/main/compiler/src/codegen/mashtree.re) and the conversion process from ANF in [codegen/transl_anf.re](https://github.com/grain-lang/grain/blob/main/compiler/src/codegen/transl_anf.re).

## Code generation

The code generation (or codegen) step is where we generate the actual WebAssembly code for the program. By this point, we should have reduced the complexity of the original program down enough that there is a straightforward set of WebAssembly instructions for each action that needs to happen. We use a project called [Binaryen](https://github.com/WebAssembly/binaryen) to generate our wasm code, via [Binaryen.ml](https://github.com/grain-lang/binaryen.ml). You can get a general idea of how Binaryen works from the example in the [Binaryen.ml README](https://github.com/grain-lang/binaryen.ml/blob/main/README.md). You can then see how we use it in Grain in [codegen/compcore.re](https://github.com/grain-lang/grain/blob/main/compiler/src/codegen/compcore.re).

If you're curious about the wasm spec in general, you can check it out [here](https://webassembly.github.io/spec/core/index.html).

## Linking

Each Grain source file is compiled to a Grain-specific wasm file. To create the final program, we merge all of the files together in a step known as linking. This takes place in [linking/link.re](https://github.com/grain-lang/grain/blob/main/compiler/src/linking/link.re)

## Emission

Lastly, we write the wasm to a file. And that's it! If you want an in-depth dive into any of the stages of the compiler, feel free to ask in the Discord and someone will be more than happy to walk you through it.
