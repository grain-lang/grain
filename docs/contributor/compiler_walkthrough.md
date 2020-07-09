# A Walkthrough of the Grain Compiler

This guide will take you through all of the phases of the compiler to give you a general sense of how we go from a `.gr` file to a `.wasm` file.

We'll largely be following the `next_state` function in [compile.re](https://github.com/grain-lang/grain/blob/master/compiler/src/compile.re).

## Lexing

Lexing is the process of breaking up a string input into tokens. A Grain program string is tokenized into things like:

* keywords (`let`, `import`, `data`, `assert`, etc.)
* constants (`17`, `'foobar'`, etc.)
* delimiters (`{`, `}`, `[`, `]`, `,`, `;`, etc.)
* operators (`*`, `+`, `==`, `&&`, etc.)
* identifiers (`myVar`, `List`, etc.)
* comments (`# this is a comment`, etc.)

To make this happen, we use [ocamllex](https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html). `ocamllex` is a tool that generates OCaml code to do this based on rules we've defined in [parsing/lexer.mll](https://github.com/grain-lang/grain/blob/master/compiler/src/parsing/lexer.mll).

## Parsing

Once we've got our tokens, we move on to parsing. The goal of parsing is to take our tokens and produce an abstract syntax tree, or AST. An AST is a representation of the program's structure in a format that's easier for us to work with than the tokens themselves. For example, think of the tokens `1` `+` `2` `+` `3`. In reality, the `+` operator only works on two operands at once, so after parsing, we end up with a tree that looks like this:

```plaintext
     add
    /   \
  add    3
 /   \
1     2
```

Writing a parser by hand would be annoying, so instead we use [dypgen](http://dypgen.free.fr/). `dypgen` is a dynamic parser generator that produces OCaml code for a parser based on some rules we've defined. We call these rules a "grammar" and you can find the grammar for the Grain language in [parsing/parser.dyp](https://github.com/grain-lang/grain/blob/master/compiler/src/parsing/parser.dyp). If you'd like to learn more about BNF grammars, check out [this resource](http://people.cs.ksu.edu/~schmidt/300s05/Lectures/GrammarNotes/bnf.html).

The definition for the Grain AST (which we often refer to as the parsetree) can be found in [parsing/parsetree.re](https://github.com/grain-lang/grain/blob/master/compiler/src/parsing/parsetree.re).

## Well-formedness

This is just a fancy term for asking the question "does this program—for the most part—make sense?" In Grain, type identifers must always start with a capital letter, so there's a well-formedness check that enforces this. In general, we like to be as lenient as possible while parsing and provide helpful error messages from well-formedness checks. If a user writes a program like `data foo = ...`, it's much better to say `Error: 'foo' should be capitalized` rather than `Syntax error`.

You can find the Grain well-formedness checks in [parsing/well_formedness.re](https://github.com/grain-lang/grain/blob/master/compiler/src/parsing/well_formedness.re).

## Typechecking

Grain implements a [Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system). This is by far the most academically challenging step of the compilation process. As such, the Grain typechecker is largely borrowed from the [OCaml compiler](https://github.com/ocaml/ocaml) (yay open source 🎉). This is the process that infers the type of all Grain expressions, and makes sure they line up.

The internals pretty much never need to be touched 🙏, though it's sometimes necessary to make changes to how we make calls to the typechecker in [typed/typemod.re](https://github.com/grain-lang/grain/blob/master/compiler/src/typed/typemod.re) or [typed/typecore.re](https://github.com/grain-lang/grain/blob/master/compiler/src/typed/typecore.re).

After typechecking a module, we're left with a typedtree. You can find the definition in [typed/typedtree.re](https://github.com/grain-lang/grain/blob/master/compiler/src/typed/typedtree.re).

## Linearization

In this step, we convert the typedtree into [A-normal Form](https://en.wikipedia.org/wiki/A-normal_form), or ANF. This purpose of this step is to create a linear set of expressions that could be performed in order from start to finish. For example, given the expression `foo(3 * 4, bar(5))`, we'd want to produce:

```plaintext
$arg1 := 3 * 4
$arg2 := bar(5)
foo($arg1, $arg2)
```

This gets us a step closer to actually starting to emit wasm instructions. The linearization step produces an anftree, and you can find the definition in [middle_end/anftree.re](https://github.com/grain-lang/grain/blob/master/compiler/src/middle_end/anftree.re).

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

You can find the entrypoints into optimization in [middle_end/optimize.re](https://github.com/grain-lang/grain/blob/master/compiler/src/middle_end/optimize.re).

## Mashing

We couldn't think of a better name for this stage, but it's (mostly) the last representation before outputting the actual WebAssembly instructions. It's here that we decide what actually gets allocated in memory and how we retrieve and change things in memory. You can find the mashtree [here](https://github.com/grain-lang/grain/blob/master/compiler/src/codegen/mashtree.re) and the conversion process from ANF in [codegen/transl_anf.re](https://github.com/grain-lang/grain/blob/master/compiler/src/codegen/transl_anf.re).

## Code generation

The code generation (or codegen) step is where we generate the actual WebAssembly code for the program. By this point, we should have reduced the complexity of the original program down enough that there is a straightforward set of wasm instructions for each action that needs to happen. You can see what some of this looks like in [codegen/compcore.re](https://github.com/grain-lang/grain/blob/master/compiler/src/codegen/compcore.re).

You can check out the online version of the [wasm spec](https://webassembly.github.io/spec/core/index.html), but if you're looking for the OCaml types that Grain uses, you can check that out in [the Grain fork of the wasm-spec repository](https://github.com/grain-lang/wasm-spec).

## Emission

Lastly, we write the wasm to a file. And that's it! If you want an in-depth dive into any of the stages of the compiler, feel free to ask in the Discord and someone will be more than happy to walk you through it.
