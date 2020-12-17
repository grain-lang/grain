# Standard Library

Grain aims to have a comprehensive and consistent standard library.

To us, this means that someone should be able to reach for the standard library to perform most of their day-to-day work, and it will always work the way they expect in the context of Grain.

## Guidelines

Here are some guidelines for making additions to the standard library while also keeping it high-quality and consistent!

1. New data types should exist in their own modules. For example, the `Range` enum is the data type exported from `range.gr`. An exception to this are data types that are ubiquitous (`Option`/`Result`) or require compiler support (`List`).
1. Prefer data constructors when possible. Only use separate constructor methods, like `make` or `init`, if needed to set an initialization value or to hide internals of your data structure.
1. All methods that operate on a data type should exist in the same module as that data type. For example, all `Map` methods exist in the `map.gr` file.
1. Fallible methods should almost always return an `Option` or `Result`. Usage of `fail` should be reserved for exeptional cases, such as argument validation producing an index out-of-bounds failure.
    * `Option` should be preferred if you might or might not be able to get some value.
    * `Result` is useful if you have multiple failures, or if the consumer might need additional context around the failure.
1. If possible, keep dependencies on other standard library modules to a minimum. For example, `Array` re-implements some simple `List` operations to avoid depending on the entire `List` module.

## AssemblyScript

You maybe notice `.ts` files inside the `stdlib-external` directory. These are actually [AssemblyScript](https://www.assemblyscript.org/) files that we compile to WebAssembly and consume as externals.

The primary reason for this is that AssemblyScript provides a nicer interface for writing WebAssembly than handcrafting it. It also gives us access to things Grain itself disallows, such as direct access into memory representations.

However, our usage comes with some restrictions:

* We compile it with their runtime disabled, which means we don't have access to a lot of the niceties they provide.
* Anything that pre-populates or allocates memory cannot be used. This includes strings, even static ones.
