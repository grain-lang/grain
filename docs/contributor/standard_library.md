# Standard Library

Grain aims to have a comprehensive and consistent standard library.

To us, this means that someone should be able to reach for the standard library to perform most of their day-to-day work, and it will always work the way they expect in the context of Grain.

## Guidelines

Here are some guidelines for making additions to the standard library while also keeping it high-quality and consistent!

1. New data types should exist in their own modules. For example, the `Range` enum is the data type exported from `range.gr`. An exception to this are data types that are ubiquitous (`Option`/`Result`/`List`), which should live in `pervasives.gr`.
1. Prefer data constructors when possible. Only use separate constructor functions, like `make` or `init`, if needed to set an initialization value or to hide internals of your data structure.
1. All functions that operate on a data type should exist in the same module as that data type. For example, all `Map` methods exist in the `map.gr` file.
1. Fallible functions should almost always return an `Option` or `Result`. Usage of `fail` should be reserved for exceptional cases, such as argument validation producing an index out-of-bounds failure.
    * `Option` should be preferred if you might or might not be able to get some value. Typically, these would be functions that could return `null` in other languages when they didn't produce a value. For example, `List.find` returns `None` when no item in the list matches the condition.
    * `Result` is useful if you have multiple failures, or if the consumer might need additional context around the failure. Typically, these functions would throw exceptions in other languages. For example, `Number.parseInt` returns `Err(reason)` when it fails to parse a string into an integer.
1. If possible, keep dependencies on other standard library modules to a minimum. For example, `Array` re-implements some simple `List` operations to avoid depending on the entire `List` module.
