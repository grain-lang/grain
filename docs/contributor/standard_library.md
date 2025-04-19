# Standard Library

Grain aims to cultivate a **comprehensive and consistent standard library (stdlib)**.

To us, this means that users should be able to reach for the stdlib to perform most day-to-day tasks, and it should behave predictably within the context of Grain.

## Guidelines

Here are some guidelines for making additions to the stdlib while maintaining a high-quality and consistent user experience.

### Scope

The stdlib is intended to support common day-to-day needs of Grain developers while remaining lightweight, reliable, and easy to maintain. Standard library modules include functionality that:

- Serves a broad set of programs
- Enhances the developer experience by being ergonomic and consistent
- Works reliably in a standard WebAssembly and [WASI](https://github.com/WebAssembly/WASI) environments
- Avoids unnecessary complexity and maintenance burdens

If a feature fits naturally into everyday development, improves clarity or safety, and can be implemented cleanly within these constraints, it might fit well into the stdlib. For example, while the `Json` and `Regexp` modules are relatively complex, they are ubiquitous and require little ongoing work to maintain. In contrast, something more niche like a `Protobuf` module is likely better off as a community library as it evolves independently and requiring active maintenance, check out [awesome-grain](https://github.com/grain-lang/awesome-grain) for some community libraries.

### Organization

A consistent and well-structured stdlib makes it easier for developers to find what they need and understand how to use it. In order to keep things clean and predictable, we organize the stdlib around clear, single-purpose modules.

Each module should represent a singular focused concept, like a data structure (i.e. `Map`, `Set`) or utility (i.e. `Marshal`, `Json`). Data types should live within their respective modules unless they are ubiquitous and require deep language integration such as `Option`, `Result`, `List` or `Range` which live either directly in the compiler, or `Pervasives`.

Within an individual module exports should be grouped by functionality, with common exports being closer to the top of the module. Modified functionality such as immutable variants (i.e `Map.Immutable`, `Set.Immutable`) should exist within submodules.

The goal is to make the stdlib intuitive to explore and easy to maintain, 
with as little surprise as possible for contributors and users alike.

### Testing

Every exposed function or module in the stdlib should be thoroughly tested to ensure reliability, correctness, and predictability. Good testing helps to prevent regressions as features and changes are introduced. Tests exist in `../../compiler/test/stdlib/<module>.test.gr` when adding a new module, make sure to add `assertStdlib("<module>.test");` to `../../compiler/test/suites/stdlib.re`.

Testing should focus on:
- **Correctness**: Ensure the function or module behaves as expected under both normal and edge cases
  - The `Number` library is a great example where we test each unique `Number` layout along with various edge cases
- **Consistency**: Ensure the behavior of a function or module is consistent across different environments (i.e. `Windows`, Mac`, `Linux`)
- **Simplicity**: Keep tests straightforward and focused. Each test should try to verify a single, simple behavior
- **Methodical**: Tests should be ordered in a methodical manner isolating failures
  - As an example, if you are testing `Uint8` ensure you test `Uint8.(==)` before using it within other tests so we can quickly identify failures 

A solid test suite helps to maintain the quality and stability of the stdlib as a whole, while providing confidence that behaviors are consistent and reliable.

### Documentation

Every exposed `type` `module` and `value` **must** include a graindoc docblock, [Documentation can be found here](https://grain-lang.org/docs/tooling/graindoc). As we strive for consistency and clarity, a great starting place for new documentation is finding existing documentation with similar functionality and adapting it your needs. We haven't gotten around to documenting all of our documentation patterns yet but a non inclusive list can be found [here](https://github.com/grain-lang/grain/issues/828).

### Common Patterns

#### Failable Functions

Failable functions (functions that may fail), should almost always return either an `Option` or `Result`.

- `Option` is preferred when a function may or may not return a value
  - These are cases where `null` might typically be returned in other languages
  - Example: `List.find` which returns `Some(index)` containing the index of the first element found or `None` otherwise
- `Result` is preferred when a function may fail
  - There are numerous failure modes
  - The caller might need more information about what went wrong
  - Example: `Number.parseInt` which returns `Ok(value)` containing the parsed number on a successful parse or `Err(err)` containing a variant of `ParseIntError`
- `throw`/`fail` is reserved for rare edge cases
  - The failure is very rare and recovery is unlikely
    - Currently as we don't have exception handling users cannot recover when these occur
  - Example: `Number.(/)` which may `fail` if you try to divide by zero
