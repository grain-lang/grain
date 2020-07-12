# Grain In-Memory Data Representations

There are two places that Grain data can live—on the stack or on the heap. All values are either the literal value, or a pointer to a value that lives on the heap. Currently, all values on the stack are represented as either signed or unsigned 32-bit numbers. All values on the heap are 32-bit sequences, with an exception for strings.

## Value tagging

Values in Grain are tagged. This means that we reserve a number of bits to signal the type of the value. Normally, since Grain is a statically typed language, we wouldn't need to tag the values because the types are guaranteed at compile-time. However, since our values are tagged, at runtime we can determine the type of a value. This is what allows us to have a generic `print` function that works for any data type—even user defined types.

You can find all of the tags in the code in [codegen/value_tags.re](https://github.com/grain-lang/grain/blob/master/compiler/src/codegen/value_tags.re), but they'll be broken down here.

### Numbers

The last bit of a value determines if that value is a number or something else. If it's 0, the value is a number. Conveniently, this means that every Grain number is stored as `n * 2`. This means that addition and subtraction don't require any additional instructions to compute, since `2a + 2b = 2(a + b)`. There are other tricks like this for the other operations that allow us to avoid unnecessary untagging.

The downside of this tag for numbers is the loss of one bit of information. There are 2^31 possible values for signed numbers in Grain rather than 2^32.

To tag a number, we perform a shift left by 1. To untag a number, we perform an arithmetic (signed) shift right by 1.

### Other values

For other values, we look at the last 3 bits to determine the type.

```plaintext
tuples*   0b001
closures* 0b101
generic*  0b011
enums     0b111

*pointer to a heap value
```

Untagging (and tagging) is done by XORing the value with the tag listed above (and there are utilities in the code to do this). After untagging a heap pointer, it's just a regular pointer that points at the location of the real value on the heap. For generic heap values, the first 32-bit value tells you what kind of heap value it is (string, record, array, ADT, etc.), and you can find these corresponding tags in `value_tags.ml` as well.

## Structure of Heap Data

### Tuples

Tuples are the simplest Grain data structure.

```plaintext
╔══════╦════════╤═══════╤═══════╤═════╤═══════╗
║ size ║ 32     │ 32    │ 32    │ 32  │ 32    ║
╠══════╬════════╪═══════╪═══════╪═════╪═══════╣
║ what ║ length │ elt_0 │ elt_1 │ ... │ elt_n ║
╚══════╩════════╧═══════╧═══════╧═════╧═══════╝
```

The length is **untagged**, but all other elements are regular Grain values.

### Arrays

Please note that arrays and lists are different. If you're looking to see how lists are represented, view the section on ADTs.

```plaintext
╔══════╦═══════════╤════════╤═══════╤═══════╤═════╤═══════╗
║ size ║ 32        │ 32     │ 32    │ 32    │ 32  │ 32    ║
╠══════╬═══════════╪════════╪═══════╪═══════╪═════╪═══════╣
║ what ║ value tag │ length │ elt_0 │ elt_1 │ ... │ elt_n ║
╚══════╩═══════════╧════════╧═══════╧═══════╧═════╧═══════╝
```

The length is **untagged**, but all other elements are regular Grain values, even the array value tag.

### Records

Records store a little more information to enable printing.

```plaintext
╔══════╦═══════════╤════════════╤══════════╤════════╤═══════╤═════╤═══════╗
║ size ║ 32        │ 32         │ 32       │ 32     │ 32    │ 32  │ 32    ║
╠══════╬═══════════╪════════════╪══════════╪════════╪═══════╪═════╪═══════╣
║ what ║ value tag │ module tag │ type tag │ length │ elt_0 │ ... │ elt_n ║
╚══════╩═══════════╧════════════╧══════════╧════════╧═══════╧═════╧═══════╝
```

The module tag tells us which module the record type is defined in, and the type tag tells us which type in the module this record corresponds to.

The length is **untagged**, but all other elements are regular Grain values, even the tags.

### Algebraic Data Types (ADTs)

ADTs also store a little more information to enable printing. All user-defined types and some built-in types are ADTs, including lists.

```plaintext
╔══════╦═══════════╤════════════╤══════════╤═════════════╤═══════╤═══════╤═════╤═══════╗
║ size ║ 32        │ 32         │ 32       │ 32          │ 32    │ 32    │ 32  │ 32    ║
╠══════╬═══════════╪════════════╪══════════╪═════════════╪═══════╪═══════╪═════╪═══════╣
║ what ║ value tag │ module tag │ type tag │ variant tag │ arity │ elt_0 │ ... │ elt_n ║
╚══════╩═══════════╧════════════╧══════════╧═════════════╧═══════╧═══════╧═════╧═══════╝
```

The module tag tells us which module this ADT is defined in, the type tag tells us which type in the module this ADT corresponds to, and the variant tag tells us which ADT variant is allocated here.

The arity is **untagged**, but all other elements are regular Grain values, even the tags.

### Closures (Lambdas)

Closures contain all of the values closed over by a function, i.e. in the code:

```grain
let x = 5
let foo = () => x
```

The value `x` gets "saved" so later when we call `foo`, we still have access to `x`. The heap layout looks like this:

```plaintext
╔══════╦═══════╤════════════╤══════╤═══════╤═════╤═══════╗
║ size ║ 32    │ 32         │ 32   │ 32    │ 32  │ 32    ║
╠══════╬═══════╪════════════╪══════╪═══════╪═════╪═══════╣
║ what ║ arity │ *wasm func │ size │ val_0 │ ... │ val_n ║
╚══════╩═══════╧════════════╧══════╧═══════╧═════╧═══════╝
```

The arity represents the number of arguments to the function. `*wasm func` is a pointer to where the assembled wasm function is located in the WebAssembly function table. `size` is the number of values stored in the closure.

The arity, wasm function pointer, and size are all **untagged**. All other elements are regular Grain values.

### Strings

Strings are currently the only data type that stores data in 8-bit chunks rather than 32 bits.

```plaintext
╔══════╦═══════════╤══════╤════════╤════════╤═════╤════════╗
║ size ║ 32        │ 32   │ 8      │ 8      │ 8   │ 8      ║
╠══════╬═══════════╪══════╪════════╪════════╪═════╪════════╣
║ what ║ value tag │ size │ byte_0 │ byte_1 │ ... │ byte_n ║
╚══════╩═══════════╧══════╧════════╧════════╧═════╧════════╝
```

The size is **untagged**. Note that Grain strings are UTF-8 encoded, so one byte does not necessarily fully represent one character. As such, the size set here is the size of the string in bytes, rather than the actual number of characters. The size will always be greater than or equal to the number of characters in the string.

[More information on strings](./string.md)
