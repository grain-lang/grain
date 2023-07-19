# Grain In-Memory Data Representations

There are two places that Grain data can live—on the stack or on the heap. All values are either the literal value, or a pointer to a value that lives on the heap. Currently, all values on the stack are represented as either signed or unsigned 32-bit numbers. All values on the heap are 32-bit sequences, with an exception for strings.

## Value tagging

Values in Grain are tagged. This means that we reserve a number of bits to signal the type of the value. Conveniently, 32-bit pointers are multiples of 8, so we know that anything that isn't a multiple of 8 isn't a pointer. This leaves us with 7 possible tag values, as long as the data can fit into 29 bits (with one exception, which you can see in the section on numbers). Normally, since Grain is a statically typed language, we wouldn't need to tag the values because the types are guaranteed at compile-time. However, since our values are tagged, at runtime we can determine the type of a value. This is what allows us to have generic `print` and `==` functions that work for any data type—even user defined types.

You can find all of the tags in the code in [codegen/value_tags.re](https://github.com/grain-lang/grain/blob/main/compiler/src/codegen/value_tags.re), but they'll be broken down here.

### Stack tags

We look at the last 3 bits of an i32 on the stack to determine its type.

```plaintext
simple number    0bxx1
pointer*         0b000
short values     0b010
reserved         0b100
constants        0b110

*pointer to a heap value, with a separate tag describing the value.
```

Since heap-allocated values (`pointers` in the chart above) are "tagged" with `0b000`, nothing needs to be done to tag or untag the value. If it's necessary, you can determine the type of the heap object from the first 32-bit value you find at that memory address—it corresponds to the type (string, record, array, ADT, etc.), and you can find these corresponding heap tags in `value_tags.ml` as well.

## Structure of Stack-Allocated Data

### Simple numbers

Typically, tags span 3 bits. That's usually fine, but if we limited numbers to 29 bits, we could only support ~500 million possible numbers, which is a bit lame. For this reason, we dedicate the final bit of our 32-bit values to numbers—if the final bit is a 1, it's a "simple" 31-bit integer. This leaves us with three other possible tags— `0b010`, `0b100`, and `0b110`.

To avoid overwriting data, we shift simple numbers to the left by 1 bit, then set the last bit to the value 1. This means that every Grain simple number is stored as `2n + 1`. The process of converting a simple number to this representation is called "tagging". Untagging and re-tagging can be costly, but thankfully, math is our friend. Addition on two Grain simple numbers `x` and `y` could be done by computing `2((x - 1)/2 + (y - 1)/2) + 1`, but some quick simplification shows we only need to compute `x + y - 1`, which makes basic addition only two WebAssembly instructions. There are other tricks like this for the other operations that allow us to avoid unnecessary untagging. (Additionally, since the tag information is held in the final bit, untagging can be done with a single instruction, `shr_s` by 1, if necessary.)

31 bits allow us to represent more than 2 billion numbers (from -1073741824 to 1073741823), which is a few more than the 500 million we'd get from 29 bits. If larger numbers (or non-integer numbers) are needed, then Grain will fall back onto one of the other (heap-allocated) number types. The heap-allocated numbers are described in detail later.

### Short values

Short values (currently `Char`, `Int8`, `Int16`, `Uint8`, and `Uint16`) use the 3-bit tag `0b010`. Since all of these values are relatively compact, they are combined into the same tag for the sake of tag space conservation. The valid values for all of these types require <= 24 bits to store, so the leading 24 bits of the 32-bit stack value are allocated for storing the actual data of the value; the following 5 bits are allocated for tagging the different short value types; and the final 3 bits are of course allocated for the "short values" tag (`0b010`). The short value subtypes and their tags are:

```plaintext
Char      0b00000
Int8      0b00001
Int16     0b00010
Uint8     0b00011
Uint16    0b00100
```

With this layout, to tag a short value we can shift the value left by 8 bits, set the following 5 bits to the short value tag corresponding to the data type, and set the last 3 bits to `0b010`. Just like with simple numbers, there are some tricks we can do to avoid untagging and retagging when manipulating short values.

#### Char

The Grain `Char` type is represented as a Unicode scalar value. Unicode scalar values exist in the range 0x0-10FFFF, which means it only takes 21 bits to store a USV.

#### Uint8/Uint16

Storing short unsigned short integers is fairly straightforward: `Uint8`s use the trailing 8 bits and `Uint16`s use the trailing 16 bits of the 24 allocated for short value data. The remaining leading bits will all be 0s.

#### Int8/Int16

Short signed integers are stored in a similar manner to short unsigned integers; the only difference is that the leading bits extend the sign of the integer being represented. In other words, for non-negative values the leading bits will all be 0s and for negative values the leading bits will all be 1s. This is done for sake of making certain signed operations (e.g. comparisons, division) on short signed integers more convenient. Since WebAssembly does not natively support 8/16-bit integer operations, this schema allows regular `i32` operations (used on the entire 32-bit stack value) to substitute for native signed 8/16-bit operations.

## Structure of Heap-Allocated Data

### Tuples

Most heap-allocated values look much like tuples, so it'd good to understand its layout.

```plaintext
╔══════╦═══════════╤════════╤═══════╤═══════╤═════╤═══════╗
║ size ║ 32        │ 32     │ 32    │ 32    │ 32  │ 32    ║
╠══════╬═══════════╪════════╪═══════╪═══════╪═════╪═══════╣
║ what ║ value tag │ length │ elt_0 │ elt_1 │ ... │ elt_n ║
╚══════╩═══════════╧════════╧═══════╧═══════╧═════╧═══════╝
```

The length is **untagged**, but all other elements are regular Grain values.

### Arrays

Please note that arrays and lists are different data structures. If you're looking to see how lists are represented, view the section on ADTs.

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
╔══════╦═══════════╤═══════════╤══════════╤════════╤═══════╤═════╤═══════╗
║ size ║ 32        │ 32        │ 32       │ 32     │ 32    │ 32  │ 32    ║
╠══════╬═══════════╪═══════════╪══════════╪════════╪═══════╪═════╪═══════╣
║ what ║ value tag │ type hash │ type tag │ length │ elt_0 │ ... │ elt_n ║
╚══════╩═══════════╧═══════════╧══════════╧════════╧═══════╧═════╧═══════╝
```

The type hash is a unique value for the record type, and the type tag tells us which type in the module this record corresponds to.

The value tag and length are **untagged**, but all other elements are regular Grain values.

### Algebraic Data Types (ADTs)

ADTs also store a little more information to enable printing. All user-defined types and some built-in types are ADTs, including lists.

```plaintext
╔══════╦═══════════╤═══════════╤══════════╤═════════════╤═══════╤═══════╤═════╤═══════╗
║ size ║ 32        │ 32        │ 32       │ 32          │ 32    │ 32    │ 32  │ 32    ║
╠══════╬═══════════╪═══════════╪══════════╪═════════════╪═══════╪═══════╪═════╪═══════╣
║ what ║ value tag │ type hash │ type tag │ variant tag │ arity │ elt_0 │ ... │ elt_n ║
╚══════╩═══════════╧═══════════╧══════════╧═════════════╧═══════╧═══════╧═════╧═══════╝
```

The type hash is a unique value for the enum type, the type tag tells us which type in the module this ADT corresponds to, and the variant tag tells us which ADT variant is allocated here.

The value tag and arity are **untagged**, but all other elements are regular Grain values.

### Closures (Lambdas)

Closures contain all of the values closed over by a function, i.e. in the code:

```grain
let x = 5
let foo = () => x
```

The value `x` gets "saved" so later when we call `foo`, we still have access to `x`. The heap layout looks like this:

```plaintext
╔══════╦═══════════╤═══════╤════════════╤══════╤═══════╤═════╤═══════╗
║ size ║ 32        │ 32    │ 32         │ 32   │ 32    │ 32  │ 32    ║
╠══════╬═══════════╪═══════╪════════════╪══════╪═══════╪═════╪═══════╣
║ what ║ value tag │ arity │ *wasm func │ size │ val_0 │ ... │ val_n ║
╚══════╩═══════════╧═══════╧════════════╧══════╧═══════╧═════╧═══════╝
```

The arity represents the number of arguments to the function. `*wasm func` is a pointer to where the assembled wasm function is located in the WebAssembly function table. `size` is the number of values stored in the closure.

The value tag, arity, wasm function pointer, and size are all **untagged**. All other elements are regular Grain values.

### Strings

Bytes and Strings are currently the only data types that store data in 8-bit chunks rather than 32 bits.

```plaintext
╔══════╦═══════════╤══════╤════════╤════════╤═════╤════════╗
║ size ║ 32        │ 32   │ 8      │ 8      │ 8   │ 8      ║
╠══════╬═══════════╪══════╪════════╪════════╪═════╪════════╣
║ what ║ value tag │ size │ byte_0 │ byte_1 │ ... │ byte_n ║
╚══════╩═══════════╧══════╧════════╧════════╧═════╧════════╝
```

The size is **untagged**. Note that Grain strings are UTF-8 encoded, so one byte does not necessarily fully represent one character. As such, the size set here is the size of the string in bytes, rather than the actual number of characters. The size will always be greater than or equal to the number of characters in the string.

For completeness, here are the layouts of UTF-8 byte sequences:

```plaintext
╔═══════════╦══════════════════╦═════════════════╦══════════╦══════════╦══════════╦══════════╗
║ Num bytes ║ First code point ║ Last code point ║  Byte 1  ║  Byte 2  ║  Byte 3  ║  Byte 4  ║
╠═══════════╬══════════════════╬═════════════════╬══════════╬══════════╬══════════╬══════════╣
║         1 ║ U+0000           ║ U+007F          ║ 0xxxxxxx ║          ║          ║          ║
║         2 ║ U+0080           ║ U+07FF          ║ 110xxxxx ║ 10xxxxxx ║          ║          ║
║         3 ║ U+0800           ║ U+FFFF          ║ 1110xxxx ║ 10xxxxxx ║ 10xxxxxx ║          ║
║         4 ║ U+10000          ║ U+10FFFF        ║ 11110xxx ║ 10xxxxxx ║ 10xxxxxx ║ 10xxxxxx ║
╚═══════════╩══════════════════╩═════════════════╩══════════╩══════════╩══════════╩══════════╝
```

[More information on strings.](./string.md)

### Bytes

Bytes and Strings are currently the only data types that store data in 8-bit chunks rather than 32 bits.

```plaintext
╔══════╦═══════════╤══════╤════════╤════════╤═════╤════════╗
║ size ║ 32        │ 32   │ 8      │ 8      │ 8   │ 8      ║
╠══════╬═══════════╪══════╪════════╪════════╪═════╪════════╣
║ what ║ value tag │ size │ byte_0 │ byte_1 │ ... │ byte_n ║
╚══════╩═══════════╧══════╧════════╧════════╧═════╧════════╝
```

The size is **untagged**.

### Heap-Allocated Numbers

All heap-allocated numbers have the following structure on the heap.

```plaintext
╔══════╦═══════════╤══════════════════╤════════════╗
║ size ║ 32        │ 32               │ *          ║
╠══════╬═══════════╪══════════════════╪════════════╣
║ what ║ value tag │ boxed number tag │ payload    ║
╚══════╩═══════════╧══════════════════╧════════════╝
```

The boxed number tag describes which variant the structure is an instance of, and, correspondingly,
what the shape of the rest of the structure is.

#### Int64

The payload for Int64 values is a single, signed, 64-bit integer.

```plaintext
╔══════╦═══════════╤══════════════════╤══════════════╗
║ size ║ 32        │ 32               │ 64           ║
╠══════╬═══════════╪══════════════════╪══════════════╣
║ what ║ value tag │ boxed number tag │ value (i64)  ║
╚══════╩═══════════╧══════════════════╧══════════════╝
```

#### Float64

The payload for Float64 values is a single, signed, 64-bit float.

```plaintext
╔══════╦═══════════╤══════════════════╤══════════════╗
║ size ║ 32        │ 32               │ 64           ║
╠══════╬═══════════╪══════════════════╪══════════════╣
║ what ║ value tag │ boxed number tag │ value (f64)  ║
╚══════╩═══════════╧══════════════════╧══════════════╝
```

#### Rational

The payload for rational numbers consists of two numbers. A 32-bit
pointer to a BigInt denotes the numerator of the represented fraction,
and 32-bit pointer to a BigInt denotes the denominator of the represented fraction.

```plaintext
╔══════╦═══════════╤══════════════════╤══════════════════════════════════════════════════════╗
║ size ║ 32        │ 32               │ 32                      │ 32                         ║
╠══════╬═══════════╪══════════════════╪═════════════════════════╪════════════════════════════╣
║ what ║ value tag │ boxed number tag │ numerator (ptr<BigInt>) │ denominator (ptr<BigInt>)  ║
╚══════╩═══════════╧══════════════════╧═════════════════════════╧════════════════════════════╝
```

#### BigInt

The payload for big integers consists of an unsigned 32-bit integer denoting
the number of "limbs" (64-bit dwords needed to encode the magnitude), a 16-bit
bitflag, 16 bits of reserved space, and the 64-bit limbs containing the value.

```plaintext
╔══════╦═══════════╤══════════════════╤═════════════════╤═══════════════════════════════════════════════════════════╗
║ size ║ 32        │ 32               │ 32              │ 16              │ 16                 │ 64 * size          ║
╠══════╬═══════════╪══════════════════╪═════════════════╪═════════════════╪═════════════════════════════════════════╣
║ what ║ value tag │ boxed number tag │ size            | flags           │ <reserved>         | limbs (u64)        ║
╚══════╩═══════════╧══════════════════╧═════════════════╧═════════════════╧═════════════════════════════════════════╝
```

### Alternative Heap-Allocated Numbers

Some number types (`Int32`, `Float32`, `Uint32`, and `Uint64`) are not encapsulated into the unified `Number` type and are rather their own unique types.

#### Int32

The payload for Int32 values is a single, signed, 32-bit integer.

```plaintext
╔══════╦═══════════╤══════════════╗
║ size ║ 32        │ 32           ║
╠══════╬═══════════╪══════════════╣
║ what ║ value tag │ value (i32)  ║
╚══════╩═══════════╧══════════════╝
```

#### Float32

The payload for Float32 values is a single, signed, 32-bit float.

```plaintext
╔══════╦═══════════╤══════════════╗
║ size ║ 32        │ 32           ║
╠══════╬═══════════╪══════════════╣
║ what ║ value tag │ value (f32)  ║
╚══════╩═══════════╧══════════════╝
```

#### Uint32

The payload for Uint32 values is a single, unsigned, 32-bit integer.

```plaintext
╔══════╦═══════════╤══════════════╗
║ size ║ 32        │ 32           ║
╠══════╬═══════════╪══════════════╣
║ what ║ value tag │ value (i32)  ║
╚══════╩═══════════╧══════════════╝
```

#### Uint64

The payload for Uint64 values is a single, unsigned, 64-bit integer. 32 bits
of padding are added after the value tag in order to play nicely with 8-byte
memory alignment.

```plaintext
╔══════╦═══════════╤═════════╤══════════════╗
║ size ║ 32        │ 32      │ 64           ║
╠══════╬═══════════╪═════════╪══════════════╣
║ what ║ value tag │ padding │ value (i64)  ║
╚══════╩═══════════╧═════════╧══════════════╝
```
