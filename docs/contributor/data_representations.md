# Grain In-Memory Data Representations

As of Grain `v0.8` [Wasm GC](https://github.com/WebAssembly/gc) is used to represent all grain values. This means that all values are represented either as an `ref i31` or `struct`. We use `ref i31`s to represent "stack" values, which are values that can be stored directly in a WebAssembly `i32`, and we use `struct`s to represent "heap" values, which are values that require more than 32 bits of data to represent. The layout of the `struct`s is described in the section on heap-allocated data.

While this document describes the structure of grain values directly it is preferable to use helper functions provided by the runtime through `runtime/unsafe/dataStructures` when manipulating grain values.

## Value tagging

Values in Grain are tagged. This means that we reserve a number of bits to signal the type of the value. Normally, since Grain is a statically typed language, we wouldn't need to tag the values because the types are guaranteed at compile-time. However, since our values are tagged, at runtime we can determine the type of a value. This is what allows us to have generic `print`, `==`, `compare` and `marshal` functions that work for any data type—even user defined types.

You can find all of the tags in [codegen/value_tags.re](https://github.com/grain-lang/grain/blob/main/compiler/src/codegen/value_tags.re), but they'll also be broken down here.

### Stack tags

We look at the last 3 bits of a `ref i31` to determine its type.

```plaintext
simple number    0bxx1
short values     0b010
reserved         0b100
constants        0b110
```

Since all values are stored as `WasmRef`s we can use `WasmRef.isRefI31` from `runtime/unsafe/wasmref` to check if the value is a "stack" value, otherwise we can assume that it is a grain value however to be sure `isGrainHeapValue` can be used to ensure that the value is a grain value as opposed to a generic WebAssembly reference type. See the section on heap-allocated data for more information on determining the type of "heap" values.

## Structure of Stack-Allocated Data

### Simple numbers

Typically, tags span 3 bits. That's usually fine, but if we limited numbers to 28 bits, we could only support ~250 million possible numbers, which is a bit sad. For this reason, we dedicate the final bit of our 31-bit values to numbers—if the final bit is a 1, it's a "simple" 30-bit integer. This leaves us with three other possible tags— `0b010`, `0b100`, and `0b110` for the other "stack" tags.

To avoid overwriting data, we shift simple numbers to the left by 1 bit, then set the last bit to the value 1. This means that every Grain simple number is stored as `2n + 1`. The process of converting a simple number to this representation is called "tagging". Untagging and re-tagging can be costly, but thankfully, math is our friend. Addition on two Grain simple numbers `x` and `y` could be done by computing `2((x - 1)/2 + (y - 1)/2) + 1`, but some quick simplification shows we only need to compute `x + y - 1`, which makes basic addition only two WebAssembly instructions. There are other tricks like this for the other operations that allow us to avoid unnecessary untagging. (Additionally, since the tag information is held in the final bit, untagging can be done with a single instruction, `shr_s` by 1, if necessary.)

30 bits allow us to represent more than 1 billion numbers (from -536,870,912 to 536,870,911), which is four times as many as the 250 million we'd get from 28 bits. If larger numbers (or non-integer numbers) are needed, then Grain will fall back onto one of the other (heap-allocated) number types. The heap-allocated numbers are described in detail later.

### Short values

Short values (currently `Char`, `Int8`, `Int16`, `Uint8`, and `Uint16`) use the 3-bit tag `0b010`. Since all of these values are relatively compact, they are combined into the same tag for the sake of tag space conservation. The valid values for all of these types require <= 23 bits to store, so the leading 23 bits of the 31-bit stack value are allocated for storing the actual data of the value; the following 5 bits are allocated for tagging the different short value types; and the final 3 bits are of course allocated for the "short values" tag (`0b010`). The short value subtypes and their tags are:

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

Storing short unsigned short integers is fairly straightforward: `Uint8`s use the trailing 8 bits and `Uint16`s use the trailing 16 bits of the 23 allocated for short value data. The remaining leading bits will all be 0s.

#### Int8/Int16

Short signed integers are stored in a similar manner to short unsigned integers; the only difference is that the leading bits extend the sign of the integer being represented. In other words, for non-negative values the leading bits will all be 0s and for negative values the leading bits will all be 1s. This is done for sake of making certain signed operations (e.g. comparisons, division) on short signed integers more convenient. Since WebAssembly does not natively support 8/16-bit integer operations, this schema allows regular `i32` operations (used on the entire 32-bit stack value) to substitute for native signed 8/16-bit operations.

## Structure of Heap-Allocated Data

While this section describes the structure of "heap" allocated data, the actual definitions can be found in [codegen/data_representations.re](https://github.com/grain-lang/grain/blob/main/compiler/src/codegen/data_representations.re).

### Type Hierarchy

All heap-allocated Grain values extend from a common type hierarchy using Wasm GC structural subtyping:

```wasm
(type $GrainValue (struct
  (field $valueTag i32)
))

(type $GrainCompoundValue (sub $GrainValue (struct
  (field $valueTag i32)
  (field $cycleMarker (mut i8))
  (field $data (array (mut ref any)))
)))
```

`$GrainValue` is the root supertype for all heap-allocated Grain values. It contains only the `$valueTag` field, which identifies the type of the value at runtime. Types like `$GrainInt32`, `$GrainFloat32`, `$GrainString`, and `$GrainBytes` extend directly from `$GrainValue`.

`$GrainCompoundValue` extends `$GrainValue` and serves as the supertype for compound data structures that may participate in cycles (tuples, arrays, records, variants, and closures). It adds a `$cycleMarker` field used by the runtime to detect reference cycles during operations like equality checks and printing, and a `$data` field that holds the value's elements.

### Tuples

Most heap-allocated values look much like tuples, so it'd good to understand its layout.

```wasm
(type $GrainTuple (struct
  (field $valueTag i32)
  (field $cycleMarker (mut i8))
  (field $data (array (mut ref any)))
))
```

### Arrays

Please note that arrays and lists are different data structures. If you're looking to see how lists are represented, view the section on ADTs.

```wasm
(type $GrainArray (struct
  (field $valueTag i32)
  (field $cycleMarker (mut i8))
  (field $data (array (mut ref any)))
))
```

### Records

Records store a little more information to enable printing.

```wasm
(type $GrainRecord (struct
  (field $valueTag i32)
  (field $cycleMarker (mut i8))
  (field $data (array (mut ref any)))
  (field $typeHash (ref i31)) ; SimpleNumber
  (field $typeId (ref i31)) ; SimpleNumber
))
```

The type hash is a unique value for the record type, and the type tag tells us which type in the module this record corresponds to. Both the type hash and the type tag are stored as simple numbers.

### Algebraic Data Types (ADTs)

ADTs also store a little more information to enable printing. All user-defined types and some built-in types are ADTs, including lists.

```wasm
(type $GrainVariant (struct
  (field $valueTag i32)
  (field $cycleMarker (mut i8))
  (field $data (array (mut ref any)))
  (field $typeHash (ref i31)) ; SimpleNumber
  (field $typeId (ref i31)) ; SimpleNumber
  (field $variantTag (ref i31)) ; SimpleNumber
))
```

The type hash is a unique value for the enum type, the type tag tells us which type in the module this ADT corresponds to, and the variant tag tells us which ADT variant is allocated here. These are all stored as simple numbers.

### Closures (Lambdas)

Closures contain all of the values closed over by a function, i.e. in the code:

```grain
let x = 5
let foo = () => x
```

The value `x` gets "saved" so later when we call `foo`, we still have access to `x`. The heap layout looks like this:

```wasm
(type $GrainClosure (struct
  (field $valueTag i32)
  (field $cycleMarker (mut i8))
  (field $data (array (mut ref any)))
  (field $wasmFunc (mut ref null func))
))
```

The `$data` field contains the values required by the closure. While `$wasmFunc` is a reference to the closures function itself.

### Strings

```wasm
(type $GrainString (struct
  (field $valueTag i32)
  (field $data (array (mut i8)))
))
```

Note Grain strings are UTF-8 encoded, so one byte does not necessarily fully represent one character. As such, the array length is the size of the string in bytes, rather than the actual number of characters. The size will always be greater than or equal to the number of characters in the string.

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

```wasm
(type $GrainBytes (struct
  (field $valueTag i32)
  (field $data (array (mut i8)))
))
```

### Heap-Allocated Numbers

All heap-allocated numbers have the following structure on the heap.

```wasm
(type $GrainBoxedNumber (struct
  (field $valueTag i32)
  (field $numberTag i32)
  ...payload
))
```

The boxed number tag describes which variant the structure is an instance of, and, correspondingly,
what the shape of the rest of the structure is.

#### Int64

The payload for Int64 values is a single, signed, 64-bit integer.

```wasm
(type $GrainInt64 (struct
  (field $valueTag i32)
  (field $numberTag i32)
  (field $value i64)
))
```

#### Float64

The payload for Float64 values is a single, signed, 64-bit float.

```wasm
(type $GrainFloat64 (struct
  (field $valueTag i32)
  (field $numberTag i32)
  (field $value f64)
))
```

#### Rational

The payload for rational numbers consists of two `wasmRef`s that contain the numerator and denominator of the fraction represented as BigInts.

```wasm
(type $GrainRational (struct
  (field $valueTag i32)
  (field $numberTag i32)
  (field $numerator (ref any)) ; BigInt
  (field $denominator (ref any)) ; BigInt
))
```

#### BigInt

The payload for big integers consist of a field for bitflags (e.g. to indicate the sign of the integer) and an array of 64-bit integers that represent the magnitude of the big integer. More information of the data layout of big integers can be found in the runtime in `runtime/bigint`

```wasm
(type $GrainBigInt (struct
  (field $valueTag i32)
  (field $numberTag i32)
  (field $flags (mut i8))
  (field $limbs (array (mut i64)))
))
```

### Alternative Heap-Allocated Numbers

Some number types (`Int32`, `Float32`, `Uint32`, and `Uint64`) are not encapsulated into the unified `Number` type and are rather their own unique types.

#### Int32

The payload for Int32 values is a single, signed, 32-bit integer.

```wasm
(type $GrainInt32 (struct
  (field $valueTag i32)
  (field $value i32)
))
```

#### Float32

The payload for Float32 values is a single, signed, 32-bit float.

```wasm
(type $GrainFloat32 (struct
  (field $valueTag i32)
  (field $value f32)
))
```

#### Uint32

The payload for Uint32 values is a single, unsigned, 32-bit integer.

```wasm
(type $GrainUint32 (struct
  (field $valueTag i32)
  (field $value i32)
))
```

#### Uint64

The payload for Uint64 values is a single, unsigned, 64-bit integer.

```wasm
(type $GrainUint64 (struct
  (field $valueTag i32)
  (field $value i64)
))
```
