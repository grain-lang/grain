---
title: DataStructures
---

Low-level utilties for working with Grain's built-in data structures, such as,
tuples, arrays, records, ADTs, strings, bytes, numbers, etc.

This modules provides unsafe functions and should be used with extreme caution;
incorrect usage will lead to runtime errors or undefined behavior, the core team
does not guarantee the stability of these APIs, and they may change without a
major version bump, use at your own risk.

## Values

Functions and constants included in the DataStructures module.

### DataStructures.**getHeapTag**

```grain
getHeapTag: (ref: WasmRef) => WasmI32
```

Gets the tag of a grain heap value.

Parameters:

| param | type      | description                |
| ----- | --------- | -------------------------- |
| `ref` | `WasmRef` | The reference to the value |

Returns:

| type      | description                                                                           |
| --------- | ------------------------------------------------------------------------------------- |
| `WasmI32` | The tag of the value, which can be used to determine its type and how to work with it |

### DataStructures.**getBoxedNumberTag**

```grain
getBoxedNumberTag: (ref: WasmRef) => WasmI32
```

Gets the tag of a boxed number.

NOTE: If the provided value is not a boxed number, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                             |
| ----- | --------- | --------------------------------------- |
| `ref` | `WasmRef` | The reference to the boxed number value |

Returns:

| type      | description          |
| --------- | -------------------- |
| `WasmI32` | The boxed Number tag |

### DataStructures.**loadCycleMarker**

```grain
loadCycleMarker: (ref: WasmRef) => WasmI32
```

Gets a compound Grain value's cycle marker.

NOTE: If the provided value is not a compound value, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description   |
| ----- | --------- | ------------- |
| `ref` | `WasmRef` | The value ref |

Returns:

| type      | description      |
| --------- | ---------------- |
| `WasmI32` | The cycle marker |

### DataStructures.**storeCycleMarker**

```grain
storeCycleMarker: (ref: WasmRef, value: WasmI32) => Void
```

Sets a compound Grain value's cycle marker.

NOTE: If the provided value is not a compound value, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param   | type      | description             |
| ------- | --------- | ----------------------- |
| `ref`   | `WasmRef` | The value ref           |
| `value` | `WasmI32` | The cycle marker to set |

### DataStructures.**tagSimpleNumber**

```grain
tagSimpleNumber: (num: WasmI32) => Number
```

Tag a simple number.

Parameters:

| param | type      | description       |
| ----- | --------- | ----------------- |
| `num` | `WasmI32` | The number to tag |

Returns:

| type     | description       |
| -------- | ----------------- |
| `Number` | The tagged number |

### DataStructures.**untagSimpleNumber**

```grain
untagSimpleNumber: (num: Number) => WasmI32
```

Untag a simple number.

Parameters:

| param | type     | description         |
| ----- | -------- | ------------------- |
| `num` | `Number` | The number to untag |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmI32` | The untagged number |

### DataStructures.**tagChar**

```grain
tagChar: (char: WasmI32) => Char
```

Tag a char.

Parameters:

| param  | type      | description    |
| ------ | --------- | -------------- |
| `char` | `WasmI32` | The usv to tag |

Returns:

| type   | description     |
| ------ | --------------- |
| `Char` | The tagged char |

### DataStructures.**untagChar**

```grain
untagChar: (char: Char) => WasmI32
```

Untag a char.

Parameters:

| param  | type   | description       |
| ------ | ------ | ----------------- |
| `char` | `Char` | The char to untag |

Returns:

| type      | description      |
| --------- | ---------------- |
| `WasmI32` | The untagged usv |

### DataStructures.**tagInt8**

```grain
tagInt8: (int: WasmI32) => Int8
```

Tag an int8.

Parameters:

| param | type      | description     |
| ----- | --------- | --------------- |
| `int` | `WasmI32` | The int8 to tag |

Returns:

| type   | description     |
| ------ | --------------- |
| `Int8` | The tagged int8 |

### DataStructures.**untagInt8**

```grain
untagInt8: (int: Int8) => WasmI32
```

Untag an int8.

Parameters:

| param | type   | description       |
| ----- | ------ | ----------------- |
| `int` | `Int8` | The int8 to untag |

Returns:

| type      | description       |
| --------- | ----------------- |
| `WasmI32` | The untagged int8 |

### DataStructures.**tagInt16**

```grain
tagInt16: (int: WasmI32) => Int16
```

Tag an int16.

Parameters:

| param | type      | description      |
| ----- | --------- | ---------------- |
| `int` | `WasmI32` | The int16 to tag |

Returns:

| type    | description      |
| ------- | ---------------- |
| `Int16` | The tagged int16 |

### DataStructures.**untagInt16**

```grain
untagInt16: (int: Int16) => WasmI32
```

Untag an int16.

Parameters:

| param | type    | description        |
| ----- | ------- | ------------------ |
| `int` | `Int16` | The int16 to untag |

Returns:

| type      | description        |
| --------- | ------------------ |
| `WasmI32` | The untagged int16 |

### DataStructures.**tagUint8**

```grain
tagUint8: (int: WasmI32) => Uint8
```

Tag a uint8.

Parameters:

| param | type      | description      |
| ----- | --------- | ---------------- |
| `int` | `WasmI32` | The uint8 to tag |

Returns:

| type    | description      |
| ------- | ---------------- |
| `Uint8` | The tagged uint8 |

### DataStructures.**untagUint8**

```grain
untagUint8: (int: Uint8) => WasmI32
```

Untag a uint8.

Parameters:

| param | type    | description        |
| ----- | ------- | ------------------ |
| `int` | `Uint8` | The uint8 to untag |

Returns:

| type      | description        |
| --------- | ------------------ |
| `WasmI32` | The untagged uint8 |

### DataStructures.**tagUint16**

```grain
tagUint16: (int: WasmI32) => Uint16
```

Tag a uint16.

Parameters:

| param | type      | description       |
| ----- | --------- | ----------------- |
| `int` | `WasmI32` | The uint16 to tag |

Returns:

| type     | description       |
| -------- | ----------------- |
| `Uint16` | The tagged uint16 |

### DataStructures.**untagUint16**

```grain
untagUint16: (int: Uint16) => WasmI32
```

Untag a uint16.

Parameters:

| param | type     | description         |
| ----- | -------- | ------------------- |
| `int` | `Uint16` | The uint16 to untag |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmI32` | The untagged uint16 |

## DataStructures.Tuple

Utilities for working with Grain's tuple data structure.

### Types

Type declarations included in the DataStructures.Tuple module.

#### DataStructures.Tuple.**TupleRef**

```grain
type TupleRef
```

The reference type for a Grain tuple.

### Values

Functions and constants included in the DataStructures.Tuple module.

#### DataStructures.Tuple.**make**

```grain
make: (size: WasmI32) => TupleRef
```

Creates a new tuple of the specified size, with all elements initialized to `(ref i31 0)`.

Parameters:

| param  | type      | description                                          |
| ------ | --------- | ---------------------------------------------------- |
| `size` | `WasmI32` | The number of elements to be contained in this tuple |

Returns:

| type       | description                |
| ---------- | -------------------------- |
| `TupleRef` | The reference to the tuple |

#### DataStructures.Tuple.**fromGrain**

```grain
fromGrain: (tuple: a) => TupleRef
```

Converts a grain tuple to a reference that can be used with the rest of the APIs in this module.

NOTE: If the provided value is not a tuple, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param   | type | description                |
| ------- | ---- | -------------------------- |
| `tuple` | `a`  | The Grain tuple to convert |

Returns:

| type       | description                |
| ---------- | -------------------------- |
| `TupleRef` | The reference to the tuple |

#### DataStructures.Tuple.**toGrain**

```grain
toGrain: (ref: TupleRef) => a
```

Converts a tuple reference to a Grain tuple.

NOTE:
The type information of the tuple is not preserved in the reference form,
the caller must ensure that the value is of the expected type and arity,

Parameters:

| param | type       | description              |
| ----- | ---------- | ------------------------ |
| `ref` | `TupleRef` | The reference to convert |

Returns:

| type | description     |
| ---- | --------------- |
| `a`  | The Grain tuple |

#### DataStructures.Tuple.**getSize**

```grain
getSize: (ref: TupleRef) => WasmI32
```

Gets the size (number of elements) of a Grain tuple.

Parameters:

| param | type       | description                            |
| ----- | ---------- | -------------------------------------- |
| `ref` | `TupleRef` | The reference to the Grain tuple value |

Returns:

| type      | description           |
| --------- | --------------------- |
| `WasmI32` | The size of the tuple |

#### DataStructures.Tuple.**getData**

```grain
getData: (ref: TupleRef) => WasmRef.WasmArrayRef.WasmArrayRef
```

Gets the backing array containing the content of a Grain tuple.

Parameters:

| param | type       | description                            |
| ----- | ---------- | -------------------------------------- |
| `ref` | `TupleRef` | The reference to the Grain tuple value |

Returns:

| type                                | description                           |
| ----------------------------------- | ------------------------------------- |
| `WasmRef.WasmArrayRef.WasmArrayRef` | The reference to the underlying array |

## DataStructures.Array

Utilities for working with Grain's array data structure.

### Types

Type declarations included in the DataStructures.Array module.

#### DataStructures.Array.**ArrayRef**

```grain
type ArrayRef
```

The reference type for a Grain array.

### Values

Functions and constants included in the DataStructures.Array module.

#### DataStructures.Array.**make**

```grain
make: (size: WasmI32, initial: WasmRef) => ArrayRef
```

Creates a new array of the specified size, with all elements initialized to the initial value.

Parameters:

| param     | type      | description                                          |
| --------- | --------- | ---------------------------------------------------- |
| `size`    | `WasmI32` | The number of elements to be contained in this array |
| `initial` | `WasmRef` | The initial value to fill the array with             |

Returns:

| type       | description                |
| ---------- | -------------------------- |
| `ArrayRef` | The reference to the array |

#### DataStructures.Array.**fromGrain**

```grain
fromGrain: (arr: Array<a>) => ArrayRef
```

Converts a grain array to a reference that can be used with the rest of the APIs in this module.

Parameters:

| param | type       | description                |
| ----- | ---------- | -------------------------- |
| `arr` | `Array<a>` | The Grain array to convert |

Returns:

| type       | description                |
| ---------- | -------------------------- |
| `ArrayRef` | The reference to the array |

#### DataStructures.Array.**toGrain**

```grain
toGrain: (ref: ArrayRef) => Array<a>
```

Converts an array reference to a Grain array.

NOTE:
The type information of the array items are not preserved in the reference form,
the caller must ensure that the items are of the expected type, otherwise using the
resulting Grain array may lead to runtime errors or undefined behavior.

Parameters:

| param | type       | description              |
| ----- | ---------- | ------------------------ |
| `ref` | `ArrayRef` | The reference to convert |

Returns:

| type       | description     |
| ---------- | --------------- |
| `Array<a>` | The Grain array |

#### DataStructures.Array.**getSize**

```grain
getSize: (ref: ArrayRef) => WasmI32
```

Gets the size (number of elements) of a Grain array.

Parameters:

| param | type       | description                            |
| ----- | ---------- | -------------------------------------- |
| `ref` | `ArrayRef` | The reference to the Grain array value |

Returns:

| type      | description           |
| --------- | --------------------- |
| `WasmI32` | The size of the array |

#### DataStructures.Array.**getData**

```grain
getData: (ref: ArrayRef) => WasmRef.WasmArrayRef.WasmArrayRef
```

Gets the backing array containing the content of a Grain array.

Parameters:

| param | type       | description                            |
| ----- | ---------- | -------------------------------------- |
| `ref` | `ArrayRef` | The reference to the Grain array value |

Returns:

| type                                | description                           |
| ----------------------------------- | ------------------------------------- |
| `WasmRef.WasmArrayRef.WasmArrayRef` | The reference to the underlying array |

## DataStructures.Closure

Utilities for working with Grain's record closure structure.

### Values

Functions and constants included in the DataStructures.Closure module.

#### DataStructures.Closure.**getData**

```grain
getData: (ref: WasmRef) => WasmRef.WasmArrayRef.WasmArrayRef
```

Gets the underlying array containing the content of a Grain lambda closure.

NOTE: If the provided value is not a lambda closure, the behavior is undefined
      and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                                     |
| ----- | --------- | ----------------------------------------------- |
| `ref` | `WasmRef` | The reference to the Grain lambda closure value |

Returns:

| type                                | description                           |
| ----------------------------------- | ------------------------------------- |
| `WasmRef.WasmArrayRef.WasmArrayRef` | The reference to the underlying array |

## DataStructures.Record

Utilities for working with Grain's record data structure.

### Values

Functions and constants included in the DataStructures.Record module.

#### DataStructures.Record.**make**

```grain
make:
  (valuesArray: WasmRef.WasmArrayRef.WasmArrayRef, typeHash: WasmRef,
   typeId: WasmRef) => WasmRef
```

Creates a new record of the specified size, with all fields initialized to `(ref i31 0)`.

Parameters:

| param         | type                                | description                                  |
| ------------- | ----------------------------------- | -------------------------------------------- |
| `valuesArray` | `WasmRef.WasmArrayRef.WasmArrayRef` | The WasmArrayRef backing the record's fields |
| `typeHash`    | `WasmRef`                           | The records type hash                        |
| `typeId`      | `WasmRef`                           | The records type id                          |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmRef` | The reference to the record |

#### DataStructures.Record.**getTypeHash**

```grain
getTypeHash: (ref: WasmRef) => Number
```

Gets a record's type hash.

NOTE: If the provided value is not a record, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                 |
| ----- | --------- | --------------------------- |
| `ref` | `WasmRef` | The reference to the record |

Returns:

| type     | description     |
| -------- | --------------- |
| `Number` | : The type hash |

#### DataStructures.Record.**getTypeId**

```grain
getTypeId: (ref: WasmRef) => Number
```

Gets a record's type id.

NOTE: If the provided value is not a record, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                 |
| ----- | --------- | --------------------------- |
| `ref` | `WasmRef` | The reference to the record |

Returns:

| type     | description   |
| -------- | ------------- |
| `Number` | : The type id |

#### DataStructures.Record.**getSize**

```grain
getSize: (ref: WasmRef) => WasmI32
```

Get the size (arity) of a Grain record value.

NOTE: If the provided value is not an record variant, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                             |
| ----- | --------- | --------------------------------------- |
| `ref` | `WasmRef` | The reference to the Grain record value |

Returns:

| type      | description                  |
| --------- | ---------------------------- |
| `WasmI32` | The size of the record value |

#### DataStructures.Record.**getData**

```grain
getData: (ref: WasmRef) => WasmRef.WasmArrayRef.WasmArrayRef
```

Gets the backing array containing the content of a Grain record value.

NOTE: If the provided value is not an record variant, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                             |
| ----- | --------- | --------------------------------------- |
| `ref` | `WasmRef` | The reference to the Grain record value |

Returns:

| type                                | description                           |
| ----------------------------------- | ------------------------------------- |
| `WasmRef.WasmArrayRef.WasmArrayRef` | The reference to the underlying array |

## DataStructures.ADT

Utilities for working with Grain's ADT data structure.

### Values

Functions and constants included in the DataStructures.ADT module.

#### DataStructures.ADT.**make**

```grain
make:
  (valuesArray: WasmRef.WasmArrayRef.WasmArrayRef, typeHash: WasmRef,
   typeId: WasmRef, variantId: WasmRef) => WasmRef
```

Creates a new ADT value (enum variant) with the specified type and variant information,
and the provided values array as its content.

Parameters:

| param         | type                                | description                                   |
| ------------- | ----------------------------------- | --------------------------------------------- |
| `valuesArray` | `WasmRef.WasmArrayRef.WasmArrayRef` | The `WasmArrayRef` backing the ADT's contents |
| `typeHash`    | `WasmRef`                           | The ADT's type hash                           |
| `typeId`      | `WasmRef`                           | The ADT's type id                             |
| `variantId`   | `WasmRef`                           | The ADT variant id                            |

Returns:

| type      | description                    |
| --------- | ------------------------------ |
| `WasmRef` | The reference to the ADT value |

#### DataStructures.ADT.**getTypeHash**

```grain
getTypeHash: (ref: WasmRef) => Number
```

Gets a variant's type hash.

NOTE: If the provided value is not an ADT variant, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                  |
| ----- | --------- | ---------------------------- |
| `ref` | `WasmRef` | The reference to the variant |

Returns:

| type     | description     |
| -------- | --------------- |
| `Number` | : The type hash |

#### DataStructures.ADT.**getTypeId**

```grain
getTypeId: (ref: WasmRef) => Number
```

Gets a variant's type id.

NOTE: If the provided value is not an ADT variant, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                  |
| ----- | --------- | ---------------------------- |
| `ref` | `WasmRef` | The reference to the variant |

Returns:

| type     | description   |
| -------- | ------------- |
| `Number` | : The type id |

#### DataStructures.ADT.**getVariantId**

```grain
getVariantId: (ref: WasmRef) => Number
```

Gets the variant id of an ADT value.

Parameters:

| param | type      | description                  |
| ----- | --------- | ---------------------------- |
| `ref` | `WasmRef` | The reference to the variant |

Returns:

| type     | description                     |
| -------- | ------------------------------- |
| `Number` | The variant id of the ADT value |

#### DataStructures.ADT.**getSize**

```grain
getSize: (ref: WasmRef) => WasmI32
```

Get the size (arity) of a Grain ADT value.

NOTE: If the provided value is not an ADT variant, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                          |
| ----- | --------- | ------------------------------------ |
| `ref` | `WasmRef` | The reference to the Grain ADT value |

Returns:

| type      | description               |
| --------- | ------------------------- |
| `WasmI32` | The size of the ADT value |

#### DataStructures.ADT.**getData**

```grain
getData: (ref: WasmRef) => WasmRef.WasmArrayRef.WasmArrayRef
```

Gets the backing array containing the content of a Grain ADT value.

NOTE: If the provided value is not an ADT variant, the behavior is undefined and will lead to runtime errors, use with caution.

Parameters:

| param | type      | description                          |
| ----- | --------- | ------------------------------------ |
| `ref` | `WasmRef` | The reference to the Grain ADT value |

Returns:

| type                                | description                           |
| ----------------------------------- | ------------------------------------- |
| `WasmRef.WasmArrayRef.WasmArrayRef` | The reference to the underlying array |

## DataStructures.String

Utilities for working with Grain's string data structure.

### Types

Type declarations included in the DataStructures.String module.

#### DataStructures.String.**StringRef**

```grain
type StringRef
```

The reference type for a Grain string.

### Values

Functions and constants included in the DataStructures.String module.

#### DataStructures.String.**make**

```grain
make: (size: WasmI32) => StringRef
```

Creates a new string of the specified size, with all characters initialized to `U+0000`.

Parameters:

| param  | type      | description                                   |
| ------ | --------- | --------------------------------------------- |
| `size` | `WasmI32` | The size (in bytes) of the string to allocate |

Returns:

| type        | description                 |
| ----------- | --------------------------- |
| `StringRef` | The reference to the string |

#### DataStructures.String.**fromGrain**

```grain
fromGrain: (str: String) => StringRef
```

Converts a grain string to a reference that can be used with the rest of the APIs in this module.

Parameters:

| param | type     | description                 |
| ----- | -------- | --------------------------- |
| `str` | `String` | The Grain string to convert |

Returns:

| type        | description                 |
| ----------- | --------------------------- |
| `StringRef` | The reference to the string |

#### DataStructures.String.**toGrain**

```grain
toGrain: (ref: StringRef) => String
```

Converts a string reference to a Grain string.

Parameters:

| param | type        | description              |
| ----- | ----------- | ------------------------ |
| `ref` | `StringRef` | The reference to convert |

Returns:

| type     | description      |
| -------- | ---------------- |
| `String` | The Grain string |

#### DataStructures.String.**getSize**

```grain
getSize: (ref: StringRef) => WasmI32
```

Gets the size of a Grain string in bytes.

Parameters:

| param | type        | description                             |
| ----- | ----------- | --------------------------------------- |
| `ref` | `StringRef` | The reference to the Grain String value |

Returns:

| type      | description            |
| --------- | ---------------------- |
| `WasmI32` | The size of the string |

#### DataStructures.String.**getData**

```grain
getData: (ref: StringRef) => WasmRef.WasmArrayRef.WasmArrayRef
```

Gets the backing array containing the content of a Grain string.

Grain strings are backed by a byte array of UTF-8 encoded characters,
more information about the string layout can be found in the contributor docs.

Parameters:

| param | type        | description                             |
| ----- | ----------- | --------------------------------------- |
| `ref` | `StringRef` | The reference to the Grain String value |

Returns:

| type                                | description                           |
| ----------------------------------- | ------------------------------------- |
| `WasmRef.WasmArrayRef.WasmArrayRef` | The reference to the underlying array |

## DataStructures.Bytes

Utilities for working with Grain's bytes data structure.

### Types

Type declarations included in the DataStructures.Bytes module.

#### DataStructures.Bytes.**BytesRef**

```grain
type BytesRef
```

The reference type for Grain bytes.

### Values

Functions and constants included in the DataStructures.Bytes module.

#### DataStructures.Bytes.**make**

```grain
make: (size: WasmI32) => BytesRef
```

Creates a new bytes buffer of the specified size, with all bytes initialized to 0.

Parameters:

| param  | type      | description                                        |
| ------ | --------- | -------------------------------------------------- |
| `size` | `WasmI32` | The number of bytes to be contained in this buffer |

Returns:

| type       | description                |
| ---------- | -------------------------- |
| `BytesRef` | The reference to the bytes |

#### DataStructures.Bytes.**fromGrain**

```grain
fromGrain: (bytes: Bytes) => BytesRef
```

Converts a grain bytes value to a reference that can be used with the rest of the APIs in this module.

Parameters:

| param   | type    | description                      |
| ------- | ------- | -------------------------------- |
| `bytes` | `Bytes` | The Grain bytes value to convert |

Returns:

| type       | description                      |
| ---------- | -------------------------------- |
| `BytesRef` | The reference to the bytes value |

#### DataStructures.Bytes.**toGrain**

```grain
toGrain: (ref: BytesRef) => Bytes
```

Converts a bytes reference to a Grain bytes value.

Parameters:

| param | type       | description              |
| ----- | ---------- | ------------------------ |
| `ref` | `BytesRef` | The reference to convert |

Returns:

| type    | description           |
| ------- | --------------------- |
| `Bytes` | The Grain bytes value |

#### DataStructures.Bytes.**getSize**

```grain
getSize: (ref: BytesRef) => WasmI32
```

Gets the size of a Grain Bytes value.

Parameters:

| param | type       | description                            |
| ----- | ---------- | -------------------------------------- |
| `ref` | `BytesRef` | The reference to the Grain Bytes value |

Returns:

| type      | description           |
| --------- | --------------------- |
| `WasmI32` | The size of the bytes |

#### DataStructures.Bytes.**getData**

```grain
getData: (ref: BytesRef) => WasmRef.WasmArrayRef.WasmArrayRef
```

Gets the backing array containing the bytes of a Grain Bytes value.

Parameters:

| param | type       | description                            |
| ----- | ---------- | -------------------------------------- |
| `ref` | `BytesRef` | The reference to the Grain Bytes value |

Returns:

| type                                | description                           |
| ----------------------------------- | ------------------------------------- |
| `WasmRef.WasmArrayRef.WasmArrayRef` | The reference to the underlying array |

## DataStructures.Int32

Utilities for working with Grain's int32 data structure.

### Values

Functions and constants included in the DataStructures.Int32 module.

#### DataStructures.Int32.**make**

```grain
make: (int: WasmI32) => WasmRef
```

Creates a new Int32 with the specified value.

Parameters:

| param | type      | description        |
| ----- | --------- | ------------------ |
| `int` | `WasmI32` | The value to store |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the Int32 |

#### DataStructures.Int32.**getValue**

```grain
getValue: (ref: WasmRef) => WasmI32
```

Gets the value of an Int32

Parameters:

| param | type      | description                      |
| ----- | --------- | -------------------------------- |
| `ref` | `WasmRef` | The reference to the Int32 value |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmI32` | The inner i32 value |

## DataStructures.Uint32

Utilities for working with Grain's uint32 data structure.

### Values

Functions and constants included in the DataStructures.Uint32 module.

#### DataStructures.Uint32.**make**

```grain
make: (int: WasmI32) => WasmRef
```

Creates a new Uint32 with the specified value.

Parameters:

| param | type      | description        |
| ----- | --------- | ------------------ |
| `int` | `WasmI32` | The value to store |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmRef` | The reference to the Uint32 |

#### DataStructures.Uint32.**getValue**

```grain
getValue: (ref: WasmRef) => WasmI32
```

Gets the value of a Uint32

Parameters:

| param | type      | description                       |
| ----- | --------- | --------------------------------- |
| `ref` | `WasmRef` | The reference to the Uint32 value |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmI32` | The inner i32 value |

## DataStructures.Int64

Utilities for working with Grain's int64 data structure.

### Values

Functions and constants included in the DataStructures.Int64 module.

#### DataStructures.Int64.**make**

```grain
make: (int: WasmI64) => WasmRef
```

Creates a new Int64 with the specified value.

Parameters:

| param | type      | description        |
| ----- | --------- | ------------------ |
| `int` | `WasmI64` | The value to store |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmRef` | The reference to the Int64 |

#### DataStructures.Int64.**getValue**

```grain
getValue: (ref: WasmRef) => WasmI64
```

Gets the value of an Int64

Parameters:

| param | type      | description                      |
| ----- | --------- | -------------------------------- |
| `ref` | `WasmRef` | The reference to the Int64 value |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmI64` | The inner i64 value |

## DataStructures.Uint64

Utilities for working with Grain's uint64 data structure.

### Values

Functions and constants included in the DataStructures.Uint64 module.

#### DataStructures.Uint64.**make**

```grain
make: (int: WasmI64) => WasmRef
```

Creates a new Uint64 with the specified value.

Parameters:

| param | type      | description        |
| ----- | --------- | ------------------ |
| `int` | `WasmI64` | The value to store |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmRef` | The reference to the Uint64 |

#### DataStructures.Uint64.**getValue**

```grain
getValue: (ref: WasmRef) => WasmI64
```

Gets the value of a Uint64

Parameters:

| param | type      | description                       |
| ----- | --------- | --------------------------------- |
| `ref` | `WasmRef` | The reference to the Uint64 value |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmI64` | The inner i64 value |

## DataStructures.Float32

Utilities for working with Grain's float32 data structure.

### Values

Functions and constants included in the DataStructures.Float32 module.

#### DataStructures.Float32.**make**

```grain
make: (float: WasmF32) => WasmRef
```

Creates a new Float32 with the specified value.

Parameters:

| param   | type      | description        |
| ------- | --------- | ------------------ |
| `float` | `WasmF32` | The value to store |

Returns:

| type      | description                  |
| --------- | ---------------------------- |
| `WasmRef` | the reference to the Float32 |

#### DataStructures.Float32.**getValue**

```grain
getValue: (ref: WasmRef) => WasmF32
```

Gets the value of a Float32

Parameters:

| param | type      | description                        |
| ----- | --------- | ---------------------------------- |
| `ref` | `WasmRef` | The reference to the Float32 value |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmF32` | The inner f32 value |

## DataStructures.Float64

Utilities for working with Grain's float64 data structure.

### Values

Functions and constants included in the DataStructures.Float64 module.

#### DataStructures.Float64.**make**

```grain
make: (float: WasmF64) => WasmRef
```

Creates a new Float64 with the specified value.

Parameters:

| param   | type      | description        |
| ------- | --------- | ------------------ |
| `float` | `WasmF64` | The value to store |

Returns:

| type      | description                  |
| --------- | ---------------------------- |
| `WasmRef` | The reference to the Float64 |

#### DataStructures.Float64.**getValue**

```grain
getValue: (ref: WasmRef) => WasmF64
```

Gets the value of a Float64

Parameters:

| param | type      | description                        |
| ----- | --------- | ---------------------------------- |
| `ref` | `WasmRef` | The reference to the Float64 value |

Returns:

| type      | description         |
| --------- | ------------------- |
| `WasmF64` | The inner f64 value |

## DataStructures.Rational

Utilities for working with Grain's rational data structure.

### Values

Functions and constants included in the DataStructures.Rational module.

#### DataStructures.Rational.**make**

```grain
make: (numerator: WasmRef, denominator: WasmRef) => WasmRef
```

Creates a new Rational with the specified numerator and denominator values.

Parameters:

| param         | type      | description                    |
| ------------- | --------- | ------------------------------ |
| `numerator`   | `WasmRef` | The numerator value to store   |
| `denominator` | `WasmRef` | The denominator value to store |

Returns:

| type      | description                   |
| --------- | ----------------------------- |
| `WasmRef` | The reference to the Rational |

#### DataStructures.Rational.**getNumerator**

```grain
getNumerator: (ref: WasmRef) => WasmRef
```

Gets the numerator of a Rational

Parameters:

| param | type      | description                         |
| ----- | --------- | ----------------------------------- |
| `ref` | `WasmRef` | The reference to the Rational value |

Returns:

| type      | description            |
| --------- | ---------------------- |
| `WasmRef` | The rational numerator |

#### DataStructures.Rational.**getDenominator**

```grain
getDenominator: (ref: WasmRef) => WasmRef
```

Gets the denominator of a Rational

Parameters:

| param | type      | description                         |
| ----- | --------- | ----------------------------------- |
| `ref` | `WasmRef` | The reference to the Rational value |

Returns:

| type      | description              |
| --------- | ------------------------ |
| `WasmRef` | The rational denominator |

## DataStructures.BigInt

Utilities for working with Grain's bigint data structure.

### Values

Functions and constants included in the DataStructures.BigInt module.

#### DataStructures.BigInt.**make**

```grain
make: (size: WasmI32) => WasmRef
```

Creates a new BigInt with the specified number of limbs, with all limbs initialized to 0, and returns a reference to it.

Parameters:

| param  | type      | description         |
| ------ | --------- | ------------------- |
| `size` | `WasmI32` | The number of limbs |

Returns:

| type      | description                 |
| --------- | --------------------------- |
| `WasmRef` | The reference to the BigInt |

#### DataStructures.BigInt.**getSize**

```grain
getSize: (ref: WasmRef) => WasmI32
```

Gets number of limbs of a BigInt.

Parameters:

| param | type      | description                       |
| ----- | --------- | --------------------------------- |
| `ref` | `WasmRef` | The reference to the BigInt value |

Returns:

| type      | description                |
| --------- | -------------------------- |
| `WasmI32` | The number of BigInt limbs |

#### DataStructures.BigInt.**getFlags**

```grain
getFlags: (ref: WasmRef) => WasmI32
```

Gets the flags of a BigInt.

Parameters:

| param | type      | description                       |
| ----- | --------- | --------------------------------- |
| `ref` | `WasmRef` | The reference to the BigInt value |

Returns:

| type      | description      |
| --------- | ---------------- |
| `WasmI32` | The BigInt flags |

#### DataStructures.BigInt.**setFlags**

```grain
setFlags: (ref: WasmRef, flags: WasmI32) => Void
```

Sets the flags of a BigInt.

Parameters:

| param   | type      | description                       |
| ------- | --------- | --------------------------------- |
| `ref`   | `WasmRef` | The reference to the BigInt value |
| `flags` | `WasmI32` | The BigInt flags to set           |

#### DataStructures.BigInt.**getLimbData**

```grain
getLimbData: (ref: WasmRef) => WasmRef.WasmArrayRef.WasmArrayRef
```

Gets the reference to the underlying array containing the BigInt limbs,
the layout of which can be found in the contributor docs.

Parameters:

| param | type      | description                       |
| ----- | --------- | --------------------------------- |
| `ref` | `WasmRef` | The reference to the BigInt value |

Returns:

| type                                | description                       |
| ----------------------------------- | --------------------------------- |
| `WasmRef.WasmArrayRef.WasmArrayRef` | The reference to the BigInt limbs |

