---
title: Int32
---

Operations on the Int32 type.

```grain
import Int32 from "int32"
```

## Values

Functions for working with the `Innt32` data type.

### Int32.**fromNumber**

```grain
fromNumber : Number -> Int32
```

Coverts the argument from a `Number` to a `Int32`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The `Number` to convert|

Returns:

|type|description|
|----|-----------|
|`Int32`|The converted `Number`|

### Int32.**toNumber**

```grain
toNumber : Int32 -> Number
```

Coverts the argument from a `Int32` to a `Number`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The `Int32` to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The converted `Int32`|

### Int32.**incr**

```grain
incr : Int32 -> Int32
```

Increments the number by 1.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The `Int32` to increment|

Returns:

|type|description|
|----|-----------|
|`Int32`|The incremented `Int32`|

### Int32.**decr**

```grain
decr : Int32 -> Int32
```

Decrements the number by 1.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The `Int32` to decrement|

Returns:

|type|description|
|----|-----------|
|`Int32`|The decremented `Int32`|

### Int32.**add**

```grain
add : (Int32, Int32) -> Int32
```

Integer addition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The sum of the two operands|

### Int32.**sub**

```grain
sub : (Int32, Int32) -> Int32
```

Integer subtraction.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The difference of the two operands|

### Int32.**mul**

```grain
mul : (Int32, Int32) -> Int32
```

Integer multiplication.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The product of the two operands|

### Int32.**div**

```grain
div : (Int32, Int32) -> Int32
```

Integer division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The quotient of the two operands|

### Int32.**divU**

```grain
divU : (Int32, Int32) -> Int32
```

Unsigned integer division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The quotient of the two operands|

### Int32.**rem**

```grain
rem : (Int32, Int32) -> Int32
```

Integer remainder.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The signed remainder from a division of its two operands|

### Int32.**remU**

```grain
remU : (Int32, Int32) -> Int32
```

Unsigned integer remainder.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The unsigned remainder from a division of its two operands|

### Int32.**mod**

```grain
( mod ) : (Int32, Int32) -> Int32
```

Integer modulo.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The remainder from a division of its two operands|

### Int32.**clz**

```grain
clz : Int32 -> Int32
```

Integer count leading zeros.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The number of leading zeros in its operand|

### Int32.**ctz**

```grain
ctz : Int32 -> Int32
```

Integer count trailing zeros.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The number of trailing zeros in its operand|

### Int32.**popcnt**

```grain
popcnt : Int32 -> Int32
```

Integer population count.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The number of `1-bits` in its operand|

### Int32.**rotl**

```grain
rotl : (Int32, Int32) -> Int32
```

Integer rotate left.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The value of the first operand rotated to the left by the shift count|

### Int32.**rotr**

```grain
rotr : (Int32, Int32) -> Int32
```

Integer rotate right.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first operand|
|`y`|`Int32`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int32`|The value of the first operand rotated to the right by the shift count|

## Comparisons

Comparison functions for working with integers.

### Int32.**eq**

```grain
eq : (Int32, Int32) -> Bool
```

Integer equal.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`y`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the arguments are equal|

### Int32.**ne**

```grain
ne : (Int32, Int32) -> Bool
```

Integer inequality.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`y`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the arguments are not equal|

### Int32.**eqz**

```grain
eqz : Int32 -> Bool
```

Integer equal to zero.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean indicating if the argument is equal to zero|

### Int32.**lt**

```grain
lt : (Int32, Int32) -> Bool
```

Checks if the first argument is strictly less than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`y`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is less then the second argument|

### Int32.**gt**

```grain
gt : (Int32, Int32) -> Bool
```

Checks if the first argument is strictly greater than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`y`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is greater then the second argument|

### Int32.**lte**

```grain
lte : (Int32, Int32) -> Bool
```

Checks if the first argument is strictly less then or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`y`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is less then or equal to the second argument|

### Int32.**gte**

```grain
gte : (Int32, Int32) -> Bool
```

Checks if the first argument is strictly greater then or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`y`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is greater then or equal to the second argument|

## Logical operations

logical operations for working with Integers.

### Int32.**lnot**

```grain
lnot : Int32 -> Int32
```

Computes the bitwise logical "not" of the argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|

Returns:

|type|description|
|----|-----------|
|`Int32`|The bitwise logical "not" of the argument|

### Int32.**land**

```grain
( land ) : (Int32, Int32) -> Int32
```

Computes the bitwise logical "and" of the argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|

Returns:

|type|description|
|----|-----------|
|`Int32`|The bitwise logical "and" of the argument|

### Int32.**lor**

```grain
( lor ) : (Int32, Int32) -> Int32
```

Computes the bitwise logical "or" of the argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|

Returns:

|type|description|
|----|-----------|
|`Int32`|The bitwise logical "or" of the argument|

### Int32.**lxor**

```grain
( lxor ) : (Int32, Int32) -> Int32
```

Computes the bitwise logical "xor" of the argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`x`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Int32`|The bitwise logical "xor" of the argument|

### Int32.**shl**

```grain
shl : (Int32, Int32) -> Int32
```

Computes the bitwise arithmetic (signed) shift right of the first argument by the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`x`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Int32`|The bitwise logical shift left of the first argument by the second argument|

### Int32.**shr**

```grain
shr : (Int32, Int32) -> Int32
```

Computes the bitwise logical shift right of the first argument by the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`x`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Int32`|The bitwise arithmetic (signed) shift right of the first argument by the second argument|

### Int32.**shrU**

```grain
shrU : (Int32, Int32) -> Int32
```

Computes the unsigned bitwise logical shift right of the first argument by the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int32`|The first argument|
|`x`|`Int32`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Int32`|The bitwise arithmetic (unsigned) shift right of the first argument by the second argument|

