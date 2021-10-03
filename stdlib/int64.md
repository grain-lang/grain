---
title: Int64
---

Operations on the Int64 type.

```grain
import Int64 from "int64"
```

## Values

Functions for working with the `Int64` data type.

### Int64.**fromNumber**

```grain
fromNumber : Number -> Int64
```

Coverts the argument from a `Number` to a `Int64`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Number`|The `Number` to convert|

Returns:

|type|description|
|----|-----------|
|`Int64`|The converted `Number`|

### Int64.**toNumber**

```grain
toNumber : Int64 -> Number
```

Coverts the argument from a `Int64` to a `Number`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The `Int64` to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The converted `Int64`|

### Int64.**incr**

```grain
incr : Int64 -> Int64
```

Increments the number by 1.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The `Int64` to increment|

Returns:

|type|description|
|----|-----------|
|`Int64`|The incremented `Int64`|

### Int64.**decr**

```grain
decr : Int64 -> Int64
```

Decrements the number by 1.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The `Int64` to decrement|

Returns:

|type|description|
|----|-----------|
|`Int64`|The decremented `Int64`|

### Int64.**add**

```grain
add : (Int64, Int64) -> Int64
```

Integer addition.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The sum of the two operands|

### Int64.**sub**

```grain
sub : (Int64, Int64) -> Int64
```

Integer subtraction.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The difference of the two operands|

### Int64.**mul**

```grain
mul : (Int64, Int64) -> Int64
```

Integer multiplication.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The product of the two operands|

### Int64.**div**

```grain
div : (Int64, Int64) -> Int64
```

Integer division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The quotient of the two operands|

### Int64.**divU**

```grain
divU : (Int64, Int64) -> Int64
```

Unsigned integer division.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The quotient of the two operands|

### Int64.**rem**

```grain
rem : (Int64, Int64) -> Int64
```

Integer remainder.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The signed remainder from a division of its two operands|

### Int64.**remU**

```grain
remU : (Int64, Int64) -> Int64
```

Unsigned integer remainder.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The unsigned remainder from a division of its two operands|

### Int64.**mod**

```grain
( mod ) : (Int64, Int64) -> Int64
```

Integer modulo.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The remainder from a division of its two operands|

### Int64.**clz**

```grain
clz : Int64 -> Int64
```

Integer count leading zeros.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The number of leading zeros in its operand|

### Int64.**ctz**

```grain
ctz : Int64 -> Int64
```

Integer count trailing zeros.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The number of trailing zeros in its operand|

### Int64.**popcnt**

```grain
popcnt : Int64 -> Int64
```

Integer population count.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The number of `1-bits` in its operand|

### Int64.**rotl**

```grain
rotl : (Int64, Int64) -> Int64
```

Integer rotate left.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The value of the first operand rotated to the left by the shift count|

### Int64.**rotr**

```grain
rotr : (Int64, Int64) -> Int64
```

Integer rotate right.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first operand|
|`y`|`Int64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Int64`|The value of the first operand rotated to the right by the shift count|

## Comparisons

Comparison functions for working with integers.

### Int64.**eq**

```grain
eq : (Int64, Int64) -> Bool
```

Integer equal.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`y`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the arguments are equal|

### Int64.**ne**

```grain
ne : (Int64, Int64) -> Bool
```

Integer inequality.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`y`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the arguments are not equal|

### Int64.**eqz**

```grain
eqz : Int64 -> Bool
```

Integer equal to zero.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean indicating if the argument is equal to zero|

### Int64.**lt**

```grain
lt : (Int64, Int64) -> Bool
```

Checks if the first argument is strictly less than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`y`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is less then the second argument|

### Int64.**gt**

```grain
gt : (Int64, Int64) -> Bool
```

Checks if the first argument is strictly greater than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`y`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is greater then the second argument|

### Int64.**lte**

```grain
lte : (Int64, Int64) -> Bool
```

Checks if the first argument is strictly less then or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`y`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is less then or equal to the second argument|

### Int64.**gte**

```grain
gte : (Int64, Int64) -> Bool
```

Checks if the first argument is strictly greater then or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`y`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|A boolean representing if the first argument is greater then or equal to the second argument|

## Logical operations

logical operations for working with Integers.

### Int64.**lnot**

```grain
lnot : Int64 -> Int64
```

Computes the bitwise logical "not" of the argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise logical "not" of the argument|

### Int64.**land**

```grain
( land ) : (Int64, Int64) -> Int64
```

Computes the bitwise logical "and" of the argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise logical "and" of the argument|

### Int64.**lor**

```grain
( lor ) : (Int64, Int64) -> Int64
```

Computes the bitwise logical "or" of the argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise logical "or" of the argument|

### Int64.**lxor**

```grain
( lxor ) : (Int64, Int64) -> Int64
```

Computes the bitwise logical "xor" of the argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`x`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise logical "xor" of the argument|

### Int64.**shl**

```grain
shl : (Int64, Int64) -> Int64
```

Computes the bitwise arithmetic (signed) shift right of the first argument by the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`x`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise logical shift left of the first argument by the second argument|

### Int64.**shr**

```grain
shr : (Int64, Int64) -> Int64
```

Computes the bitwise logical shift right of the first argument by the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`x`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise arithmetic (signed) shift right of the first argument by the second argument|

### Int64.**shrU**

```grain
shrU : (Int64, Int64) -> Int64
```

Computes the unsigned bitwise logical shift right of the first argument by the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Int64`|The first argument|
|`x`|`Int64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Int64`|The bitwise arithmetic (unsigned) shift right of the first argument by the second argument|

