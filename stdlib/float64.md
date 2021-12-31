---
title: Float64
---

Utilities for working with the Float64 data type.

```grain
import Float64 from "float64"
```

## Values

Functions for working with the Float64 data type.

### Float64.**fromNumber**

```grain
fromNumber : Number -> Float64
```

Converts a Number to a Float64.

Parameters:

|param|type|description|
|-----|----|-----------|
|`number`|`Number`|The Number to convert|

Returns:

|type|description|
|----|-----------|
|`Float64`|The Number represented as a Float64|

### Float64.**toNumber**

```grain
toNumber : Float64 -> Number
```

Converts a Float64 to a Number.

Parameters:

|param|type|description|
|-----|----|-----------|
|`float`|`Float64`|The Float64 to convert|

Returns:

|type|description|
|----|-----------|
|`Number`|The Float64 represented as a Number|

### Float64.**add**

```grain
add : (Float64, Float64) -> Float64
```

Computes the sum of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The sum of the two operands|

### Float64.**sub**

```grain
sub : (Float64, Float64) -> Float64
```

Computes the difference of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The difference of the two operands|

### Float64.**mul**

```grain
mul : (Float64, Float64) -> Float64
```

Computes the product of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The product of the two operands|

### Float64.**div**

```grain
div : (Float64, Float64) -> Float64
```

Computes the quotient of its operands.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first operand|
|`y`|`Float64`|The second operand|

Returns:

|type|description|
|----|-----------|
|`Float64`|The quotient of the two operands|

## Comparisons

Comparison functions for working with floating points.

### Float64.**lt**

```grain
lt : (Float64, Float64) -> Bool
```

Checks if the first argument is strictly less than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first argument|
|`y`|`Float64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first argument is less then the second argument, `false` otherwise|

### Float64.**gt**

```grain
gt : (Float64, Float64) -> Bool
```

Checks if the first argument is strictly greater than the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first argument|
|`y`|`Float64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first argument is greater then the second argument, `false` otherwise|

### Float64.**lte**

```grain
lte : (Float64, Float64) -> Bool
```

Checks if the first argument is less than or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first argument|
|`y`|`Float64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first argument is less than or equal to the second argument, `false` otherwise|

### Float64.**gte**

```grain
gte : (Float64, Float64) -> Bool
```

Checks if the first argument is greater than or equal to the second argument.

Parameters:

|param|type|description|
|-----|----|-----------|
|`x`|`Float64`|The first argument|
|`y`|`Float64`|The second argument|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the first argument is greater than or equal to the second argument, `false` otherwise|

## Constants

Constants related to floating points.

### Float64.**infinity**

```grain
infinity : Float64
```

Infinity represented as a `Float64`.

### Float64.**nan**

```grain
nan : Float64
```

Nan represented as a `Float64`.

