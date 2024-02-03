---
title: TypeGrainDoc
---

## Types

Type declarations included in the TypeGrainDoc module.

### TypeGrainDoc.**R**

```grain
record R {
  x: Number,
  y: String,
  z: String,
}
```

A record

Fields:

|name|type|description|
|----|----|-----------|
|`x`|`Number`|Record field x|
|`y`|`String`|Record field y<br/>Second line|
|`z`|`String`||

### TypeGrainDoc.**R2**

```grain
record R2 {
  x2: Number,
  y2: String,
  z2: String,
}
```

A record that should not have any fields printed

### TypeGrainDoc.**E**

```grain
enum E {
  Variant1,
  Variant2(String, Number),
  Variant3{
    a: Number,
    f: Number => Number,
  },
  Variant4,
  Variant5{
    a: Number,
    b: Number,
  },
}
```

An enum

Variants:

```grain
Variant1
```

Enum variant

```grain
Variant2(String, Number)
```

Another variant

```grain
Variant3{
  a: Number,
  f: Number => Number,
}
```

Record variant

Fields:

|name|type|description|
|----|----|-----------|
|`a`|`Number`|Record field a|
|`f`|`Number => Number`|Function|

```grain
Variant5{
  a: Number,
  b: Number,
}
```

Fields:

|name|type|description|
|----|----|-----------|
|`a`|`Number`|Record field|
|`b`|`Number`||

### TypeGrainDoc.**E2**

```grain
enum E2 {
  Var1,
  Var2(String, Number),
  Var3{
    a: Number,
    f: Number => Number,
  },
}
```

An enum that should not have any variants printed

### TypeGrainDoc.**E3**

```grain
enum E3 {
  Rec{
    a: Number,
    b: String,
  },
}
```

Variants:

```grain
Rec{
  a: Number,
  b: String,
}
```

Fields:

|name|type|description|
|----|----|-----------|
|`a`|`Number`|A description|
|`b`|`String`||

### TypeGrainDoc.**Num**

```grain
type Num = Number
```

A regular type

