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
}
```

A record

Fields:

|name|type|description|
|----|----|-----------|
|`x`|`Number`|Record field x|
|`y`|`String`|Record field y<br />Second line|

### TypeGrainDoc.**E**

```grain
enum E {
  Variant1,
  Variant2(String, Number),
  Variant3{
    a: Number,
    f: Number => Number,
  },
}
```

An enum

Variants:

|name|data|description|
|----|----|-----------|
|`Variant1`||Enum variant|
|`Variant2`|`(String, Number)`|Another variant|
|`Variant3`|<table><thead><tr><th>name</th><th>type</th><th>description</th></tr></thead><tbody><tr><td>`a`</td><td>`Number`</td><td>Record field a</td></tr><tr><td>`f`</td><td>`Number => Number`</td><td>Function</td></tr></tbody></table><br /><br />|Record variant|

### TypeGrainDoc.**Num**

```grain
type Num = Number
```

A regular type

