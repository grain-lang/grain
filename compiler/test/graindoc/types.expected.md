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

|field name|description|
|----------|-----------|
|`x`|Record field x|
|`y`|Record field y<br />Second line|

### TypeGrainDoc.**E**

```grain
enum E {
  Variant1,
  Variant2(String, Number),
}
```

An enum

Variants:

|variant name|description|
|------------|-----------|
|`Variant1`|Enum variant|
|`Variant2`|Another variant|

### TypeGrainDoc.**Num**

```grain
type Num = Number
```

A regular type

