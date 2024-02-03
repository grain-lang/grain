---
title: DescriptionGrainDoc
---

## Types

Type declarations included in the DescriptionGrainDoc module.

### DescriptionGrainDoc.**DocAlias**

```grain
type DocAlias = Bool
```

Alias Doc

### DescriptionGrainDoc.**DocRecord**

```grain
record DocRecord {
  docField: Bool,
}
```

Record Doc

### DescriptionGrainDoc.**DocEnum**

```grain
enum DocEnum {
  DocCase,
}
```

Enum Doc

## Values

Functions and constants included in the DescriptionGrainDoc module.

### DescriptionGrainDoc.**docValue**

```grain
docValue : Number
```

Value Doc

### DescriptionGrainDoc.**docFunction**

```grain
docFunction : (x: Number) => Number
```

Function Doc

