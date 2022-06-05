### Pervasives.**incr**

```grain
incr : Number -> Number
```

### Pervasives.**decr**

```grain
decr : Number -> Number
```

### Pervasives.**+**

```grain
(+) : (Number, Number) -> Number
```

### Pervasives.**-**

```grain
(-) : (Number, Number) -> Number
```

### Pervasives.*****

```grain
(*) : (Number, Number) -> Number
```

### Pervasives.**/**

```grain
(/) : (Number, Number) -> Number
```

### Pervasives.**%**

```grain
(%) : (Number, Number) -> Number
```

### Pervasives.**<**

```grain
(<) : (Number, Number) -> Bool
```

### Pervasives.**>**

```grain
(>) : (Number, Number) -> Bool
```

### Pervasives.**<=**

```grain
(<=) : (Number, Number) -> Bool
```

### Pervasives.**>=**

```grain
(>=) : (Number, Number) -> Bool
```

### Pervasives.**lnot**

```grain
lnot : Number -> Number
```

### Pervasives.**&**

```grain
(&) : (Number, Number) -> Number
```

### Pervasives.**|**

```grain
(|) : (Number, Number) -> Number
```

### Pervasives.**^**

```grain
(^) : (Number, Number) -> Number
```

### Pervasives.**<<**

```grain
(<<) : (Number, Number) -> Number
```

### Pervasives.**>>>**

```grain
(>>>) : (Number, Number) -> Number
```

### Pervasives.**>>**

```grain
(>>) : (Number, Number) -> Number
```

### Pervasives.**!**

```grain
(!) : Bool -> Bool
```

### Pervasives.**&&**

```grain
(&&) : (Bool, Bool) -> Bool
```

### Pervasives.**||**

```grain
(||) : (Bool, Bool) -> Bool
```

### Pervasives.**box**

```grain
box : a -> Box<a>
```

### Pervasives.**unbox**

```grain
unbox : Box<a> -> a
```

### Pervasives.**ignore**

```grain
ignore : a -> Void
```

### Pervasives.**assert**

```grain
assert : Bool -> Void
```

### Pervasives.**throw**

```grain
throw : Exception -> a
```

### Pervasives.**fail**

```grain
fail : String -> a
```

### Pervasives.**toString**

```grain
toString : a -> String
```

### Pervasives.**print**

```grain
print : a -> Void
```

### Pervasives.**++**

```grain
(++) : (String, String) -> String
```

### Pervasives.**==**

```grain
(==) : (a, a) -> Bool
```

### Pervasives.**!=**

```grain
(!=) : (a, a) -> Bool
```

### Pervasives.**is**

```grain
is : (a, a) -> Bool
```

### Pervasives.**isnt**

```grain
isnt : (a, a) -> Bool
```

### Pervasives.**List**

```grain
enum List<a> {
  [],
  [...](a, List<a>),
}
```

### Pervasives.**cons**

> **Deprecated:** This will be removed in a future release of Grain.

```grain
cons : (a, List<a>) -> List<a>
```

### Pervasives.**empty**

```grain
empty : List<a>
```

### Pervasives.**Option**

```grain
enum Option<a> {
  Some(a),
  None,
}
```

### Pervasives.**Result**

```grain
enum Result<t, e> {
  Ok(t),
  Err(e),
}
```

### Pervasives.**identity**

```grain
identity : a -> a
```

### Pervasives.**setupExceptions**

```grain
setupExceptions : () -> Void
```

