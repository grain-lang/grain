### List.**init**

```grain
init : (Number, (Number -> a)) -> List<a>
```

### List.**length**

```grain
length : List<a> -> Number
```

### List.**sum**

```grain
sum : List<Number> -> Number
```

### List.**reverse**

```grain
reverse : List<a> -> List<a>
```

### List.**append**

```grain
append : (List<a>, List<a>) -> List<a>
```

### List.**contains**

```grain
contains : (a, List<a>) -> Bool
```

### List.**reduce**

```grain
reduce : (((a, b) -> a), a, List<b>) -> a
```

### List.**reduceRight**

```grain
reduceRight : (((a, b) -> b), b, List<a>) -> b
```

### List.**map**

```grain
map : ((a -> b), List<a>) -> List<b>
```

### List.**mapi**

```grain
mapi : (((a, Number) -> b), List<a>) -> List<b>
```

### List.**flatMap**

```grain
flatMap : ((a -> List<b>), List<a>) -> List<b>
```

### List.**every**

```grain
every : ((a -> Bool), List<a>) -> Bool
```

### List.**some**

```grain
some : ((a -> Bool), List<a>) -> Bool
```

### List.**forEach**

```grain
forEach : ((a -> b), List<a>) -> Void
```

### List.**forEachi**

```grain
forEachi : (((a, Number) -> b), List<a>) -> Void
```

### List.**filter**

```grain
filter : ((a -> Bool), List<a>) -> List<a>
```

### List.**filteri**

```grain
filteri : (((a, Number) -> Bool), List<a>) -> List<a>
```

### List.**reject**

```grain
reject : ((a -> Bool), List<a>) -> List<a>
```

### List.**head**

```grain
head : List<a> -> Option<a>
```

### List.**tail**

```grain
tail : List<a> -> Option<List<a>>
```

### List.**nth**

```grain
nth : (Number, List<a>) -> Option<a>
```

### List.**flatten**

```grain
flatten : List<List<a>> -> List<a>
```

### List.**insert**

```grain
insert : (a, Number, List<a>) -> List<a>
```

### List.**count**

```grain
count : ((a -> Bool), List<a>) -> Number
```

### List.**part**

```grain
part : (Number, List<a>) -> (List<a>, List<a>)
```

### List.**rotate**

```grain
rotate : (Number, List<a>) -> List<a>
```

### List.**unique**

```grain
unique : List<a> -> List<a>
```

### List.**drop**

```grain
drop : (Number, List<a>) -> List<a>
```

### List.**dropWhile**

```grain
dropWhile : ((a -> Bool), List<a>) -> List<a>
```

### List.**take**

```grain
take : (Number, List<a>) -> List<a>
```

### List.**takeWhile**

```grain
takeWhile : ((a -> Bool), List<a>) -> List<a>
```

### List.**find**

```grain
find : ((a -> Bool), List<a>) -> Option<a>
```

### List.**findIndex**

```grain
findIndex : ((a -> Bool), List<a>) -> Option<Number>
```

### List.**product**

```grain
product : (List<a>, List<b>) -> List<(a, b)>
```

### List.**sub**

```grain
sub : (Number, Number, List<a>) -> List<a>
```

### List.**join**

```grain
join : (String, List<String>) -> String
```

### List.**revAppend**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.5</code></summary>
No other changes yet.
</details>

```grain
revAppend : (List<a>, List<a>) -> List<a>
```

Reverses the first list and appends the second list to the end.

Parameters:

|param|type|description|
|-----|----|-----------|
|`list1`|`List<a>`|The list to reverse|
|`list2`|`List<a>`|The list to append|

### List.**sort**

<details disabled>
<summary tabindex="-1">Added in <code>0.4.5</code></summary>
No other changes yet.
</details>

```grain
sort : (((a, a) -> Number), List<a>) -> List<a>
```

Sorts the given list based on a given comparator function. The resulting list is sorted in increasing order.

Ordering is calculated using a comparator function which takes two list elements and must return 0 if both are equal, a positive number if the first is greater, and a negative number if the first is smaller.

Parameters:

|param|type|description|
|-----|----|-----------|
|`comp`|`(a, a) -> Number`|The comparator function used to indicate sort order|
|`list`|`List<a>`|The list to be sorted|

