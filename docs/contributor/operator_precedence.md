# Grain's Operator Precedence

This table shows which operators take precedence over other operators, and can be used as a reference when parsing. 

Grain also allows the definition of custom infix operators. Custom operators in Grain must be prefixed by one of the operators in the table below. Its precedence is defined by its prefix.

In addition to these prefixes, the parser accepts the characters `$&*/+-=><^|!?%:.` for use in the names of custom operators.

It should be noted that `/*` and `//` are disallowed as operators as they denote comments.

| Precedence | Operator type | Associativity | Operators |
|-|-|-|-|
| 180 | Grouping | n/a | `( … )` |
| 170 | Member Access<br>Computed Member Access<br>Function Call | left-to-right<br>left-to-right<br>left-to-right | `… . …`<br>`… [ … ]`<br>`… ( … )` |
| 160 | Postfix Increment<br>Postfix Decrement | n/a | NYI |
| 150 | Logical NOT<br>Bitwise NOT<br>Unary Negation<br>Prefix Increment<br>Prefix Decrement | right-to-left | `! …`<br>NYI<br>`- …`<br>NYI<br>NYI |
| 140 | Annotation | left-to-right | `… : …` |
| 130 | Exponentiation | right-to-left | NYI |
| 120 | Multiplication<br>Division<br>Modulus | left-to-right | `… * …`<br>`… / …`<br>`… % …` |
| 110 | Addition<br>Subtraction<br>String Concatenation | left-to-right | `… + …`<br>`… - …`<br>`… ++ …` |
| 100 | Bitwise Shift Left<br>Bitwise Shift Right<br>Bitwise Arithmetic Shift Right | left-to-right | `… << …` <br> `… >> …` <br> `… >>> …` |
| 90 | Less Than<br>Less Than or Equal To<br>Greater Than<br>Greater Than or Equal To | left-to-right | `… < …`<br>`… <= …`<br>`… > …`<br>`… >= …` |
| 80 | Structural Equality<br>Structural Inequality<br>Physical Equality<br>Physical Inequality | left-to-right | `… == …`<br>`… != …`<br>`… is …`<br>`… isnt …` |
| 70 | Bitwise AND | left-to-right | `… & …` |
| 60 | Bitwise XOR | left-to-right | `… ^ …` |
| 50 | Bitwise OR | left-to-right | `… \| …` |
| 40 | Logical AND | left-to-right | `… && …` |
| 30 | Logical OR<br>Coalesing (NYI) | left-to-right | `… \|\| …`<br>`… ?? …` |
| 20 | Ternary Conditional | right-to-left | NYI |
| 10 | Assignment | right-to-left | `… = …`<br>`… := …`<br>`… += …`<br>`… -= …`<br>`… *= …`<br>`… /= …`<br>`… %= …` |
