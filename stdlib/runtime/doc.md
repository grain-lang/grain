---
title: Doc
---

  The Doc module implements a document IR and engine for pretty-printing code.
  Concatenation of Doc.t nodes is O(1) and printing a document is O(n) to the
  size of the document.

  The most important aspect of the engine are groups and how breaks interact
  with them. By default, the engine will print a group by either breaking none
  of the break hints in that group if the entire group would fit on that line
  (known as Flat mode) or all of the break hints in that group if the group
  would not fit if printed in Flat mode (known as Breaking mode). This covers
  95% of formatting use cases, and users should tend to reach for default
  groups before considering one of the alternatives. For the remaining 5% of
  use cases, groups can also be created in FitGroups mode or FitAll mode. In
  FitGroups mode, the engine will attempt to print as many subgroups in Flat
  mode as possible on each line, breaking only when necessary. In FitAll mode,
  the engine will attempt to print as many subgroups in Breaking mode as
  possible on each line.

  Hardlines should be avoided. Instead, emit break hints and allow the engine
  to decide when breaks should be made. If hardlines must be used, consider
  using the group's ~print_width parameter to manually specify how wide the
  engine should consider the group. By default, a group is only considered as
  wide as the content leading to the first hardline.

  That's most of what you need to know to effectively use this module! Further
  details on each node are provided below for maintainers or curious consumers.

  IR nodes:
    • Empty
      Has no effect on the output of the printing engine.
    • GroupBreaker
      Causes the enclosing group to be printed in Breaking mode.
    • String
      Prints the string as-is. The `string` function is Utf8-aware.
    • Blank
      Prints the specified number of spaces.
    • BreakHint
      Tells the engine that a break can be made here. If the engine decides not
      to print a break, it prints the supplied document instead.
    • Hardline
      Forces the engine to print a newline character. Width calculations for
      the current line are truncated at the Hardline. If the `phantom` field is
      set to `true`, instead the Hardline is calculated as a zero-width non-
      breaking character (the newline is emitted in the output, but
      calculations assume it's just not there).
    • IfBroken
      If the engine has broken the current group, prints the `breaking`
      document and prints the `flat` document otherwise. Note that for FitAll
      and FitGroups groups, the `flat` document would be printed if the
      IfBroken node appears before the point at which the group is broken, as
      the engine makes that decision while printing the group (unlike default
      groups, where the engine makes this decision before printing the group).
    • Indent
      Introduces indentation equal to the number of spaces specified when the
      enclosing group is broken. When newline characters are emitted, they are
      followed by space characters equal to the amount of indentation that has
      been applied by all groups, unless this would lead to trailing
      whitespace. Note that if the enclosing group has not been broken, the
      indentation will not apply. For example, in this document,
        group(~kind=FitGroups, indent(2,
          group(indent(2, string("foo") ++ break ++ string("bar")))
        ))
      if the break hint is broken by the engine, `bar`'s indentation level will
      only be two spaces, as the outer group could never be broken by
      the engine.
    • Group
      ~kind=Auto
        The engine checks if the group would fit on the current line if printed
        in Flat mode. If so, it prints the group in Flat mode and Breaking mode
        otherwise.
      ~kind=FitGroups
        The engine begins printing the group. When it encounters a break hint,
        it checks if the following node would fit on the current line. If that
        node is a Group, its Flat mode width is used for the check. If the node
        would not fit, a break is emitted.
      ~kind=FitAll
        The engine begins printing the group. When it encounters a break hint,
        it checks if the following node would fit on the current line. If that
        node is a Group, its Breaking mode width is used for the check. If the
        node would not fit, a break is emitted.
    • Concat
      Prints the first document followed by the second document. Keeps track of
      the combined width to allow the engine to make constant-time decisions
      about line breaks.

## Types

Type declarations included in the Doc module.

### Doc.**EOL**

```grain
enum EOL {
  CRLF,
  LF,
}
```

End-of-line styles for the printing engine.

Variants:

```grain
CRLF
```

Carriage return + line feed

```grain
LF
```

Line feed

### Doc.**LayoutNode**

```grain
type LayoutNode
```

The document IR used for printing.

### Doc.**GroupType**

```grain
enum GroupType {
  Auto,
  FitGroups,
  FitAll,
}
```

The type of group to create.

Variants:

```grain
Auto
```

The engine checks if the group would fit on the current line if printed
in Flat mode. If so, it prints the group in Flat mode and Breaking mode
otherwise.

```grain
FitGroups
```

The engine begins printing the group. When it encounters a break hint,
it checks if the following node would fit on the current line. If that
node is a Group, its Flat mode width is used for the check. If the node
would not fit, a break is emitted.

```grain
FitAll
```

The engine begins printing the group. When it encounters a break hint,
it checks if the following node would fit on the current line. If that
node is a Group, its Breaking mode width is used for the check. If the
node would not fit, a break is emitted.

### Doc.**Width**

```grain
type Width
```

Represents a width that may or may not account for line breaks.

## Doc.Builder

Utilities for constructing the document IR.

### Values

Functions and constants included in the Doc.Builder module.

#### Doc.Builder.**empty**

```grain
empty: LayoutNode
```

An empty node that has no effect on the output of the printing engine.

#### Doc.Builder.**groupBreaker**

```grain
groupBreaker: LayoutNode
```

A node that causes the enclosing group to be printed in Breaking mode.

#### Doc.Builder.**string**

```grain
string: (str: String) => LayoutNode
```

A node that prints the string as-is. The `string`
function is Utf8-aware.

Parameters:

| param | type     | description         |
| ----- | -------- | ------------------- |
| `str` | `String` | The string to print |

Returns:

| type         | description                         |
| ------------ | ----------------------------------- |
| `LayoutNode` | A LayoutNode that prints the string |

#### Doc.Builder.**asciiString**

```grain
asciiString: (str: String) => LayoutNode
```

A node that prints a constant string as-is.

NOTE: This should only be used for ASCII strings, as the width is
calculated based on the number of bytes, which is not accurate for
non-ASCII strings.

Parameters:

| param | type     | description               |
| ----- | -------- | ------------------------- |
| `str` | `String` | The ascii string to print |

Returns:

| type         | description                               |
| ------------ | ----------------------------------------- |
| `LayoutNode` | A LayoutNode that prints the ascii string |

#### Doc.Builder.**blank**

```grain
blank: (c: Number) => LayoutNode
```

A node that prints the specified number of spaces.

Parameters:

| param | type     | description                   |
| ----- | -------- | ----------------------------- |
| `c`   | `Number` | The number of spaces to print |

Returns:

| type         | description                         |
| ------------ | ----------------------------------- |
| `LayoutNode` | A LayoutNode that prints the spaces |

#### Doc.Builder.**hardline**

```grain
hardline: LayoutNode
```

A node that forces the engine to print a newline character.
Width calculations for the current line are truncated at the Hardline.

#### Doc.Builder.**phantomHardline**

```grain
phantomHardline: LayoutNode
```

A node that forces the engine to print a newline character.
Width calculations for the current line are truncated at the Hardline.
The Hardline is calculated as a zero-width non-breaking character (the
newline is emitted in the output, but calculations assume it's just not
there).

#### Doc.Builder.**ifBroken**

```grain
ifBroken: (breaking: LayoutNode, flat: LayoutNode) => LayoutNode
```

Constructs a node where if the engine has broken the current group,
prints the `breaking` document and prints the `flat` document otherwise.
Note that for FitAll and FitGroups groups, the `flat` document would be
printed if the IfBroken node appears before the point at which the group
is broken, as the engine makes that decision while printing the group
(unlike default groups, where the engine makes this decision before
printing the group).

Parameters:

| param      | type         | description                                      |
| ---------- | ------------ | ------------------------------------------------ |
| `breaking` | `LayoutNode` | The document to print if the group is broken     |
| `flat`     | `LayoutNode` | The document to print if the group is not broken |

Returns:

| type         | description                                                 |
| ------------ | ----------------------------------------------------------- |
| `LayoutNode` | A LayoutNode that conditionally prints one of the documents |

#### Doc.Builder.**indent**

```grain
indent: (?count: Number, doc: LayoutNode) => LayoutNode
```

Constructs a node that introduces indentation equal to the number of spaces
specified when the enclosing group is broken. When newline characters are emitted,
they are followed by space characters equal to the amount of indentation that has
been applied by all groups, unless this would lead to trailing whitespace.
Note that if the enclosing group has not been broken, the
indentation will not apply. For example, in this document,
```grain
group(~kind=FitGroups, indent(2,
 group(indent(2, string("foo") ++ break ++ string("bar")))
))
if the break hint is broken by the engine, `bar`'s indentation level will
only be two spaces, as the outer group could never be broken by
the engine.

Parameters:

| param    | type         | description                                    |
| -------- | ------------ | ---------------------------------------------- |
| `?count` | `Number`     | The number of spaces to indent by (default: 2) |
| `doc`    | `LayoutNode` | The document to indent                         |

Returns:

| type         | description                            |
| ------------ | -------------------------------------- |
| `LayoutNode` | A LayoutNode that indents the document |

#### Doc.Builder.**group**

```grain
group:
  (?printWidth: Option<Number>, ?kind: GroupType, doc: LayoutNode) =>
   LayoutNode
```

Constructs a node that creates a group around the supplied document,
allowing the printing mode to be controlled.

Parameters:

| param         | type             | description                                              |
| ------------- | ---------------- | -------------------------------------------------------- |
| `?printWidth` | `Option<Number>` | An optional width to consider the group as when printing |
| `?kind`       | `GroupType`      | The kind of group to create (default: Auto)              |
| `doc`         | `LayoutNode`     | The document to group                                    |

Returns:

| type         | description                           |
| ------------ | ------------------------------------- |
| `LayoutNode` | A LayoutNode that groups the document |

#### Doc.Builder.**(++)**

```grain
(++): (left: LayoutNode, right: LayoutNode) => LayoutNode
```

A node that prints the first document followed by the second document.
While keeping track of the combined width to allow the engine to make
constant-time decisions about line breaks.

Parameters:

| param   | type         | description         |
| ------- | ------------ | ------------------- |
| `left`  | `LayoutNode` | The first document  |
| `right` | `LayoutNode` | The second document |

Returns:

| type         | description                                  |
| ------------ | -------------------------------------------- |
| `LayoutNode` | A LayoutNode that concatenates the documents |

#### Doc.Builder.**concatMap**

```grain
concatMap:
  (sep: ((a, a) => LayoutNode), lead: (a => LayoutNode),
   trail: (a => LayoutNode), func: ((final: Bool, a) => LayoutNode),
   lst: List<a>) => LayoutNode
```

Maps over a list, applying the given function to each element
and concatenating the results.

Parameters:

| param   | type                             | description                                                          |
| ------- | -------------------------------- | -------------------------------------------------------------------- |
| `sep`   | `(a, a) => LayoutNode`           | A function that produces a separator to be placed between elements   |
| `lead`  | `a => LayoutNode`                | A function that produces a leading document before the first element |
| `trail` | `a => LayoutNode`                | A function that produces a trailing document after the last element  |
| `func`  | `(final: Bool, a) => LayoutNode` | A function that produces a document for each element                 |
| `lst`   | `List<a>`                        | The list of elements to map over                                     |

Returns:

| type         | description                                                    |
| ------------ | -------------------------------------------------------------- |
| `LayoutNode` | A LayoutNode that represents the concatenated mapped documents |

#### Doc.Builder.**concatArrayMap**

```grain
concatArrayMap:
  (sep: ((a, a) => LayoutNode), lead: (a => LayoutNode),
   trail: (a => LayoutNode), func: ((final: Bool, a) => LayoutNode),
   arr: Array<a>) => LayoutNode
```

Maps over an array, applying the given function to each element
and concatenating the results.

Parameters:

| param   | type                             | description                                                          |
| ------- | -------------------------------- | -------------------------------------------------------------------- |
| `sep`   | `(a, a) => LayoutNode`           | A function that produces a separator to be placed between elements   |
| `lead`  | `a => LayoutNode`                | A function that produces a leading document before the first element |
| `trail` | `a => LayoutNode`                | A function that produces a trailing document after the last element  |
| `func`  | `(final: Bool, a) => LayoutNode` | A function that produces a document for each element                 |
| `arr`   | `Array<a>`                       | The array of elements to map over                                    |

Returns:

| type         | description                                                    |
| ------------ | -------------------------------------------------------------- |
| `LayoutNode` | A LayoutNode that represents the concatenated mapped documents |

#### Doc.Builder.**breakableSpace**

```grain
breakableSpace: LayoutNode
```

A node that represents a space that may be broken
if necessary.

#### Doc.Builder.**_break**

```grain
_break: LayoutNode
```

A node that represents a break.

#### Doc.Builder.**space**

```grain
space: LayoutNode
```

A node that represents a space.

#### Doc.Builder.**comma**

```grain
comma: LayoutNode
```

A node that represents a comma.

#### Doc.Builder.**commaBreakableSpace**

```grain
commaBreakableSpace: LayoutNode
```

A node that represents a comma followed by a breakable space.

#### Doc.Builder.**parens**

```grain
parens:
  (?wrap: ((doc: LayoutNode) => LayoutNode), doc: LayoutNode) => LayoutNode
```

Constructs a node wrapping the given document in parentheses.

Parameters:

| param   | type                              | description                                             |
| ------- | --------------------------------- | ------------------------------------------------------- |
| `?wrap` | `(doc: LayoutNode) => LayoutNode` | An optional wrapping function to apply (default: group) |
| `doc`   | `LayoutNode`                      | The document to wrap                                    |

Returns:

| type         | description                                         |
| ------------ | --------------------------------------------------- |
| `LayoutNode` | A LayoutNode that wraps the document in parentheses |

#### Doc.Builder.**braces**

```grain
braces:
  (?wrap: ((doc: LayoutNode) => LayoutNode), doc: LayoutNode) => LayoutNode
```

Constructs a node wrapping the given document in braces.

Parameters:

| param   | type                              | description                                             |
| ------- | --------------------------------- | ------------------------------------------------------- |
| `?wrap` | `(doc: LayoutNode) => LayoutNode` | An optional wrapping function to apply (default: group) |
| `doc`   | `LayoutNode`                      | The document to wrap                                    |

Returns:

| type         | description                                    |
| ------------ | ---------------------------------------------- |
| `LayoutNode` | A LayoutNode that wraps the document in braces |

#### Doc.Builder.**arrayBrackets**

```grain
arrayBrackets:
  (?wrap: ((doc: LayoutNode) => LayoutNode), doc: LayoutNode) => LayoutNode
```

Constructs a node wrapping the given document in array brackets.

Parameters:

| param   | type                              | description                                             |
| ------- | --------------------------------- | ------------------------------------------------------- |
| `?wrap` | `(doc: LayoutNode) => LayoutNode` | An optional wrapping function to apply (default: group) |
| `doc`   | `LayoutNode`                      | The document to wrap                                    |

Returns:

| type         | description                                            |
| ------------ | ------------------------------------------------------ |
| `LayoutNode` | A LayoutNode that wraps the document in array brackets |

#### Doc.Builder.**listBrackets**

```grain
listBrackets:
  (?wrap: ((doc: LayoutNode) => LayoutNode), doc: LayoutNode) => LayoutNode
```

Constructs a node wrapping the given document in list brackets.

Parameters:

| param   | type                              | description                                             |
| ------- | --------------------------------- | ------------------------------------------------------- |
| `?wrap` | `(doc: LayoutNode) => LayoutNode` | An optional wrapping function to apply (default: group) |
| `doc`   | `LayoutNode`                      | The document to wrap                                    |

Returns:

| type         | description                                           |
| ------------ | ----------------------------------------------------- |
| `LayoutNode` | A LayoutNode that wraps the document in list brackets |

#### Doc.Builder.**angleBrackets**

```grain
angleBrackets:
  (?wrap: ((doc: LayoutNode) => LayoutNode), doc: LayoutNode) => LayoutNode
```

Constructs a node wrapping the given document in angle brackets.

Parameters:

| param   | type                              | description                                             |
| ------- | --------------------------------- | ------------------------------------------------------- |
| `?wrap` | `(doc: LayoutNode) => LayoutNode` | An optional wrapping function to apply (default: group) |
| `doc`   | `LayoutNode`                      | The document to wrap                                    |

Returns:

| type         | description                                            |
| ------------ | ------------------------------------------------------ |
| `LayoutNode` | A LayoutNode that wraps the document in angle brackets |

#### Doc.Builder.**doubleQuotes**

```grain
doubleQuotes: (doc: LayoutNode) => LayoutNode
```

Constructs a node wrapping the given document in double quotes.

Parameters:

| param | type         | description          |
| ----- | ------------ | -------------------- |
| `doc` | `LayoutNode` | The document to wrap |

Returns:

| type         | description                                           |
| ------------ | ----------------------------------------------------- |
| `LayoutNode` | A LayoutNode that wraps the document in double quotes |

#### Doc.Builder.**singleQuotes**

```grain
singleQuotes: (doc: LayoutNode) => LayoutNode
```

Constructs a node wrapping the given document in single quotes.

Parameters:

| param | type         | description          |
| ----- | ------------ | -------------------- |
| `doc` | `LayoutNode` | The document to wrap |

Returns:

| type         | description                                           |
| ------------ | ----------------------------------------------------- |
| `LayoutNode` | A LayoutNode that wraps the document in single quotes |

#### Doc.Builder.**trailingComma**

```grain
trailingComma: LayoutNode
```

A node that represents a trailing comma to be added if the
current group is broken.

## Doc.Engine

The printing engine for LayoutNode documents.

### Values

Functions and constants included in the Doc.Engine module.

#### Doc.Engine.**print**

```grain
print:
  (write: (String => a), eol: EOL, lineWidth: Number, doc: LayoutNode) =>
   Void
```

Prints the given document using the provided write function.

Parameters:

| param       | type          | description                                               |
| ----------- | ------------- | --------------------------------------------------------- |
| `write`     | `String => a` | A function that takes a string and writes it to an output |
| `eol`       | `EOL`         | The end-of-line style to use                              |
| `lineWidth` | `Number`      | The maximum line width to use                             |
| `doc`       | `LayoutNode`  | The document to print                                     |

Returns:

| type   | description |
| ------ | ----------- |
| `Void` | Void        |

#### Doc.Engine.**toString**

```grain
toString: (eol: EOL, lineWidth: Number, doc: LayoutNode) => String
```

Converts the given document to a string using the specified
end-of-line style and line width.

Parameters:

| param       | type         | description                         |
| ----------- | ------------ | ----------------------------------- |
| `eol`       | `EOL`        | The end-of-line style to use        |
| `lineWidth` | `Number`     | The maximum line width to use       |
| `doc`       | `LayoutNode` | The document to convert to a string |

Returns:

| type     | description                               |
| -------- | ----------------------------------------- |
| `String` | The string representation of the document |

