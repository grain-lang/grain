---
title: Path
---

Utilities for working with system paths. This module treats paths purely as a data representation and does not provide functionality for interacting with the file system.

This module explicitly encodes the difference between absolute and relative paths as part of the type.

Paths in this module abide by a special POSIX-like representation/grammar
rather than one defined by a specific operating system. The rules are as
follows:

- Path separators are denoted by `/`; occurrences of `\` will be treated as part of a path segment
- The root of an absolute path is denoted by a `/` prefix for POSIX roots or `<driveLabel>:/` for Windows roots
- The path segment `.` indicates the relative "current" directory of a path, and `..` indicates the parent directory of a path
- Literal `/` characters can be included as part of a path segment if they are escaped by a preceding backslash

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
import Path from "path"
```

## Types

Type declarations included in the Path module.

### Path.**AbsoluteRoot**

```grain
enum AbsoluteRoot {
  Root,
  Drive(String),
}
```

Represents an absolute path's anchor point.

### Path.**BasePath**

```grain
type BasePath<a>
```

### Path.**Relative**

```grain
type Relative
```

Opaque type representing a relative path.

### Path.**Absolute**

```grain
type Absolute
```

Opaque type representing an absolute path.

### Path.**PathEncoding**

```grain
enum PathEncoding {
  Windows,
  Posix,
}
```

Represents a platform-specific path encoding scheme.

### Path.**Path**

```grain
type Path<a>
```

Opaque type representing a system path.

### Path.**RelativizationError**

```grain
enum RelativizationError {
  DifferentRoots,
  ImpossibleRelativization,
}
```

Represents possible errors for the `relativeTo` operation.

## Values

Functions for working with Paths.

### Path.**absolute**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
absolute : String -> Option<Path<Absolute>>
```

Constructs a new absolute `Path` from a string representing an absolute
path, using the special path grammar defined in the `Path` module.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pathStr`|`String`|A string representing an absolute path|

Returns:

|type|description|
|----|-----------|
|`Option<Path<Absolute>>`|A `Some` variant containing the absolute path if it is successfully parsed or `None` otherwise|

### Path.**relative**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
relative : String -> Option<Path<Relative>>
```

Constructs a new relative `Path` from a string representing an relative
path, using the special path grammar defined in the `Path` module.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pathStr`|`String`|A string representing an relative path|

Returns:

|type|description|
|----|-----------|
|`Option<Path<Relative>>`|A `Some` variant containing the relative path if it is successfully parsed or `None` otherwise|

### Path.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
toString : (Path<a>, PathEncoding) -> String
```

Converts the given `Path` into a string, using the specified path encoding.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a>`|The `Path` to convert to a string|
|`encoding`|`PathEncoding`|The `PathEncoding` to use to represent the path as a string|

Returns:

|type|description|
|----|-----------|
|`String`|A string representing the given path|

Examples:

```grain
Path.toString(myPath, Path.Windows)
```

### Path.**append**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
append : (Path<a>, Path<Relative>) -> Path<a>
```

Creates a new path from a path with a relative path segment appended to it.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a>`|The base path to append the segment to|
|`toAppend`|`Path<Relative>`|The relative path to append|

Returns:

|type|description|
|----|-----------|
|`Path<a>`|A new path with the `toAppend` path appended to the `path`|

### Path.**appendString**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
appendString : (Path<a>, String) -> Option<Path<a>>
```

Appends a string representing a relative path segment to the given path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a>`|Path to append to|
|`toAppend`|`String`|Path portion to append, as a string|

Returns:

|type|description|
|----|-----------|
|`Option<Path<a>>`|A new path with the segment appended to it|

### Path.**DirsUp**

```grain
type DirsUp
```

### Path.**relativeTo**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
relativeTo :
  (Path<a>, Path<a>) -> Result<Path<Relative>, RelativizationError>
```

Attempts to construct a new relative path which will lead to the destination
path from the source path. If the source and destination are both absolute
paths but are on different roots, `Err(Path.DifferentRoots)` will be
returned, and if the route to the destination cannot be concretely
determined from the source, `Err(Path.ImpossibleRelativization)` will be
returned.

Parameters:

|param|type|description|
|-----|----|-----------|
|`source`|`Path<a>`|The source path to find a path from|
|`dest`|`Path<a>`|The destination path to find the path to|

Returns:

|type|description|
|----|-----------|
|`Result<Path<Relative>, RelativizationError>`|An `Ok` variant containing the relative path if successfully parsed or `Err` otherwise|

Examples:

```grain
// relativeTo(/usr, /usr/bin) => ./bin
```

```grain
// relativeTo(/home/me, /home/me) => .
```

```grain
// relativeTo(/usr/bin, /etc) => ../../etc
```

```grain
// relativeTo(.., ../../thing) => ../thing
```

```grain
// relativeTo(/usr/bin, C:/Users) => Err(DifferentRoots)
```

```grain
// relativeTo(../here, ./there) => Err(ImpossibleRelativization)
```

### Path.**parent**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
parent : Path<a> -> Path<a>
```

Returns the path corresponding to the parent directory of the given path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a>`|The path to get the parent of|

Returns:

|type|description|
|----|-----------|
|`Path<a>`|A path corresponding to the parent directory of the given path|

### Path.**name**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
name : Path<a> -> Option<String>
```

Returns the base name (named final segment) of a path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a>`|The path to get the name of|

Returns:

|type|description|
|----|-----------|
|`Option<String>`|A `Some` variant containing the name of the path, or `None` if the path does not have one|

Examples:

```grain
Path.name(Option.unwrap(Path.relative("./dir/file.txt"))) // Some("file.txt")
```

```grain
Path.name(Option.unwrap(Path.relative(".."))) // None
```

### Path.**stem**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
stem : Path<a> -> Option<String>
```

Returns the name of the path without the extension.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a>`|The path to get the stem of|

Returns:

|type|description|
|----|-----------|
|`Option<String>`|A `Some` variant containing the stem of the path, or `None` if the path does not have one|

### Path.**extension**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
extension : Path<a> -> Option<String>
```

Returns the extension on the name of the path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a>`|The path to get the extension of|

Returns:

|type|description|
|----|-----------|
|`Option<String>`|A `Some` variant containing the extension of the path, or `None` if the path does not have one|

### Path.**root**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
root : Path<Absolute> -> AbsoluteRoot
```

Returns the root of the absolute path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<Absolute>`|The absolute path to get the root of|

Returns:

|type|description|
|----|-----------|
|`AbsoluteRoot`|The root of the path|

