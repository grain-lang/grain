---
title: Path
---

Utilities for working with system paths. This module treats paths purely as a data representation and does not provide functionality for interacting with the file system.

This module explicitly encodes whether a path is absolute or relative, and
whether it refers to a file or a directory, as part of the `Path` type.

Paths in this module abide by a special POSIX-like representation/grammar
rather than one defined by a specific operating system. The rules are as
follows:

- Path separators are denoted by `/`; occurrences of `\` will be treated as part of a path segment
- Absolute paths may be rooted either at the POSIX-like root `/` or at Windows-like drive roots like `C:/`
- Paths referencing files must not include trailing forward slashes, but paths referencing directories may
- The path segment `.` indicates the relative "current" directory of a path, and `..` indicates the parent directory of a path

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
  Drive(Char),
}
```

Represents an absolute path's anchor point.

### Path.**BasePath**

```grain
type BasePath<a>
```

### Path.**FileType**

```grain
type FileType<a>
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

### Path.**File**

```grain
type File
```

Opaque type representing an path referencing a file.

### Path.**Directory**

```grain
type Directory
```

Opaque type representing an path referencing a directory.

### Path.**Path**

```grain
type Path<a, b>
```

Opaque type representing a system path, parameterized on
(relative|absolute, file|directory).

### Path.**PathWithType**

```grain
enum PathWithType {
  AbsoluteFile(Path<Absolute, File>),
  AbsoluteDir(Path<Absolute, Directory>),
  RelativeFile(Path<Relative, File>),
  RelativeDir(Path<Relative, Directory>),
}
```

Wraps each possibile path type to a variant; can be used to dynamically
determine the type of the path wrapped in cases where it is needed. It is
recommended to only make use of this type in cases where it is unavoidable.

### Path.**PathEncoding**

```grain
enum PathEncoding {
  Windows,
  Posix,
}
```

Represents a platform-specific path encoding scheme.

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

### Path.**absoluteFile**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
absoluteFile : String -> Option<Path<Absolute, File>>
```

Constructs a new absolute file `Path` from a string representing an absolute
path, using the special path grammar defined by this module.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pathStr`|`String`|A string representing an absolute path|

Returns:

|type|description|
|----|-----------|
|`Option<Path<Absolute, File>>`|A `Some` variant containing the absolute file path if it is successfully parsed or `None` otherwise|

Examples:

```grain
Path.absoluteFile("/usr/bin/bash")
```

```grain
Path.absoluteFile("C:/Users/me/file.txt")
```

```grain
Path.absoluteFile("C:\\Users\\me\\file.txt") == None // backslashes are not treated as path separators
```

```grain
Path.absoluteFile("/Users/me/dir/") == None // trailing slashes not allowed
```

```grain
Path.absoluteFile("./file.txt") == None // this is a relative path
```

### Path.**absoluteDir**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
absoluteDir : String -> Option<Path<Absolute, Directory>>
```

Constructs a new absolute directory `Path` from a string representing an absolute
path, using the special path grammar defined by this module.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pathStr`|`String`|A string representing an absolute path|

Returns:

|type|description|
|----|-----------|
|`Option<Path<Absolute, Directory>>`|A `Some` variant containing the absolute directory path if it is successfully parsed or `None` otherwise|

Examples:

```grain
Path.absoluteDir("/usr/bin")
```

```grain
Path.absoluteDir("/Users/me/") // trailing slashes ok
```

```grain
Path.absoluteDir("./dir") == None // this is a relative path
```

### Path.**relativeFile**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
relativeFile : String -> Option<Path<Relative, File>>
```

Constructs a new relative file `Path` from a string representing an relative
path, using the special path grammar defined by this module.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pathStr`|`String`|A string representing an relative path|

Returns:

|type|description|
|----|-----------|
|`Option<Path<Relative, File>>`|A `Some` variant containing the relative file path if it is successfully parsed or `None` otherwise|

Examples:

```grain
Path.relativeFile("./dir/file.txt")
```

```grain
Path.relativeFile("/usr/bin/script.sh") == None // this is an absolute path
```

```grain
Path.relativeFile("./abc/") == None // trailing slash not allowed
```

### Path.**relativeDir**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
relativeDir : String -> Option<Path<Relative, Directory>>
```

Constructs a new relative directory `Path` from a string representing an relative
path, using the special path grammar defined by this module.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pathStr`|`String`|A string representing an relative path|

Returns:

|type|description|
|----|-----------|
|`Option<Path<Relative, Directory>>`|A `Some` variant containing the relative directory path if it is successfully parsed or `None` otherwise|

Examples:

```grain
Path.relativeDir("dir")
```

```grain
Path.relativeDir("./dir/") // trailing slashes ok
```

```grain
Path.relativeDir("/usr/bin") == None // this is an absolute path
```

### Path.**wrapWithType**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
wrapWithType : Path<a, b> -> PathWithType
```

Wraps a path with its corresponding concrete path type, in order to allow
dynamically checking the path type.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a, b>`|The path to wrap|

Returns:

|type|description|
|----|-----------|
|`PathWithType`|A `PathWithType` wrapping the path with its type|

### Path.**append**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
append : (Path<a, Directory>, Path<Relative, b>) -> Path<a, b>
```

Creates a new path from a path with a relative path segment appended to it.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a, Directory>`|The base path to append the segment to|
|`toAppend`|`Path<Relative, b>`|The relative path to append|

Returns:

|type|description|
|----|-----------|
|`Path<a, b>`|A new path with the `toAppend` path appended to the `path`|

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
  (Path<a, b>, Path<a, c>) -> Result<Path<Relative, c>, RelativizationError>
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
|`source`|`Path<a, b>`|The source path to find a path from|
|`dest`|`Path<a, c>`|The destination path to find the path to|

Returns:

|type|description|
|----|-----------|
|`Result<Path<Relative, c>, RelativizationError>`|An `Ok` variant containing the relative path if successfully parsed or `Err` otherwise|

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

### Path.**isDescendantOf**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
isDescendantOf : (Path<a, Directory>, Path<a, c>) -> Bool
```

Determines if a path is a descendant of another path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`base`|`Path<a, Directory>`|The path to consider as the possible ancestor path|
|`path`|`Path<a, c>`|The path to consider as the possible descendant path|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the second path is a descendant of the first path and `false` otherwise|

Examples:

```grain
// isDescendantOf(/usr, /usr/bin/bash) == true
```

```grain
// isDescendantOf(/usr, /etc) == false
```

```grain
// isDescendantOf(../dir1, ./dir2) == false
```

### Path.**parent**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
parent : Path<a, b> -> Path<a, Directory>
```

Returns the path corresponding to the parent directory of the given path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a, b>`|The path to get the parent of|

Returns:

|type|description|
|----|-----------|
|`Path<a, Directory>`|A path corresponding to the parent directory of the given path|

### Path.**name**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
name : Path<a, b> -> Option<String>
```

Returns the base name (named final segment) of a path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a, b>`|The path to get the name of|

Returns:

|type|description|
|----|-----------|
|`Option<String>`|A `Some` variant containing the name of the path, or `None` if the path does not have one|

Examples:

```grain
Path.name(Option.unwrap(Path.relativeFile("./dir/file.txt"))) // Some("file.txt")
```

```grain
Path.name(Option.unwrap(Path.relativeDir(".."))) // None
```

### Path.**stem**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
stem : Path<a, File> -> Option<String>
```

Returns the name of the file path without the extension.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a, File>`|The path to get the stem of|

Returns:

|type|description|
|----|-----------|
|`Option<String>`|A `Some` variant containing the stem of the file path, or `None` if the path does not have one|

### Path.**extension**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
extension : Path<a, File> -> Option<String>
```

Returns the extension on the name of the file path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a, File>`|The path to get the extension of|

Returns:

|type|description|
|----|-----------|
|`Option<String>`|A `Some` variant containing the extension of the file path, or `None` if the path does not have one|

### Path.**root**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
root : Path<Absolute, b> -> AbsoluteRoot
```

Returns the root of the absolute path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<Absolute, b>`|The absolute path to get the root of|

Returns:

|type|description|
|----|-----------|
|`AbsoluteRoot`|The root of the path|

### Path.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
toString : (Path<a, b>, PathEncoding) -> String
```

Converts the given `Path` into a string, using the specified path encoding.
A trailing slash is added to directory paths.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path<a, b>`|The `Path` to convert to a string|
|`encoding`|`PathEncoding`|The `PathEncoding` to use to represent the path as a string|

Returns:

|type|description|
|----|-----------|
|`String`|A string representing the given path|

Examples:

```grain
Path.toString(myPath, Path.Windows)
```

