---
title: Path
---

Utilities for working with system paths.

This module treats paths purely as a data representation and does not
provide functionality for interacting with the file system.

This module explicitly encodes whether a path is absolute or relative, and
whether it refers to a file or a directory, as part of the `Path` type.

Paths in this module abide by a special POSIX-like representation/grammar
rather than one defined by a specific operating system. The rules are as
follows:

- Path separators are denoted by `/` for POSIX-like paths
- Absolute paths may be rooted either at the POSIX-like root `/` or at Windows-like drive roots like `C:/`
- Paths referencing files must not include trailing forward slashes, but paths referencing directories may
- The path segment `.` indicates the relative "current" directory of a path, and `..` indicates the parent directory of a path

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
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

### Path.**Relative**

```grain
type Relative
```

Represents a relative path.

### Path.**Absolute**

```grain
type Absolute
```

Represents an absolute path.

### Path.**File**

```grain
type File
```

Represents a path referencing a file.

### Path.**Directory**

```grain
type Directory
```

Represents a path referencing a directory.

### Path.**TypedPath**

```grain
type TypedPath<a, b>
```

Represents a path typed on (`Absolute` or `Relative`) and (`File` or
`Directory`)

### Path.**Path**

```grain
enum Path {
  AbsoluteFile(TypedPath<Absolute, File>),
  AbsoluteDir(TypedPath<Absolute, Directory>),
  RelativeFile(TypedPath<Relative, File>),
  RelativeDir(TypedPath<Relative, Directory>),
}
```

Represents a system path.

### Path.**Platform**

```grain
enum Platform {
  Windows,
  Posix,
}
```

Represents a platform-specific path encoding scheme.

### Path.**PathOperationError**

```grain
enum PathOperationError {
  IncompatiblePathType,
}
```

Represents an error that can occur when finding a property of a path.

### Path.**AppendError**

```grain
enum AppendError {
  AppendToFile,
  AppendAbsolute,
}
```

Represents an error that can occur when appending paths.

### Path.**AncestryStatus**

```grain
enum AncestryStatus {
  Descendant,
  Ancestor,
  Self,
  NoLineage,
}
```

Represents the status of an ancestry check between two paths.

### Path.**IncompatibilityError**

```grain
enum IncompatibilityError {
  DifferentRoots,
  DifferentBases,
}
```

Represents an error that can occur when the types of paths are incompatible
for an operation.

### Path.**RelativizationError**

```grain
enum RelativizationError {
  Incompatible(IncompatibilityError),
  ImpossibleRelativization,
}
```

Represents possible errors for the `relativeTo` operation.

## Values

Functions for working with Paths.

### Path.**fromString**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
fromString : String -> Path
```

Parses a path string into a `Path`. Paths will be parsed as file paths
rather than directory paths if there is ambiguity.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pathStr`|`String`|The string to parse as a path|

Returns:

|type|description|
|----|-----------|
|`Path`|The path wrapped with details encoded within the type|

Examples:

```grain
fromString("/bin/") // an absolute Path referencing the directory /bin/
```

```grain
fromString("file.txt") // a relative Path referencing the file ./file.txt
```

```grain
fromString(".") // a relative Path referencing the current directory
```

### Path.**fromPlatformString**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
fromPlatformString : (String, Platform) -> Path
```

Parses a path string into a `Path` using the path separators appropriate to
the given platform (`/` for `Posix` and either `/` or `\` for `Windows`).
Paths will be parsed as file paths rather than directory paths if there is
ambiguity.

Parameters:

|param|type|description|
|-----|----|-----------|
|`pathStr`|`String`|The string to parse as a path|
|`platform`|`Platform`|The platform whose path separators should be used for parsing|

Returns:

|type|description|
|----|-----------|
|`Path`|The path wrapped with details encoded within the type|

Examples:

```grain
fromPlatformString("/bin/", Posix) // an absolute Path referencing the directory /bin/
```

```grain
fromPlatformString("C:\\file.txt", Windows) // a relative Path referencing the file C:\file.txt
```

### Path.**toString**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
toString : Path -> String
```

Converts the given `Path` into a string, using the `/` path separator.
A trailing slash is added to directory paths.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The path to convert to a string|

Returns:

|type|description|
|----|-----------|
|`String`|A string representing the given path|

Examples:

```grain
toString(fromString("/file.txt")) == "/file.txt"
```

```grain
toString(fromString("dir/")) == "./dir/"
```

### Path.**toPlatformString**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
toPlatformString : (Path, Platform) -> String
```

Converts the given `Path` into a string, using the canonical path separator
appropriate to the given platform (`/` for `Posix` and `\` for `Windows`).
A trailing slash is added to directory paths.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The path to convert to a string|
|`platform`|`Platform`|The `Platform` to use to represent the path as a string|

Returns:

|type|description|
|----|-----------|
|`String`|A string representing the given path|

Examples:

```grain
toPlatformString(fromString("dir/"), Posix) == "./dir/"
```

```grain
toPlatformString(fromString("C:/file.txt"), Windows) == "C:\\file.txt"
```

### Path.**isDirectory**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
isDirectory : Path -> Bool
```

Determines whether the path is a directory path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The path to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the path is a directory path or `false` otherwise|

Examples:

```grain
isDirectory(fromString("file.txt")) == false
```

```grain
isDirectory(fromString("/bin/")) == true
```

### Path.**isAbsolute**

```grain
isAbsolute : Path -> Bool
```

Determines whether the path is an absolute path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The path to inspect|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if the path is absolute or `false` otherwise|

Examples:

```grain
isAbsolute(fromString("/Users/me")) == true
```

```grain
isAbsolute(fromString("./file.txt")) == false
```

### Path.**append**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
append : (Path, Path) -> Result<Path, AppendError>
```

Creates a new path by appending a relative path segment to a directory path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The base path|
|`toAppend`|`Path`|The relative path to append|

Returns:

|type|description|
|----|-----------|
|`Result<Path, AppendError>`|`Ok(path)` combining the base and appended paths or `Err(err)` if the paths are incompatible|

Examples:

```grain
append(fromString("./dir/"), fromString("file.txt")) == Ok(fromString("./dir/file.txt"))
```

```grain
append(fromString("a.txt"), fromString("b.sh")) == Err(AppendToFile) // cannot append to file path
```

```grain
append(fromString("./dir/"), fromString("/dir2")) == Err(AppendAbsolute) // cannot append an absolute path
```

### Path.**relativeTo**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
relativeTo : (Path, Path) -> Result<Path, RelativizationError>
```

Attempts to construct a new relative path which will lead to the destination
path from the source path.

If the source and destination are incompatible in their bases, the result
will be `Err(IncompatibilityError)`.

If the route to the destination cannot be concretely determined from the
source, the result will be `Err(ImpossibleRelativization)`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`source`|`Path`|The source path|
|`dest`|`Path`|The destination path to resolve|

Returns:

|type|description|
|----|-----------|
|`Result<Path, RelativizationError>`|`Ok(path)` containing the relative path if successfully resolved or `Err(err)` otherwise|

Examples:

```grain
relativeTo(fromString("/usr"), fromString("/usr/bin")) == Ok(fromString("./bin"))
```

```grain
relativeTo(fromString("/home/me"), fromString("/home/me")) == Ok(fromString("."))
```

```grain
relativeTo(fromString("/file.txt"), fromString("/etc/")) == Ok(fromString("../etc/"))
```

```grain
relativeTo(fromString(".."), fromString("../../thing")) Ok(fromString("../thing"))
```

```grain
relativeTo(fromString("/usr/bin"), fromString("C:/Users")) == Err(Incompatible(DifferentRoots))
```

```grain
relativeTo(fromString("../here"), fromString("./there")) == Err(ImpossibleRelativization)
```

### Path.**ancestry**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
ancestry : (Path, Path) -> Result<AncestryStatus, IncompatibilityError>
```

Determines the relative ancestry betwen two paths.

Parameters:

|param|type|description|
|-----|----|-----------|
|`base`|`Path`|The first path to consider|
|`path`|`Path`|The second path to consider|

Returns:

|type|description|
|----|-----------|
|`Result<AncestryStatus, IncompatibilityError>`|`Ok(ancestryStatus)` with the relative ancestry between the paths if they are compatible or `Err(err)` if they are incompatible|

Examples:

```grain
ancestry(fromString("/usr"), fromString("/usr/bin/bash")) == Ok(Ancestor)
```

```grain
ancestry(fromString("/Users/me"), fromString("/Users")) == Ok(Descendant)
```

```grain
ancestry(fromString("/usr"), fromString("/etc")) == Ok(Neither)
```

```grain
ancestry(fromString("C:/dir1"), fromString("/dir2")) == Err(DifferentRoots)
```

### Path.**parent**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
parent : Path -> Path
```

Retrieves the path corresponding to the parent directory of the given path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The path to inspect|

Returns:

|type|description|
|----|-----------|
|`Path`|A path corresponding to the parent directory of the given path|

Examples:

```grain
parent(fromString("./dir/inner")) == fromString("./dir/")
```

```grain
parent(fromString("/")) == fromString("/")
```

### Path.**basename**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
basename : Path -> Option<String>
```

Retrieves the basename (named final segment) of a path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The path to inspect|

Returns:

|type|description|
|----|-----------|
|`Option<String>`|`Some(path)` containing the basename of the path or `None` if the path does not have one|

Examples:

```grain
basename(fromString("./dir/file.txt")) == Some("file.txt")
```

```grain
basename(fromString(".."))) == None
```

### Path.**stem**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
stem : Path -> Result<String, PathOperationError>
```

Retrieves the basename of a file path without the extension.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The path to inspect|

Returns:

|type|description|
|----|-----------|
|`Result<String, PathOperationError>`|`Ok(path)` containing the stem of the file path or `Err(err)` if the path is a directory path|

Examples:

```grain
stem(fromString("file.txt")) == Ok("file")
```

```grain
stem(fromString(".gitignore")) == Ok(".gitignore")
```

```grain
stem(fromString(".a.tar.gz")) == Ok(".a")
```

```grain
stem(fromString("/dir/")) == Err(IncompatiblePathType) // can only take stem of a file path
```

### Path.**extension**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
extension : Path -> Result<String, PathOperationError>
```

Retrieves the extension on the basename of a file path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The path to inspect|

Returns:

|type|description|
|----|-----------|
|`Result<String, PathOperationError>`|`Ok(path)` containing the extension of the file path or `Err(err)` if the path is a directory path|

Examples:

```grain
extension(fromString("file.txt")) == Ok(".txt")
```

```grain
extension(fromString(".gitignore")) == Ok("")
```

```grain
extension(fromString(".a.tar.gz")) == Ok(".tar.gz")
```

```grain
extension(fromString("/dir/")) == Err(IncompatiblePathType) // can only take extension of a file path
```

### Path.**root**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
root : Path -> Result<AbsoluteRoot, PathOperationError>
```

Retrieves the root of the absolute path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`Path`|The path to inspect|

Returns:

|type|description|
|----|-----------|
|`Result<AbsoluteRoot, PathOperationError>`|`Ok(root)` containing the root of the path or `Err(err)` if the path is a relative path|

Examples:

```grain
root(fromString("C:/Users/me/")) == Ok(Drive('C'))
```

```grain
root(fromString("/home/me/")) == Ok(Root)
```

```grain
root(fromString("./file.txt")) == Err(IncompatiblePathType)
```

