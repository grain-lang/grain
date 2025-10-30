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
from "path" include Path
```

```grain
let p = Path.fromString("./tmp/file.txt")
```

## Types

Type declarations included in the Path module.

### Path.**AbsoluteRoot**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
enum AbsoluteRoot {
  Root,
  Drive(Char),
}
```

Represents an absolute path's anchor point.

### Path.**Relative**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
type Relative
```

Represents a relative path.

### Path.**Absolute**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
type Absolute
```

Represents an absolute path.

### Path.**File**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
type File
```

Represents a path referencing a file.

### Path.**Directory**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
type Directory
```

Represents a path referencing a directory.

### Path.**TypedPath**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
type TypedPath<a, b>
```

Represents a path typed on (`Absolute` or `Relative`) and (`File` or
`Directory`)

### Path.**Path**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

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

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
enum Platform {
  Windows,
  Posix,
}
```

Represents a platform-specific path encoding scheme.

### Path.**PathOperationError**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
enum PathOperationError {
  IncompatiblePathType,
}
```

Represents an error that can occur when finding a property of a path.

### Path.**AppendError**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
enum AppendError {
  AppendToFile,
  AppendAbsolute,
}
```

Represents an error that can occur when appending paths.

### Path.**AncestryStatus**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

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

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
enum IncompatibilityError {
  DifferentRoots,
  DifferentBases,
}
```

Represents an error that can occur when the types of paths are incompatible
for an operation.

### Path.**RelativizationError**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
enum RelativizationError {
  Incompatible(IncompatibilityError),
  ImpossibleRelativization,
}
```

Represents possible errors for the `relativeTo` operation.

## Values

Functions and constants included in the Path module.

### Path.**fromString**

<details>
<summary>Added in <code>0.5.5</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Merged with `fromPlatformString`; modified signature to accept platform</td></tr>
</tbody>
</table>
</details>

```grain
fromString: (pathStr: String, ?platform: Platform) => Path
```

Parses a path string into a `Path` using the path separators appropriate to
the given platform (`/` for `Posix` and either `/` or `\` for `Windows`).
Paths will be parsed as file paths rather than directory paths if there is
ambiguity.

Parameters:

| param       | type       | description                                                   |
| ----------- | ---------- | ------------------------------------------------------------- |
| `pathStr`   | `String`   | The string to parse as a path                                 |
| `?platform` | `Platform` | The platform whose path separators should be used for parsing |

Returns:

| type   | description                                           |
| ------ | ----------------------------------------------------- |
| `Path` | The path wrapped with details encoded within the type |

Examples:

```grain
Path.fromString("file.txt") // a relative Path referencing the file ./file.txt
```

```grain
Path.fromString(".") // a relative Path referencing the current directory
```

```grain
Path.fromString("/bin/", Path.Posix) // an absolute Path referencing the directory /bin/
```

```grain
Path.fromString("C:\\file.txt", Path.Windows) // a relative Path referencing the file C:\file.txt
```

### Path.**toString**

<details>
<summary>Added in <code>0.5.5</code></summary>
<table>
<thead>
<tr><th>version</th><th>changes</th></tr>
</thead>
<tbody>
<tr><td><code>0.6.0</code></td><td>Merged with `toPlatformString`; modified signature to accept platform</td></tr>
</tbody>
</table>
</details>

```grain
toString: (path: Path, ?platform: Platform) => String
```

Converts the given `Path` into a string, using the canonical path separator
appropriate to the given platform (`/` for `Posix` and `\` for `Windows`).
A trailing slash is added to directory paths.

Parameters:

| param       | type       | description                                             |
| ----------- | ---------- | ------------------------------------------------------- |
| `path`      | `Path`     | The path to convert to a string                         |
| `?platform` | `Platform` | The `Platform` to use to represent the path as a string |

Returns:

| type     | description                          |
| -------- | ------------------------------------ |
| `String` | A string representing the given path |

Examples:

```grain
Path.toString(Path.fromString("/file.txt")) == "/file.txt"
```

```grain
Path.toString(Path.fromString("dir/"), Path.Posix) == "./dir/"
```

```grain
Path.toString(Path.fromString("C:/file.txt"), Path.Windows) == "C:\\file.txt"
```

### Path.**isDirectory**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
isDirectory: (path: Path) => Bool
```

Determines whether the path is a directory path.

Parameters:

| param  | type   | description         |
| ------ | ------ | ------------------- |
| `path` | `Path` | The path to inspect |

Returns:

| type   | description                                                 |
| ------ | ----------------------------------------------------------- |
| `Bool` | `true` if the path is a directory path or `false` otherwise |

Examples:

```grain
Path.isDirectory(Path.fromString("file.txt")) == false
```

```grain
Path.isDirectory(Path.fromString("/bin/")) == true
```

### Path.**isAbsolute**

```grain
isAbsolute: (path: Path) => Bool
```

Determines whether the path is an absolute path.

Parameters:

| param  | type   | description         |
| ------ | ------ | ------------------- |
| `path` | `Path` | The path to inspect |

Returns:

| type   | description                                         |
| ------ | --------------------------------------------------- |
| `Bool` | `true` if the path is absolute or `false` otherwise |

Examples:

```grain
Path.isAbsolute(Path.fromString("/Users/me")) == true
```

```grain
Path.isAbsolute(Path.fromString("./file.txt")) == false
```

### Path.**append**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
append: (path: Path, toAppend: Path) => Result<Path, AppendError>
```

Creates a new path by appending a relative path segment to a directory path.

Parameters:

| param      | type   | description                 |
| ---------- | ------ | --------------------------- |
| `path`     | `Path` | The base path               |
| `toAppend` | `Path` | The relative path to append |

Returns:

| type                        | description                                                                                  |
| --------------------------- | -------------------------------------------------------------------------------------------- |
| `Result<Path, AppendError>` | `Ok(path)` combining the base and appended paths or `Err(err)` if the paths are incompatible |

Examples:

```grain
Path.append(Path.fromString("./dir/"), Path.fromString("file.txt")) == Ok(Path.fromString("./dir/file.txt"))
```

```grain
Path.append(Path.fromString("a.txt"), Path.fromString("b.sh")) == Err(Path.AppendToFile) // cannot append to file path
```

```grain
Path.append(Path.fromString("./dir/"), Path.fromString("/dir2")) == Err(Path.AppendAbsolute) // cannot append an absolute path
```

### Path.**relativeTo**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
relativeTo: (source: Path, dest: Path) => Result<Path, RelativizationError>
```

Attempts to construct a new relative path which will lead to the destination
path from the source path.

If the source and destination are incompatible in their bases, the result
will be `Err(Path.IncompatibilityError)`.

If the route to the destination cannot be concretely determined from the
source, the result will be `Err(Path.ImpossibleRelativization)`.

Parameters:

| param    | type   | description                     |
| -------- | ------ | ------------------------------- |
| `source` | `Path` | The source path                 |
| `dest`   | `Path` | The destination path to resolve |

Returns:

| type                                | description                                                                              |
| ----------------------------------- | ---------------------------------------------------------------------------------------- |
| `Result<Path, RelativizationError>` | `Ok(path)` containing the relative path if successfully resolved or `Err(err)` otherwise |

Examples:

```grain
Path.relativeTo(Path.fromString("/usr"), Path.fromString("/usr/bin")) == Ok(Path.fromString("./bin"))
```

```grain
Path.relativeTo(Path.fromString("/home/me"), Path.fromString("/home/me")) == Ok(Path.fromString("."))
```

```grain
Path.relativeTo(Path.fromString("/file.txt"), Path.fromString("/etc/")) == Ok(Path.fromString("../etc/"))
```

```grain
Path.relativeTo(Path.fromString(".."), Path.fromString("../../thing")) Ok(Path.fromString("../thing"))
```

```grain
Path.relativeTo(Path.fromString("/usr/bin"), Path.fromString("C:/Users")) == Err(Path.Incompatible(Path.DifferentRoots))
```

```grain
Path.relativeTo(Path.fromString("../here"), Path.fromString("./there")) == Err(Path.ImpossibleRelativization)
```

### Path.**ancestry**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
ancestry:
  (base: Path, path: Path) => Result<AncestryStatus, IncompatibilityError>
```

Determines the relative ancestry between two paths.

Parameters:

| param  | type   | description                 |
| ------ | ------ | --------------------------- |
| `base` | `Path` | The first path to consider  |
| `path` | `Path` | The second path to consider |

Returns:

| type                                           | description                                                                                                                     |
| ---------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `Result<AncestryStatus, IncompatibilityError>` | `Ok(ancestryStatus)` with the relative ancestry between the paths if they are compatible or `Err(err)` if they are incompatible |

Examples:

```grain
Path.ancestry(Path.fromString("/usr"), Path.fromString("/usr/bin/bash")) == Ok(Path.Ancestor)
```

```grain
Path.ancestry(Path.fromString("/Users/me"), Path.fromString("/Users")) == Ok(Path.Descendant)
```

```grain
Path.ancestry(Path.fromString("/usr"), Path.fromString("/etc")) == Ok(Path.Neither)
```

```grain
Path.ancestry(Path.fromString("C:/dir1"), Path.fromString("/dir2")) == Err(Path.DifferentRoots)
```

### Path.**parent**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
parent: (path: Path) => Path
```

Retrieves the path corresponding to the parent directory of the given path.

Parameters:

| param  | type   | description         |
| ------ | ------ | ------------------- |
| `path` | `Path` | The path to inspect |

Returns:

| type   | description                                                    |
| ------ | -------------------------------------------------------------- |
| `Path` | A path corresponding to the parent directory of the given path |

Examples:

```grain
Path.parent(Path.fromString("./dir/inner")) == Path.fromString("./dir/")
```

```grain
Path.parent(Path.fromString("/")) == Path.fromString("/")
```

### Path.**basename**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
basename: (path: Path) => Option<String>
```

Retrieves the basename (named final segment) of a path.

Parameters:

| param  | type   | description         |
| ------ | ------ | ------------------- |
| `path` | `Path` | The path to inspect |

Returns:

| type             | description                                                                              |
| ---------------- | ---------------------------------------------------------------------------------------- |
| `Option<String>` | `Some(path)` containing the basename of the path or `None` if the path does not have one |

Examples:

```grain
Path.basename(Path.fromString("./dir/file.txt")) == Some("file.txt")
```

```grain
Path.basename(Path.fromString(".."))) == None
```

### Path.**stem**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
stem: (path: Path) => Result<String, PathOperationError>
```

Retrieves the basename of a file path without the extension.

Parameters:

| param  | type   | description         |
| ------ | ------ | ------------------- |
| `path` | `Path` | The path to inspect |

Returns:

| type                                 | description                                                                                   |
| ------------------------------------ | --------------------------------------------------------------------------------------------- |
| `Result<String, PathOperationError>` | `Ok(path)` containing the stem of the file path or `Err(err)` if the path is a directory path |

Examples:

```grain
Path.stem(Path.fromString("file.txt")) == Ok("file")
```

```grain
Path.stem(Path.fromString(".gitignore")) == Ok(".gitignore")
```

```grain
Path.stem(Path.fromString(".a.tar.gz")) == Ok(".a")
```

```grain
Path.stem(Path.fromString("/dir/")) == Err(Path.IncompatiblePathType) // can only take stem of a file path
```

### Path.**extension**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
extension: (path: Path) => Result<String, PathOperationError>
```

Retrieves the extension on the basename of a file path.

Parameters:

| param  | type   | description         |
| ------ | ------ | ------------------- |
| `path` | `Path` | The path to inspect |

Returns:

| type                                 | description                                                                                        |
| ------------------------------------ | -------------------------------------------------------------------------------------------------- |
| `Result<String, PathOperationError>` | `Ok(path)` containing the extension of the file path or `Err(err)` if the path is a directory path |

Examples:

```grain
Path.extension(Path.fromString("file.txt")) == Ok(".txt")
```

```grain
Path.extension(Path.fromString(".gitignore")) == Ok("")
```

```grain
Path.extension(Path.fromString(".a.tar.gz")) == Ok(".tar.gz")
```

```grain
Path.extension(Path.fromString("/dir/")) == Err(Path.IncompatiblePathType) // can only take extension of a file path
```

### Path.**removeExtension**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
removeExtension: (path: Path) => Path
```

Removes the extension from a path, if there is no extension, returns the path as is.

Parameters:

| param  | type   | description        |
| ------ | ------ | ------------------ |
| `path` | `Path` | The path to modify |

Returns:

| type   | description                         |
| ------ | ----------------------------------- |
| `Path` | The path with the extension removed |

Examples:

```grain
Path.removeExtension(Path.fromString("file.txt")) == Path.fromString("file")
```

```grain
Path.removeExtension(Path.fromString(".gitignore")) == Path.fromString(".gitignore")
```

```grain
Path.removeExtension(Path.fromString("./dir/file")) == Path.fromString("dir/file")
```

```grain
Path.removeExtension(Path.fromString("./dir/")) == Path.fromString("dir/")
```

### Path.**updateExtension**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
updateExtension: (path: Path, extension: String) => Path
```

Updates the file extension of the given path.

Parameters:

| param       | type     | description        |
| ----------- | -------- | ------------------ |
| `path`      | `Path`   | The path to modify |
| `extension` | `String` | The new extension  |

Returns:

| type   | description       |
| ------ | ----------------- |
| `Path` | The modified path |

Examples:

```grain
Path.updateExtension(Path.fromString("file.txt"), "ext") == Path.fromString("file.ext")
```

```grain
Path.updateExtension(Path.fromString("file.txt"), "") == Path.fromString("file.")
```

```grain
Path.updateExtension(Path.fromString(".gitignore"), "ext") == Path.fromString(".gitignore.ext")
```

```grain
Path.updateExtension(Path.fromString("./dir/file"), "ext") == Path.fromString("dir/file.ext")
```

```grain
Path.updateExtension(Path.fromString("./dir/"), "ext") == Path.fromString("dir/")
```

### Path.**root**

<details disabled>
<summary tabindex="-1">Added in <code>0.5.5</code></summary>
No other changes yet.
</details>

```grain
root: (path: Path) => Result<AbsoluteRoot, PathOperationError>
```

Retrieves the root of the absolute path.

Parameters:

| param  | type   | description         |
| ------ | ------ | ------------------- |
| `path` | `Path` | The path to inspect |

Returns:

| type                                       | description                                                                             |
| ------------------------------------------ | --------------------------------------------------------------------------------------- |
| `Result<AbsoluteRoot, PathOperationError>` | `Ok(root)` containing the root of the path or `Err(err)` if the path is a relative path |

Examples:

```grain
Path.root(Path.fromString("C:/Users/me/")) == Ok(Path.Drive('C'))
```

```grain
Path.root(Path.fromString("/home/me/")) == Ok(Path.Root)
```

```grain
Path.root(Path.fromString("./file.txt")) == Err(Path.IncompatiblePathType)
```

