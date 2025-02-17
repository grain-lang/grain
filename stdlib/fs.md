---
title: Fs
---

Utilities for high-level file system interactions. Utilizes WASI Preview 1 for underlying API

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
from "fs" include Fs
```

```grain
Fs.Utf8.readFile(Path.fromString("baz.txt"))
```

```grain
Fs.Utf8.writeFile(Path.fromString("baz.txt"), "Hello World\n")
```

```grain
Fs.copy(Path.fromString("foo.txt"), Path.fromString("foocopy.txt"))
```

## Types

Type declarations included in the Fs module.

### Fs.**FileError**

```grain
enum FileError {
  NoPreopenedDirectories,
  PermissionDenied,
  AddressInUse,
  AddressNotAvailable,
  AddressFamilyNotSupported,
  ResourceUnavailableOrOperationWouldBlock,
  ConnectionAlreadyInProgress,
  BadFileDescriptor,
  BadMessage,
  DeviceOrResourceBusy,
  OperationCanceled,
  NoChildProcesses,
  ConnectionAborted,
  ConnectionRefused,
  ConnectionReset,
  ResourceDeadlockWouldOccur,
  DestinationAddressRequired,
  MathematicsArgumentOutOfDomainOfFunction,
  FileExists,
  BadAddress,
  FileTooLarge,
  HostIsUnreachable,
  IdentifierRemoved,
  IllegalByteSequence,
  OperationInProgress,
  InterruptedFunction,
  InvalidArgument,
  IOError,
  SocketIsConnected,
  IsADirectory,
  TooManyLevelsOfSymbolicLinks,
  FileDescriptorValueTooLarge,
  TooManyLinks,
  MessageTooLarge,
  FilenameTooLong,
  NetworkIsDown,
  ConnectionAbortedByNetwork,
  NetworkUnreachable,
  TooManyFilesOpenInSystem,
  NoBufferSpaceAvailable,
  NoSuchDevice,
  NoSuchFileOrDirectory,
  ExecutableFileFormatError,
  NoLocksAvailable,
  NotEnoughSpace,
  NoMessageOfTheDesiredType,
  ProtocolNotAvailable,
  NoSpaceLeftOnDevice,
  FunctionNotSupported,
  TheSocketIsNotConnected,
  NotADirectoryOrASymbolicLinkToADirectory,
  DirectoryNotEmpty,
  StateNotRecoverable,
  NotASocket,
  NotSupportedOrOperationNotSupportedOnSocket,
  InappropriateIOControlOperation,
  NoSuchDeviceOrAddress,
  ValueTooLargeToBeStoredInDataType,
  PreviousOwnerDied,
  OperationNotPermitted,
  BrokenPipe,
  ProtocolError,
  ProtocolNotSupported,
  ProtocolWrongTypeForSocket,
  ResultTooLarge,
  ReadOnlyFileSystem,
  InvalidSeek,
  NoSuchProcess,
  ConnectionTimedOut,
  TextFileBusy,
  CrossDeviceLink,
  ExtensionCapabilitiesInsufficient,
  UnknownFileError(Number),
}
```

Potential errors that can be raised from system interactions.

### Fs.**SpecialFileType**

```grain
enum SpecialFileType {
  Unknown,
  BlockDevice,
  CharacterDevice,
  SocketDatagram,
  SocketStream,
}
```

Represents non-standard system file types.

### Fs.**FileType**

```grain
enum FileType {
  File,
  Directory,
  SymbolicLink,
  Special(SpecialFileType),
}
```

Represents different system file types.

### Fs.**Stats**

```grain
record Stats {
  fileType: FileType,
  size: Number,
  accessedTimestamp: Number,
  modifiedTimestamp: Number,
  changedTimestamp: Number,
}
```

Represents metadata about a file.

Fields:

|name|type|description|
|----|----|-----------|
|`fileType`|`FileType`||
|`size`|`Number`|File size in bytes|
|`accessedTimestamp`|`Number`|Last accessed timestamp in nanoseconds|
|`modifiedTimestamp`|`Number`|Last modified timestamp in nanoseconds|
|`changedTimestamp`|`Number`|Last file status change timestamp in nanoseconds|

### Fs.**DirectoryEntry**

```grain
record DirectoryEntry {
  name: String,
  fileType: FileType,
}
```

Represents information about an item in a directory.

### Fs.**RemoveMode**

```grain
enum RemoveMode {
  RemoveFile,
  RemoveEmptyDirectory,
  RemoveRecursive,
}
```

The type of removal operation to perform when calling `remove`.

### Fs.**CopyMode**

```grain
enum CopyMode {
  CopyFile,
  CopyRecursive,
}
```

The type of copy operation to perform when calling `copy`.

### Fs.**WriteMode**

```grain
enum WriteMode {
  Truncate,
  Append,
}
```

The type of write operation to perform when calling `writeFile`.

## Values

Functions and constants included in the Fs module.

### Fs.**remove**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
remove :
  (?removeMode: RemoveMode, ?baseDirPath: Option<Path.Path>, path: Path.Path) =>
   Result<Void, FileError>
```

Removes a file or directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?removeMode`|`RemoveMode`|The type of removal to perform; `RemoveFile` by default|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory in which path resolution starts|
|`path`|`Path.Path`|The path of the file or directory to remove|

Returns:

|type|description|
|----|-----------|
|`Result<Void, FileError>`|`Ok(void)` if the operation succeeds, `Err(err)` if a file system error is encountered|

Examples:

```grain
Fs.remove(Path.fromString("file.txt")) // removes a file
```

```grain
Fs.remove(removeMode=Fs.RemoveEmptyDirectory, Path.fromString("dir")) // removes an empty directory
```

```grain
Fs.remove(removeMode=Fs.RemoveRecursive, Path.fromString("dir")) // removes the directory and its contents
```

### Fs.**readDir**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
readDir :
  (?baseDirPath: Option<Path.Path>, path: Path.Path) =>
   Result<List<DirectoryEntry>, FileError>
```

Reads the contents of a directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory in which resolution should begin|
|`path`|`Path.Path`|The path to the directory to read|

Returns:

|type|description|
|----|-----------|
|`Result<List<DirectoryEntry>, FileError>`|`Ok(contents)` containing the directory contents or `Err(err)` if a file system error is encountered|

### Fs.**createDir**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
createDir :
  (?baseDirPath: Option<Path.Path>, path: Path.Path) =>
   Result<Void, FileError>
```

Creates a new empty directory at the given path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory in which resolution should begin|
|`path`|`Path.Path`|The path to create the new directory, relative to the base directory|

Returns:

|type|description|
|----|-----------|
|`Result<Void, FileError>`|`Ok(void)` if the operation succeeds, `Err(err)` if a file system error is encountered|

### Fs.**createSymlink**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
createSymlink :
  (linkContents: Path.Path, ?targetBaseDirPath: Option<Path.Path>,
   targetPath: Path.Path) => Result<Void, FileError>
```

Creates a new symbolic link with the given contents.

Parameters:

|param|type|description|
|-----|----|-----------|
|`linkContents`|`Path.Path`|The path to store into the link|
|`?targetBaseDirPath`|`Option<Path.Path>`|The path to the directory in which the target path resolution starts|
|`targetPath`|`Path.Path`|The path to the target of the link|

Returns:

|type|description|
|----|-----------|
|`Result<Void, FileError>`|`Ok(void)` if the operation succeeds, `Err(err)` if a file system error or relativization error is encountered|

### Fs.**stats**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
stats :
  (?followSymlink: Bool, ?baseDirPath: Option<Path.Path>, path: Path.Path) =>
   Result<Stats, FileError>
```

Queries information about a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?followSymlink`|`Bool`|Whether to follow symlinks or not; if `true` then the stats of a valid symlink's underlying file will be returned. `true` by default|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory in which the path resolution starts|
|`path`|`Path.Path`|The path of the file to query|

Returns:

|type|description|
|----|-----------|
|`Result<Stats, FileError>`|`Ok(stats)` containing metadata or `Err(err)` if a file system error is encountered|

### Fs.**exists**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
exists : (?baseDirPath: Option<Path.Path>, path: Path.Path) => Bool
```

Polls whether or not a file or directory exists at the given path.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory in which the path resolution starts|
|`path`|`Path.Path`|The path of the file to query|

Returns:

|type|description|
|----|-----------|
|`Bool`|`true` if a file or directory exists at the path or `false` otherwise|

### Fs.**readLink**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
readLink :
  (?baseDirPath: Option<Path.Path>, path: Path.Path) =>
   Result<Path.Path, FileError>
```

Reads the contents of a symbolic link.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory to begin path resolution|
|`path`|`Path.Path`|The path to the link to read|

Returns:

|type|description|
|----|-----------|
|`Result<Path.Path, FileError>`|`Ok(path)` containing the link contents or `Err(err)` if a file system error is encountered|

### Fs.**copy**

```grain
copy :
  (?copyMode: CopyMode, ?followSymlink: Bool,
   ?sourceBaseDirPath: Option<Path.Path>, sourcePath: Path.Path,
   ?targetBaseDirPath: Option<Path.Path>, targetPath: Path.Path) =>
   Result<Void, FileError>
```

Copies a file or directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?copyMode`|`CopyMode`|The type of copy to perform; `CopyFile` by default|
|`?followSymlink`|`Bool`|Whether to follow symlinks or not; if `true` then the stats of a valid symlink's underlying file will be returned. `true` by default|
|`?sourceBaseDirPath`|`Option<Path.Path>`|The path to the directory in which the source path resolution starts|
|`sourcePath`|`Path.Path`|The path of the file or directory to copy|
|`?targetBaseDirPath`|`Option<Path.Path>`|The path to the directory in which the target path resolution starts|
|`targetPath`|`Path.Path`|The path to copy the file or directory to|

Returns:

|type|description|
|----|-----------|
|`Result<Void, FileError>`|`Ok(void)` if the operation succeeds, `Err(err)` if a file system error is encountered|

### Fs.**rename**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
rename :
  (?sourceBaseDirPath: Option<Path.Path>, sourcePath: Path.Path,
   ?targetBaseDirPath: Option<Path.Path>, targetPath: Path.Path) =>
   Result<Void, FileError>
```

Renames a file or directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?sourceBaseDirPath`|`Option<Path.Path>`|The path to the directory in which the source path resolution starts|
|`sourcePath`|`Path.Path`|The path of the file to rename|
|`?targetBaseDirPath`|`Option<Path.Path>`|The path to the directory in which the target path resolution starts|
|`targetPath`|`Path.Path`|The new path of the file|

Returns:

|type|description|
|----|-----------|
|`Result<Void, FileError>`|`Ok(void)` if the operation succeeds, `Err(err)` if a file system error is encountered|

## Fs.Binary

Functionality for reading and writing `Bytes` to files.

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

### Values

Functions and constants included in the Fs.Binary module.

#### Fs.Binary.**readFile**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
readFile :
  (?sync: Bool, ?baseDirPath: Option<Path.Path>, path: Path.Path) =>
   Result<Bytes, FileError>
```

Read the contents of a file as `Bytes`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?sync`|`Bool`|Whether to synchronously read; `true` by default|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory to begin path resolution|
|`path`|`Path.Path`|The file path to read from|

Returns:

|type|description|
|----|-----------|
|`Result<Bytes, FileError>`|`Ok(contents)` containing the bytes read if successful or `Err(err)` if a file system error is encountered|

#### Fs.Binary.**writeFile**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
writeFile :
  (?writeMode: WriteMode, ?sync: Bool, ?baseDirPath: Option<Path.Path>,
   path: Path.Path, data: Bytes) => Result<Void, FileError>
```

Write `Bytes` to a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?writeMode`|`WriteMode`|The type of write operation to perform; `Truncate` by default|
|`?sync`|`Bool`|Whether to synchronously write; `true` by default|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory to begin path resolution|
|`path`|`Path.Path`|The file path to write to|
|`data`|`Bytes`|The bytes to write to the file|

Returns:

|type|description|
|----|-----------|
|`Result<Void, FileError>`|`Ok(void)` if the operation is successful or `Err(err)` if a file system error is encountered|

## Fs.Utf8

Functionality for reading and writing `String`s to files.

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

### Values

Functions and constants included in the Fs.Utf8 module.

#### Fs.Utf8.**readFile**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
readFile :
  (?sync: Bool, ?baseDirPath: Option<Path.Path>, path: Path.Path) =>
   Result<String, FileError>
```

Read the contents of a file as a `String`.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?sync`|`Bool`|Whether to synchronously read; `true` by default|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory to begin path resolution|
|`path`|`Path.Path`|The file path to read from|

Returns:

|type|description|
|----|-----------|
|`Result<String, FileError>`|`Ok(contents)` containing the string read if successful or `Err(err)` if a file system error is encountered|

#### Fs.Utf8.**writeFile**

<details disabled>
<summary tabindex="-1">Added in <code>0.7.0</code></summary>
No other changes yet.
</details>

```grain
writeFile :
  (?writeMode: WriteMode, ?sync: Bool, ?baseDirPath: Option<Path.Path>,
   path: Path.Path, data: String) => Result<Void, FileError>
```

Write a `String` to a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`?writeMode`|`WriteMode`|The type of write operation to perform; `Truncate` by default|
|`?sync`|`Bool`|Whether to synchronously write; `true` by default|
|`?baseDirPath`|`Option<Path.Path>`|The path to the directory to begin path resolution|
|`path`|`Path.Path`|The file path to write to|
|`data`|`String`|The string to write to the file|

Returns:

|type|description|
|----|-----------|
|`Result<Void, FileError>`|`Ok(void)` if the operation is successful or `Err(err)` if a file system error is encountered|

