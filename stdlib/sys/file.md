---
title: File
---

Utilities for accessing the filesystem & working with files.

Many of the functions in this module are not intended to be used directly, but rather for other libraries to be built on top of them.

```grain
include "sys/file"
```

## Types

Type declarations included in the File module.

### File.**FileDescriptor**

```grain
enum FileDescriptor {
  FileDescriptor(Number),
}
```

Represents a handle to an open file on the system.

### File.**LookupFlag**

```grain
enum LookupFlag {
  SymlinkFollow,
}
```

Flags that determine how paths should be resolved when looking up a file or directory.

### File.**OpenFlag**

```grain
enum OpenFlag {
  Create,
  Directory,
  Exclusive,
  Truncate,
}
```

Flags that determine how a file or directory should be opened.

### File.**Rights**

```grain
enum Rights {
  FdDatasync,
  FdRead,
  FdSeek,
  FdSetFlags,
  FdSync,
  FdTell,
  FdWrite,
  FdAdvise,
  FdAllocate,
  PathCreateDirectory,
  PathCreateFile,
  PathLinkSource,
  PathLinkTarget,
  PathOpen,
  FdReaddir,
  PathReadlink,
  PathRenameSource,
  PathRenameTarget,
  PathFilestats,
  PathSetSize,
  PathSetTimes,
  FdFilestats,
  FdSetSize,
  FdSetTimes,
  PathSymlink,
  PathRemoveDirectory,
  PathUnlinkFile,
  PollFdReadwrite,
  SockShutdown,
}
```

Flags that determine which rights a `FileDescriptor` should have
and which rights new `FileDescriptor`s should inherit from it.

### File.**FdFlag**

```grain
enum FdFlag {
  Append,
  Dsync,
  Nonblock,
  Rsync,
  Sync,
}
```

Flags that determine the mode(s) that a `FileDescriptor` operates in.

### File.**Filetype**

```grain
enum Filetype {
  Unknown,
  BlockDevice,
  CharacterDevice,
  Directory,
  RegularFile,
  SocketDatagram,
  SocketStream,
  SymbolicLink,
}
```

The type of file a `FileDescriptor` refers to.

### File.**Whence**

```grain
enum Whence {
  Set,
  Current,
  End,
}
```

Flags that determine where seeking should begin in a file.

### File.**Stats**

```grain
record Stats {
  filetype: Filetype,
  flags: List<FdFlag>,
  rights: List<Rights>,
  rightsInheriting: List<Rights>,
}
```

Information about a `FileDescriptor`.

### File.**Filestats**

```grain
record Filestats {
  device: Int64,
  inode: Int64,
  filetype: Filetype,
  linkcount: Int64,
  size: Int64,
  accessed: Int64,
  modified: Int64,
  changed: Int64,
}
```

Information about the file that a `FileDescriptor` refers to.

### File.**DirectoryEntry**

```grain
record DirectoryEntry {
  inode: Int64,
  filetype: Filetype,
  path: String,
}
```

An entry in a directory.

### File.**Prestat**

```grain
enum Prestat {
  Dir{
    prefix: String,
    fd: FileDescriptor,
  },
}
```

Information about a preopened directory

## Values

Functions and constants included in the File module.

### File.**stdin**

```grain
stdin : FileDescriptor
```

The `FileDescriptor` for `stdin`.

### File.**stdout**

```grain
stdout : FileDescriptor
```

The `FileDescriptor` for `stdout`.

### File.**stderr**

```grain
stderr : FileDescriptor
```

The `FileDescriptor` for `stderr`.

### File.**pathOpen**

```grain
pathOpen :
  (dirFd: FileDescriptor, dirFlags: List<LookupFlag>, path: String,
   openFlags: List<OpenFlag>, rights: List<Rights>,
   rightsInheriting: List<Rights>, flags: List<FdFlag>) =>
   Result<FileDescriptor, Exception>
```

Open a file or directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`dirFd`|`FileDescriptor`|The directory in which path resolution starts|
|`dirFlags`|`List<LookupFlag>`|Flags which affect path resolution|
|`path`|`String`|The path to the file or directory|
|`openFlags`|`List<OpenFlag>`|Flags that decide how the path will be opened|
|`rights`|`List<Rights>`|The rights that dictate what may be done with the returned file descriptor|
|`rightsInheriting`|`List<Rights>`|The rights that dictate what may be done with file descriptors derived from this file descriptor|
|`flags`|`List<FdFlag>`|Flags which affect read/write operations on this file descriptor|

Returns:

|type|description|
|----|-----------|
|`Result<FileDescriptor, Exception>`|`Ok(fd)` of the opened file or directory if successful or `Err(exception)` otherwise|

### File.**fdRead**

```grain
fdRead :
  (fd: FileDescriptor, size: Number) => Result<(Bytes, Number), Exception>
```

Read from a file descriptor.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to read from|
|`size`|`Number`|The maximum number of bytes to read from the file descriptor|

Returns:

|type|description|
|----|-----------|
|`Result<(Bytes, Number), Exception>`|`Ok((contents, numBytes))` of bytes read and the number of bytes read if successful or `Err(exception)` otherwise|

### File.**fdPread**

```grain
fdPread :
  (fd: FileDescriptor, offset: Int64, size: Number) =>
   Result<(Bytes, Number), Exception>
```

Read from a file descriptor without updating the file descriptor's offset.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to read from|
|`offset`|`Int64`|The position within the file to begin reading|
|`size`|`Number`|The maximum number of bytes to read from the file descriptor|

Returns:

|type|description|
|----|-----------|
|`Result<(Bytes, Number), Exception>`|`Ok((contents, numBytes))` of bytes read and the number of bytes read if successful or `Err(exception)` otherwise|

### File.**fdPrestatGet**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
fdPrestatGet : (fd: FileDescriptor) => Result<Prestat, Exception>
```

Get information about a preopened directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to check|

Returns:

|type|description|
|----|-----------|
|`Result<Prestat, Exception>`|`Ok(Dir{prefix, fd})` if successful or `Err(exception)` otherwise|

### File.**fdWrite**

```grain
fdWrite : (fd: FileDescriptor, data: Bytes) => Result<Number, Exception>
```

Write to a file descriptor.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to which data will be written|
|`data`|`Bytes`|The data to be written|

Returns:

|type|description|
|----|-----------|
|`Result<Number, Exception>`|`Ok(numBytes)` of the number of bytes written if successful or `Err(Exception)` otherwise|

### File.**fdPwrite**

```grain
fdPwrite :
  (fd: FileDescriptor, data: Bytes, offset: Int64) =>
   Result<Number, Exception>
```

Write to a file descriptor without updating the file descriptor's offset.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to which data will be written|
|`data`|`Bytes`|The data to be written|
|`offset`|`Int64`|The position within the file to begin writing|

Returns:

|type|description|
|----|-----------|
|`Result<Number, Exception>`|`Ok(numBytes)` of the number of bytes written if successful or `Err(exception)` otherwise|

### File.**fdAllocate**

```grain
fdAllocate :
  (fd: FileDescriptor, offset: Int64, size: Int64) => Result<Void, Exception>
```

Allocate space within a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor in which space will be allocated|
|`offset`|`Int64`|The position within the file to begin writing|
|`size`|`Int64`|The number of bytes to allocate|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdClose**

```grain
fdClose : (fd: FileDescriptor) => Result<Void, Exception>
```

Close a file descriptor.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to close|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdDatasync**

```grain
fdDatasync : (fd: FileDescriptor) => Result<Void, Exception>
```

Synchronize the data of a file to disk.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to synchronize|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdSync**

```grain
fdSync : (fd: FileDescriptor) => Result<Void, Exception>
```

Synchronize the data and metadata of a file to disk.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to synchronize|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdStats**

```grain
fdStats : (fd: FileDescriptor) => Result<Stats, Exception>
```

Retrieve information about a file descriptor.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of which to retrieve information|

Returns:

|type|description|
|----|-----------|
|`Result<Stats, Exception>`|`Ok(stats)` of the `Stats` associated with the file descriptor if successful or `Err(exception)` otherwise|

### File.**fdSetFlags**

```grain
fdSetFlags :
  (fd: FileDescriptor, flags: List<FdFlag>) => Result<Void, Exception>
```

Update the flags associated with a file descriptor.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to update flags|
|`flags`|`List<FdFlag>`|The flags to apply to the file descriptor|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdSetRights**

```grain
fdSetRights :
  (fd: FileDescriptor, rights: List<Rights>, rightsInheriting: List<Rights>) =>
   Result<Void, Exception>
```

Update the rights associated with a file descriptor.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to update rights|
|`rights`|`List<Rights>`|Rights to apply to the file descriptor|
|`rightsInheriting`|`List<Rights>`|Inheriting rights to apply to the file descriptor|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdFilestats**

```grain
fdFilestats : (fd: FileDescriptor) => Result<Filestats, Exception>
```

Retrieve information about a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the file to retrieve information|

Returns:

|type|description|
|----|-----------|
|`Result<Filestats, Exception>`|`Ok(info)` of the `Filestats` associated with the file descriptor if successful or `Err(exception)` otherwise|

### File.**fdSetSize**

```grain
fdSetSize : (fd: FileDescriptor, size: Int64) => Result<Void, Exception>
```

Set (truncate) the size of a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the file to truncate|
|`size`|`Int64`|The number of bytes to retain in the file|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdSetAccessTime**

```grain
fdSetAccessTime :
  (fd: FileDescriptor, timestamp: Int64) => Result<Void, Exception>
```

Set the access (created) time of a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the file to update|
|`timestamp`|`Int64`|The time to set|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdSetAccessTimeNow**

```grain
fdSetAccessTimeNow : (fd: FileDescriptor) => Result<Void, Exception>
```

Set the access (created) time of a file to the current time.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the file to update|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdSetModifiedTime**

```grain
fdSetModifiedTime :
  (fd: FileDescriptor, timestamp: Int64) => Result<Void, Exception>
```

Set the modified time of a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the file to update|
|`timestamp`|`Int64`|The time to set|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdSetModifiedTimeNow**

```grain
fdSetModifiedTimeNow : (fd: FileDescriptor) => Result<Void, Exception>
```

Set the modified time of a file to the current time.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the file to update|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdReaddir**

```grain
fdReaddir : (fd: FileDescriptor) => Result<Array<DirectoryEntry>, Exception>
```

Read the entires of a directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The directory to read|

Returns:

|type|description|
|----|-----------|
|`Result<Array<DirectoryEntry>, Exception>`|`Ok(dirEntries)` of an array of `DirectoryEntry` for each entry in the directory if successful or `Err(exception)` otherwise|

### File.**fdRenumber**

```grain
fdRenumber :
  (fromFd: FileDescriptor, toFd: FileDescriptor) => Result<Void, Exception>
```

Atomically replace a file descriptor by renumbering another file descriptor.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fromFd`|`FileDescriptor`|The file descriptor to renumber|
|`toFd`|`FileDescriptor`|The file descriptor to overwrite|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**fdSeek**

```grain
fdSeek :
  (fd: FileDescriptor, offset: Int64, whence: Whence) =>
   Result<Int64, Exception>
```

Update a file descriptor's offset.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to operate on|
|`offset`|`Int64`|The number of bytes to move the offset|
|`whence`|`Whence`|The location from which the offset is relative|

Returns:

|type|description|
|----|-----------|
|`Result<Int64, Exception>`|`Ok(offset)` of the new offset of the file descriptor, relative to the start of the file, if successful or `Err(exception)` otherwise|

### File.**fdTell**

```grain
fdTell : (fd: FileDescriptor) => Result<Int64, Exception>
```

Read a file descriptor's offset.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor to read the offset|

Returns:

|type|description|
|----|-----------|
|`Result<Int64, Exception>`|`Ok(offset)` of the offset of the file descriptor, relative to the start of the file, if successful or `Err(exception)` otherwise|

### File.**pathCreateDirectory**

```grain
pathCreateDirectory :
  (fd: FileDescriptor, path: String) => Result<Void, Exception>
```

Create a directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`path`|`String`|The path to the new directory|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**pathFilestats**

```grain
pathFilestats :
  (fd: FileDescriptor, dirFlags: List<LookupFlag>, path: String) =>
   Result<Filestats, Exception>
```

Retrieve information about a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`dirFlags`|`List<LookupFlag>`|Flags which affect path resolution|
|`path`|`String`|The path to retrieve information about|

Returns:

|type|description|
|----|-----------|
|`Result<Filestats, Exception>`|`Ok(info)` of the `Filestats` associated with the file descriptor if successful or `Err(exception)` otherwise|

### File.**pathSetAccessTime**

```grain
pathSetAccessTime :
  (fd: FileDescriptor, dirFlags: List<LookupFlag>, path: String,
   timestamp: Int64) => Result<Void, Exception>
```

Set the access (created) time of a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`dirFlags`|`List<LookupFlag>`|Flags which affect path resolution|
|`path`|`String`|The path to set the time|
|`timestamp`|`Int64`|The time to set|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**pathSetAccessTimeNow**

```grain
pathSetAccessTimeNow :
  (fd: FileDescriptor, dirFlags: List<LookupFlag>, path: String) =>
   Result<Void, Exception>
```

Set the access (created) time of a file to the current time.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`dirFlags`|`List<LookupFlag>`|Flags which affect path resolution|
|`path`|`String`|The path to set the time|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**pathSetModifiedTime**

```grain
pathSetModifiedTime :
  (fd: FileDescriptor, dirFlags: List<LookupFlag>, path: String,
   timestamp: Int64) => Result<Void, Exception>
```

Set the modified time of a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`dirFlags`|`List<LookupFlag>`|Flags which affect path resolution|
|`path`|`String`|The path to set the time|
|`timestamp`|`Int64`|The time to set|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**pathSetModifiedTimeNow**

```grain
pathSetModifiedTimeNow :
  (fd: FileDescriptor, dirFlags: List<LookupFlag>, path: String) =>
   Result<Void, Exception>
```

Set the modified time of a file to the current time.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`dirFlags`|`List<LookupFlag>`|Flags which affect path resolution|
|`path`|`String`|The path to set the time|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**pathLink**

```grain
pathLink :
  (sourceFd: FileDescriptor, dirFlags: List<LookupFlag>, sourcePath: 
   String, targetFd: FileDescriptor, targetPath: String) =>
   Result<Void, Exception>
```

Create a hard link.

Parameters:

|param|type|description|
|-----|----|-----------|
|`sourceFd`|`FileDescriptor`|The file descriptor of the directory in which the source path resolution starts|
|`dirFlags`|`List<LookupFlag>`|Flags which affect path resolution|
|`sourcePath`|`String`|The path to the source of the link|
|`targetFd`|`FileDescriptor`|The file descriptor of the directory in which the target path resolution starts|
|`targetPath`|`String`|The path to the target of the link|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**pathSymlink**

```grain
pathSymlink :
  (fd: FileDescriptor, sourcePath: String, targetPath: String) =>
   Result<Void, Exception>
```

Create a symlink.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`sourcePath`|`String`|The path to the source of the link|
|`targetPath`|`String`|The path to the target of the link|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**pathUnlink**

```grain
pathUnlink : (fd: FileDescriptor, path: String) => Result<Void, Exception>
```

Unlink a file.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`path`|`String`|The path of the file to unlink|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**pathReadlink**

```grain
pathReadlink :
  (fd: FileDescriptor, path: String, size: Number) =>
   Result<(String, Number), Exception>
```

Read the contents of a symbolic link.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`path`|`String`|The path to the symlink|
|`size`|`Number`|The number of bytes to read|

Returns:

|type|description|
|----|-----------|
|`Result<(String, Number), Exception>`|`Ok((contents, numBytes))` of the bytes read and the number of bytes read if successful or `Err(exception)` otherwise|

### File.**pathRemoveDirectory**

```grain
pathRemoveDirectory :
  (fd: FileDescriptor, path: String) => Result<Void, Exception>
```

Remove a directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`fd`|`FileDescriptor`|The file descriptor of the directory in which path resolution starts|
|`path`|`String`|The path to the directory to remove|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**pathRename**

```grain
pathRename :
  (sourceFd: FileDescriptor, sourcePath: String, targetFd: FileDescriptor,
   targetPath: String) => Result<Void, Exception>
```

Rename a file or directory.

Parameters:

|param|type|description|
|-----|----|-----------|
|`sourceFd`|`FileDescriptor`|The file descriptor of the directory in which the source path resolution starts|
|`sourcePath`|`String`|The path of the file to rename|
|`targetFd`|`FileDescriptor`|The file descriptor of the directory in which the target path resolution starts|
|`targetPath`|`String`|The new path of the file|

Returns:

|type|description|
|----|-----------|
|`Result<Void, Exception>`|`Ok(void)` if successful or `Err(exception)` otherwise|

### File.**open**

<details disabled>
<summary tabindex="-1">Added in <code>next</code></summary>
No other changes yet.
</details>

```grain
open :
  (path: String, openFlags: List<OpenFlag>, rights: List<Rights>,
   rightsInheriting: List<Rights>, flags: List<FdFlag>) =>
   Result<FileDescriptor, Exception>
```

Similar to `pathOpen`, but resolves the path relative to preopened directories.

Parameters:

|param|type|description|
|-----|----|-----------|
|`path`|`String`|The path to the file or directory|
|`openFlags`|`List<OpenFlag>`|Flags that decide how the path will be opened|
|`rights`|`List<Rights>`|The rights that dictate what may be done with the returned file descriptor|
|`rightsInheriting`|`List<Rights>`|The rights that dictate what may be done with file descriptors derived from this file descriptor|
|`flags`|`List<FdFlag>`|Flags which affect read/write operations on this file descriptor|

Returns:

|type|description|
|----|-----------|
|`Result<FileDescriptor, Exception>`|`Ok(fd)` of the opened file or directory if successful or `Err(exception)` otherwise|

