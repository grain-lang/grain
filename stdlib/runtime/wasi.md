---
title: Wasi
---

## Values

Functions and constants included in the Wasi module.

### Wasi.**argsGet**

```grain
argsGet : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**argsSizesGet**

```grain
argsSizesGet : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**environGet**

```grain
environGet : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**environSizesGet**

```grain
environSizesGet : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**procExit**

```grain
procExit : WasmI32 => Void
```

### Wasi.**procRaise**

```grain
procRaise : WasmI32 => WasmI32
```

### Wasi.**schedYield**

```grain
schedYield : () => WasmI32
```

### Wasi.**randomGet**

```grain
randomGet : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**clockTimeGet**

```grain
clockTimeGet : (WasmI32, WasmI64, WasmI32) => WasmI32
```

### Wasi.**pathOpen**

```grain
pathOpen :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI64, WasmI64, WasmI32,
   WasmI32) => WasmI32
```

### Wasi.**fdRead**

```grain
fdRead : (WasmI32, WasmI32, WasmI32, WasmI32) => WasmI32
```

### Wasi.**fdPread**

```grain
fdPread : (WasmI32, WasmI32, WasmI32, WasmI64, WasmI32) => WasmI32
```

### Wasi.**fdPrestatGet**

```grain
fdPrestatGet : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**fdPrestatDirName**

```grain
fdPrestatDirName : (WasmI32, WasmI32, WasmI32) => WasmI32
```

### Wasi.**fdWrite**

```grain
fdWrite : (WasmI32, WasmI32, WasmI32, WasmI32) => WasmI32
```

Invokes the `fd_write` system call.

Parameters:

|param|type|description|
|-----|----|-----------|
|`file_descriptor`|`WasmI32`|The file descriptor to write to|
|`iovs`|`WasmI32`|The pointer to the array of iovs to write|
|`iovs_len`|`WasmI32`|The length of the array of iovs|
|`nwritten`|`WasmI32`|Where to store the number of bytes written|

Returns:

|type|description|
|----|-----------|
|`WasmI32`|The number of bytes written|

### Wasi.**fdPwrite**

```grain
fdPwrite : (WasmI32, WasmI32, WasmI32, WasmI64, WasmI32) => WasmI32
```

### Wasi.**fdAllocate**

```grain
fdAllocate : (WasmI32, WasmI64, WasmI64) => WasmI32
```

### Wasi.**fdClose**

```grain
fdClose : WasmI32 => WasmI32
```

### Wasi.**fdDatasync**

```grain
fdDatasync : WasmI32 => WasmI32
```

### Wasi.**fdSync**

```grain
fdSync : WasmI32 => WasmI32
```

### Wasi.**fdFdstatGet**

```grain
fdFdstatGet : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**fdFdstatSetFlags**

```grain
fdFdstatSetFlags : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**fdFdstatSetRights**

```grain
fdFdstatSetRights : (WasmI32, WasmI64, WasmI64) => WasmI32
```

### Wasi.**fdFilestatGet**

```grain
fdFilestatGet : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**fdFilestatSetSize**

```grain
fdFilestatSetSize : (WasmI32, WasmI64) => WasmI32
```

### Wasi.**fdFilestatSetTimes**

```grain
fdFilestatSetTimes : (WasmI32, WasmI64, WasmI64, WasmI32) => WasmI32
```

### Wasi.**fdReaddir**

```grain
fdReaddir : (WasmI32, WasmI32, WasmI32, WasmI64, WasmI32) => WasmI32
```

### Wasi.**fdRenumber**

```grain
fdRenumber : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**fdSeek**

```grain
fdSeek : (WasmI32, WasmI64, WasmI32, WasmI32) => WasmI32
```

### Wasi.**fdTell**

```grain
fdTell : (WasmI32, WasmI32) => WasmI32
```

### Wasi.**pathCreateDirectory**

```grain
pathCreateDirectory : (WasmI32, WasmI32, WasmI32) => WasmI32
```

### Wasi.**pathFilestatGet**

```grain
pathFilestatGet : (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) => WasmI32
```

### Wasi.**pathFilestatSetTimes**

```grain
pathFilestatSetTimes :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI64, WasmI64, WasmI32) => WasmI32
```

### Wasi.**pathLink**

```grain
pathLink :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) => WasmI32
```

### Wasi.**pathSymlink**

```grain
pathSymlink : (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) => WasmI32
```

### Wasi.**pathUnlinkFile**

```grain
pathUnlinkFile : (WasmI32, WasmI32, WasmI32) => WasmI32
```

### Wasi.**pathReadlink**

```grain
pathReadlink :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) => WasmI32
```

### Wasi.**pathRemoveDirectory**

```grain
pathRemoveDirectory : (WasmI32, WasmI32, WasmI32) => WasmI32
```

### Wasi.**pathRename**

```grain
pathRename :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) => WasmI32
```

