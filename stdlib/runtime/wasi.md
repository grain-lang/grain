### Wasi.**args_get**

```grain
args_get : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**args_sizes_get**

```grain
args_sizes_get : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**environ_get**

```grain
environ_get : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**environ_sizes_get**

```grain
environ_sizes_get : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**proc_exit**

```grain
proc_exit : WasmI32 -> Void
```

### Wasi.**proc_raise**

```grain
proc_raise : WasmI32 -> WasmI32
```

### Wasi.**sched_yield**

```grain
sched_yield : () -> WasmI32
```

### Wasi.**random_get**

```grain
random_get : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**clock_time_get**

```grain
clock_time_get : (WasmI32, WasmI64, WasmI32) -> WasmI32
```

### Wasi.**path_open**

```grain
path_open :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI64, WasmI64, WasmI32,
   WasmI32) -> WasmI32
```

### Wasi.**fd_read**

```grain
fd_read : (WasmI32, WasmI32, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**fd_pread**

```grain
fd_pread : (WasmI32, WasmI32, WasmI32, WasmI64, WasmI32) -> WasmI32
```

### Wasi.**fd_write**

```grain
fd_write : (WasmI32, WasmI32, WasmI32, WasmI32) -> WasmI32
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

### Wasi.**fd_pwrite**

```grain
fd_pwrite : (WasmI32, WasmI32, WasmI32, WasmI64, WasmI32) -> WasmI32
```

### Wasi.**fd_allocate**

```grain
fd_allocate : (WasmI32, WasmI64, WasmI64) -> WasmI32
```

### Wasi.**fd_close**

```grain
fd_close : WasmI32 -> WasmI32
```

### Wasi.**fd_datasync**

```grain
fd_datasync : WasmI32 -> WasmI32
```

### Wasi.**fd_sync**

```grain
fd_sync : WasmI32 -> WasmI32
```

### Wasi.**fd_fdstat_get**

```grain
fd_fdstat_get : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**fd_fdstat_set_flags**

```grain
fd_fdstat_set_flags : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**fd_fdstat_set_rights**

```grain
fd_fdstat_set_rights : (WasmI32, WasmI64, WasmI64) -> WasmI32
```

### Wasi.**fd_filestat_get**

```grain
fd_filestat_get : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**fd_filestat_set_size**

```grain
fd_filestat_set_size : (WasmI32, WasmI64) -> WasmI32
```

### Wasi.**fd_filestat_set_times**

```grain
fd_filestat_set_times : (WasmI32, WasmI64, WasmI64, WasmI32) -> WasmI32
```

### Wasi.**fd_readdir**

```grain
fd_readdir : (WasmI32, WasmI32, WasmI32, WasmI64, WasmI32) -> WasmI32
```

### Wasi.**fd_renumber**

```grain
fd_renumber : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**fd_seek**

```grain
fd_seek : (WasmI32, WasmI64, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**fd_tell**

```grain
fd_tell : (WasmI32, WasmI32) -> WasmI32
```

### Wasi.**path_create_directory**

```grain
path_create_directory : (WasmI32, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**path_filestat_get**

```grain
path_filestat_get : (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**path_filestat_set_times**

```grain
path_filestat_set_times :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI64, WasmI64, WasmI32) -> WasmI32
```

### Wasi.**path_link**

```grain
path_link :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**path_symlink**

```grain
path_symlink : (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**path_unlink_file**

```grain
path_unlink_file : (WasmI32, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**path_readlink**

```grain
path_readlink :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**path_remove_directory**

```grain
path_remove_directory : (WasmI32, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**path_rename**

```grain
path_rename :
  (WasmI32, WasmI32, WasmI32, WasmI32, WasmI32, WasmI32) -> WasmI32
```

### Wasi.**_CLOCK_REALTIME**

```grain
_CLOCK_REALTIME : WasmI32
```

### Wasi.**_CLOCK_MONOTONIC**

```grain
_CLOCK_MONOTONIC : WasmI32
```

### Wasi.**_CLOCK_PROCESS_CPUTIME**

```grain
_CLOCK_PROCESS_CPUTIME : WasmI32
```

### Wasi.**_CLOCK_THREAD_CPUTIME**

```grain
_CLOCK_THREAD_CPUTIME : WasmI32
```

### Wasi.**_TIME_SET_ATIM**

```grain
_TIME_SET_ATIM : WasmI32
```

### Wasi.**_TIME_SET_ATIM_NOW**

```grain
_TIME_SET_ATIM_NOW : WasmI32
```

### Wasi.**_TIME_SET_MTIM**

```grain
_TIME_SET_MTIM : WasmI32
```

### Wasi.**_TIME_SET_MTIM_NOW**

```grain
_TIME_SET_MTIM_NOW : WasmI32
```

### Wasi.**_LOOKUP_FLAG_SYMLINK_FOLLOW**

```grain
_LOOKUP_FLAG_SYMLINK_FOLLOW : WasmI32
```

### Wasi.**_OPEN_FLAG_CREAT**

```grain
_OPEN_FLAG_CREAT : WasmI32
```

### Wasi.**_OPEN_FLAG_DIRECTORY**

```grain
_OPEN_FLAG_DIRECTORY : WasmI32
```

### Wasi.**_OPEN_FLAG_EXCL**

```grain
_OPEN_FLAG_EXCL : WasmI32
```

### Wasi.**_OPEN_FLAG_TRUNC**

```grain
_OPEN_FLAG_TRUNC : WasmI32
```

### Wasi.**_FDFLAG_APPEND**

```grain
_FDFLAG_APPEND : WasmI32
```

### Wasi.**_FDFLAG_DSYNC**

```grain
_FDFLAG_DSYNC : WasmI32
```

### Wasi.**_FDFLAG_NONBLOCK**

```grain
_FDFLAG_NONBLOCK : WasmI32
```

### Wasi.**_FDFLAG_RSYNC**

```grain
_FDFLAG_RSYNC : WasmI32
```

### Wasi.**_FDFLAG_SYNC**

```grain
_FDFLAG_SYNC : WasmI32
```

### Wasi.**_WHENCE_SET**

```grain
_WHENCE_SET : WasmI32
```

### Wasi.**_WHENCE_CUR**

```grain
_WHENCE_CUR : WasmI32
```

### Wasi.**_WHENCE_END**

```grain
_WHENCE_END : WasmI32
```

### Wasi.**_ESUCCESS**

```grain
_ESUCCESS : WasmI32
```

### Wasi.**_ETOOBIG**

```grain
_ETOOBIG : WasmI32
```

### Wasi.**_EACCES**

```grain
_EACCES : WasmI32
```

### Wasi.**_EADDRINUSE**

```grain
_EADDRINUSE : WasmI32
```

### Wasi.**_EADDRNOTAVAIL**

```grain
_EADDRNOTAVAIL : WasmI32
```

### Wasi.**_EAFNOSUPPORT**

```grain
_EAFNOSUPPORT : WasmI32
```

### Wasi.**_EAGAIN**

```grain
_EAGAIN : WasmI32
```

### Wasi.**_EALREADY**

```grain
_EALREADY : WasmI32
```

### Wasi.**_EBADF**

```grain
_EBADF : WasmI32
```

### Wasi.**_EBADMSG**

```grain
_EBADMSG : WasmI32
```

### Wasi.**_EBUSY**

```grain
_EBUSY : WasmI32
```

### Wasi.**_ECANCELED**

```grain
_ECANCELED : WasmI32
```

### Wasi.**_ECHILD**

```grain
_ECHILD : WasmI32
```

### Wasi.**_ECONNABORTED**

```grain
_ECONNABORTED : WasmI32
```

### Wasi.**_ECONNREFUSED**

```grain
_ECONNREFUSED : WasmI32
```

### Wasi.**_ECONNRESET**

```grain
_ECONNRESET : WasmI32
```

### Wasi.**_EDEADLK**

```grain
_EDEADLK : WasmI32
```

### Wasi.**_EDESTADDRREQ**

```grain
_EDESTADDRREQ : WasmI32
```

### Wasi.**_EDOM**

```grain
_EDOM : WasmI32
```

### Wasi.**_EDQUOT**

```grain
_EDQUOT : WasmI32
```

### Wasi.**_EEXIST**

```grain
_EEXIST : WasmI32
```

### Wasi.**_EFAULT**

```grain
_EFAULT : WasmI32
```

### Wasi.**_EFBIG**

```grain
_EFBIG : WasmI32
```

### Wasi.**_EHOSTUNREACH**

```grain
_EHOSTUNREACH : WasmI32
```

### Wasi.**_EIDRM**

```grain
_EIDRM : WasmI32
```

### Wasi.**_EILSEQ**

```grain
_EILSEQ : WasmI32
```

### Wasi.**_EINPROGRESS**

```grain
_EINPROGRESS : WasmI32
```

### Wasi.**_EINTR**

```grain
_EINTR : WasmI32
```

### Wasi.**_EINVAL**

```grain
_EINVAL : WasmI32
```

### Wasi.**_EIO**

```grain
_EIO : WasmI32
```

### Wasi.**_EISCONN**

```grain
_EISCONN : WasmI32
```

### Wasi.**_EISDIR**

```grain
_EISDIR : WasmI32
```

### Wasi.**_ELOOP**

```grain
_ELOOP : WasmI32
```

### Wasi.**_EMFILE**

```grain
_EMFILE : WasmI32
```

### Wasi.**_EMLINK**

```grain
_EMLINK : WasmI32
```

### Wasi.**_EMSGSIZE**

```grain
_EMSGSIZE : WasmI32
```

### Wasi.**_EMULTIHOP**

```grain
_EMULTIHOP : WasmI32
```

### Wasi.**_ENAMETOOLONG**

```grain
_ENAMETOOLONG : WasmI32
```

### Wasi.**_ENETDOWN**

```grain
_ENETDOWN : WasmI32
```

### Wasi.**_ENETRESET**

```grain
_ENETRESET : WasmI32
```

### Wasi.**_ENETUNREACH**

```grain
_ENETUNREACH : WasmI32
```

### Wasi.**_ENFILE**

```grain
_ENFILE : WasmI32
```

### Wasi.**_ENOBUFS**

```grain
_ENOBUFS : WasmI32
```

### Wasi.**_ENODEV**

```grain
_ENODEV : WasmI32
```

### Wasi.**_ENOENT**

```grain
_ENOENT : WasmI32
```

### Wasi.**_ENOEXEC**

```grain
_ENOEXEC : WasmI32
```

### Wasi.**_ENOLCK**

```grain
_ENOLCK : WasmI32
```

### Wasi.**_ENOLINK**

```grain
_ENOLINK : WasmI32
```

### Wasi.**_ENOMEM**

```grain
_ENOMEM : WasmI32
```

### Wasi.**_ENOMSG**

```grain
_ENOMSG : WasmI32
```

### Wasi.**_ENOPROTOOPT**

```grain
_ENOPROTOOPT : WasmI32
```

### Wasi.**_ENOSPC**

```grain
_ENOSPC : WasmI32
```

### Wasi.**_ENOSYS**

```grain
_ENOSYS : WasmI32
```

### Wasi.**_ENOTCONN**

```grain
_ENOTCONN : WasmI32
```

### Wasi.**_ENOTDIR**

```grain
_ENOTDIR : WasmI32
```

### Wasi.**_ENOTEMPTY**

```grain
_ENOTEMPTY : WasmI32
```

### Wasi.**_ENOTRECOVERABLE**

```grain
_ENOTRECOVERABLE : WasmI32
```

### Wasi.**_ENOTSOCK**

```grain
_ENOTSOCK : WasmI32
```

### Wasi.**_ENOTSUP**

```grain
_ENOTSUP : WasmI32
```

### Wasi.**_ENOTTY**

```grain
_ENOTTY : WasmI32
```

### Wasi.**_ENXIO**

```grain
_ENXIO : WasmI32
```

### Wasi.**_EOVERFLOW**

```grain
_EOVERFLOW : WasmI32
```

### Wasi.**_EOWNERDEAD**

```grain
_EOWNERDEAD : WasmI32
```

### Wasi.**_EPERM**

```grain
_EPERM : WasmI32
```

### Wasi.**_EPIPE**

```grain
_EPIPE : WasmI32
```

### Wasi.**_EPROTO**

```grain
_EPROTO : WasmI32
```

### Wasi.**_EPROTONOSUPPORT**

```grain
_EPROTONOSUPPORT : WasmI32
```

### Wasi.**_EPROTOTYPE**

```grain
_EPROTOTYPE : WasmI32
```

### Wasi.**_ERANGE**

```grain
_ERANGE : WasmI32
```

### Wasi.**_EROFS**

```grain
_EROFS : WasmI32
```

### Wasi.**_ESPIPE**

```grain
_ESPIPE : WasmI32
```

### Wasi.**_ESRCH**

```grain
_ESRCH : WasmI32
```

### Wasi.**_ESTALE**

```grain
_ESTALE : WasmI32
```

### Wasi.**_ETIMEDOUT**

```grain
_ETIMEDOUT : WasmI32
```

### Wasi.**_ETXTBSY**

```grain
_ETXTBSY : WasmI32
```

### Wasi.**_EXDEV**

```grain
_EXDEV : WasmI32
```

### Wasi.**_ENOTCAPABLE**

```grain
_ENOTCAPABLE : WasmI32
```

### Wasi.**stringOfSystemError**

```grain
stringOfSystemError : a -> String
```

