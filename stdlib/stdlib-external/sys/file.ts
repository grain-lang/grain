import { malloc, free, throwError } from "../ascutils/grainRuntime";

import {
  errno,
  lookupflags,
  oflags,
  rights,
  fdflags,
  fdstat,
  filestat,
  fstflags,
  path_open,
  fd_read,
  fd_pread,
  fd_write,
  fd_pwrite,
  fd_allocate,
  fd_close,
  fd_datasync,
  fd_sync,
  fd_fdstat_get,
  fd_fdstat_set_flags,
  fd_fdstat_set_rights,
  fd_filestat_get,
  fd_filestat_set_size,
  fd_filestat_set_times,
  fd_readdir,
  fd_renumber,
  fd_seek,
  fd_tell,
  path_create_directory,
  path_filestat_get,
  path_filestat_set_times,
  path_link,
  path_symlink,
  path_unlink_file,
  path_readlink,
  path_remove_directory,
  path_rename,
} from "bindings/wasi";

import { GRAIN_ERR_SYSTEM } from "../ascutils/errors";

import { GRAIN_VOID } from "../ascutils/primitives";

import { loadAdtVal, loadAdtVariant, stringSize, allocateString, allocateTuple, allocateInt64, allocateArray, storeInTuple, tagSimpleNumber } from '../ascutils/dataStructures'

namespace glookupflag {
  // @ts-ignore: decorator
  @inline
  export const SymlinkFollow: u32 = 0
}

namespace goflag {
  // @ts-ignore: decorator
  @inline
  export const Create: u32 = 1
  // @ts-ignore: decorator
  @inline
  export const Directory: u32 = 3
  // @ts-ignore: decorator
  @inline
  export const Exclusive: u32 = 5
  // @ts-ignore: decorator
  @inline
  export const Truncate: u32 = 7
}

namespace grights {
  // @ts-ignore decorator
  @inline
  export const FdDatasync: u32 = 1
  // @ts-ignore decorator
  @inline
  export const FdRead: u32 = 3
  // @ts-ignore decorator
  @inline
  export const FdSeek: u32 = 5
  // @ts-ignore decorator
  @inline
  export const FdSetFlags: u32 = 7
  // @ts-ignore decorator
  @inline
  export const FdSync: u32 = 9
  // @ts-ignore decorator
  @inline
  export const FdTell: u32 = 11
  // @ts-ignore decorator
  @inline
  export const FdWrite: u32 = 13
  // @ts-ignore decorator
  @inline
  export const FdAdvise: u32 = 15
  // @ts-ignore decorator
  @inline
  export const FdAllocate: u32 = 17
  // @ts-ignore decorator
  @inline
  export const PathCreateDirectory: u32 = 19
  // @ts-ignore decorator
  @inline
  export const PathCreateFile: u32 = 21
  // @ts-ignore decorator
  @inline
  export const PathLinkSource: u32 = 23
  // @ts-ignore decorator
  @inline
  export const PathLinkTarget: u32 = 25
  // @ts-ignore decorator
  @inline
  export const PathOpen: u32 = 27
  // @ts-ignore decorator
  @inline
  export const FdReaddir: u32 = 29
  // @ts-ignore decorator
  @inline
  export const PathReadlink: u32 = 31
  // @ts-ignore decorator
  @inline
  export const PathRenameSource: u32 = 33
  // @ts-ignore decorator
  @inline
  export const PathRenameTarget: u32 = 35
  // @ts-ignore decorator
  @inline
  export const PathFilestats: u32 = 37
  // @ts-ignore decorator
  @inline
  export const PathSetSize: u32 = 39
  // @ts-ignore decorator
  @inline
  export const PathSetTimes: u32 = 41
  // @ts-ignore decorator
  @inline
  export const FdFilestats: u32 = 43
  // @ts-ignore decorator
  @inline
  export const FdSetSize: u32 = 45
  // @ts-ignore decorator
  @inline
  export const FdSetTimes: u32 = 47
  // @ts-ignore decorator
  @inline
  export const PathSymlink: u32 = 49
  // @ts-ignore decorator
  @inline
  export const PathRemoveDirectory: u32 = 51
  // @ts-ignore decorator
  @inline
  export const PathUnlinkFile: u32 = 53
  // @ts-ignore decorator
  @inline
  export const PollFdReadwrite: u32 = 55
  // @ts-ignore decorator
  @inline
  export const SockShutdown: u32 = 57
}

namespace gfdflag {
  // @ts-ignore decorator
  @inline
  export const Append: u32 = 1
  // @ts-ignore decorator
  @inline
  export const Dsync: u32 = 3
  // @ts-ignore decorator
  @inline
  export const Nonblock: u32 = 5
  // @ts-ignore decorator
  @inline
  export const Rsync: u32 = 7
  // @ts-ignore decorator
  @inline
  export const Sync: u32 = 9
}

function combineLookupflags(dirflags: u32): u32 {
  let combinedDirFlags: u32 = 0
  let listPtr = dirflags
  while (loadAdtVariant(listPtr) !== 0) {
    let adt = loadAdtVal(listPtr, 0)
    switch (loadAdtVariant(adt)) {
      case glookupflag.SymlinkFollow: {
        combinedDirFlags = combinedDirFlags | lookupflags.SYMLINK_FOLLOW
        break
      }
    }
    listPtr = loadAdtVal(listPtr, 1)
  }
  return combinedDirFlags
}

function combineOFlags(openflags: u32): u16 {
  let combinedOFlags: u16 = 0
  let listPtr = openflags
  while (loadAdtVariant(listPtr) !== 0) {
    let adt = loadAdtVal(listPtr, 0)
    switch (loadAdtVariant(adt)) {
      case goflag.Create: {
        combinedOFlags = combinedOFlags | oflags.CREAT
        break
      }
      case goflag.Directory: {
        combinedOFlags = combinedOFlags | oflags.DIRECTORY
        break
      }
      case goflag.Exclusive: {
        combinedOFlags = combinedOFlags | oflags.EXCL
        break
      }
      case goflag.Truncate: {
        combinedOFlags = combinedOFlags | oflags.TRUNC
        break
      }
    }
    listPtr = loadAdtVal(listPtr, 1)
  }
  return combinedOFlags
}

function combineRights(grightsl: u32): u64 {
  let combinedRights: u64 = 0
  let listPtr = grightsl
  while (loadAdtVariant(listPtr) !== 0) {
    let adt = loadAdtVal(listPtr, 0)
    switch (loadAdtVariant(adt)) {
      case grights.FdDatasync: {
        combinedRights = combinedRights | rights.FD_DATASYNC
        break
      }
      case grights.FdRead: {
        combinedRights = combinedRights | rights.FD_READ
        break
      }
      case grights.FdSeek: {
        combinedRights = combinedRights | rights.FD_SEEK
        break
      }
      case grights.FdSetFlags: {
        combinedRights = combinedRights | rights.FD_FDSTAT_SET_FLAGS
        break
      }
      case grights.FdSync: {
        combinedRights = combinedRights | rights.FD_SYNC
        break
      }
      case grights.FdTell: {
        combinedRights = combinedRights | rights.FD_TELL
        break
      }
      case grights.FdWrite: {
        combinedRights = combinedRights | rights.FD_WRITE
        break
      }
      case grights.FdAdvise: {
        combinedRights = combinedRights | rights.FD_ADVISE
        break
      }
      case grights.FdAllocate: {
        combinedRights = combinedRights | rights.FD_ALLOCATE
        break
      }
      case grights.PathCreateDirectory: {
        combinedRights = combinedRights | rights.PATH_CREATE_DIRECTORY
        break
      }
      case grights.PathCreateFile: {
        combinedRights = combinedRights | rights.PATH_CREATE_FILE
        break
      }
      case grights.PathLinkSource: {
        combinedRights = combinedRights | rights.PATH_LINK_SOURCE
        break
      }
      case grights.PathLinkTarget: {
        combinedRights = combinedRights | rights.PATH_LINK_TARGET
        break
      }
      case grights.PathOpen: {
        combinedRights = combinedRights | rights.PATH_OPEN
        break
      }
      case grights.FdReaddir: {
        combinedRights = combinedRights | rights.FD_READDIR
        break
      }
      case grights.PathReadlink: {
        combinedRights = combinedRights | rights.PATH_READLINK
        break
      }
      case grights.PathRenameSource: {
        combinedRights = combinedRights | rights.PATH_RENAME_SOURCE
        break
      }
      case grights.PathRenameTarget: {
        combinedRights = combinedRights | rights.PATH_RENAME_TARGET
        break
      }
      case grights.PathFilestats: {
        combinedRights = combinedRights | rights.PATH_FILESTAT_GET
        break
      }
      case grights.PathSetSize: {
        combinedRights = combinedRights | rights.PATH_FILESTAT_SET_SIZE
        break
      }
      case grights.PathSetTimes: {
        combinedRights = combinedRights | rights.PATH_FILESTAT_SET_TIMES
        break
      }
      case grights.FdFilestats: {
        combinedRights = combinedRights | rights.FD_FILESTAT_GET
        break
      }
      case grights.FdSetSize: {
        combinedRights = combinedRights | rights.FD_FILESTAT_SET_SIZE
        break
      }
      case grights.FdSetTimes: {
        combinedRights = combinedRights | rights.FD_FILESTAT_SET_TIMES
        break
      }
      case grights.PathSymlink: {
        combinedRights = combinedRights | rights.RIGHT_PATH_SYMLINK
        break
      }
      case grights.PathRemoveDirectory: {
        combinedRights = combinedRights | rights.PATH_REMOVE_DIRECTORY
        break
      }
      case grights.PathUnlinkFile: {
        combinedRights = combinedRights | rights.PATH_UNLINK_FILE
        break
      }
      case grights.PollFdReadwrite: {
        combinedRights = combinedRights | rights.POLL_FD_READWRITE
        break
      }
      case grights.SockShutdown: {
        combinedRights = combinedRights | rights.SOCK_SHUTDOWN
        break
      }
    }
    listPtr = loadAdtVal(listPtr, 1)
  }

  return combinedRights
}

function combineFdFlags(flagsl: u32): u16 {
  let combinedFdFlags: u16 = 0
  let listPtr = flagsl
  while (loadAdtVariant(listPtr) !== 0) {
    let adt = loadAdtVal(listPtr, 0)
    switch (loadAdtVariant(adt)) {
      case gfdflag.Append: {
        combinedFdFlags = combinedFdFlags | fdflags.APPEND
        break
      }
      case gfdflag.Dsync: {
        combinedFdFlags = combinedFdFlags | fdflags.DSYNC
        break
      }
      case gfdflag.Nonblock: {
        combinedFdFlags = combinedFdFlags | fdflags.NONBLOCK
        break
      }
      case gfdflag.Rsync: {
        combinedFdFlags = combinedFdFlags | fdflags.RSYNC
        break
      }
      case gfdflag.Sync: {
        combinedFdFlags = combinedFdFlags | fdflags.SYNC
        break
      }
    }
    listPtr = loadAdtVal(listPtr, 1)
  }

  return combinedFdFlags
}

export function pathOpen(
  dirfd: u32,
  dirflags: u32,
  path: u32,
  openflags: u32,
  fsRightsBase: u32,
  fsRightsInheriting: u32,
  fsFlags: u32
): u32 {
  let dirfdval = loadAdtVal(dirfd, 0) >> 1
  
  let combinedDirFlags = combineLookupflags(dirflags)

  let pathLength = stringSize(path)
  path += 8 // Offset the string pointer to the start of the string

  let combinedOFlags = combineOFlags(openflags)

  let rightsBase = combineRights(fsRightsBase)
  let rightsInheriting = combineRights(fsRightsInheriting)
  
  let combinedFsFlags = combineFdFlags(fsFlags)

  let newFd = malloc(6 * 4)
  memory.copy(newFd, dirfd, 6 * 4)

  let fdPtr = newFd + (5 * 4)
  
  let err = path_open(
    dirfdval,
    combinedDirFlags,
    path,
    pathLength,
    combinedOFlags,
    rightsBase,
    rightsInheriting,
    combinedFsFlags,
    fdPtr
  )
  if (err !== errno.SUCCESS) {
    free(newFd)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  // Tag fd
  store<u32>(newFd, tagSimpleNumber(load<u32>(newFd, 5 * 4)), 5 * 4)
  
  return newFd
}

export function fdRead(fdPtr: u32, n: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  n = n >> 1

  let iovs = malloc(3 * 4)
  let strPtr = allocateString(n)

  store<u32>(iovs, strPtr + (2 * 4))
  store<u32>(iovs, n, 4)

  let nread = iovs + (3 * 4)

  let err = fd_read(fd, iovs, 1, nread)
  if (err !== errno.SUCCESS) {
    free(iovs)
    free(strPtr)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  nread = load<u32>(nread)
  
  let tuple = allocateTuple(2)

  storeInTuple(tuple, 0, strPtr)
  storeInTuple(tuple, 1, tagSimpleNumber(nread))
  store<u32>(strPtr, nread, 4)

  free(iovs)

  return tuple
}

export function fdPread(fdPtr: u32, offsetPtr: u32, n: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let offset = load<u64>(offsetPtr, 4)

  n = n >> 1

  let iovs = malloc(3 * 4)
  let strPtr = allocateString(n)

  store<u32>(iovs, strPtr + (2 * 4))
  store<u32>(iovs, n, 4)

  let nread = iovs + (3 * 4)

  let err = fd_pread(fd, iovs, 1, offset, nread)
  if (err !== errno.SUCCESS) {
    free(iovs)
    free(strPtr)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  nread = load<u32>(nread)
  
  let tuple = allocateTuple(2)

  storeInTuple(tuple, 0, strPtr)
  storeInTuple(tuple, 1, tagSimpleNumber(nread))
  store<u32>(strPtr, nread, 4)

  free(iovs)

  return tuple
}

export function fdWrite(fdPtr: u32, data: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let iovs = malloc(3 * 4)
  let strPtr = data

  store<u32>(iovs, strPtr + (2 * 4))
  store<u32>(iovs, load<u32>(strPtr, 4), 4)

  let nwritten = iovs + (3 * 4)

  let err = fd_write(fd, iovs, 1, nwritten)
  if (err !== errno.SUCCESS) {
    free(iovs)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  nwritten = load<u32>(nwritten)
  
  free(iovs)

  return tagSimpleNumber(nwritten)
}

export function fdPwrite(fdPtr: u32, data: u32, offsetPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1
  
  let iovs = malloc(3 * 4)
  let strPtr = data
  
  store<u32>(iovs, strPtr + (2 * 4))
  store<u32>(iovs, load<u32>(strPtr, 4), 4)

  let offset = load<u64>(offsetPtr, 4)

  let nwritten = iovs + (3 * 4)

  let err = fd_pwrite(fd, iovs, 1, offset, nwritten)
  if (err !== errno.SUCCESS) {
    free(iovs)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  nwritten = load<u32>(nwritten)
  
  free(iovs)

  return tagSimpleNumber(nwritten)
}

export function fdAllocate(fdPtr: u32, offsetPtr: u32, sizePtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let offset = load<u64>(offsetPtr, 4)

  let size = load<u64>(sizePtr, 4)
  
  let err = fd_allocate(fd, offset, size)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdClose(fdPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let err = fd_close(fd)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdDatasync(fdPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let err = fd_datasync(fd)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdSync(fdPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let err = fd_sync(fd)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdStats(fdPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let structPtr = malloc(24)
  
  let err = fd_fdstat_get(fd, changetype<fdstat>(structPtr))
  if (err !== errno.SUCCESS) {
    free(structPtr)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  let filetype = load<u8>(structPtr)
  let fdflags = load<u16>(structPtr, 4)
  let rights = load<u64>(structPtr, 8)
  let rightsInheriting = load<u64>(structPtr, 16)

  let fdflagsPtr = allocateInt64()
  store<u64>(fdflagsPtr, fdflags, 4)

  let rightsPtr = allocateInt64()
  store<u64>(rightsPtr, rights, 4)

  let rightsInheritingPtr = allocateInt64()
  store<u64>(rightsInheritingPtr, rightsInheriting, 4)

  let tuple = allocateTuple(4)
  storeInTuple(tuple, 0, tagSimpleNumber(filetype))
  storeInTuple(tuple, 1, fdflagsPtr)
  storeInTuple(tuple, 2, rightsPtr)
  storeInTuple(tuple, 3, rightsInheritingPtr)

  free(structPtr)

  return tuple
}

export function fdSetFlags(fdPtr: u32, flagsPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let flags: u16 = 0
  let listPtr = flagsPtr
  while (loadAdtVariant(listPtr) !== 0) {
    let adt = loadAdtVal(listPtr, 0)
    switch (loadAdtVariant(adt)) {
      case gfdflag.Append: {
        flags = flags | fdflags.APPEND
        break
      }
      case gfdflag.Dsync: {
        flags = flags | fdflags.DSYNC
        break
      }
      case gfdflag.Nonblock: {
        flags = flags | fdflags.NONBLOCK
        break
      }
      case gfdflag.Rsync: {
        flags = flags | fdflags.RSYNC
        break
      }
      case gfdflag.Sync: {
        flags = flags | fdflags.SYNC
        break
      }
    }
    listPtr = loadAdtVal(listPtr, 1)
  }
  
  let err = fd_fdstat_set_flags(fd, flags)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdSetRights(fdPtr: u32, rightsPtr: u32, rightsInheritingPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let rights = combineRights(rightsPtr)
  let rightsInheriting = combineRights(rightsInheritingPtr)
  
  let err = fd_fdstat_set_rights(fd, rights, rightsInheriting)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdFilestats(fdPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let stats = malloc(64)

  let filestats = changetype<filestat>(stats)
  
  let err = fd_filestat_get(fd, filestats)
  if (err !== errno.SUCCESS) {
    free(stats)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  let tuple = allocateTuple(8)

  let dev = allocateInt64()
  store<u64>(dev, filestats.dev, 4)
  let ino = allocateInt64()
  store<u64>(ino, filestats.ino, 4)
  let nlink = allocateInt64()
  store<u64>(nlink, filestats.nlink, 4)
  let size = allocateInt64()
  store<u64>(size, filestats.size, 4)
  let atim = allocateInt64()
  store<u64>(atim, filestats.atim, 4)
  let mtim = allocateInt64()
  store<u64>(mtim, filestats.mtim, 4)
  let ctim = allocateInt64()
  store<u64>(ctim, filestats.ctim, 4)

  storeInTuple(tuple, 0, dev)
  storeInTuple(tuple, 1, ino)
  storeInTuple(tuple, 2, tagSimpleNumber(filestats.filetype))
  storeInTuple(tuple, 3, nlink)
  storeInTuple(tuple, 4, size)
  storeInTuple(tuple, 5, atim)
  storeInTuple(tuple, 6, mtim)
  storeInTuple(tuple, 7, ctim)

  free(stats)

  return tuple
}

export function fdSetSize(fdPtr: u32, sizePtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let size = load<u64>(sizePtr, 4)

  let err = fd_filestat_set_size(fd, size)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdSetAccessTime(fdPtr: u32, timePtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let time = load<u64>(timePtr, 4)

  let err = fd_filestat_set_times(fd, time, 0, fstflags.SET_ATIM)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdSetAccessTimeNow(fdPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let err = fd_filestat_set_times(fd, 0, 0, fstflags.SET_ATIM_NOW)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdSetModifiedTime(fdPtr: u32, timePtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let time = load<u64>(timePtr, 4)

  let err = fd_filestat_set_times(fd, 0, time, fstflags.SET_MTIM)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdSetModifiedTimeNow(fdPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let err = fd_filestat_set_times(fd, 0, 0, fstflags.SET_MTIM_NOW)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdReaddir(fdPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  const structWidth = 24

  let bufUsed = malloc(4)
  let cookie: u64 = 0
  
  let buf = malloc(structWidth)
  let bufLen = structWidth
  
  let err = fd_readdir(fd, buf, bufLen, cookie, bufUsed)
  if (err !== errno.SUCCESS) {
    free(buf)
    free(bufUsed)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  let used = load<u32>(bufUsed)
  
  if (used <= 0) {
    free(buf)
    free(bufUsed)
    return allocateArray(0)
  }

  bufLen = load<u32>(buf, 16) + structWidth * 2

  free(buf)
  
   // simple linked list
  // ptr +0 -> current buffer
  // ptr +4 -> bufs
  let bufs = 0

  let numEntries = 0

  while (true) {
    numEntries++
    
    buf = malloc(bufLen)
    let cons = malloc(8)
    store<u32>(cons, buf)
    store<u32>(cons, bufs, 4)
    bufs = cons

    let err = fd_readdir(fd, buf, bufLen, cookie, bufUsed)
    if (err !== errno.SUCCESS) {
      while (bufs !== 0) {
        free(load<u32>(bufs))
        let next = load<u32>(bufs, 4)
        free(bufs)
        bufs = next
      }
      free(bufUsed)
      throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
    }

    if (load<u32>(bufUsed) !== bufLen) {
      break
    } else {
      let curLen = load<u32>(buf, 16)
      cookie = load<u64>(buf)
      let nextDirentPtr = buf + structWidth + curLen
      bufLen = load<u32>(nextDirentPtr, 16) + structWidth * 2
    }
  }

  free(bufUsed)

  let arr = allocateArray(numEntries)

  for (let i = numEntries - 1; i >= 0; i--) {
    let dirent = load<u32>(bufs)
    let tuple = allocateTuple(3)

    let inode = allocateInt64()
    store<u64>(inode, load<u64>(dirent, 8), 4)

    let dirnameLen = load<u32>(dirent, 16)
    let dirname = allocateString(dirnameLen)
    memory.copy(dirname + 8, dirent + structWidth, dirnameLen)

    let filetype = tagSimpleNumber(load<u8>(dirent, 20))
    
    storeInTuple(tuple, 0, inode)
    storeInTuple(tuple, 1, dirname)
    storeInTuple(tuple, 2, filetype)

    store<u32>(arr + i * 4, tuple, 8)

    let next = load<u32>(bufs, 4)
    free(bufs)
    free(dirent)

    bufs = next
  }
  
  return arr
}

export function fdRenumber(fromFdPtr: u32, toFdPtr: u32): u32 {
  let fromFd = loadAdtVal(fromFdPtr, 0) >> 1

  let toFd = loadAdtVal(toFdPtr, 0) >> 1

  let err = fd_renumber(fromFd, toFd)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function fdSeek(fdPtr: u32, offsetPtr: u32, whencePtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let offset = load<u64>(offsetPtr, 4)

  let whence = u8(load<u32>(whencePtr, 12) >> 1)

  let newoffset = allocateInt64()
  let newoffsetPtr = newoffset + 4
  
  let err = fd_seek(fd, offset, whence, newoffsetPtr)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return newoffset
}

export function fdTell(fdPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let offset = allocateInt64()
  let offsetPtr = offset + 4
  
  let err = fd_tell(fd, offsetPtr)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return offset
}

export function pathCreateDirectory(fdPtr: u32, stringPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  
  let size = load<u32>(stringPtr, 4)

  stringPtr += 8
  
  let err = path_create_directory(fd, stringPtr, size)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function pathFilestats(fdPtr: u32, dirflags: u32, pathPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let combinedDirFlags = combineLookupflags(dirflags)

  let pathSize = load<u32>(pathPtr, 4)
  pathPtr += 8
  
  let stats = malloc(64)

  let filestats = changetype<filestat>(stats)
  
  let err = path_filestat_get(fd, combinedDirFlags, pathPtr, pathSize, filestats)
  if (err !== errno.SUCCESS) {
    free(stats)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  let tuple = allocateTuple(8)

  let dev = allocateInt64()
  store<u64>(dev, filestats.dev, 4)
  let ino = allocateInt64()
  store<u64>(ino, filestats.ino, 4)
  let nlink = allocateInt64()
  store<u64>(nlink, filestats.nlink, 4)
  let size = allocateInt64()
  store<u64>(size, filestats.size, 4)
  let atim = allocateInt64()
  store<u64>(atim, filestats.atim, 4)
  let mtim = allocateInt64()
  store<u64>(mtim, filestats.mtim, 4)
  let ctim = allocateInt64()
  store<u64>(ctim, filestats.ctim, 4)

  storeInTuple(tuple, 0, dev)
  storeInTuple(tuple, 1, ino)
  storeInTuple(tuple, 2, tagSimpleNumber(filestats.filetype))
  storeInTuple(tuple, 3, nlink)
  storeInTuple(tuple, 4, size)
  storeInTuple(tuple, 5, atim)
  storeInTuple(tuple, 6, mtim)
  storeInTuple(tuple, 7, ctim)

  free(stats)

  return tuple
}

export function pathSetAccessTime(fdPtr: u32, dirflags: u32, pathPtr: u32, timePtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let combinedDirFlags = combineLookupflags(dirflags)

  let pathSize = load<u32>(pathPtr, 4)
  pathPtr += 8

  let time = load<u64>(timePtr, 4)

  let err = path_filestat_set_times(fd, combinedDirFlags, pathPtr, pathSize, time, 0, fstflags.SET_ATIM)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function pathSetAccessTimeNow(fdPtr: u32, dirflags: u32, pathPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let combinedDirFlags = combineLookupflags(dirflags)

  let pathSize = load<u32>(pathPtr, 4)
  pathPtr += 8

  let err = path_filestat_set_times(fd, combinedDirFlags, pathPtr, pathSize, 0, 0, fstflags.SET_ATIM_NOW)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function pathSetModifiedTime(fdPtr: u32, dirflags: u32, pathPtr: u32, timePtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let combinedDirFlags = combineLookupflags(dirflags)

  let pathSize = load<u32>(pathPtr, 4)
  pathPtr += 8

  let time = load<u64>(timePtr, 4)

  let err = path_filestat_set_times(fd, combinedDirFlags, pathPtr, pathSize, 0, time, fstflags.SET_MTIM)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function pathSetModifiedTimeNow(fdPtr: u32, dirflags: u32, pathPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let combinedDirFlags = combineLookupflags(dirflags)

  let pathSize = load<u32>(pathPtr, 4)
  pathPtr += 8

  let err = path_filestat_set_times(fd, combinedDirFlags, pathPtr, pathSize, 0, 0, fstflags.SET_MTIM_NOW)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function pathLink(sourceFdPtr: u32, dirflags: u32, sourcePtr: u32, targetFdPtr: u32, targetPtr: u32): u32 {
  let sourceFd = loadAdtVal(sourceFdPtr, 0) >> 1

  let targetFd = loadAdtVal(targetFdPtr, 0) >> 1

  let combinedDirFlags = combineLookupflags(dirflags)

  let sourceSize = load<u32>(sourcePtr, 4)
  sourcePtr += 8

  let targetSize = load<u32>(targetPtr, 4)
  targetPtr += 8

  let err = path_link(sourceFd, combinedDirFlags, sourcePtr, sourceSize, targetFd, targetPtr, targetSize)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function pathSymlink(fdPtr: u32, sourcePtr: u32, targetPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let sourceSize = load<u32>(sourcePtr, 4)
  sourcePtr += 8

  let targetSize = load<u32>(targetPtr, 4)
  targetPtr += 8

  let err = path_symlink(sourcePtr, sourceSize, fd, targetPtr, targetSize)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function pathUnlink(fdPtr: u32, pathPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let pathSize = load<u32>(pathPtr, 4)
  pathPtr += 8

  let err = path_unlink_file(fd, pathPtr, pathSize)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function pathReadlink(fdPtr: u32, pathPtr: u32, size: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let pathSize = load<u32>(pathPtr, 4)
  pathPtr += 8

  size = size >> 1

  let grainStrPtr = allocateString(size)
  let strPtr = grainStrPtr + 8

  let nread = malloc(4)

  let err = path_readlink(fd, pathPtr, pathSize, strPtr, size, nread)
  if (err !== errno.SUCCESS) {
    free(grainStrPtr)
    free(nread)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  let tuple = allocateTuple(2)

  storeInTuple(tuple, 0, grainStrPtr)
  storeInTuple(tuple, 1, tagSimpleNumber(load<u32>(nread)))

  free(nread)

  return tuple
}

export function pathRemoveDirectory(fdPtr: u32, pathPtr: u32): u32 {
  let fd = loadAdtVal(fdPtr, 0) >> 1

  let pathSize = load<u32>(pathPtr, 4)
  pathPtr += 8

  let err = path_remove_directory(fd, pathPtr, pathSize)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}

export function pathRename(sourceFdPtr: u32, sourcePtr: u32, targetFdPtr: u32, targetPtr: u32): u32 {
  let sourceFd = loadAdtVal(sourceFdPtr, 0) >> 1

  let targetFd = loadAdtVal(targetFdPtr, 0) >> 1

  let sourceSize = load<u32>(sourcePtr, 4)
  sourcePtr += 8

  let targetSize = load<u32>(targetPtr, 4)
  targetPtr += 8

  let err = path_rename(sourceFd, sourcePtr, sourceSize, targetFd, targetPtr, targetSize)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  return GRAIN_VOID
}
