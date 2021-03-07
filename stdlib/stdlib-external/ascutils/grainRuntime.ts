@external("GRAIN$MODULE$runtime/malloc", "malloc")
declare function mallocExt(closure: u32, bytes: u32): u32
@external("GRAIN$MODULE$runtime/malloc", "GRAIN$EXPORT$malloc")
declare let mallocClosure: u32
@external("GRAIN$MODULE$runtime/malloc", "free")
declare function freeExt(closure: u32, ptr: u32): u32
@external("GRAIN$MODULE$runtime/malloc", "GRAIN$EXPORT$free")
declare let freeClosure: u32

export declare function incRef(ptr: u32): u32
export declare function decRef(ptr: u32): u32

// @ts-ignore: decorator
@inline
export function malloc(nb: u32): u32 {
  return mallocExt(mallocClosure, nb);
}

// @ts-ignore: decorator
@inline
export function free(ptr: u32): u32 {
  return freeExt(freeClosure, ptr);
}

export function calloc(nb: u32): u32 {
  const ret = malloc(nb)
  if (ret == -1) return ret
  memory.fill(ret, 0, nb)
  return ret
}

export declare function throwError(code: u32, value1: u32, value2: u32): u32
