export declare function malloc(bytes: u32): u32
export declare function free(ptr: u32): u32
export declare function incRef(ptr: u32): u32
export declare function decRef(ptr: u32): u32

export function calloc(nb: u32): u32 {
  const ret = malloc(nb)
  if (ret == -1) return ret
  memory.fill(ret, 0, nb)
  return ret
}

export declare function throwError(code: u32, value1: u32, value2: u32): u32
