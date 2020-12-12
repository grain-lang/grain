export declare function malloc(bytes: u32): u32
export declare function free(ptr: u32): u32

// [TODO] make more efficient (some sort of u32/u64)
export function calloc(nb: u32): u32 {
  const ret = malloc(nb)
  if (ret == -1) return ret
  for (let i : u32= 0; i < nb; ++i) {
    store<u8>(ret, 0, 0)
  }
  return ret
}

export declare function throwError(code: u32, value1: u32, value2: u32): u32
