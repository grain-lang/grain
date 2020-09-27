import { GRAIN_GENERIC_HEAP_TAG_TYPE } from './ascutils/tags'
import { newInt64, rawInt64Ptr } from "./ascutils/dataStructures";
import { GRAIN_FALSE, GRAIN_TRUE } from "./ascutils/primitives";
import {
  GRAIN_ERR_DIVISION_BY_ZERO,
  GRAIN_ERR_MODULO_BY_ZERO,
} from './ascutils/errors'
import { throwError } from './ascutils/grainRuntime'

// @ts-ignore: decorator
@inline
function loadI64(xptr: u32): i64 {
  return load<i64>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4)
}

// @ts-ignore: decorator
@inline
function loadU64(xptr: u32): u64 {
  return load<u64>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4)
}

export function incr(x: u32): u32 {
  const result = loadI64(x) + 1
  return newInt64(result)
}

export function decr(x: u32): u32 {
  const result = loadI64(x) - 1
  return newInt64(result)
}

export function add(x: u32, y: u32): u32 {
  const result = loadI64(x) + loadI64(y)
  return newInt64(result)
}

export function sub(x: u32, y: u32): u32 {
  const result = loadI64(x) - loadI64(y)
  return newInt64(result)
}

export function mul(x: u32, y: u32): u32 {
  const result = loadI64(x) * loadI64(y)
  return newInt64(result)
}

export function div(x: u32, y: u32): u32 {
  const yval = loadI64(y)
  if (yval == 0) {
    throwError(GRAIN_ERR_DIVISION_BY_ZERO, 0, 0)
  }
  const result = loadI64(x) / yval
  return newInt64(result)
}

export function divU(x: u32, y: u32): u32 {
  const yval = loadU64(y)
  if (yval == 0) {
    throwError(GRAIN_ERR_DIVISION_BY_ZERO, 0, 0)
  }
  const result = loadU64(x) / yval
  return newInt64(result)
}

export function rem(x: u32, y: u32): u32 {
  const yval = loadI64(y)
  if (yval == 0) {
    throwError(GRAIN_ERR_DIVISION_BY_ZERO, 0, 0)
  }
  const result = loadI64(x) % yval
  return newInt64(result)
}

export function remU(x: u32, y: u32): u32 {
  const yval = loadU64(y)
  if (yval == 0) {
    throwError(GRAIN_ERR_DIVISION_BY_ZERO, 0, 0)
  }
  const result = loadU64(x) % yval
  return newInt64(result)
}

export function mod(x: u32, y: u32): u32 {
  const xval = rawInt64Ptr(x)
  const yval = rawInt64Ptr(y)

  if (yval == 0) {
    throwError(GRAIN_ERR_MODULO_BY_ZERO, 0, 0)
  }

  const bitmask : u64 = 0x8000000000000000
  // AssemblyScript's % is the remainder operator, so we implement modulo manually
  if ((xval & bitmask) == (yval & bitmask)) {
    // Signs match
    return newInt64(xval % yval)
  } else {
    // Signs are different
    const modval = abs<i64>(xval) % abs<i64>(yval)
    return newInt64(modval != 0 ? (abs<i64>(yval) - modval) * (yval < 0 ? -1 : 1) : modval)
  }
}

export function lt(x: u32, y: u32): u32 {
  const result = loadI64(x) < loadI64(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function gt(x: u32, y: u32): u32 {
  const result = loadI64(x) > loadI64(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function lte(x: u32, y: u32): u32 {
  const result = loadI64(x) <= loadI64(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function gte(x: u32, y: u32): u32 {
  const result = loadI64(x) >= loadI64(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function lnot(x: u32): u32 {
  const result = ~loadI64(x)
  return newInt64(result)
}

export function land(x: u32, y: u32): u32 {
  const result = loadI64(x) & loadI64(y)
  return newInt64(result)
}

export function lor(x: u32, y: u32): u32 {
  const result = loadI64(x) | loadI64(y)
  return newInt64(result)
}

export function lxor(x: u32, y: u32): u32 {
  const result = loadI64(x) ^ loadI64(y)
  return newInt64(result)
}

export function shl(x: u32, y: u32): u32 {
  const result = loadI64(x) << loadI64(y)
  return newInt64(result)
}

export function shr(x: u32, y: u32): u32 {
  const result = loadI64(x) >> loadI64(y)
  return newInt64(result)
}

export function shrU(x: u32, y: u32): u32 {
  const result = loadI64(x) >>> loadI64(y)
  return newInt64(result)
}

