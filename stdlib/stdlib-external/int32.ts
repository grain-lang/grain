import { GRAIN_GENERIC_HEAP_TAG_TYPE } from './ascutils/tags'
import { newInt32, rawInt32Ptr, loadInt32, loadInt32Unsigned } from "./ascutils/dataStructures";
import { GRAIN_FALSE, GRAIN_TRUE } from "./ascutils/primitives";
import {
  GRAIN_ERR_DIVISION_BY_ZERO,
  GRAIN_ERR_MODULO_BY_ZERO,
} from './ascutils/errors'
import { throwError } from './ascutils/grainRuntime'


export function incr(x: u32): u32 {
  const result = loadInt32(x) + 1
  return newInt32(result)
}

export function decr(x: u32): u32 {
  const result = loadInt32(x) - 1
  return newInt32(result)
}

export function add(x: u32, y: u32): u32 {
  const result = loadInt32(x) + loadInt32(y)
  return newInt32(result)
}

export function sub(x: u32, y: u32): u32 {
  const result = loadInt32(x) - loadInt32(y)
  return newInt32(result)
}

export function mul(x: u32, y: u32): u32 {
  const result = loadInt32(x) * loadInt32(y)
  return newInt32(result)
}

export function div(x: u32, y: u32): u32 {
  const yval = loadInt32(y)
  if (yval == 0) {
    throwError(GRAIN_ERR_DIVISION_BY_ZERO, 0, 0)
  }
  const result = loadInt32(x) / yval
  return newInt32(result)
}

export function divU(x: u32, y: u32): u32 {
  const yval = loadInt32Unsigned(y)
  if (yval == 0) {
    throwError(GRAIN_ERR_DIVISION_BY_ZERO, 0, 0)
  }
  const result = loadInt32Unsigned(x) / yval
  return newInt32(result)
}

export function rem(x: u32, y: u32): u32 {
  const yval = loadInt32(y)
  if (yval == 0) {
    throwError(GRAIN_ERR_DIVISION_BY_ZERO, 0, 0)
  }
  const result = loadInt32(x) % yval
  return newInt32(result)
}

export function remU(x: u32, y: u32): u32 {
  const yval = loadInt32Unsigned(y)
  if (yval == 0) {
    throwError(GRAIN_ERR_DIVISION_BY_ZERO, 0, 0)
  }
  const result = loadInt32Unsigned(x) % yval
  return newInt32(result)
}

export function mod(x: u32, y: u32): u32 {
  const xval = rawInt32Ptr(x)
  const yval = rawInt32Ptr(y)

  if (yval == 0) {
    throwError(GRAIN_ERR_MODULO_BY_ZERO, 0, 0)
  }

  const bitmask : u32 = 0x80000000
  // AssemblyScript's % is the remainder operator, so we implement modulo manually
  if ((xval & bitmask) == (yval & bitmask)) {
    // Signs match
    return newInt32(xval % yval)
  } else {
    // Signs are different
    const modval = abs<i32>(xval) % abs<i32>(yval)
    return newInt32(modval != 0 ? (abs<i32>(yval) - modval) * (yval < 0 ? -1 : 1) : modval)
  }
}

export function lt(x: u32, y: u32): u32 {
  const result = loadInt32(x) < loadInt32(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function gt(x: u32, y: u32): u32 {
  const result = loadInt32(x) > loadInt32(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function lte(x: u32, y: u32): u32 {
  const result = loadInt32(x) <= loadInt32(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function gte(x: u32, y: u32): u32 {
  const result = loadInt32(x) >= loadInt32(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function lnot(x: u32): u32 {
  const result = ~loadInt32(x)
  return newInt32(result)
}

export function land(x: u32, y: u32): u32 {
  const result = loadInt32(x) & loadInt32(y)
  return newInt32(result)
}

export function lor(x: u32, y: u32): u32 {
  const result = loadInt32(x) | loadInt32(y)
  return newInt32(result)
}

export function lxor(x: u32, y: u32): u32 {
  const result = loadInt32(x) ^ loadInt32(y)
  return newInt32(result)
}

export function shl(x: u32, y: u32): u32 {
  const result = loadInt32(x) << loadInt32(y)
  return newInt32(result)
}

export function shr(x: u32, y: u32): u32 {
  const result = loadInt32(x) >> loadInt32(y)
  return newInt32(result)
}

export function shrU(x: u32, y: u32): u32 {
  const result = loadInt32(x) >>> loadInt32(y)
  return newInt32(result)
}

