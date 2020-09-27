import { GRAIN_GENERIC_HEAP_TAG_TYPE } from './ascutils/tags'
import { newFloat64 } from "./ascutils/dataStructures";
import { GRAIN_FALSE, GRAIN_TRUE } from "./ascutils/primitives";

// @ts-ignore: decorator
@inline
function loadF64(xptr: u32): f64 {
  return load<f64>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4)
}

export function add(x: u32, y: u32): u32 {
  const result = loadF64(x) + loadF64(y)
  return newFloat64(result)
}

export function sub(x: u32, y: u32): u32 {
  const result = loadF64(x) - loadF64(y)
  return newFloat64(result)
}

export function mul(x: u32, y: u32): u32 {
  const result = loadF64(x) * loadF64(y)
  return newFloat64(result)
}

export function div(x: u32, y: u32): u32 {
  const result = loadF64(x) / loadF64(y)
  return newFloat64(result)
}

export function lt(x: u32, y: u32): u32 {
  const result = loadF64(x) < loadF64(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function gt(x: u32, y: u32): u32 {
  const result = loadF64(x) > loadF64(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function lte(x: u32, y: u32): u32 {
  const result = loadF64(x) <= loadF64(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function gte(x: u32, y: u32): u32 {
  const result = loadF64(x) >= loadF64(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}
