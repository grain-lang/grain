import { GRAIN_GENERIC_HEAP_TAG_TYPE } from './ascutils/tags'
import { newFloat32, loadFloat32 } from "./ascutils/dataStructures";
import { GRAIN_FALSE, GRAIN_TRUE } from "./ascutils/primitives";

export function add(x: u32, y: u32): u32 {
  const result = loadFloat32(x) + loadFloat32(y)
  return newFloat32(result)
}

export function sub(x: u32, y: u32): u32 {
  const result = loadFloat32(x) - loadFloat32(y)
  return newFloat32(result)
}

export function mul(x: u32, y: u32): u32 {
  const result = loadFloat32(x) * loadFloat32(y)
  return newFloat32(result)
}

export function div(x: u32, y: u32): u32 {
  const result = loadFloat32(x) / loadFloat32(y)
  return newFloat32(result)
}

export function lt(x: u32, y: u32): u32 {
  const result = loadFloat32(x) < loadFloat32(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function gt(x: u32, y: u32): u32 {
  const result = loadFloat32(x) > loadFloat32(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function lte(x: u32, y: u32): u32 {
  const result = loadFloat32(x) <= loadFloat32(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}

export function gte(x: u32, y: u32): u32 {
  const result = loadFloat32(x) >= loadFloat32(y)
  return result ? GRAIN_TRUE : GRAIN_FALSE
}
