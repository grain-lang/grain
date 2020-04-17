import { malloc } from './grainRuntime'

import {
  GRAIN_ARRAY_HEAP_TAG,
  GRAIN_STRING_HEAP_TAG,
  GRAIN_INT64_HEAP_TAG
} from './tags'

/**
 * Allocates a new Grain array.
 *
 * @param {u32} numElts The number of elements to be contained in this array
 * @returns {u32} The (untagged) pointer to the array
 */
export function allocateArray(numElts: u32): u32 {
  let arr = malloc((numElts + 2) * 4)

  store<u32>(arr, GRAIN_ARRAY_HEAP_TAG)
  store<u32>(arr, numElts, 4)

  return arr
}

/**
 * Allocates a new Grain string.
 *
 * @param {u32} size The size (in bytes) of the string to allocate
 * @returns {u32} The (untagged) pointer to the string
 */
export function allocateString(size: u32): u32 {
  let str = malloc(size + 8)

  store<u32>(str, GRAIN_STRING_HEAP_TAG)
  store<u32>(str, size, 4)

  return str
}

/**
 * Allocates a new Int64.
 *
 * @returns {u32}
 */
export function allocateInt64(): u32 {
  let ptr = malloc(12)

  store<u32>(ptr, GRAIN_INT64_HEAP_TAG)

  return ptr
}
