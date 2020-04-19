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
 * Allocates a new Grain tuple.
 *
 * @param {u32} numElts The number of elements to be contained in this tuple
 * @returns {u32} The (untagged) pointer to the tuple
 */
export function allocateTuple(numElts: u32): u32 {
  let tuple = malloc((numElts + 1) * 4)

  store<u32>(tuple, numElts)

  return tuple
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

/**
 * Load a value from an ADT.
 *
 * @export
 * @param {u32} ptr Untagged pointer to the ADT
 * @param {u32} idx Index (from zero) of the item
 * @returns {u32} The value located at the index
 */
// @ts-ignore: decorator
@inline
export function loadAdtVal(ptr: u32, idx: u32): u32 {
  return load<u32>(ptr + (idx * 4), 5 * 4)
}

/**
 * Load the (tagged) variant of an ADT.
 *
 * @export
 * @param {u32} ptr Untagged pointer to the ADT
 * @returns {u32} The (tagged) ADT variant id
 */
// @ts-ignore: decorator
@inline
export function loadAdtVariant(ptr: u32): u32 {
  return load<u32>(ptr, 3 * 4)
}

/**
 * Load an untagged string's size.
 *
 * @export
 * @param {u32} ptr Untagged pointer to the string
 * @returns {u32} The (untagged) string size (in bytes)
 */
// @ts-ignore: decorator
@inline
export function stringSize(ptr: u32): u32 {
  return load<u32>(ptr, 4)
}
