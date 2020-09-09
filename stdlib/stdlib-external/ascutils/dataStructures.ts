import { malloc } from './grainRuntime'

import {
  GRAIN_ARRAY_HEAP_TAG,
  GRAIN_STRING_HEAP_TAG,
  GRAIN_BOXED_NUM_HEAP_TAG,
  GRAIN_INT32_BOXED_NUM_TAG,
  GRAIN_INT64_BOXED_NUM_TAG,
  GRAIN_FLOAT32_BOXED_NUM_TAG,
  GRAIN_FLOAT64_BOXED_NUM_TAG,
  GRAIN_RATIONAL_BOXED_NUM_TAG,
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

// [TODO] should probably migrate over the accessors in numbers.ts
// INT32/INT64

/**
 * Allocates a new Int64.
 *
 * @returns {u32}
 */
export function allocateInt64(): u32 {
  let ptr = malloc(16)

  store<u32>(ptr, GRAIN_BOXED_NUM_HEAP_TAG)
  store<u32>(ptr + 4, GRAIN_INT64_BOXED_NUM_TAG)

  return ptr
}

/**
 * Allocates a new Int64 with a prepopulated value
 * @param value The value to store
 */
export function newInt64(value: i64): u32 {
  let ptr = allocateInt64()
  store<i64>(ptr + 8, value)
  return ptr
}

/**
 * Returns a pointer to the heap location containing this boxed number's Int64
 * @param wrappedInt64 The boxed int64 to return
 */
export function rawInt64Ptr(wrappedInt64: u32): u32 {
  return wrappedInt64 + 8
}

/**
 * Allocates a new Int32.
 *
 * @returns {u32}
 */
export function allocateInt32(): u32 {
  let ptr = malloc(12)

  store<u32>(ptr, GRAIN_BOXED_NUM_HEAP_TAG)
  store<u32>(ptr + 4, GRAIN_INT32_BOXED_NUM_TAG)

  return ptr
}

/**
 * Allocates a new Int32 with a prepopulated value
 * @param value The value to store
 */
export function newInt32(value: i32): u32 {
  let ptr = allocateInt32()
  store<i32>(ptr + 8, value)
  return ptr
}

/**
 * Returns a pointer to the heap location containing this boxed number's Int32
 * @param wrappedInt32 The boxed int32 to return
 */
export function rawInt32Ptr(wrappedInt32: u32): u32 {
  return wrappedInt32 + 8
}

// FLOATS

/**
 * Allocates a new Float32.
 *
 * @returns {u32}
 */
export function allocateFloat32(): u32 {
  let ptr = malloc(12)

  store<u32>(ptr, GRAIN_BOXED_NUM_HEAP_TAG)
  store<u32>(ptr + 4, GRAIN_FLOAT32_BOXED_NUM_TAG)

  return ptr
}

/**
 * Allocates a new Float32 with a prepopulated value
 * @param value The value to store
 */
export function newFloat32(value: f32): u32 {
  let ptr = allocateFloat32()
  store<f32>(ptr + 8, value)
  return ptr
}

/**
 * Returns a pointer to the heap location containing this boxed number's Float32
 * @param wrappedFloat32 The boxed float32 to return
 */
export function rawFloat32Ptr(wrappedFloat32: u32): u32 {
  return wrappedFloat32 + 8
}

/**
 * Allocates a new Float64.
 *
 * @returns {u32}
 */
export function allocateFloat64(): u32 {
  let ptr = malloc(16)

  store<u32>(ptr, GRAIN_BOXED_NUM_HEAP_TAG)
  store<u32>(ptr + 4, GRAIN_FLOAT64_BOXED_NUM_TAG)

  return ptr
}

/**
 * Allocates a new Float64 with a prepopulated value
 * @param value The value to store
 */
export function newFloat64(value: f64): u32 {
  let ptr = allocateFloat64()
  store<f64>(ptr + 8, value)
  return ptr
}

/**
 * Returns a pointer to the heap location containing this boxed number's Float64
 * @param wrappedFloat64 The boxed float64 to return
 */
export function rawFloat64Ptr(wrappedFloat64: u32): u32 {
  return wrappedFloat64 + 8
}

// RATIONALS

/**
 * Allocates a new Float32.
 *
 * @returns {u32}
 */
export function allocateRational(): u32 {
  let ptr = malloc(16)

  store<u32>(ptr, GRAIN_BOXED_NUM_HEAP_TAG)
  store<u32>(ptr + 4, GRAIN_RATIONAL_BOXED_NUM_TAG)

  return ptr
}

/**
 * Allocates a new Float32 with a prepopulated value
 * @param value The value to store
 */
export function newRational(numerator: i32, denominator: i32): u32 {
  let ptr = allocateRational()
  store<i32>(ptr + 8, numerator)
  store<i32>(ptr + 12, denominator)
  return ptr
}

/**
 * Returns a pointer to the heap location containing this boxed number's Rational numerator
 * @param wrappedRational The boxed rational to return
 */
export function rawRationalNumeratorPtr(wrappedRational: u32): u32 {
  return wrappedRational + 8
}

/**
 * Returns a pointer to the heap location containing this boxed number's Rational numerator
 * @param wrappedRational The boxed rational to return
 */
export function rawRationalDenominatorPtr(wrappedRational: u32): u32 {
  return wrappedRational + 12
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
