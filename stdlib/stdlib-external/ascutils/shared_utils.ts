import { 
  GRAIN_NUMBER_TAG_MASK, 
  GRAIN_NUMBER_TAG_TYPE,
  GRAIN_GENERIC_HEAP_TAG_TYPE,

  GRAIN_INT32_BOXED_NUM_TAG,
  GRAIN_INT64_BOXED_NUM_TAG,
  GRAIN_RATIONAL_BOXED_NUM_TAG,
  GRAIN_FLOAT32_BOXED_NUM_TAG,
  GRAIN_FLOAT64_BOXED_NUM_TAG
} from './tags';

import {
  newInt32,
  newInt64
} from './dataStructures'

export {
  coerceFloat64,
  reducedInteger
}

// @ts-ignore: decorator
@inline
export function untagSimple(x: u32): i32 {
  return <i32>(x) >> 1
}

// @ts-ignore: decorator
@inline
export function tagSimple(x: i32): u32 {
  return x << 1
}

// @ts-ignore: decorator
@inline
export function isSimpleNumber(x: u32): bool {
  return (x & GRAIN_NUMBER_TAG_MASK) == GRAIN_NUMBER_TAG_TYPE
}

// @ts-ignore: decorator
@inline
export function boxedNumberTag(xptr: u32): u32 {
  return load<u32>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 1 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedInt32Number(xptr: u32): i32 {
  return load<i32>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedInt64Number(xptr: u32): i64 {
  return load<i64>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedRationalNumerator(xptr: u32): i32 {
  return load<i32>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedRationalDenominator(xptr: u32): u32 {
  return load<u32>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 3 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedFloat32Number(xptr: u32): f32 {
  return load<f32>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedFloat64Number(xptr: u32): f64 {
  return load<f64>(xptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4)
}

function coerceFloat64(x: u32): f64 {
  if (isSimpleNumber(x)) {
    return <f64>(untagSimple(x))
  }
  let xtag = boxedNumberTag(x)
  switch (xtag) {
    case GRAIN_INT32_BOXED_NUM_TAG: {
      return <f64>(boxedInt32Number(x))
    } case GRAIN_INT64_BOXED_NUM_TAG: {
      return <f64>(boxedInt64Number(x))
    } case GRAIN_RATIONAL_BOXED_NUM_TAG: {
      return <f64>(boxedRationalNumerator(x)) / <f64>(boxedRationalDenominator(x))
    } case GRAIN_FLOAT32_BOXED_NUM_TAG: {
      return <f64>(boxedFloat32Number(x))
    } case GRAIN_FLOAT64_BOXED_NUM_TAG: {
      return boxedFloat64Number(x)
    } default: {
      return unreachable()
    }
  }
}

// @ts-ignore: decorator
@inline
export function reducedInteger(x: i64): u32 {
  if ((x > I32.MAX_VALUE) || (x < I32.MIN_VALUE)) {
    return newInt64(x)
  } else if ((x > (I32.MAX_VALUE >> 1)) || (x < (I32.MIN_VALUE >> 1))) {
    return newInt32(<i32>(x))
  } else {
    return tagSimple(<i32>(x))
  }
}