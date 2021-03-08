import {
  GRAIN_NUMBER_TAG_TYPE,
  GRAIN_GENERIC_HEAP_TAG_TYPE,

  GRAIN_NUMBER_TAG_MASK,
  GRAIN_GENERIC_TAG_MASK,

  GRAIN_BOXED_NUM_HEAP_TAG,
  GRAIN_FLOAT32_BOXED_NUM_TAG,
  GRAIN_FLOAT64_BOXED_NUM_TAG,
  GRAIN_INT32_BOXED_NUM_TAG,
  GRAIN_INT64_BOXED_NUM_TAG,
  GRAIN_RATIONAL_BOXED_NUM_TAG,
} from './ascutils/tags'

// Accessor functions

/* Memory Layout:
 * [GRAIN_BOXED_NUM_HEAP_TAG : u32, <boxed_num tag> : u32, <number-specific payload>...]
 * (payload depends on boxed_num tag...see below)
 *
 * Payloads:
 * For Int32:
 * [number: i32]
 *
 * For Int64:
 * [number: i64]
 *
 * For Float32:
 * [number: f32]
 *
 * For Float64:
 * [number: f64]
 *
 * For Rational:
 * [numerator: i32, denominator: u32]
 */

// @ts-ignore: decorator
@inline
export function boxedNumberTag(xptr: u32): u32 {
  return load<u32>(xptr, 1 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedInt32Number(xptr: u32): i32 {
  return load<i32>(xptr, 2 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedInt64Number(xptr: u32): i64 {
  return load<i64>(xptr, 2 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedFloat32Number(xptr: u32): f32 {
  return load<f32>(xptr, 2 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedFloat64Number(xptr: u32): f64 {
  return load<f64>(xptr, 2 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedRationalNumerator(xptr: u32): i32 {
  return load<i32>(xptr, 2 * 4)
}

// @ts-ignore: decorator
@inline
export function boxedRationalDenominator(xptr: u32): u32 {
  return load<u32>(xptr, 3 * 4)
}

// @ts-ignore: decorator
@inline
function untagSimple(x: u32): i32 {
  return <i32>(x) >> 1
}

// @ts-ignore: decorator
@inline
function isSimpleNumber(x: u32): bool {
  return (x & GRAIN_NUMBER_TAG_MASK) == GRAIN_NUMBER_TAG_TYPE
}

// @ts-ignore: decorator
@inline
function isBoxedNumber(x: u32): bool {
  return ((x & GRAIN_GENERIC_TAG_MASK) == GRAIN_GENERIC_HEAP_TAG_TYPE) &&
  (load<u32>(x) == GRAIN_BOXED_NUM_HEAP_TAG)
}

// @ts-ignore: decorator
@inline
export function isNumber(x: u32): bool {
  // x is a number if it is a literal number or a boxed_num heap value
  return isSimpleNumber(x) || isBoxedNumber(x)
}

/** Number-aware equality checking
  * The basic idea is that we first figure out the type of the
  * number on the LHS, and then figure out if the RHS number is equal
  * to that number
  *
  * NOTE: The preconditions in these functions are important, so do NOT
  *       export them!
  */

function numberEqualSimpleHelp(x: u32, y: u32): bool {
  // PRECONDITION: x is a "simple" number (value tag is 0) and x !== y and isNumber(y)
  if (isSimpleNumber(y)) {
    // x !== y, so they must be different
    return false
  }
  let xval = untagSimple(x) // <- actual int value of x
  let yBoxedNumberTag = boxedNumberTag(y)
  switch (yBoxedNumberTag) {
    case GRAIN_INT32_BOXED_NUM_TAG: {
      let yBoxedVal = boxedInt32Number(y)
      return <i32>(xval) == yBoxedVal
    } case GRAIN_INT64_BOXED_NUM_TAG: {
      let yBoxedVal = boxedInt64Number(y)
      return <i64>(xval) == yBoxedVal
    } case GRAIN_RATIONAL_BOXED_NUM_TAG: {
      // NOTE: we always store in most reduced form, so a rational and an int are never equal
      return false
    } case GRAIN_FLOAT32_BOXED_NUM_TAG: {
      let yBoxedVal = boxedFloat32Number(y)
      return F32.isSafeInteger(yBoxedVal) && <i64>(xval) == <i64>(yBoxedVal)
    } case GRAIN_FLOAT64_BOXED_NUM_TAG: {
      let yBoxedVal = boxedFloat64Number(y)
      return F64.isSafeInteger(yBoxedVal) && <i64>(xval) == <i64>(yBoxedVal)
    } default: {
      return unreachable()
    }
  }
}


function numberEqualInt64Help(xBoxedVal: i64, y: u32): bool {
  // PRECONDITION: x !== y and isNumber(y)
  // Basic number:
  if ((y & GRAIN_NUMBER_TAG_MASK) == GRAIN_NUMBER_TAG_TYPE) {
    return xBoxedVal == <i64>(untagSimple(y))
  }
  // Boxed number:
  let yBoxedNumberTag = boxedNumberTag(y)
  switch (yBoxedNumberTag) {
    case GRAIN_INT32_BOXED_NUM_TAG: {
      let yBoxedVal = boxedInt32Number(y)
      return xBoxedVal == <i64>(yBoxedVal)
    } case GRAIN_INT64_BOXED_NUM_TAG: {
      let yBoxedVal = boxedInt64Number(y)
      return xBoxedVal == yBoxedVal
    } case GRAIN_RATIONAL_BOXED_NUM_TAG: {
      // NOTE: we always store in most reduced form, so a rational and an int are never equal
      return false
    } case GRAIN_FLOAT32_BOXED_NUM_TAG: {
      let yBoxedVal = boxedFloat32Number(y)
      return F32.isSafeInteger(yBoxedVal) && xBoxedVal == <i64>(yBoxedVal)
    } case GRAIN_FLOAT64_BOXED_NUM_TAG: {
      let yBoxedVal = boxedFloat64Number(y)
      return F64.isSafeInteger(yBoxedVal) && xBoxedVal == <i64>(yBoxedVal)
    } default: {
      return unreachable()
    }
  }
}

function numberEqualInt32Help(xBoxedVal: i32, y: u32): bool {
  // We can just pretend it's 64-bit for the equality check
  return numberEqualInt64Help(<i64>(xBoxedVal), y)
}

function numberEqualRationalHelp(xptr: u32, y: u32): bool {
  // PRECONDITION: x is rational and x !== y and isNumber(y)
  // Basic number: (we know it's not equal, since we never store ints as rationals)
  if ((y & GRAIN_NUMBER_TAG_MASK) == GRAIN_NUMBER_TAG_TYPE) {
    return false
  }
  let xNumerator = boxedRationalNumerator(xptr)
  let xDenominator = boxedRationalDenominator(xptr)
  // Boxed number:
  let yBoxedNumberTag = boxedNumberTag(y)
  switch (yBoxedNumberTag) {
    case GRAIN_INT32_BOXED_NUM_TAG: {
      return false
    } case GRAIN_INT64_BOXED_NUM_TAG: {
      return false
    } case GRAIN_RATIONAL_BOXED_NUM_TAG: {
      let yNumerator = boxedRationalNumerator(y)
      let yDenominator = boxedRationalDenominator(y)
      return (xNumerator == yNumerator) && (xDenominator == yDenominator)
    } case GRAIN_FLOAT32_BOXED_NUM_TAG: {
      let yBoxedVal = boxedFloat32Number(y)
      let xAsFloat = <f64>(xNumerator) / <f64>(xDenominator)
      // [TODO] (#303) maybe we should have some sort of tolerance?
      return xAsFloat == yBoxedVal
    } case GRAIN_FLOAT64_BOXED_NUM_TAG: {
      let yBoxedVal = boxedFloat64Number(y)
      let xAsFloat = <f64>(xNumerator) / <f64>(xDenominator)
      // [TODO] (#303) maybe we should have some sort of tolerance?
      return xAsFloat == yBoxedVal
    } default: {
      return unreachable()
    }
  }
}

function numberEqualFloat64Help(x: f64, y: u32): bool {
  let xIsInteger = F64.isInteger(x)
  // Basic number:
  if ((y & GRAIN_NUMBER_TAG_MASK) == GRAIN_NUMBER_TAG_TYPE) {
    return xIsInteger && x == <f64>(untagSimple(y))
  }
  // Boxed number
  let yBoxedNumberTag = boxedNumberTag(y)
  switch (yBoxedNumberTag) {
    case GRAIN_INT32_BOXED_NUM_TAG: {
      let yBoxedVal = boxedInt32Number(y)
      return F64.isSafeInteger(x) && <i64>(x) == <i64>(yBoxedVal)
    } case GRAIN_INT64_BOXED_NUM_TAG: {
      let yBoxedVal = boxedInt64Number(y)
      return F64.isSafeInteger(x) && <i64>(x) == yBoxedVal
    } case GRAIN_RATIONAL_BOXED_NUM_TAG: {
      let yNumerator = boxedRationalNumerator(y)
      let yDenominator = boxedRationalDenominator(y)
      let yAsFloat = <f64>(yNumerator) / <f64>(yDenominator)
      return x == yAsFloat
    } case GRAIN_FLOAT32_BOXED_NUM_TAG: {
      let yBoxedVal = boxedFloat32Number(y)
      // [TODO] (#303) maybe we should have some sort of tolerance?
      return x == <f64>(yBoxedVal)
    } case GRAIN_FLOAT64_BOXED_NUM_TAG: {
      let yBoxedVal = boxedFloat64Number(y)
      // [TODO] (#303) maybe we should have some sort of tolerance?
      return x == yBoxedVal
    } default: {
      return unreachable()
    }
  }
}

function numberEqualFloat32Help(x: f32, y: u32): bool {
  return numberEqualFloat64Help(<f64>(x), y)
}

export function numberEqual(x: u32, y: u32): bool {
  // Short circuit if value/pointer is the same
  if (x === y) return true
  if (!isNumber(x)) return false
  if (!isNumber(y)) return false
  if (isSimpleNumber(x)) {
    return numberEqualSimpleHelp(x, y)
  }
  // Boxed number
  let xBoxedNumberTag = boxedNumberTag(x)
  switch (xBoxedNumberTag) {
    case GRAIN_INT32_BOXED_NUM_TAG: {
      let xBoxedVal = boxedInt32Number(x)
      return numberEqualInt32Help(xBoxedVal, y)
    } case GRAIN_INT64_BOXED_NUM_TAG: {
      let xBoxedVal = boxedInt64Number(x)
      return numberEqualInt64Help(xBoxedVal, y)
    } case GRAIN_RATIONAL_BOXED_NUM_TAG: {
      return numberEqualRationalHelp(x, y)
    } case GRAIN_FLOAT32_BOXED_NUM_TAG: {
      return numberEqualFloat32Help(boxedFloat32Number(x), y)
    } case GRAIN_FLOAT64_BOXED_NUM_TAG: {
      return numberEqualFloat64Help(boxedFloat64Number(x), y)
    } default: {
      return unreachable()
    }
  }
}
