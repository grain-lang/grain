import { GRAIN_ADT_HEAP_TAG, GRAIN_ARRAY_HEAP_TAG, GRAIN_RECORD_HEAP_TAG, GRAIN_CHAR_HEAP_TAG, GRAIN_STRING_HEAP_TAG, GRAIN_BOXED_NUM_HEAP_TAG, GRAIN_LAMBDA_HEAP_TAG, GRAIN_TUPLE_HEAP_TAG, GRAIN_INT32_BOXED_NUM_TAG, GRAIN_INT64_BOXED_NUM_TAG, GRAIN_RATIONAL_BOXED_NUM_TAG, GRAIN_FLOAT32_BOXED_NUM_TAG, GRAIN_FLOAT64_BOXED_NUM_TAG, GRAIN_GENERIC_HEAP_TAG_TYPE } from '../ascutils/tags'
import { stringSize, allocateString, loadInt32, loadInt64, loadFloat32, loadFloat64, loadRationalNumerator, loadRationalDenominator, singleByteString, twoByteString } from '../ascutils/dataStructures'
import { GRAIN_FALSE, GRAIN_TRUE, GRAIN_VOID } from '../ascutils/primitives'
import { incRef, decRef } from '../ascutils/grainRuntime'
import { dtoa, itoa32, itoa64 } from '../ascutils/numberUtils'
import { CharCode } from '../ascutils/charCodes'
import { equal } from '../equal'

// Introspection helpers for to-string (will eventually be part of the AS runtime)
// @ts-ignore: decorator
@external("grainRuntime", "variantExists")
declare function variantExists(moduleId: u32, typeId: u32, variantId: u32): bool

// @ts-ignore: decorator
@external("grainRuntime", "getVariantName")
declare function getVariantName(moduleId: u32, typeId: u32, variantId: u32): u32

// @ts-ignore: decorator
@external("grainRuntime", "getVariantArity")
declare function getVariantArity(moduleId: u32, typeId: u32, variantId: u32): i32

// @ts-ignore: decorator
@external("grainRuntime", "recordTypeExists")
declare function recordTypeExists(moduleId: u32, typeId: u32): bool

// @ts-ignore: decorator
@external("grainRuntime", "getRecordArity")
declare function getRecordArity(moduleId: u32, typeId: u32): i32

// @ts-ignore: decorator
@external("grainRuntime", "getRecordFieldName")
declare function getRecordFieldName(moduleId: u32, typeId: u32, idx: u32): u32


export function concat(s1: u32, s2: u32): u32 {
  const size1 = stringSize(s1)
  const size2 = stringSize(s2)

  const newString = allocateString(size1 + size2)

  memory.copy(newString + 8, s1 + 8, size1)
  memory.copy(newString + 8 + size1, s2 + 8, size2)

  return newString
}

// @ts-ignore: decorator
@inline
function leftLiteralConcat1(c: u8, gs: u32): u32 {
  const gsSize = stringSize(gs)
  let newString = allocateString(gsSize + 1)
  memory.copy(newString + 9, gs + 8, gsSize)
  store<u8>(newString, c, 8)
  return newString
}

// @ts-ignore: decorator
@inline
function leftLiteralConcat2(c1: u8, c2: u8, gs: u32): u32 {
  const gsSize = stringSize(gs)
  let newString = allocateString(gsSize + 2)
  memory.copy(newString + 10, gs + 8, gsSize)
  store<u8>(newString, c1, 8)
  store<u8>(newString, c2, 9)
  return newString
}


// @ts-ignore: decorator
@inline
function rightLiteralConcat1(gs: u32, c: u8): u32 {
  const gsSize = stringSize(gs)
  let newString = allocateString(gsSize + 1)
  memory.copy(newString + 8, gs + 8, gsSize)
  store<u8>(newString + 8 + gsSize, c)
  return newString
}

// @ts-ignore: decorator
@inline
function rightLiteralConcat2(gs: u32, c1: u8, c2: u8): u32 {
  const gsSize = stringSize(gs)
  let newString = allocateString(gsSize + 2)
  memory.copy(newString + 8, gs + 8, gsSize)
  store<u8>(newString + 8 + gsSize, c1)
  store<u8>(newString + 8 + gsSize, c2, 1)
  return newString
}

function grainListToString(ptr: u32, extraIndents: u32): u32 {
  let cur = ptr
  let ret = singleByteString(CharCode.LBRACK)
  let isFirst = true

  while (true) {
    let variantId = load<i32>(cur, 4 * 3) >> 1 // tagged number
    if (variantId === 0) {
      break;
    } else {
      if (!isFirst) {
        let oldRet = ret
        ret = rightLiteralConcat2(ret, CharCode.COMMA, CharCode.SPACE)
        decRef(oldRet)
      }
      isFirst = false
      let itemString = grainToStringHelp(load<u32>(cur, 4 * 5), extraIndents, false)
      let oldRet = ret
      ret = concat(ret, itemString)
      decRef(oldRet)
      cur = load<u32>(cur, 4 * 6)
    }
  }
  let oldRet = ret
  ret = rightLiteralConcat1(ret, CharCode.RBRACK)
  decRef(oldRet)
  return ret
}


function quoteString(ptr: u32): u32 {
  let length = stringSize(ptr)
  let ret = allocateString(length + 2)
  store<u8>(ret + 8, CharCode.QUOTE)
  memory.copy(ret + 9, ptr + 8, length)
  store<u8>(ret + 9 + length, CharCode.QUOTE)
  return ret
}

// For performance, we intern the constants produced by grainHeapValueToString. This is handled here.
let ADT_VALUE_STRING: u32 = -1
let LIST_VARIANT_STRING: u32 = -1
let RECORD_VALUE_STRING: u32 = -1
let CYCLIC_TUPLE_STRING: u32 = -1
let LAMBDA_STRING: u32 = -1
let TRUE_STRING: u32 = -1
let FALSE_STRING: u32 = -1
let VOID_STRING: u32 = -1
let UNKNOWN_VALUE_STRING: u32 = -1

function getAdtValueString(): u32 {
  if (ADT_VALUE_STRING == <u32>(-1)) {
    let newString = allocateString(11)
    store<u8>(newString, CharCode.LANGLE, 8)
    store<u8>(newString, CharCode.a, 8 + 1)
    store<u8>(newString, CharCode.d, 8 + 2)
    store<u8>(newString, CharCode.t, 8 + 3)
    store<u8>(newString, CharCode.SPACE, 8 + 4)
    store<u8>(newString, CharCode.v, 8 + 5)
    store<u8>(newString, CharCode.a, 8 + 6)
    store<u8>(newString, CharCode.l, 8 + 7)
    store<u8>(newString, CharCode.u, 8 + 8)
    store<u8>(newString, CharCode.e, 8 + 9)
    store<u8>(newString, CharCode.RANGLE, 8 + 10)
    ADT_VALUE_STRING = newString
    incRef(ADT_VALUE_STRING) // <- avoid value getting GC'd
  }
  return ADT_VALUE_STRING
}


function getListVariantString(): u32 {
  if (LIST_VARIANT_STRING == <u32>(-1)) {
    let newString = allocateString(5)
    store<u8>(newString, CharCode.LBRACK, 8)
    store<u8>(newString, CharCode.DOT, 8 + 1)
    store<u8>(newString, CharCode.DOT, 8 + 2)
    store<u8>(newString, CharCode.DOT, 8 + 3)
    store<u8>(newString, CharCode.RBRACK, 8 + 4)
    LIST_VARIANT_STRING = newString
    incRef(LIST_VARIANT_STRING) // <- avoid value getting GC'd
  }
  return LIST_VARIANT_STRING
}


function getRecordValueString(): u32 {
  if (RECORD_VALUE_STRING == <u32>(-1)) {
    let newString = allocateString(14)
    store<u8>(newString, CharCode.LANGLE, 8)
    store<u8>(newString, CharCode.r, 8 + 1)
    store<u8>(newString, CharCode.e, 8 + 2)
    store<u8>(newString, CharCode.c, 8 + 3)
    store<u8>(newString, CharCode.o, 8 + 4)
    store<u8>(newString, CharCode.r, 8 + 5)
    store<u8>(newString, CharCode.d, 8 + 6)
    store<u8>(newString, CharCode.SPACE, 8 + 7)
    store<u8>(newString, CharCode.v, 8 + 8)
    store<u8>(newString, CharCode.a, 8 + 9)
    store<u8>(newString, CharCode.l, 8 + 10)
    store<u8>(newString, CharCode.u, 8 + 11)
    store<u8>(newString, CharCode.e, 8 + 12)
    store<u8>(newString, CharCode.RANGLE, 8 + 13)
    RECORD_VALUE_STRING = newString
    incRef(RECORD_VALUE_STRING) // <- avoid value getting GC'd
  }
  return RECORD_VALUE_STRING
}


function getCyclicTupleString(): u32 {
  if (CYCLIC_TUPLE_STRING == <u32>(-1)) {
    let newString = allocateString(14)
    store<u8>(newString, CharCode.LANGLE, 8)
    store<u8>(newString, CharCode.c, 8 + 1)
    store<u8>(newString, CharCode.y, 8 + 2)
    store<u8>(newString, CharCode.c, 8 + 3)
    store<u8>(newString, CharCode.l, 8 + 4)
    store<u8>(newString, CharCode.i, 8 + 5)
    store<u8>(newString, CharCode.c, 8 + 6)
    store<u8>(newString, CharCode.SPACE, 8 + 7)
    store<u8>(newString, CharCode.t, 8 + 8)
    store<u8>(newString, CharCode.u, 8 + 9)
    store<u8>(newString, CharCode.p, 8 + 10)
    store<u8>(newString, CharCode.l, 8 + 11)
    store<u8>(newString, CharCode.e, 8 + 12)
    store<u8>(newString, CharCode.RANGLE, 8 + 13)
    CYCLIC_TUPLE_STRING = newString
    incRef(CYCLIC_TUPLE_STRING) // <- avoid value getting GC'd
  }
  return CYCLIC_TUPLE_STRING
}


function getLambdaString(): u32 {
  if (LAMBDA_STRING == <u32>(-1)) {
    let newString = allocateString(8)
    store<u8>(newString, CharCode.LANGLE, 8)
    store<u8>(newString, CharCode.l, 8 + 1)
    store<u8>(newString, CharCode.a, 8 + 2)
    store<u8>(newString, CharCode.m, 8 + 3)
    store<u8>(newString, CharCode.b, 8 + 4)
    store<u8>(newString, CharCode.d, 8 + 5)
    store<u8>(newString, CharCode.a, 8 + 6)
    store<u8>(newString, CharCode.RANGLE, 8 + 7)
    LAMBDA_STRING = newString
    incRef(LAMBDA_STRING) // <- avoid value getting GC'd
  }
  return LAMBDA_STRING
}


function getTrueString(): u32 {
  if (TRUE_STRING == <u32>(-1)) {
    let newString = allocateString(4)
    store<u8>(newString, CharCode.t, 8)
    store<u8>(newString, CharCode.r, 8 + 1)
    store<u8>(newString, CharCode.u, 8 + 2)
    store<u8>(newString, CharCode.e, 8 + 3)
    TRUE_STRING = newString
    incRef(TRUE_STRING) // <- avoid value getting GC'd
  }
  return TRUE_STRING
}


function getFalseString(): u32 {
  if (FALSE_STRING == <u32>(-1)) {
    let newString = allocateString(5)
    store<u8>(newString, CharCode.f, 8)
    store<u8>(newString, CharCode.a, 8 + 1)
    store<u8>(newString, CharCode.l, 8 + 2)
    store<u8>(newString, CharCode.s, 8 + 3)
    store<u8>(newString, CharCode.e, 8 + 4)
    FALSE_STRING = newString
    incRef(FALSE_STRING) // <- avoid value getting GC'd
  }
  return FALSE_STRING
}


function getVoidString(): u32 {
  if (VOID_STRING == <u32>(-1)) {
    let newString = allocateString(4)
    store<u8>(newString, CharCode.v, 8)
    store<u8>(newString, CharCode.o, 8 + 1)
    store<u8>(newString, CharCode.i, 8 + 2)
    store<u8>(newString, CharCode.d, 8 + 3)
    VOID_STRING = newString
    incRef(VOID_STRING) // <- avoid value getting GC'd
  }
  return VOID_STRING
}


function getUnknownValueString(): u32 {
  if (UNKNOWN_VALUE_STRING == <u32>(-1)) {
    let newString = allocateString(15)
    store<u8>(newString, CharCode.LANGLE, 8)
    store<u8>(newString, CharCode.U, 8 + 1)
    store<u8>(newString, CharCode.n, 8 + 2)
    store<u8>(newString, CharCode.k, 8 + 3)
    store<u8>(newString, CharCode.n, 8 + 4)
    store<u8>(newString, CharCode.o, 8 + 5)
    store<u8>(newString, CharCode.w, 8 + 6)
    store<u8>(newString, CharCode.n, 8 + 7)
    store<u8>(newString, CharCode.SPACE, 8 + 8)
    store<u8>(newString, CharCode.v, 8 + 9)
    store<u8>(newString, CharCode.a, 8 + 10)
    store<u8>(newString, CharCode.l, 8 + 11)
    store<u8>(newString, CharCode.u, 8 + 12)
    store<u8>(newString, CharCode.e, 8 + 13)
    store<u8>(newString, CharCode.RANGLE, 8 + 14)
    UNKNOWN_VALUE_STRING = newString
    incRef(UNKNOWN_VALUE_STRING) // <- avoid value getting GC'd
  }
  return UNKNOWN_VALUE_STRING
}


function grainHeapValueToString(ptr: u32, extraIndents: u32, toplevel: bool): u32 {
  // ptr can be tagged or untagged
  const tag = load<u32>(ptr)
  switch (tag) {
    case GRAIN_STRING_HEAP_TAG: {
      if (toplevel) {
        return ptr
      }
      return quoteString(ptr)
    }
    case GRAIN_CHAR_HEAP_TAG: {
      let byte = load<u8>(ptr + 4)
      let numBytes: u32 = 0
      if ((byte & 0x80) === 0x00) {
        numBytes = 1
      } else if ((byte & 0xF0) === 0xF0) {
        numBytes = 4
      } else if ((byte & 0xE0) === 0xE0) {
        numBytes = 3
      } else {
        numBytes = 2
      }
      let str: u32
      if (toplevel) {
        str = allocateString(numBytes)
        memory.copy(str + 8, ptr + 4, numBytes)
      } else {
        str = allocateString(numBytes + 2)
        store<u8>(str + 8, <u8>(0x27))
        memory.copy(str + 9, ptr + 4, numBytes)
        store<u8>(str + 9 + numBytes, <u8>(0x27))
      }
      return str
    }
    case GRAIN_ADT_HEAP_TAG: {
      // [ <value type tag>, <module_tag>, <type_tag>, <variant_tag>, <arity>, elts ... ]
      // these are tagged ints
      let moduleId = load<i32>(ptr, 4 * 1) >> 1
      let typeId = load<i32>(ptr, 4 * 2) >> 1
      let variantId = load<i32>(ptr, 4 * 3) >> 1
      // probably a linking issue!
      if (!variantExists(moduleId, typeId, variantId)) return getAdtValueString()
      let variantName: u32 = getVariantName(moduleId, typeId, variantId)
      // Check if this is a list
      // (hack to get list printing correct)
      let listVariant = getListVariantString()
      let isList = equal(variantName, listVariant) == GRAIN_TRUE
      if (isList) return grainListToString(ptr, extraIndents)
      let variantArity = getVariantArity(moduleId, typeId, variantId)
      if (variantArity == 0) return variantName
      // [NOTE] do not decRef variantName!
      let ret = rightLiteralConcat1(variantName, CharCode.LPAREN)
      for (let i = 0; i < variantArity; ++i) {
        if (i > 0) {
          let oldRet = ret
          ret = rightLiteralConcat2(ret, CharCode.COMMA, CharCode.SPACE)
          decRef(oldRet)
        }
        let oldRet = ret
        let tmp = grainToStringHelp(load<u32>(ptr + 4 * (5 + i)), extraIndents, false)
        ret = concat(ret, tmp)
        decRef(tmp)
        decRef(oldRet)
      }
      return rightLiteralConcat1(ret, CharCode.RPAREN)
    }
    case GRAIN_RECORD_HEAP_TAG: {
      // these are tagged ints
      let moduleId = load<i32>(ptr, 4 * 1) >> 1
      let typeId = load<i32>(ptr, 4 * 2) >> 1
      // probably a linking issue!
      if (!recordTypeExists(moduleId, typeId)) return getRecordValueString()
      let recordArity = getRecordArity(moduleId, typeId)
      if (recordArity == 0) return getRecordValueString()
      let lastSpacePadding = allocateString(0)
      let spacePadding = twoByteString(CharCode.SPACE, CharCode.SPACE)
      for (let i: u32 = 0; i < extraIndents; ++i) {
        decRef(lastSpacePadding)
        lastSpacePadding = spacePadding
        spacePadding = rightLiteralConcat2(spacePadding, CharCode.SPACE, CharCode.SPACE)
      }
      let ret = leftLiteralConcat2(CharCode.LBRACE, CharCode.NEWLINE, spacePadding)
      for (let i = 0; i < recordArity; ++i) {
        if (i > 0) {
          let oldRet = ret
          ret = rightLiteralConcat2(ret, CharCode.COMMA, CharCode.NEWLINE)
          decRef(oldRet)
          oldRet = ret
          ret = concat(ret, spacePadding)
          decRef(oldRet)
        }
        // [NOTE] do not decRef:
        let fieldName = getRecordFieldName(moduleId, typeId, i)
        // [NOTE] *do* decRef
        let fieldValue = grainToStringHelp(load<u32>(ptr + 4 * (4 + i)), extraIndents + 1, false)
        // [TODO] refactor to copy less here
        let oldRet = ret
        ret = concat(ret, fieldName)
        decRef(oldRet)
        oldRet = ret
        ret = rightLiteralConcat2(ret, CharCode.COLON, CharCode.SPACE)
        decRef(oldRet)
        oldRet = ret
        ret = concat(ret, fieldValue)
        decRef(oldRet)
        decRef(fieldValue)
      }
      decRef(spacePadding)
      let oldRet = ret
      ret = rightLiteralConcat1(ret, CharCode.NEWLINE)
      decRef(oldRet)
      oldRet = ret
      ret = concat(ret, lastSpacePadding)
      decRef(oldRet)
      oldRet = ret
      ret = rightLiteralConcat1(ret, CharCode.RBRACE)
      decRef(oldRet)
      decRef(lastSpacePadding)
      return ret
    }
    case GRAIN_ARRAY_HEAP_TAG: {
      let ret = allocateString(3)
      store<u8>(ret, CharCode.LBRACK, 8)
      store<u8>(ret, CharCode.RANGLE, 8 + 1)
      store<u8>(ret, CharCode.SPACE, 8 + 2)
      ret = ret
      let arity = load<i32>(ptr, 4 * 1)
      for (let i = 0; i < arity; ++i) {
        if (i > 0) {
          let oldRet = ret
          ret = rightLiteralConcat2(ret, CharCode.COMMA, CharCode.SPACE)
          decRef(oldRet)
        }
        let oldRet = ret
        let tmp = grainToStringHelp(load<u32>(ptr + 4 * (2 + i)), extraIndents, false)
        ret = concat(ret, tmp)
        decRef(tmp)
        decRef(oldRet)
      }
      let oldRet = ret
      ret = rightLiteralConcat1(ret, CharCode.RBRACK)
      decRef(oldRet)
      return ret
    }
    case GRAIN_BOXED_NUM_HEAP_TAG: {
      let numberTag = load<u32>(ptr, 4 * 1)
      switch (numberTag) {
        case GRAIN_INT32_BOXED_NUM_TAG: {
          return itoa32(loadInt32(ptr), 10)
        }
        case GRAIN_INT64_BOXED_NUM_TAG: {
          return itoa64(loadInt64(ptr), 10)
        }
        case GRAIN_RATIONAL_BOXED_NUM_TAG: {
          let numerator = loadRationalNumerator(ptr)
          let denominator = loadRationalDenominator(ptr)
          let ret = itoa32(numerator, 10)
          let oldRet = ret
          ret = rightLiteralConcat1(ret, CharCode.SLASH)
          decRef(oldRet)
          oldRet = ret
          let tmp = itoa32(denominator, 10)
          ret = concat(ret, tmp)
          decRef(tmp)
          decRef(oldRet)
          return ret
        }
        case GRAIN_FLOAT32_BOXED_NUM_TAG: {
          return dtoa(loadFloat32(ptr))
        }
        case GRAIN_FLOAT64_BOXED_NUM_TAG: {
          return dtoa(loadFloat64(ptr))
        }
      }
    }
    case GRAIN_TUPLE_HEAP_TAG: {
      let tupleLength = load<u32>(ptr, 4)
      if (tupleLength & 0x80000000) {
        return getCyclicTupleString() // ${grainValue & 0x7FFFFFFF}
      } else {
        store<u32>(ptr, 0x80000000 | tupleLength, 4)
        let ret = singleByteString(CharCode.LPAREN)
        for (let i: u32 = 0; i < tupleLength; ++i) {
          if (i > 0) {
            let oldRet = ret
            ret = rightLiteralConcat2(ret, CharCode.COMMA, CharCode.SPACE)
            decRef(oldRet)
          }
          let oldRet = ret
          let tmp = grainToStringHelp(load<u32>(ptr + (i * 4), 8), extraIndents, false)
          ret = concat(ret, tmp)
          decRef(oldRet)
          decRef(tmp)
        }
        store<u32>(ptr, tupleLength, 4)
        if (tupleLength <= 1) {
          // Special case: unary tuple
          let oldRet = ret
          ret = rightLiteralConcat1(ret, CharCode.COMMA)
          decRef(oldRet)
        }
        let oldRet = ret
        ret = rightLiteralConcat1(ret, CharCode.RPAREN)
        decRef(oldRet)
        return ret
      }
    }
    case GRAIN_LAMBDA_HEAP_TAG: {
      return getLambdaString()
    }
    default: {
      let tmp = itoa32(tag, 16)
      let tmp2Rhs = allocateString(12)
      store<u8>(tmp2Rhs, CharCode.SPACE, 8)
      store<u8>(tmp2Rhs, CharCode.PIPE, 8 + 1)
      store<u8>(tmp2Rhs, CharCode.SPACE, 8 + 2)
      store<u8>(tmp2Rhs, CharCode.v, 8 + 3)
      store<u8>(tmp2Rhs, CharCode.a, 8 + 4)
      store<u8>(tmp2Rhs, CharCode.l, 8 + 5)
      store<u8>(tmp2Rhs, CharCode.u, 8 + 6)
      store<u8>(tmp2Rhs, CharCode.e, 8 + 7)
      store<u8>(tmp2Rhs, CharCode.COLON, 8 + 8)
      store<u8>(tmp2Rhs, CharCode.SPACE, 8 + 9)
      store<u8>(tmp2Rhs, CharCode._0, 8 + 10)
      store<u8>(tmp2Rhs, CharCode.x, 8 + 11)
      let tmp2 = concat(tmp, tmp2Rhs)
      let tmp3 = itoa32(ptr, 16)
      let tmp4 = concat(tmp2, tmp3)
      let tmp5 = rightLiteralConcat1(tmp4, CharCode.RANGLE)
      let retLhs = allocateString(26)
      store<u8>(retLhs, CharCode.LANGLE, 8)
      store<u8>(retLhs, CharCode.u, 8 + 1)
      store<u8>(retLhs, CharCode.n, 8 + 2)
      store<u8>(retLhs, CharCode.k, 8 + 4)
      store<u8>(retLhs, CharCode.n, 8 + 3)
      store<u8>(retLhs, CharCode.o, 8 + 5)
      store<u8>(retLhs, CharCode.w, 8 + 6)
      store<u8>(retLhs, CharCode.n, 8 + 7)
      store<u8>(retLhs, CharCode.SPACE, 8 + 8)
      store<u8>(retLhs, CharCode.h, 8 + 9)
      store<u8>(retLhs, CharCode.e, 8 + 10)
      store<u8>(retLhs, CharCode.a, 8 + 11)
      store<u8>(retLhs, CharCode.p, 8 + 12)
      store<u8>(retLhs, CharCode.SPACE, 8 + 13)
      store<u8>(retLhs, CharCode.t, 8 + 14)
      store<u8>(retLhs, CharCode.a, 8 + 15)
      store<u8>(retLhs, CharCode.g, 8 + 16)
      store<u8>(retLhs, CharCode.SPACE, 8 + 17)
      store<u8>(retLhs, CharCode.t, 8 + 18)
      store<u8>(retLhs, CharCode.y, 8 + 19)
      store<u8>(retLhs, CharCode.p, 8 + 20)
      store<u8>(retLhs, CharCode.e, 8 + 21)
      store<u8>(retLhs, CharCode.COLON, 8 + 22)
      store<u8>(retLhs, CharCode.SPACE, 8 + 23)
      store<u8>(retLhs, CharCode._0, 8 + 24)
      store<u8>(retLhs, CharCode.x, 8 + 25)
      let ret = concat(retLhs, tmp5)
      decRef(tmp5)
      decRef(tmp4)
      decRef(tmp3)
      decRef(tmp2)
      decRef(tmp2Rhs)
      decRef(tmp)
      decRef(retLhs)
      return ret
    }
  }
}

function grainToStringHelp(grainValue: u32, extraIndents: u32, toplevel: bool): u32 {
  if (grainValue & 1) {
    // Simple (unboxed) numbers
    return itoa32(<i32>(grainValue) >> 1, 10)
  } else if ((grainValue & 7) == GRAIN_GENERIC_HEAP_TAG_TYPE) {
    return grainHeapValueToString(grainValue, extraIndents, toplevel)
  } else if (grainValue == GRAIN_TRUE) {
    return getTrueString()
  } else if (grainValue == GRAIN_FALSE) {
    return getFalseString()
  } else if (grainValue == GRAIN_VOID) {
    return getVoidString()
  } else {
    return getUnknownValueString()
  }
}

// @ts-ignore: decorator
@inline
export function grainToString(grainValue: u32): u32 {
  return grainToStringHelp(grainValue, 0, true)
}
