import { GRAIN_ADT_HEAP_TAG, GRAIN_ARRAY_HEAP_TAG, GRAIN_GENERIC_HEAP_TAG_TYPE, GRAIN_RECORD_HEAP_TAG, GRAIN_CHAR_HEAP_TAG, GRAIN_STRING_HEAP_TAG, GRAIN_BOXED_NUM_HEAP_TAG, GRAIN_INT32_BOXED_NUM_TAG, GRAIN_INT64_BOXED_NUM_TAG, GRAIN_RATIONAL_BOXED_NUM_TAG, GRAIN_FLOAT32_BOXED_NUM_TAG, GRAIN_FLOAT64_BOXED_NUM_TAG, GRAIN_TUPLE_TAG_TYPE, GRAIN_LAMBDA_TAG_TYPE } from '../ascutils/tags'
import { stringSize, allocateString, loadI32, loadI64, loadF32, loadF64, loadRationalNumerator, loadRationalDenominator, wrapString, singleCharacterString, twoCharacterString } from '../ascutils/dataStructures'
import { GRAIN_FALSE, GRAIN_TRUE, GRAIN_VOID } from '../ascutils/primitives'
import { consoleLog } from '../ascutils/console'
import { decRef } from '../ascutils/grainRuntime'
import { dtoa, itoa32, itoa64 } from '../ascutils/numberUtils'
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
  s1 = s1 ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  s2 = s2 ^ GRAIN_GENERIC_HEAP_TAG_TYPE

  const size1 = stringSize(s1)
  const size2 = stringSize(s2)

  const newString = allocateString(size1 + size2)

  memory.copy(newString + 8, s1 + 8, size1)
  memory.copy(newString + 8 + size1, s2 + 8, size2)

  return newString ^ GRAIN_GENERIC_HEAP_TAG_TYPE
}

// @ts-ignore: decorator
@inline
function leftLiteralConcat(s: string, gs: u32): u32 {
  let encString = wrapString(s)
  let ret = concat(encString, gs)
  decRef(encString)
  return ret
}


// @ts-ignore: decorator
@inline
function rightLiteralConcat(gs: u32, s: string): u32 {
  let encString = wrapString(s)
  let ret = concat(gs, encString)
  decRef(encString)
  return ret
}

function grainListToString(ptr: u32, extraIndents: u32): u32 {
  const untaggedPtr = ptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE
  let cur = untaggedPtr
  let ret = wrapString('[')
  let isFirst = true

  while (true) {
    let variantId = load<i32>(cur, 4 * 3) >> 1 // tagged number
    if (variantId === 0) {
      break;
    } else {
      if (!isFirst) {
        let oldRet = ret
        ret = rightLiteralConcat(ret, ', ')
        decRef(oldRet)
      }
      isFirst = false
      let itemString = grainToStringHelp(load<u32>(cur, 4 * 5), extraIndents)
      let oldRet = ret
      ret = concat(ret, itemString)
      decRef(oldRet)
      cur = load<u32>(cur, 4 * 6) & ~GRAIN_GENERIC_HEAP_TAG_TYPE
    }
  }
  let oldRet = ret
  ret = rightLiteralConcat(ret, ']')
  decRef(oldRet)
  return ret
}


function quoteString(ptr: u32): u32 {
  const untaggedPtr = ptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE
  let length = stringSize(untaggedPtr)
  let ret = allocateString(length + 2)
  store<u8>(ret + 8, 34) // '"'
  memory.copy(ret + 9, untaggedPtr + 8, length)
  store<u8>(ret + 9 + length, 34) // '"'
  return ret | GRAIN_GENERIC_HEAP_TAG_TYPE
}


function grainHeapValueToString(ptr: u32, extraIndents: u32): u32 {
  // ptr can be tagged or untagged
  const untaggedPtr = ptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE
  const tag = load<u32>(untaggedPtr)
  switch (tag) {
    case GRAIN_STRING_HEAP_TAG: {
      return quoteString(untaggedPtr)
    }
    case GRAIN_CHAR_HEAP_TAG: {
      let byte = load<u8>(untaggedPtr + 4)
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
      let str = allocateString(numBytes + 2)
      store<u8>(str + 8, <u8>(0x27))
      memory.copy(str + 9, untaggedPtr + 4, numBytes)
      store<u8>(str + 9 + numBytes, <u8>(0x27))
      return str | GRAIN_GENERIC_HEAP_TAG_TYPE;
    }
    case GRAIN_ADT_HEAP_TAG: {
      // [ <value type tag>, <module_tag>, <type_tag>, <variant_tag>, <arity>, elts ... ]
      // these are tagged ints
      let moduleId = load<i32>(untaggedPtr, 4 * 1) >> 1
      let typeId = load<i32>(untaggedPtr, 4 * 2) >> 1
      let variantId = load<i32>(untaggedPtr, 4 * 3) >> 1
      // probably a linking issue!
      if (!variantExists(moduleId, typeId, variantId)) return wrapString("<adt value>")
      let variantName: u32 = getVariantName(moduleId, typeId, variantId)
      // Check if this is a list
      // (hack to get list printing correct)
      let listVariant = wrapString('[...]')
      let isList = equal(variantName, listVariant) == GRAIN_TRUE
      decRef(listVariant)
      if (isList) return grainListToString(untaggedPtr, extraIndents)
      let variantArity = getVariantArity(moduleId, typeId, variantId)
      if (variantArity == 0) return variantName
      // [NOTE] do not decRef variantName!
      let ret = rightLiteralConcat(variantName, '(')
      for (let i = 0; i < variantArity; ++i) {
        if (i > 0) {
          let oldRet = ret
          ret = rightLiteralConcat(ret, ', ')
          decRef(oldRet)
        }
        let oldRet = ret
        let tmp = grainToStringHelp(load<u32>(untaggedPtr + 4 * (5 + i)), extraIndents)
        ret = concat(ret, tmp)
        decRef(tmp)
        decRef(oldRet)
      }
      return rightLiteralConcat(ret, ')')
    }
    case GRAIN_RECORD_HEAP_TAG: {
      // these are tagged ints
      let moduleId = load<i32>(untaggedPtr, 4 * 1) >> 1
      let typeId = load<i32>(untaggedPtr, 4 * 2) >> 1
      // probably a linking issue!
      if (!recordTypeExists(moduleId, typeId)) return wrapString('<record value>')
      let recordArity = getRecordArity(moduleId, typeId)
      if (recordArity == 0) return wrapString('<record value>')
      let lastSpacePadding = wrapString('')
      let spacePadding = wrapString('  ')
      for (let i: u32 = 0; i < extraIndents; ++i) {
        decRef(lastSpacePadding)
        lastSpacePadding = spacePadding
        spacePadding = rightLiteralConcat(spacePadding, '  ')
      }
      let ret = leftLiteralConcat('{\n', spacePadding)
      for (let i = 0; i < recordArity; ++i) {
        if (i > 0) {
          let oldRet = ret
          ret = rightLiteralConcat(ret, ',\n')
          decRef(oldRet)
          oldRet = ret
          ret = concat(ret, spacePadding)
          decRef(oldRet)
        }
        // [NOTE] do not decRef:
        let fieldName = getRecordFieldName(moduleId, typeId, i)
        // [NOTE] *do* decRef
        let fieldValue = grainToStringHelp(load<u32>(untaggedPtr + 4 * (4 + i)), extraIndents + 1)
        // [TODO] refactor to copy less here
        let oldRet = ret
        ret = concat(ret, fieldName)
        decRef(oldRet)
        oldRet = ret
        ret = rightLiteralConcat(ret, ': ')
        decRef(oldRet)
        oldRet = ret
        ret = concat(ret, fieldValue)
        decRef(oldRet)
        decRef(fieldValue)
      }
      decRef(spacePadding)
      let oldRet = ret
      ret = rightLiteralConcat(ret, '\n')
      decRef(oldRet)
      oldRet = ret
      ret = concat(ret, lastSpacePadding)
      decRef(oldRet)
      oldRet = ret
      ret = rightLiteralConcat(ret, '}')
      decRef(oldRet)
      decRef(lastSpacePadding)
      return ret
    }
    case GRAIN_ARRAY_HEAP_TAG: {
      let ret = wrapString("[> ")
      let arity = load<i32>(untaggedPtr, 4 * 1)
      for (let i = 0; i < arity; ++i) {
        if (i > 0) {
          let oldRet = ret
          ret = rightLiteralConcat(ret, ', ')
          decRef(oldRet)
        }
        let oldRet = ret
        let tmp = grainToStringHelp(load<u32>(untaggedPtr + 4 * (2 + i)), extraIndents)
        ret = concat(ret, tmp)
        decRef(tmp)
        decRef(oldRet)
      }
      let oldRet = ret
      ret = rightLiteralConcat(ret, ']')
      decRef(oldRet)
      return ret
    }
    case GRAIN_BOXED_NUM_HEAP_TAG: {
      let numberTag = load<u32>(untaggedPtr, 4 * 1)
      switch (numberTag) {
        case GRAIN_INT32_BOXED_NUM_TAG: {
          return itoa32(loadI32(untaggedPtr), 10)
        }
        case GRAIN_INT64_BOXED_NUM_TAG: {
          return itoa64(loadI64(untaggedPtr), 10)
        }
        case GRAIN_RATIONAL_BOXED_NUM_TAG: {
          let numerator = loadRationalNumerator(untaggedPtr)
          let denominator = loadRationalDenominator(untaggedPtr)
          let ret = itoa32(numerator, 10)
          let oldRet = ret
          ret = rightLiteralConcat(ret, '/')
          decRef(oldRet)
          oldRet = ret
          let tmp = itoa32(denominator, 10)
          ret = concat(ret, tmp)
          decRef(tmp)
          decRef(oldRet)
          return ret
        }
        case GRAIN_FLOAT32_BOXED_NUM_TAG: {
          return dtoa(loadF32(untaggedPtr))
        }
        case GRAIN_FLOAT64_BOXED_NUM_TAG: {
          return dtoa(loadF64(untaggedPtr))
        }
      }
    }
    default: {
      let tmp = itoa32(tag, 16)
      let tmp2 = rightLiteralConcat(tmp, " | value: 0x")
      let tmp3 = itoa32(ptr, 16)
      let tmp4 = concat(tmp2, tmp3)
      let tmp5 = rightLiteralConcat(tmp4, ">")
      let ret = leftLiteralConcat("<unknown heap tag type: 0x", tmp5)
      decRef(tmp5)
      decRef(tmp4)
      decRef(tmp3)
      decRef(tmp2)
      decRef(tmp)
      return ret
    }
  }
}

function grainToStringHelp(grainValue: u32, extraIndents: u32): u32 {
  if (!(grainValue & 1)) {
    // Simple (unboxed) numbers
    return itoa32(<i32>(grainValue) >> 1, 10)
  } else if ((grainValue & 7) == GRAIN_TUPLE_TAG_TYPE) {
    // [TODO]
    let ptr = grainValue ^ 1
    let tupleLength = load<u32>(ptr)
    if (tupleLength & 0x80000000) {
      return wrapString("<cyclic tuple>"); // ${grainValue & 0x7FFFFFFF}
    } else {
      store<u32>(ptr, 0x80000000 | tupleLength)
      let ret = wrapString("(")
      for (let i: u32 = 0; i < tupleLength; ++i) {
        if (i > 0) {
          let oldRet = ret
          ret = rightLiteralConcat(ret, ", ")
          decRef(oldRet)
        }
        let oldRet = ret
        let tmp = grainToStringHelp(load<u32>(ptr + ((i + 1) * 4)), extraIndents)
        ret = concat(ret, tmp)
        decRef(oldRet)
        decRef(tmp)
      }
      store<u32>(ptr, tupleLength)
      if (tupleLength <= 1) {
        // Special case: unary tuple
        let oldRet = ret
        ret = rightLiteralConcat(ret, ",")
        decRef(oldRet)
      }
      let oldRet = ret
      ret = rightLiteralConcat(ret, ")")
      decRef(oldRet)
      return ret
    }
  } else if ((grainValue & 7) == GRAIN_LAMBDA_TAG_TYPE) {
    return wrapString("<lambda>")
  } else if ((grainValue & 7) == GRAIN_GENERIC_HEAP_TAG_TYPE) {
    return grainHeapValueToString(grainValue, extraIndents)
  } else if (grainValue == GRAIN_TRUE) {
    return wrapString("true")
  } else if (grainValue == GRAIN_FALSE) {
    return wrapString("false")
  } else if (grainValue == GRAIN_VOID) {
    return wrapString("void")
  } else {
    return wrapString("<Unknown value>")
  }
}

// @ts-ignore: decorator
@inline
export function grainToString(grainValue: u32): u32 {
  return grainToStringHelp(grainValue, 0)
}

// @ts-ignore: decorator
@inline
export function grainToGrainString(grainValue: u32): u32 {
  return grainToString(grainValue)
}
