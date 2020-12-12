import { GRAIN_ADT_HEAP_TAG, GRAIN_ARRAY_HEAP_TAG, GRAIN_GENERIC_HEAP_TAG_TYPE, GRAIN_RECORD_HEAP_TAG, GRAIN_CHAR_HEAP_TAG, GRAIN_STRING_HEAP_TAG, GRAIN_BOXED_NUM_HEAP_TAG, GRAIN_INT32_BOXED_NUM_TAG, GRAIN_INT64_BOXED_NUM_TAG, GRAIN_RATIONAL_BOXED_NUM_TAG, GRAIN_FLOAT32_BOXED_NUM_TAG, GRAIN_FLOAT64_BOXED_NUM_TAG, GRAIN_TUPLE_TAG_TYPE, GRAIN_LAMBDA_TAG_TYPE } from '../ascutils/tags'
import { stringSize, allocateString, loadI32, loadI64, loadF32, loadF64, loadRationalNumerator, loadRationalDenominator, wrapString } from '../ascutils/dataStructures'
import { GRAIN_FALSE, GRAIN_TRUE, GRAIN_VOID } from '../ascutils/primitives'
import { consoleLog } from '../ascutils/console'

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


function grainListToString(ptr: u32, extraIndents: u32): string {
  const untaggedPtr = ptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE
  let cur = untaggedPtr
  let ret = '['
  let isFirst = true

  while (true) {
    let variantId = load<i32>(cur, 4 * 3) >> 1 // tagged number
    if (variantId === 0) {
      break;
    } else {
      if (!isFirst) {
        ret = ret.concat(', ')
      }
      isFirst = false
      ret = ret.concat(grainToStringHelp(load<u32>(cur, 4 * 5), extraIndents))
      cur = load<u32>(cur, 4 * 6) & ~GRAIN_GENERIC_HEAP_TAG_TYPE
    }
  }
  return ret.concat(']')
}


function grainStringToString(ptr: u32, withQuotes: bool): string {
  const untaggedPtr = ptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE
  let length = load<i32>(untaggedPtr, 4 * 1)
  let stringContents = String.UTF8.decodeUnsafe(untaggedPtr + (4 * 2), length)
  if (withQuotes) {
    return '"'.concat(stringContents).concat('"')
  } else {
    return stringContents
  }
}


function grainHeapValueToString(ptr: u32, extraIndents: u32): string {
  // ptr can be tagged or untagged
  const untaggedPtr = ptr & ~GRAIN_GENERIC_HEAP_TAG_TYPE
  consoleLog('untaggedPtr: 0x'.concat(untaggedPtr.toString(16)))
  const tag = load<u32>(untaggedPtr)
  switch (tag) {
    case GRAIN_STRING_HEAP_TAG: {
      return grainStringToString(untaggedPtr, true)
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
      let ret = str | GRAIN_GENERIC_HEAP_TAG_TYPE;
      return grainStringToString(ret, false)
    }
    case GRAIN_ADT_HEAP_TAG: {
      // [ <value type tag>, <module_tag>, <type_tag>, <variant_tag>, <arity>, elts ... ]
      // these are tagged ints
      let moduleId = load<i32>(untaggedPtr, 4 * 1) >> 1
      let typeId = load<i32>(untaggedPtr, 4 * 2) >> 1
      let variantId = load<i32>(untaggedPtr, 4 * 3) >> 1
      // probably a linking issue!
      if (!variantExists(moduleId, typeId, variantId)) return "<adt value>"
      let variantName: string = grainStringToString(getVariantName(moduleId, typeId, variantId), false)
      if (variantName == '[...]') return grainListToString(untaggedPtr, extraIndents)
      let variantArity = getVariantArity(moduleId, typeId, variantId)
      if (variantArity == 0) return variantName
      let ret = variantName.concat('(')
      for (let i = 0; i < variantArity; ++i) {
        if (i > 0) {
          ret = ret.concat(', ')
        }
        ret = ret.concat(grainToStringHelp(load<u32>(untaggedPtr + 4 * (5 + i)), extraIndents))
      }
      return ret.concat(')')
    }
    case GRAIN_RECORD_HEAP_TAG: {
      // these are tagged ints
      let moduleId = load<i32>(untaggedPtr, 4 * 1) >> 1
      let typeId = load<i32>(untaggedPtr, 4 * 2) >> 1
      // probably a linking issue!
      if (!recordTypeExists(moduleId, typeId)) return '<record value>'
      let recordArity = getRecordArity(moduleId, typeId)
      if (recordArity == 0) return '<record value>'
      let spacePadding = '  '
      for (let i: u32 = 0; i < extraIndents; ++i) {
        spacePadding = spacePadding.concat('  ')
      }
      let ret = '{\n'.concat(spacePadding)
      for (let i = 0; i < recordArity; ++i) {
        if (i > 0) {
          ret = ret.concat(',\n').concat(spacePadding)
        }
        let fieldName = grainStringToString(getRecordFieldName(moduleId, typeId, i), false)
        let fieldValue = grainToStringHelp(load<u32>(untaggedPtr + 4 * (4 + i)), extraIndents + 1)
        consoleLog(fieldName)
        consoleLog(fieldValue)
        ret = ret.concat(fieldName)
          .concat(': ')
          .concat(fieldValue)
      }
      let lastSpacePadding = ''
      for (let i: u32 = 0; i < extraIndents; ++i) {
        lastSpacePadding = lastSpacePadding.concat('  ')
      }
      return ret.concat('\n').concat(lastSpacePadding).concat('}')
    }
    case GRAIN_ARRAY_HEAP_TAG: {
      let result = "[> "
      let arity = load<i32>(untaggedPtr, 4 * 1)
      for (let i = 0; i < arity; ++i) {
        if (i > 0) {
          result = result.concat(', ')
        }
        result = result.concat(grainToStringHelp(load<u32>(untaggedPtr + 4 * (2 + i)), extraIndents))
      }
      return result.concat("]")
    }
    case GRAIN_BOXED_NUM_HEAP_TAG: {
      let numberTag = load<u32>(untaggedPtr, 4 * 1)
      switch (numberTag) {
        case GRAIN_INT32_BOXED_NUM_TAG: {
          return loadI32(untaggedPtr).toString(10)
        }
        case GRAIN_INT64_BOXED_NUM_TAG: {
          return loadI64(untaggedPtr).toString(10)
        }
        case GRAIN_RATIONAL_BOXED_NUM_TAG: {
          let numerator = loadRationalNumerator(untaggedPtr)
          let denominator = loadRationalDenominator(untaggedPtr)
          return numerator.toString(10).concat('/').concat(denominator.toString(10))
        }
        case GRAIN_FLOAT32_BOXED_NUM_TAG: {
          return loadF32(untaggedPtr).toString(10)
        }
        case GRAIN_FLOAT64_BOXED_NUM_TAG: {
          return loadF64(untaggedPtr).toString(10)
        }
      }
    }
    default: {
      return `<unknown heap type: ${tag}>`
    }
  }
}

function grainToStringHelp(grainValue: u32, extraIndents: u32): string {
  if (!(grainValue & 1)) {
    // Simple (unboxed) numbers
    return (<i32>(grainValue) >> 1).toString(10)
  } else if ((grainValue & 7) == GRAIN_TUPLE_TAG_TYPE) {
    // [TODO]
    let ptr = grainValue ^ 1
    let tupleLength = load<u32>(ptr)
    if (tupleLength & 0x80000000) {
      return `<cyclic tuple ${grainValue & 0x7FFFFFFF}>`;
    } else {
      store<u32>(ptr, 0x80000000 | tupleLength)
      let ret = "("
      for (let i: u32 = 0; i < tupleLength; ++i) {
        if (i > 0) {
          ret = ret.concat(", ")
        }
        ret = ret.concat(grainToStringHelp(load<u32>(ptr + ((i + 1) * 4)), extraIndents))
      }
      store<u32>(ptr, tupleLength)
      if (tupleLength <= 1) {
        // Special case: unary tuple
        ret = ret.concat(",")
      }
      return ret.concat(")")
    }
  } else if ((grainValue & 7) == GRAIN_LAMBDA_TAG_TYPE) {
    return "<lambda>"
  } else if ((grainValue & 7) == GRAIN_GENERIC_HEAP_TAG_TYPE) {
    return grainHeapValueToString(grainValue, extraIndents)
  } else if (grainValue == GRAIN_TRUE) {
    return "true"
  } else if (grainValue == GRAIN_FALSE) {
    return "false"
  } else if (grainValue == GRAIN_VOID) {
    return "void"
  } else {
    return `<Unknown value: ${grainValue}>`
  }
}

// @ts-ignore: decorator
@inline
export function grainToString(grainValue: u32): string {
  return grainToStringHelp(grainValue, 0)
}

// @ts-ignore: decorator
@inline
export function grainToGrainString(grainValue: u32): u32 {
  return wrapString(grainToString(grainValue))
}
