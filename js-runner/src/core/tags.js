import { GRAIN_TRUE, GRAIN_FALSE, GRAIN_VOID } from "./primitives";

export const GRAIN_NUMBER_TAG_MASK = 0b0001;
export const GRAIN_GENERIC_TAG_MASK = 0b0111;

export const GRAIN_NUMBER_TAG_TYPE = 0b0001;
export const GRAIN_CONST_TAG_TYPE = 0b0111;
export const GRAIN_GENERIC_HEAP_TAG_TYPE = 0b0000;

export const GRAIN_STRING_HEAP_TAG = 1;
export const GRAIN_CHAR_HEAP_TAG = 2;
export const GRAIN_ADT_HEAP_TAG = 3;
export const GRAIN_RECORD_HEAP_TAG = 4;
export const GRAIN_ARRAY_HEAP_TAG = 5;
export const GRAIN_BOXED_NUM_HEAP_TAG = 6;
export const GRAIN_LAMBDA_HEAP_TAG = 7;
export const GRAIN_TUPLE_HEAP_TAG = 8;

export const GRAIN_FLOAT32_BOXED_NUM_TAG = 1;
export const GRAIN_FLOAT64_BOXED_NUM_TAG = 2;
export const GRAIN_INT32_BOXED_NUM_TAG = 3;
export const GRAIN_INT64_BOXED_NUM_TAG = 4;
export const GRAIN_RATIONAL_BOXED_NUM_TAG = 5;

function getAndMask(tag) {
  switch (tag) {
    case GRAIN_NUMBER_TAG_TYPE:
      return GRAIN_NUMBER_TAG_MASK;
    default:
      return GRAIN_TUPLE_TAG_MASK;
  }
}

export function getTagType(n, quiet) {
  if (n === GRAIN_TRUE || n === GRAIN_FALSE || n === GRAIN_VOID) {
    return GRAIN_CONST_TAG_TYPE;
  } else if (n & GRAIN_NUMBER_TAG_MASK) {
    return GRAIN_NUMBER_TAG_TYPE;
  } else if ((n & GRAIN_GENERIC_TAG_MASK) === GRAIN_GENERIC_HEAP_TAG_TYPE) {
    return GRAIN_GENERIC_HEAP_TAG_TYPE;
  } else {
    if (!quiet) {
      console.warn(
        `<getTagType: Unknown value: 0x${new Number(n).toString(16)}`
      );
    }
    return;
  }
}

export function tagToString(t) {
  switch (t) {
    case GRAIN_NUMBER_TAG_TYPE:
      return "number";
    case GRAIN_TUPLE_TAG_TYPE:
      return "tuple";
    case GRAIN_LAMBDA_TAG_TYPE:
      return "lambda";
    case GRAIN_GENERIC_HEAP_TAG_TYPE:
      return "heap_value";
    case GRAIN_CONST_TAG_TYPE:
      return "const";
    default:
      return "unknown";
  }
}

export function heapTagToString(t) {
  switch (t) {
    case GRAIN_STRING_HEAP_TAG:
      return "string";
    case GRAIN_CHAR_HEAP_TAG:
      return "char";
    case GRAIN_ADT_HEAP_TAG:
      return "adt";
    case GRAIN_RECORD_HEAP_TAG:
      return "record";
    case GRAIN_ARRAY_HEAP_TAG:
      return "array";
    default:
      return "unknown";
  }
}
