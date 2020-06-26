import { memory, view } from '../runtime';
import { throwGrainError } from '../errors/errors';
import {
  GRAIN_ERR_NOT_NUMBER_GENERIC,
  GRAIN_ERR_NOT_BOOLEAN_GENERIC,
  GRAIN_ERR_NOT_TUPLE_GENERIC,
  GRAIN_ERR_NOT_LAMBDA_GENERIC,
  GRAIN_ERR_NOT_STRING_GENERIC,
  GRAIN_ERR_NOT_DOM_ELEMENT_GENERIC,
  GRAIN_ERR_NOT_ADT_VAL_GENERIC,
} from '../errors/error-codes';

import {
  GRAIN_TRUE,
  GRAIN_FALSE,
  GRAIN_VOID
} from './primitives';

export const GRAIN_NUMBER_TAG_MASK = 0b0001;
export const GRAIN_TUPLE_TAG_MASK = 0b0111;

export const GRAIN_NUMBER_TAG_TYPE       = 0b0000;
export const GRAIN_CONST_TAG_TYPE        = 0b1111;
export const GRAIN_TUPLE_TAG_TYPE        = 0b0001;
export const GRAIN_LAMBDA_TAG_TYPE       = 0b0101;
export const GRAIN_GENERIC_HEAP_TAG_TYPE = 0b0011;

export const GRAIN_STRING_HEAP_TAG = 1;
export const GRAIN_DOM_ELEM_TAG = 2;
export const GRAIN_ADT_HEAP_TAG = 3;
export const GRAIN_RECORD_HEAP_TAG = 4;
export const GRAIN_ARRAY_HEAP_TAG = 5;
export const GRAIN_INT32_HEAP_TAG = 6;
export const GRAIN_INT64_HEAP_TAG = 7;

function getAndMask(tag) {
  switch(tag) {
  case GRAIN_NUMBER_TAG_TYPE:
    return GRAIN_NUMBER_TAG_MASK;
  default:
    return GRAIN_TUPLE_TAG_MASK;
  }
}

export function getTagType(n, quiet) {
  if ((n === GRAIN_TRUE) || (n === GRAIN_FALSE) || (n === GRAIN_VOID)) {
    return GRAIN_CONST_TAG_TYPE;
  } else if (!(n & GRAIN_NUMBER_TAG_MASK)) {
    return GRAIN_NUMBER_TAG_TYPE;
  } else if ((n & GRAIN_TUPLE_TAG_MASK) === GRAIN_TUPLE_TAG_TYPE) {
    return GRAIN_TUPLE_TAG_TYPE;
  } else if ((n & GRAIN_TUPLE_TAG_MASK) === GRAIN_LAMBDA_TAG_TYPE) {
    return GRAIN_LAMBDA_TAG_TYPE;
  } else if ((n & GRAIN_TUPLE_TAG_MASK) === GRAIN_GENERIC_HEAP_TAG_TYPE) {
    return GRAIN_GENERIC_HEAP_TAG_TYPE;
  } else {
    if (!quiet) {
      console.warn(`<getTagType: Unknown value: 0x${(new Number(n)).toString(16)}`);
    }
    return;
  }
}

export function tagToString(t) {
  switch (t) {
    case GRAIN_NUMBER_TAG_TYPE:
      return 'number';
    case GRAIN_TUPLE_TAG_TYPE:
      return 'tuple';
    case GRAIN_LAMBDA_TAG_TYPE:
      return 'lambda';
    case GRAIN_GENERIC_HEAP_TAG_TYPE:
      return 'heap_value';
    case GRAIN_CONST_TAG_TYPE:
      return 'const';
    default:
      return 'unknown';
  }
}

export function heapTagToString(t) {
  switch (t) {
    case GRAIN_STRING_HEAP_TAG:
      return 'string';
    case GRAIN_DOM_ELEM_TAG:
      return 'dom_elem';
    case GRAIN_ADT_HEAP_TAG:
      return 'adt';
    case GRAIN_RECORD_HEAP_TAG:
      return 'record';
    case GRAIN_ARRAY_HEAP_TAG:
      return 'array';
    default:
      return 'unknown';
  }
}

function assertGrainTag(tag, n, err) {
  if ((n & getAndMask(tag)) !== tag) {
    throwGrainError(err, n, 0);
  }
}

function assertGrainHeapTag(tag, n, err) {
  assertGrainTag(GRAIN_GENERIC_HEAP_TAG_TYPE, n, err);
  let ptr = n ^ GRAIN_GENERIC_HEAP_TAG_TYPE;
  if (view[ptr / 4] != tag) {
    throwGrainError(err, n, 0);
  }
}

export const assertBoolean = (n, err) => {
  assertGrainTag(GRAIN_CONST_TAG_TYPE, n, err || GRAIN_ERR_NOT_BOOLEAN_GENERIC);
  if (n !== GRAIN_TRUE && n !== GRAIN_FALSE) {
    throwGrainError(err, n, 0);
  }
}

export const assertNumber = (n, err) => assertGrainTag(GRAIN_NUMBER_TAG_TYPE, n, err || GRAIN_ERR_NOT_NUMBER_GENERIC);
export const assertTuple = (n, err) => assertGrainTag(GRAIN_TUPLE_TAG_TYPE, n, err || GRAIN_ERR_NOT_TUPLE_GENERIC);
export const assertLambda = (n, err) => assertGrainTag(GRAIN_LAMBDA_TAG_TYPE, n, err || GRAIN_ERR_NOT_LAMBDA_GENERIC);
export const assertString = (n, err) => assertGrainHeapTag(GRAIN_STRING_HEAP_TAG, n, err || GRAIN_ERR_NOT_STRING_GENERIC);
export const assertDOMElement = (n, err) => assertGrainHeapTag(GRAIN_DOM_ELEM_TAG, n, err || GRAIN_ERR_NOT_DOM_ELEMENT_GENERIC);
export const assertADT = (n, err) => assertGrainHeapTag(GRAIN_ADT_HEAP_TAG, n, err || GRAIN_ERR_NOT_ADT_VAL_GENERIC)
