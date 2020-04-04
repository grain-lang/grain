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
  GRAIN_FALSE
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

export function getTagType(n) {
  if (!(n & GRAIN_NUMBER_TAG_MASK)) {
    return GRAIN_NUMBER_TAG_TYPE;
  } else if ((n & GRAIN_TUPLE_TAG_MASK) === GRAIN_TUPLE_TAG_TYPE) {
    return GRAIN_TUPLE_TAG_TYPE;
  } else if ((n & GRAIN_TUPLE_TAG_MASK) === GRAIN_LAMBDA_TAG_TYPE) {
    return GRAIN_LAMBDA_TAG_TYPE;
  } else if ((n & GRAIN_TUPLE_TAG_MASK) === GRAIN_GENERIC_HEAP_TAG_TYPE) {
    return GRAIN_GENERIC_HEAP_TAG_TYPE;
  } else if ((n === -1) || (n === 0x7FFFFFFF) || (n === 0x6FFFFFFF)) {
    return GRAIN_CONST_TAG_TYPE;
  } else {
    console.warn(`<getTagType: Unknown value: 0x${(new Number(n)).toString(16)}`);
    return undefined;
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
