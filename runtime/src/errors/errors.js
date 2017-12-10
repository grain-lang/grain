import { grainToString } from '../utils/utils';

import * from './error-codes';

export class GrainError extends Error {
  constructor(code, message) {
    super(message);
    this.name = "GrainError";
    this.code = code;
  }
}

export function throwGrainError(errorCode, value1, value2) {
  let message;
  let value1AsGrain = grainToString(value1);

  switch (errorCode) {
    case GRAIN_ERR_ARITY_MISMATCH:
      message = `arity mismatch (expected ${value1} arguments, but got ${value2})`;
      break;
    case GRAIN_ERR_NOT_NUMBER_ARITH:
      message = `arithmetic expected a number, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_NUMBER_COMP:
      message = `comparison expected a number, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_NUMBER_GENERIC:
      message = `expected a number, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_BOOLEAN_GENERIC:
      message = `expected a boolean, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_TUPLE_GENERIC:
      message = `expected a tuple, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_LAMBDA_GENERIC:
      message = `expected a lambda, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_OVERFLOW:
      message = `number overflow with value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_BOOLEAN_IF:
      message = `if expected a boolean, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_BOOLEAN_LOGIC:
      message = `logic expected a boolean, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_STRING_GENERIC:
      message = `expected a string, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_DOM_ELEMENT_GENERIC:
      message = `expected a DOM element, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_GET_NOT_TUP:
      message = `tuple access expected tuple, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_SET_NOT_TUP:
      message = `tuple assignment expected tuple, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_GET_ITEM_IDX_NOT_NUMBER:
      message = `tuple access expected number for index, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_SET_ITEM_IDX_NOT_NUMBER:
      message = `tuple assignment expected number for index, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_GET_ITEM_IDX_TOO_SMALL:
    case GRAIN_ERR_SET_ITEM_IDX_TOO_SMALL:
      message = `tuple index too small: ${value1AsGrain} (tuple arity: ${value2})`;
      break;
    case GRAIN_ERR_GET_ITEM_IDX_TOO_LARGE:
    case GRAIN_ERR_SET_ITEM_IDX_TOO_LARGE:
      message = `tuple index too large: ${value1AsGrain} (tuple arity: ${value2})`;
      break;
    case GRAIN_ERR_CALLED_NON_FUNCTION:
      message = `called non-function: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_NOT_NONNEG:
      message = `expected a nonnegative number, got value: ${value1AsGrain}`;
      break;
    case GRAIN_ERR_OUT_OF_MEMORY:
      message = `Out of memory`;
      break;
    default:
      message = `Unknown error code: ${errorCode}`;
  }

  throw new GrainError(errorCode, message);
}
