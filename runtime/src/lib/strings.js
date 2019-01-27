import { assertString, assertNumber } from '../core/tags';
import { grainToJSVal, JSToGrainVal } from '../utils/utils';
import { throwGrainError, GrainError } from '../errors/errors';
import { GRAIN_ERR_NOT_NONNEG } from '../errors/error-codes';

export function stringAppend(s1, s2) {
  assertString(s1);
  assertString(s2);
  s1 = grainToJSVal(null, s1);
  s2 = grainToJSVal(null, s2);
  let appended = s1.concat(s2);
  let ret = JSToGrainVal(appended);
  return ret;
}

export function stringLength(s) {
  assertString(s);
  return JSToGrainVal(grainToJSVal(null, s).length);
}

export function stringSlice(s, from, to) {
  assertString(s);
  assertNumber(from);
  assertNumber(to);
  s = grainToJSVal(null, s);
  if (from < 0) {
    throwGrainError(GRAIN_ERR_NOT_NONNEG, from);
  } else if ((to >> 1) > s.length) {
    throw new GrainError(-1, `Index ${to >> 1} greater than string length (${s.length})`);
  }
  from = grainToJSVal(null, from);
  to = grainToJSVal(null, to);
  let ret = s.slice(from, to);
  return JSToGrainVal(ret);
}
