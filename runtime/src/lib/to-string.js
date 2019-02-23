import { grainToString, JSToGrainVal } from '../utils/utils';

export function toString(v) {
  return JSToGrainVal(grainToString(null, v));
}

export function makeToString(boundGrainToString) {
  return v => JSToGrainVal(boundGrainToString(v));
}
