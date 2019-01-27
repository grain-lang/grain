import { grainToString, JSToGrainVal } from '../utils/utils';

export function toString(v) {
  return JSToGrainVal(grainToString(v));
}

export function makeToString(grainToString) {
  return v => JSToGrainVal(grainToString(v));
}
