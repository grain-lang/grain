import { grainToString, JSToGrainVal } from '../utils/utils';

export function toString(v) {
  return JSToGrainVal(grainToString(v));
}

export function makeToString(gtos) {
  return v => JSToGrainVal(gtos(v));
}
