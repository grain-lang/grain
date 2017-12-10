import { grainToString, JSToGrainVal } from '../utils/utils';

export default function toString(v) {
  return JSToGrainVal(grainToString(v));
}
