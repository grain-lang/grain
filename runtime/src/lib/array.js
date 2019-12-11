import { view } from '../runtime';
import { JSToGrainVal } from '../utils/utils';

export function arrayLength(array) {
  return JSToGrainVal(view[((array ^ 3) / 4) + 1])
}