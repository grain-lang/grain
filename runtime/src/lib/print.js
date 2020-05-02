import { grainToString } from '../utils/utils';
import { GRAIN_VOID } from '../core/primitives';

export function print(v) {
  //console.log(`${grainToString(null, v)} <0x${v.toString(16)}>`);
  console.log(grainToString(null, v))
  return GRAIN_VOID;
}

export function makePrint(boundGrainToString) {
  return v => {
//    console.log(`${boundGrainToString(v)} <0x${v.toString(16)}>`);
    console.log(boundGrainToString(v));
    return GRAIN_VOID;
  }
}

export function debugPrint(v) {
  console.log(`0x${v.toString(16)} (0b${v.toString(2)})`);
  return v;
}
