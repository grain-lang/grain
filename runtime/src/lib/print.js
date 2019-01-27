import { grainToString } from '../utils/utils';

export function print(v) {
  console.log(grainToString(null, v));
  return v;
}

export function makePrint(boundGrainToString) {
  return v => {
    console.log(boundGrainToString(v));
    return v;
  }
}

export function debugPrint(v) {
  console.log(`0x${v.toString(16)} (0b${v.toString(2)})`);
  return v;
}
