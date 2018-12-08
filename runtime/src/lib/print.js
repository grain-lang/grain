import { grainToString } from '../utils/utils';

export function print(v) {
  console.log(grainToString(v));
  return v;
}

export function makePrint(toString) {
  return v => {
    console.log(toString(v));
    return v;
  }
}

export function debugPrint(v) {
  console.log(`0x${v.toString(16)} (0b${v.toString(2)})`);
  return v;
}
