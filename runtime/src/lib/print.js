import { grainToString } from '../utils/utils';

export default function print(v) {
  console.log(grainToString(v));
  return v;
}
