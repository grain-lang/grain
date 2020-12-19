import { log } from '../ascutils/console'
import { GRAIN_VOID } from '../ascutils/primitives'
import { grainToString } from './string'

export function print(grainValue: u32): u32 {
  log(grainToString(grainValue))
  return GRAIN_VOID
}
