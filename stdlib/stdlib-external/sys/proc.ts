import { throwError } from "../ascutils/grainRuntime";

import {
  errno,
  proc_exit,
  proc_raise,
  sched_yield,
} from "bindings/wasi";

import { GRAIN_ERR_SYSTEM } from "../ascutils/errors";

import { GRAIN_VOID } from "../ascutils/primitives";

import { tagSimpleNumber } from "../ascutils/dataStructures"

export function exit(code: u32): u32 {
  code = code >> 1
  proc_exit(code)
  return GRAIN_VOID
}

export function sigRaise(signalPtr: u32): u32 {
  let signal = load<u32>(signalPtr, 3 * 4) >> 1
  let err = proc_raise(u8(signal))
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }
  return GRAIN_VOID
}

export function schedYield(): u32 {
  let err = sched_yield()
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }
  return GRAIN_VOID
}
