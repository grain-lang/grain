import { free, throwError } from "../ascutils/grainRuntime"

import { GRAIN_ERR_SYSTEM } from "../ascutils/errors"

import { GRAIN_GENERIC_HEAP_TAG_TYPE } from "../ascutils/tags"

import { allocateInt64 } from "../ascutils/dataStructures"

import { errno, clock_time_get, clockid } from "bindings/wasi"

function getClockTime(clockid: u32, precision: u64): u32 {
  let int64Ptr = allocateInt64();
  let timePtr = int64Ptr + 4;
  let err = clock_time_get(clockid, precision, timePtr);
  if (err !== errno.SUCCESS) {
    free(int64Ptr);
    throwError(GRAIN_ERR_SYSTEM, err, 0);
  }

  return int64Ptr | GRAIN_GENERIC_HEAP_TAG_TYPE;
}

export function realTime(): u32 {
  return getClockTime(clockid.REALTIME, 1000)
}

export function monotonicTime(): u32 {
  return getClockTime(clockid.MONOTONIC, 1)
}

export function processCpuTime(): u32 {
  return getClockTime(clockid.PROCESS_CPUTIME_ID, 1)
}

export function threadCpuTime(): u32 {
  return getClockTime(clockid.THREAD_CPUTIME_ID, 1)
}
