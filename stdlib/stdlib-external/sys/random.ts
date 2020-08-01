import { throwError } from "../ascutils/grainRuntime"

import { GRAIN_ERR_SYSTEM } from "../ascutils/errors"

import { errno, random_get } from "bindings/wasi"

export function random(): u32 {
  let buf = memory.data(4)

  let err = random_get(buf, 4)
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, err << 1, 0)
  }

  let rand = load<u32>(buf)
  return rand << 1
}
