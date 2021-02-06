import { throwError, malloc, free } from "../ascutils/grainRuntime"

import { GRAIN_ERR_SYSTEM } from "../ascutils/errors"

import { errno, random_get } from "bindings/wasi"

import { tagSimpleNumber } from "../ascutils/dataStructures"

export function random(): u32 {
  let buf = malloc(4)

  let err = random_get(buf, 4)
  if (err !== errno.SUCCESS) {
    free(buf)
    throwError(GRAIN_ERR_SYSTEM, tagSimpleNumber(err), 0)
  }

  let rand = load<u32>(buf)
  free(buf)
  return tagSimpleNumber(rand)
}
