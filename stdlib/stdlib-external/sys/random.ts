import { malloc, free, throwError } from "../ascutils/grainRuntime";

import {
  errno,
  random_get,
} from "bindings/wasi";

import { GRAIN_ERR_SYSTEM } from "../ascutils/errors";


export function random(): u32 {
  let buf = malloc(4)
  
  let err = random_get(buf, 4)
  if (err !== errno.SUCCESS) {
    free(buf)
    throwError(GRAIN_ERR_SYSTEM, err << 1, 0)
  }

  let rand = load<u32>(buf)
  
  free(buf)
  
  return rand << 1
}
