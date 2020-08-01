import { malloc, free, throwError } from "../ascutils/grainRuntime";

import { GRAIN_ERR_SYSTEM } from "../ascutils/errors";

import { GRAIN_GENERIC_HEAP_TAG_TYPE } from "../ascutils/tags";

import { allocateArray, allocateString } from "../ascutils/dataStructures";

import {
  errno,
  args_get,
  args_sizes_get,
  environ_get,
  environ_sizes_get,
} from "bindings/wasi";

export function argv(): u32 {
  let argcPtr = memory.data(8);
  let argvBufSizePtr = argcPtr + 4;

  let err = args_sizes_get(argcPtr, argvBufSizePtr);
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, err << 1, 0);
  }

  let argc = load<u32>(argcPtr);
  let argvBufSize = load<u32>(argvBufSizePtr);

  let argvPtr = malloc(argc * 4);
  let argvBufPtr = malloc(argvBufSize);

  err = args_get(argvPtr, argvBufPtr);
  if (err !== errno.SUCCESS) {
    free(argvPtr);
    free(argvBufPtr);
    throwError(GRAIN_ERR_SYSTEM, err << 1, 0);
  }

  let arr = allocateArray(argc);

  let argsLength = argc * 4;
  for (let i: u32; i < argsLength; i += 4) {
    let strPtr = load<u32>(argvPtr + i);
    let strLength = 0;
    while (load<u8>(strPtr + strLength) !== 0) {
      strLength += 1;
    }

    let grainStrPtr = allocateString(strLength);
    memory.copy(grainStrPtr + 8, strPtr, strLength);

    store<u32>(arr + i, grainStrPtr | GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4);
  }

  free(argvPtr);
  free(argvBufPtr);

  return arr | GRAIN_GENERIC_HEAP_TAG_TYPE;
}

export function env(): u32 {
  let envcPtr = memory.data(8);
  let envvBufSizePtr = envcPtr + 4;

  let err = environ_sizes_get(envcPtr, envvBufSizePtr);
  if (err !== errno.SUCCESS) {
    throwError(GRAIN_ERR_SYSTEM, err << 1, 0);
  }

  let envc = load<u32>(envcPtr);
  let envvBufSize = load<u32>(envvBufSizePtr);

  let envvPtr = malloc(envc * 4);
  let envvBufPtr = malloc(envvBufSize);

  err = environ_get(envvPtr, envvBufPtr);
  if (err !== errno.SUCCESS) {
    free(envvPtr);
    free(envvBufPtr);
    throwError(GRAIN_ERR_SYSTEM, err << 1, 0);
  }

  let arr = allocateArray(envc);

  let envsLength = envc * 4;
  for (let i: u32; i < envsLength; i += 4) {
    let strPtr = load<u32>(envvPtr + i);
    let strLength = 0;
    while (load<u8>(strPtr + strLength) !== 0) {
      strLength += 1;
    }

    let grainStrPtr = allocateString(strLength);
    memory.copy(grainStrPtr + 8, strPtr, strLength);

    store<u32>(arr + i, grainStrPtr | GRAIN_GENERIC_HEAP_TAG_TYPE, 2 * 4);
  }

  free(envvPtr);
  free(envvBufPtr);

  return arr | GRAIN_GENERIC_HEAP_TAG_TYPE;
}
