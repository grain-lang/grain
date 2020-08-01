/**
  This module implements MurmurHash3 for Grain data types.
  https://en.wikipedia.org/wiki/MurmurHash
*/

import {
  GRAIN_NUMBER_TAG_TYPE,
  GRAIN_CONST_TAG_TYPE,
  GRAIN_TUPLE_TAG_TYPE,
  GRAIN_LAMBDA_TAG_TYPE,
  GRAIN_GENERIC_HEAP_TAG_TYPE,

  GRAIN_NUMBER_TAG_MASK,
  GRAIN_GENERIC_TAG_MASK,

  GRAIN_STRING_HEAP_TAG,
  GRAIN_DOM_ELEM_TAG,
  GRAIN_ADT_HEAP_TAG,
  GRAIN_RECORD_HEAP_TAG,
  GRAIN_ARRAY_HEAP_TAG,
} from './ascutils/tags'

import {
  GRAIN_TRUE,
  GRAIN_FALSE,
  GRAIN_VOID,
} from './ascutils/primitives'

const seed: u32 = 0xe444

const MAX_HASH_DEPTH: u32 = 31

const c1: u32 = 0xcc9e2d51
const c2: u32 = 0x1b873593
const r1: u32 = 15
const r2: u32 = 13
const m: u32 = 5
const n: u32 = 0xe6546b64

let h = seed

function hash32(k: u32): void {
  k *= c1
  k = rotl<i32>(k, r1)
  k *= c2

  h = h ^ k
  h = rotl<i32>(h, r2)
  h = (h * m) + n
}

function hashRemaining(r: u32): void {
  // Note: wasm is little-endian so no swap is necessary
  r *= c1
  r = rotl<i32>(r, r1)
  r *= c2

  h = h ^ r
}

function finalize(len: u32): void {
  h = h ^ len

  h = h ^ (h >>> 16)
  h *= 0x85ebca6b
  h = h ^ (h >>> 13)
  h *= 0xc2b2ae35
  h = h ^ (h >>> 16)
}

function hashOne(val: u32, depth: u32): void {
  if (depth > MAX_HASH_DEPTH) return

  if (!(val & GRAIN_NUMBER_TAG_MASK)) {
    hash32(val)
  } else if ((val & GRAIN_GENERIC_TAG_MASK) === GRAIN_TUPLE_TAG_TYPE) {
    let tuplePtr = val ^ GRAIN_TUPLE_TAG_TYPE
    let tupleLength = load<u32>(tuplePtr)
    let l = tupleLength * 4
    for (let i: u32 = 0; i < l; i += 4) {
      hashOne(load<u32>(tuplePtr + i, 4), depth + 1)
    }
    finalize(tupleLength)
  } else if ((val & GRAIN_GENERIC_TAG_MASK) === GRAIN_LAMBDA_TAG_TYPE) {
    hash32(val)
  } else if ((val & GRAIN_GENERIC_TAG_MASK) === GRAIN_GENERIC_HEAP_TAG_TYPE) {
    let heapPtr = val ^ GRAIN_GENERIC_HEAP_TAG_TYPE
    switch (load<u32>(heapPtr)) {
      case GRAIN_STRING_HEAP_TAG: {
        let length = load<u32>(heapPtr, 4);
        let extra = length % 4
        let l = length - extra
        for (let i: u32 = 0; i < l; i += 4) {
          hash32(load<u32>(heapPtr + i, 8))
        }
        let rem: u32 = 0
        for (let i: u32 = 0; i < extra; i++) {
          rem = rem << 8
          rem = rem | load<u8>(heapPtr + l + i, 8)
        }
        if (rem != 0) hashRemaining(rem)
        finalize(length)
        break
      }
      case GRAIN_DOM_ELEM_TAG: {
        hash32(heapPtr)
        break
      }
      case GRAIN_ADT_HEAP_TAG: {
        // moduleId
        hash32(load<u32>(heapPtr, 4))
        // typeId
        hash32(load<u32>(heapPtr, 8))
        // variantId
        hash32(load<u32>(heapPtr, 12))

        let arity = load<u32>(heapPtr, 16)

        let a = arity * 4
        for (let i: u32 = 0; i < a; i += 4) {
          hashOne(load<u32>(heapPtr + i, 5 * 4), depth + 1)
        }

        finalize(arity)
        break
      }
      case GRAIN_RECORD_HEAP_TAG: {
        // moduleId
        hash32(load<u32>(heapPtr, 4))
        // typeId
        hash32(load<u32>(heapPtr, 8))

        let arity = load<u32>(heapPtr, 12)

        let a = arity * 4
        for (let i: u32 = 0; i < a; i += 4) {
          hashOne(load<u32>(heapPtr + i, 4 * 4), depth + 1)
        }
        finalize(arity)
        break
      }
      case GRAIN_ARRAY_HEAP_TAG: {
        let arity = load<u32>(heapPtr, 4)

        let a = arity * 4
        for (let i: u32 = 0; i < a; i += 4) {
          hashOne(load<u32>(heapPtr + i, 2 * 4), depth + 1)
        }
        finalize(arity)
        break
      }
      default: {
        hash32(heapPtr)
      }
    }
  } else if (val === GRAIN_TRUE) {
    hash32(val)
  } else if (val === GRAIN_FALSE) {
    hash32(val)
  } else if (val === GRAIN_VOID) {
    hash32(val)
  } else {
    hash32(val)
  }
}

export function hash(a: u32): u32 {
  h = seed

  hashOne(a, 0)
  finalize(0)

  // Tag the number on the way out.
  // Since Grain has proper modulus, negative numbers are okay.
  return h << 1
}
