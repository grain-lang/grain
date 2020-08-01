import { throwError } from './ascutils/grainRuntime'
import { GRAIN_ERR_ARRAY_INDEX_OUT_OF_BOUNDS, GRAIN_ERR_INVALID_ARGUMENT } from './ascutils/errors'
import { GRAIN_GENERIC_HEAP_TAG_TYPE } from './ascutils/tags'
import { stringSize, allocateString } from './ascutils/dataStructures'
import { GRAIN_TRUE, GRAIN_FALSE } from './ascutils/primitives'

export function length(s: u32): u32 {
  s = s ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  const size = stringSize(s)

  let len = 0
  let ptr = s + 8
  const end = ptr + size

  while (ptr < end) {
    const byte = load<u8>(ptr)
    if ((byte & 0xC0) !== 0x80) len++
    ptr++
  }

  return len << 1
}

export function byteLength(s: u32): u32 {
  s = s ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  const size = stringSize(s)

  return size << 1
}

export function concat(s1: u32, s2: u32): u32 {
  s1 = s1 ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  s2 = s2 ^ GRAIN_GENERIC_HEAP_TAG_TYPE

  const size1 = stringSize(s1)
  const size2 = stringSize(s2)

  const newString = allocateString(size1 + size2)

  memory.copy(newString + 8, s1 + 8, size1)
  memory.copy(newString + 8 + size1, s2 + 8, size2)

  return newString ^ GRAIN_GENERIC_HEAP_TAG_TYPE
}

export function slice(s: u32, from: i32, to: i32): u32 {
  const len = i32(length(s) >> 1)

  s = s ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  const size = stringSize(s)

  from = from >> 1
  to = to >> 1

  if (from < 0) from += len
  if (to < 0) to += len

  if (from > len || to > len) {
    throwError(GRAIN_ERR_ARRAY_INDEX_OUT_OF_BOUNDS, 0, 0)
  }

  if (to < from) {
    throwError(GRAIN_ERR_INVALID_ARGUMENT, to << 1, 0)
  }

  let ptr = s + 8
  let start = ptr
  let end = ptr
  const stop = ptr + size

  let idx = 0
  while (ptr < stop) {
    const byte = load<u8>(ptr)
    if ((byte & 0xC0) !== 0x80) {
      if (idx === from) start = ptr
      if (idx === to) {
        end = ptr
        break
      }
      idx++
    }
    ptr++
  }
  if (to === len) end = s + 8 + size
  if (from === to) start = end

  const newSize = end - start
  const newString = allocateString(newSize)

  memory.copy(newString + 8, start, newSize)

  return newString ^ GRAIN_GENERIC_HEAP_TAG_TYPE
}

export function contains(s: u32, p: u32): u32 {
  // "Not So Naive" string search algorithm
  // searching phase in O(nm) time complexity
  // slightly (by coefficient) sub-linear in the average case
  // http://igm.univ-mlv.fr/~lecroq/string/node13.html#SECTION00130

  s = s ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  p = p ^ GRAIN_GENERIC_HEAP_TAG_TYPE

  const n = stringSize(s)
  const m = stringSize(p)

  s += 8
  p += 8

  let j: u32 = 0, k: u32, ell: u32

  // Bail if pattern length is longer than input length
  if (m > n) return GRAIN_FALSE

  // Handle very small patterns
  if (m < 2) {
    if (m === 0) return GRAIN_TRUE
    const pat = load<u8>(p)
    while (j < n) if (pat === load<u8>(s + j)) {
      return GRAIN_TRUE
    } else j++
    return GRAIN_FALSE
  }

  // NSM preprocessing
  if (load<u8>(p) === load<u8>(p, 1)) {
    k = 2
    ell = 1
  } else {
    k = 1
    ell = 2
  }

  // NSM searching
  while (j <= n - m) {
    if (load<u8>(p, 1) !== load<u8>(s + j, 1)) {
      j += k
    } else {
      if (memory.compare(p + 2, s + j + 2, m - 2) === 0 &&
        load<u8>(p) === load<u8>(s + j))
        return GRAIN_TRUE
      j += ell
    }
  }
  return GRAIN_FALSE
}

export function startsWith(s: u32, p: u32): u32 {
  s = s ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  p = p ^ GRAIN_GENERIC_HEAP_TAG_TYPE

  const n = stringSize(s)
  const m = stringSize(p)

  s += 8
  p += 8

  // Bail if pattern length is longer than input length
  if (m > n) return GRAIN_FALSE

  return memory.compare(p, s, m) === 0
    ? GRAIN_TRUE
    : GRAIN_FALSE
}

export function endsWith(s: u32, p: u32): u32 {
  s = s ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  p = p ^ GRAIN_GENERIC_HEAP_TAG_TYPE

  const n = stringSize(s)
  const m = stringSize(p)

  s += 8
  p += 8

  // Bail if pattern length is longer than input length
  if (m > n) return GRAIN_FALSE

  return memory.compare(p, s + n - m, m) === 0
    ? GRAIN_TRUE
    : GRAIN_FALSE
}
