import { throwError } from './ascutils/grainRuntime'
import { GRAIN_ERR_ARRAY_INDEX_OUT_OF_BOUNDS, GRAIN_ERR_INVALID_ARGUMENT } from './ascutils/errors'
import { stringSize, allocateString, allocateChar, allocateArray, storeInArray, tagSimpleNumber } from './ascutils/dataStructures'
import { GRAIN_TRUE, GRAIN_FALSE } from './ascutils/primitives'

export function length(s: u32): u32 {
  const size = stringSize(s)

  let len = 0
  let ptr = s + 8
  const end = ptr + size

  while (ptr < end) {
    const byte = load<u8>(ptr)
    if ((byte & 0xC0) !== 0x80) len++
    ptr++
  }

  return tagSimpleNumber(len)
}

export function byteLength(s: u32): u32 {
  const size = stringSize(s)

  return tagSimpleNumber(size)
}

export function indexOf(p: u32, s: u32): u32 {
  const size = stringSize(s)
  const psize = stringSize(p)

  if (psize > size) return tagSimpleNumber(-1)

  let idx = 0
  let ptr = s + 8
  let pptr = p + 8
  const end = ptr + size - psize + 1

  while (ptr < end) {
    if (memory.compare(ptr, pptr, psize) === 0) {
      return tagSimpleNumber(idx)
    }
    idx++
    const byte = load<u8>(ptr)
    if ((byte & 0x80) === 0x00) {
      ptr += 1
    } else if ((byte & 0xF0) === 0xF0) {
      ptr += 4
    } else if ((byte & 0xE0) === 0xE0) {
      ptr += 3
    } else {
      ptr += 2
    }
  }

  return tagSimpleNumber(-1)
}

function explodeHelp(s: u32, chars: bool): u32 {
  const size = stringSize(s)
  const len = length(s) >> 1
  
  let ptr = s + 8
  const end = ptr + size

  let arr = allocateArray(len)
  let arrIdx = 0

  while (ptr < end) {
    const byte = load<u8>(ptr)
    let n: u32
    if ((byte & 0x80) === 0x00) {
      n = 1
    } else if ((byte & 0xF0) === 0xF0) {
      n = 4
    } else if ((byte & 0xE0) === 0xE0) {
      n = 3
    } else {
      n = 2
    }

    let c: u32
    if (chars) {
      c = allocateChar()
      memory.copy(c + 4, ptr, n)
    } else {
      c = allocateString(n)
      memory.copy(c + 8, ptr, n)
    }
    storeInArray(arr, arrIdx, c)
    arrIdx++
    ptr += n
  }

  return arr
}

export function explode(str: u32): u32 {
  return explodeHelp(str, true)
}

export function implode(arr: u32): u32 {
  let arrLength = load<u32>(arr, 4)

  let stringByteLength = 0

  for (let i: u32 = 0; i < arrLength; i++) {
    const char = load<u32>(arr + (i << 2), 8)
    const byte = load<u8>(char, 4)

    let n: u32
    if ((byte & 0x80) === 0x00) {
      n = 1
    } else if ((byte & 0xF0) === 0xF0) {
      n = 4
    } else if ((byte & 0xE0) === 0xE0) {
      n = 3
    } else {
      n = 2
    }

    stringByteLength += n
  }

  let str = allocateString(stringByteLength)
  let offset = 8

  for (let i: u32 = 0; i < arrLength; i++) {
    const char = load<u32>(arr + (i << 2), 8)
    const byte = load<u8>(char, 4)

    let n: u32
    if ((byte & 0x80) === 0x00) {
      n = 1
    } else if ((byte & 0xF0) === 0xF0) {
      n = 4
    } else if ((byte & 0xE0) === 0xE0) {
      n = 3
    } else {
      n = 2
    }

    memory.copy(str + offset, char + 4, n)
    offset += n
  }

  return str
}

export function split(p: u32, s: u32): u32 {
  const size = stringSize(s)
  const psize = stringSize(p)

  if (psize === 0) {
    return explodeHelp(s, false)
  }
  
  if (psize > size) {
    let ptr = allocateArray(1)
    storeInArray(ptr, 0, s)
    return ptr
  }

  let ptr = s + 8
  let pptr = p + 8
  const end = ptr + size - psize + 1

  let numStrings = 1

  while (ptr < end) {
    if (memory.compare(ptr, pptr, psize) === 0) {
      numStrings++
    }
    const byte = load<u8>(ptr)
    if ((byte & 0x80) === 0x00) {
      ptr += 1
    } else if ((byte & 0xF0) === 0xF0) {
      ptr += 4
    } else if ((byte & 0xE0) === 0xE0) {
      ptr += 3
    } else {
      ptr += 2
    }
  }

  ptr = s + 8
  let last = ptr
  let arr = allocateArray(numStrings)
  let arrIdx = 0

  while (ptr < end) {
    if (memory.compare(ptr, pptr, psize) === 0) {
      let strSize = ptr - last
      let str = allocateString(strSize)
      memory.copy(str + 8, last, strSize)
      storeInArray(arr, arrIdx, str)
      arrIdx++
      ptr += psize
      last = ptr
      continue
    }
    const byte = load<u8>(ptr)
    if ((byte & 0x80) === 0x00) {
      ptr += 1
    } else if ((byte & 0xF0) === 0xF0) {
      ptr += 4
    } else if ((byte & 0xE0) === 0xE0) {
      ptr += 3
    } else {
      ptr += 2
    }
  }

  // Grab last string
  let strSize = s + 8 + size - last
  let lastStr = allocateString(strSize)
  memory.copy(lastStr + 8, last, strSize)
  storeInArray(arr, arrIdx, lastStr)

  return arr
}

export function slice(from: i32, to: i32, s: u32): u32 {
  const len = i32(length(s) >> 1)

  const size = stringSize(s)

  from = from >> 1
  to = to >> 1

  if (from < 0) from += len
  if (to < 0) to += len

  if (from > len || to > len) {
    throwError(GRAIN_ERR_ARRAY_INDEX_OUT_OF_BOUNDS, 0, 0)
  }

  if (to < from) {
    throwError(GRAIN_ERR_INVALID_ARGUMENT, tagSimpleNumber(to), 0)
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

  return newString
}

export function contains(p: u32, s: u32): u32 {
  // "Not So Naive" string search algorithm
  // searching phase in O(nm) time complexity
  // slightly (by coefficient) sub-linear in the average case
  // http://igm.univ-mlv.fr/~lecroq/string/node13.html#SECTION00130

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

export function startsWith(p: u32, s: u32): u32 {
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

export function endsWith(p: u32, s: u32): u32 {
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
