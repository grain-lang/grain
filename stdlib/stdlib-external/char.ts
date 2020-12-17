import { throwError } from './ascutils/grainRuntime'
import { GRAIN_ERR_MALFORMED_UTF8 } from './ascutils/errors'
import { GRAIN_GENERIC_HEAP_TAG_TYPE } from './ascutils/tags'
import { allocateChar, allocateString } from './ascutils/dataStructures'

// Algorithm from https://encoding.spec.whatwg.org/#utf-8-decoder
export function code(c: u32): u32 {
  c = c ^ GRAIN_GENERIC_HEAP_TAG_TYPE

  let codePoint: u32 = 0
  let bytesSeen: u32 = 0
  let bytesNeeded: u32 = 0
  let lowerBoundary: u8 = 0x80
  let upperBoundary: u8 = 0xBF

  let offset: u32 = 0

  while (true) {
    const byte = load<u8>(c + offset++, 4)
    if (bytesNeeded === 0) {
      if (byte >= 0x00 && byte <= 0x7F) {
        return byte << 1
      } else if (byte >= 0xC2 && byte <= 0xDF) {
        bytesNeeded = 1
        codePoint = byte & 0x1F
      } else if (byte >= 0xE0 && byte <= 0xEF) {
        if (byte === 0xE0) lowerBoundary = 0xA0
        if (byte === 0xED) upperBoundary = 0x9F
        bytesNeeded = 2
        codePoint = byte & 0xF
      } else if (byte >= 0xF0 && byte <= 0xF4) {
        if (byte === 0xF0) lowerBoundary = 0x90
        if (byte === 0xF4) upperBoundary = 0x8F
        bytesNeeded = 3
        codePoint = byte & 0x7
      } else {
        throwError(GRAIN_ERR_MALFORMED_UTF8, 0, 0)
        unreachable()
      }
      continue
    }
    if (!(lowerBoundary <= byte && byte <= upperBoundary)) {
      throwError(GRAIN_ERR_MALFORMED_UTF8, 0, 0)
    }
    lowerBoundary = 0x80
    upperBoundary = 0xBF
    codePoint = (codePoint << 6) | (byte & 0x3F)
    bytesSeen++
    if (bytesSeen === bytesNeeded) {
      return codePoint << 1
    }
  }
}

// Algorithm from https://encoding.spec.whatwg.org/#utf-8-encoder
export function fromCode(code: u32): u32 {
  code = code >>> 1
  if (code < 0x80) {
    let char = allocateChar()
    store<u8>(char, code, 4)
    return char ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  }
  let count: u32
  let offset: u32
  if (code <= 0x07FF) {
    count = 1
    offset = 0xC0
  } else if (code <= 0xFFFF) {
    count = 2
    offset = 0xE0
  } else {
    count = 3
    offset = 0xF0
  }
  let char = allocateChar()
  store<u8>(char, (code >>> (6 * count)) + offset, 4)

  let n = 0
  while (count > 0) {
    n++
    let temp = code >>> (6 * (count - 1))
    store<u8>(char + n, 0x80 | (temp & 0x3F), 4)
    count--
  }

  return char ^ GRAIN_GENERIC_HEAP_TAG_TYPE
}

export function toString(c: u32): u32 {
  c = c ^ GRAIN_GENERIC_HEAP_TAG_TYPE
  let byte = load<u8>(c, 4)
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
  let str = allocateString(n)
  memory.copy(str + 8, c + 4, n)
  return str ^ GRAIN_GENERIC_HEAP_TAG_TYPE
}
