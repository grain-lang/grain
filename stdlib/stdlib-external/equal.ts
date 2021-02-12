import {
  GRAIN_GENERIC_HEAP_TAG_TYPE,

  GRAIN_GENERIC_TAG_MASK,

  GRAIN_STRING_HEAP_TAG,
  GRAIN_CHAR_HEAP_TAG,
  GRAIN_ADT_HEAP_TAG,
  GRAIN_RECORD_HEAP_TAG,
  GRAIN_ARRAY_HEAP_TAG,
  GRAIN_TUPLE_HEAP_TAG
} from './ascutils/tags'

import {
  GRAIN_TRUE,
  GRAIN_FALSE,
} from './ascutils/primitives'

import { isNumber, numberEqual } from './numbers'

const cycleMarker: u32 = 0x80000000

function heapEqualHelp(heapTag: u32, xptr: u32, yptr: u32): bool {
  switch (heapTag) {
    case GRAIN_ADT_HEAP_TAG: {
      // Check if the same constructor variant
      if (load<u32>(xptr, 3 * 4) !== load<u32>(yptr, 3 * 4)) {
        return false
      }

      let xarity = load<u32>(xptr, 4 * 4)
      let yarity = load<u32>(yptr, 4 * 4)

      // Check that the arities are the same
      if (xarity !== yarity) {
        return false
      }

      // Cycle check
      if ((xarity & cycleMarker) === cycleMarker) {
        return true
      }
      store<u32>(xptr, xarity ^ cycleMarker, 4 * 4)
      store<u32>(yptr, yarity ^ cycleMarker, 4 * 4)

      let result = true

      let bytes = xarity * 4
      for (let i: u32 = 0; i < bytes; i += 4) {
        if (!equalHelp(load<u32>(xptr + i, 5 * 4), load<u32>(yptr + i, 5 * 4))) {
          result = false
          break
        }
      }
      store<u32>(xptr, xarity, 4 * 4)
      store<u32>(yptr, yarity, 4 * 4)

      return result
    }
    case GRAIN_RECORD_HEAP_TAG: {
      let xlength = load<u32>(xptr, 3 * 4)
      let ylength = load<u32>(yptr, 3 * 4)

      // Check that the lengths are the same
      if (xlength !== ylength) {
        return false
      }

      // Cycle check
      if ((xlength & cycleMarker) === cycleMarker) {
        return true
      }
      store<u32>(xptr, xlength ^ cycleMarker, 3 * 4)
      store<u32>(yptr, ylength ^ cycleMarker, 3 * 4)

      let result = true

      let bytes = xlength * 4
      for (let i: u32 = 0; i < bytes; i += 4) {
        if (!equalHelp(load<u32>(xptr + i, 4 * 4), load<u32>(yptr + i, 4 * 4))) {
          result = false
          break
        }
      }
      store<u32>(xptr, xlength, 3 * 4)
      store<u32>(yptr, ylength, 3 * 4)

      return result
    }
    case GRAIN_ARRAY_HEAP_TAG: {
      let xlength = load<u32>(xptr, 4)
      let ylength = load<u32>(yptr, 4)

      // Check if the same length
      if (xlength !== ylength) {
        return false
      }

      // Cycle check
      if (xlength & cycleMarker) {
        return true
      }
      store<u32>(xptr, xlength ^ cycleMarker, 4)
      store<u32>(yptr, ylength ^ cycleMarker, 4)

      let result = true
      let bytes = xlength * 4
      for (let i: u32 = 0; i < bytes; i += 4) {
        if (!equalHelp(load<u32>(xptr + i, 2 * 4), load<u32>(yptr + i, 2 * 4))) {
          result = false
          break
        }
      }

      store<u32>(xptr, xlength, 4)
      store<u32>(yptr, ylength, 4)
      return result
    }
    case GRAIN_STRING_HEAP_TAG: {
      let xlength = load<u32>(xptr, 4)
      let ylength = load<u32>(yptr, 4)

      // Check if the same length
      if (xlength !== ylength) {
        return false
      }

      let extra = xlength % 8
      let first = xlength - extra
      for (let i: u32 = 0; i < first; i += 8) {
        if (load<u64>(xptr + i, 2 * 4) !== load<u64>(yptr + i, 2 * 4)) {
          return false
        }
      }
      for (let i: u32 = 0; i < extra; i += 1) {
        if (load<u8>(xptr + first + i, 2 * 4) !== load<u8>(yptr + first + i, 2 * 4)) {
          return false
        }
      }

      return true
    }
    case GRAIN_CHAR_HEAP_TAG: {
      const byte = load<u8>(xptr, 4)
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
      if (memory.compare(xptr + 4, yptr + 4, n) === 0) {
        return true
      }
      return false
    }
    case GRAIN_TUPLE_HEAP_TAG: {
      let xsize = load<u32>(xptr, 4)
      let ysize = load<u32>(yptr, 4)

      if (xsize !== ysize) {
        return false
      }

      if ((xsize & cycleMarker) === cycleMarker) {
        return true
      }

      store<u32>(xptr, xsize ^ cycleMarker, 4)
      store<u32>(yptr, ysize ^ cycleMarker, 4)

      let result = true
      let bytes = xsize * 4
      for (let i: u32 = 0; i < bytes; i += 4) {
        if (!equalHelp(load<u32>(xptr + i, 8), load<u32>(yptr + i, 8))) {
          result = false
          break
        }
      }

      store<u32>(xptr, xsize, 4)
      store<u32>(yptr, ysize, 4)

      return result
    }
    default: {
      // No other implementation
      return xptr === yptr
    }
  }
}

function equalHelp(x: u32, y: u32): bool {
  // Short circuit if value/pointer is the same
  if (x === y) return true
  if (isNumber(x)) return numberEqual(x, y)

  // We only need to worry about heep allocated things since everything else
  // is handled by a simple x == y check at the end
  switch (x & GRAIN_GENERIC_TAG_MASK) {
    case GRAIN_GENERIC_HEAP_TAG_TYPE: {
      return heapEqualHelp(load<u32>(x), x, y)
    }
  }

  return x === y
}

export function equal(x: u32, y: u32): u32 {
  return equalHelp(x, y) ? GRAIN_TRUE : GRAIN_FALSE
}
