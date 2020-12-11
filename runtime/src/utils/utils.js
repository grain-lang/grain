import { memory, encoder, decoder, managedMemory } from '../runtime';
import { grainHeapAllocate } from '../core/heap';
import { GrainError } from '../errors/errors';
import { grainDOMRefs } from '../lib/DOM';

import {
  GRAIN_TUPLE_TAG_TYPE,
  GRAIN_LAMBDA_TAG_TYPE,
  GRAIN_GENERIC_HEAP_TAG_TYPE,
  GRAIN_DOM_ELEM_TAG,
  GRAIN_STRING_HEAP_TAG,
  GRAIN_ADT_HEAP_TAG,
  GRAIN_RECORD_HEAP_TAG,
  GRAIN_ARRAY_HEAP_TAG,
  GRAIN_BOXED_NUM_HEAP_TAG,
  GRAIN_INT32_BOXED_NUM_TAG,
  GRAIN_INT64_BOXED_NUM_TAG,
  GRAIN_RATIONAL_BOXED_NUM_TAG,
  GRAIN_FLOAT32_BOXED_NUM_TAG,
  GRAIN_FLOAT64_BOXED_NUM_TAG,
} from '../core/tags';

import {
  GRAIN_TRUE,
  GRAIN_FALSE,
  GRAIN_VOID
} from '../core/primitives';

export function grainListToString(runtime, n) {
  const view = managedMemory.view;

  let cur = n;
  let printedVals = [];

  while (true) {
    let x = cur / 4;
    let variantId = view[x + 3] >> 1;
    if (variantId === 0) {
      break;
    } else {
      printedVals.push(grainToString(runtime, view[x + 5]));
      cur = view[x + 6] ^ 3;
    }
  }

  return `[${printedVals.join(', ' )}]`;
}

/**
 * Formats a number as hex. Useful for debug printing.
 *
 * @param {Number} minWidth [0] - Minimum length of formatted number (will be padded w/ zeros)
 */
export function toHex(n, minWidth) {
  let ret = (new Number(n >>> 0)).toString(16);
  if (minWidth && ret.length < minWidth) {
    ret = '0'.repeat(minWidth - ret.length)
  }
  return ret;
}

/**
 * Formats a number as binary. Useful for debug printing.
 *
 * @param {Number} minWidth [0] - Minimum length of formatted number (will be padded w/ zeros)
 */
export function toBinary(n, minWidth) {
  // Useful for debug printing
  let ret = (n >>> 0).toString(2);
  if (minWidth && ret.length < minWidth) {
    ret = '0'.repeat(minWidth - ret.length) + ret;
  }
  return ret;
}

function float64At(idx) {
  const f64view = managedMemory.f64view;

  if (idx % 8 == 0) {
    // if aligned, return without extra allocations
    return f64view[(idx / 8) + 1]
  }
  // TODO: (#309) Verify that we're always 8-byte aligned and don't need this case
  // not 8-byte aligned. Need load into temporary buffer
  let tmpbuf = new ArrayBuffer(8)
  let tmpview32 = new Uint32Array(tmpbuf)
  let tmpview = new Float64Array(tmpbuf)
  const view = managedMemory.view;

  tmpview32[0] = view[(idx / 4) + 2]
  tmpview32[1] = view[(idx / 4) + 3]
  return tmpview[0]
}

export function grainHeapValueToString(runtime, n) {
  const view = managedMemory.view;
  const uview = managedMemory.uview;
  const f32view = managedMemory.f32view;
  switch (view[n / 4]) {
    case GRAIN_STRING_HEAP_TAG: {
      let byteView = managedMemory.u8view;
      let length = view[(n / 4) + 1];
      let slice = byteView.slice(n + 8, n + 8 + length);
      return `"${decoder.decode(slice)}"`;
    }
    case GRAIN_DOM_ELEM_TAG: {
      return grainDOMRefs[view[(n + 4) / 4]].toString();
    }
    case GRAIN_ADT_HEAP_TAG: {
      let x = n / 4;
      // ADT string coercion is tricky, so these log statements can help
      // debug issues which might crop up:
      // [ <value type tag>, <module_tag>, <type_tag>, <variant_tag>, <arity>, elts ... ]
      // console.log(`<ADT Value: (${view[x + 1]}, ${view[x + 2]}, ${view[x + 3]}, ${view[x + 4]})>`);
      if (runtime) {
        // In-memory tags are tagged ints
        let moduleId = view[x + 1] >> 1;
        let typeId = view[x + 2] >> 1;
        let variantId = view[x + 3] >> 1;
        // console.log(`\tValue Type: ${view[x]}`);
        // console.log(`\tModules: ${JSON.stringify(runtime.idMap)}`);
        // console.log(`\tModule ID: ${moduleId}; Type ID: ${typeId}; Variant ID: ${variantId}`);
        let moduleName = runtime.idMap[moduleId];
        // console.log(`\tModule Name: ${moduleName}`);
        let module = runtime.modules[moduleName];
        // console.log(`\tModule: ${module}`);
        let tyinfo = module.types[typeId];
        // console.log(`\tType Info: ${JSON.stringify(tyinfo)}`);

        if (!tyinfo || Object.keys(tyinfo).length === 0) return '<adt value>';

        let info = tyinfo[variantId];
        // console.log(`\tVariant: ${info}`);

        if (!info) return '<adt value>';
        
        let [variantName, arity] = info;

        // Dirty hack to support list printing
        if (variantName === '[...]') return grainListToString(runtime, n);

        let printedVals = [];
        for (let i = 0; i < arity; ++i) {
          printedVals.push(grainToString(runtime, view[x + 5 + i]));
        }
        if (arity === 0) {
          return variantName;
        } else {
          return `${variantName}(${printedVals.join(", ")})`;
        }
      }
      return "<adt value>";
    }
    case GRAIN_RECORD_HEAP_TAG: {
      let x = n / 4;
      // Record string coercion is tricky, so these log statements can help debug issues which might crop up
      if (runtime) {
        // In-memory tags are tagged ints
        let moduleId = view[x + 1] >> 1;
        let typeId = view[x + 2] >> 1;
        // console.log(`\tModules: ${JSON.stringify(runtime.idMap)}`);
        let moduleName = runtime.idMap[moduleId];
        // console.log(`\tModule Name: ${moduleName}`);
        let module = runtime.modules[moduleName];
        // console.log(`\tModule: ${module}`);
        let tyinfo = module.types[typeId];
        // console.log(`\tType Info: ${JSON.stringify(tyinfo)}`);

        if (Object.keys(tyinfo).length === 0) return '<record value>'

        let values = [];
        for (let [field, idx] of Object.entries(tyinfo)) {
          values.push(`${field}: ${grainToString(runtime, view[x + 4 + idx]).replace(/\n/g, '\n  ')}`)
        }
        return `{\n  ${values.join(',\n  ')}\n}`
      }
      return "<record value>";
    }
    case GRAIN_ARRAY_HEAP_TAG: {
      let x = n / 4;

      let arity = view[x + 1];

      let values = [];
      for (let i = 0; i < arity; i++) {
        values.push(grainToString(runtime, view[x + 2 + i]))
      }
      return `[> ${values.join(', ')}]`
    }
    case GRAIN_BOXED_NUM_HEAP_TAG: {
      let x = n / 4
      let tag = view[x + 1]
      switch (tag) {
        case GRAIN_INT32_BOXED_NUM_TAG: {
          return view[x + 2].toString(10)
        }
        case GRAIN_INT64_BOXED_NUM_TAG: {
          let low = uview[x + 2]
          let high = view[x + 3]
          let negative = high < 0

          let digits = []
          if (negative) {
            high = ~high
            low = ~low + 1
          }

          let digit = ((high % 10) * (2 ** 32) + low) % 10
          digits.unshift(digit)

          while (low >= 10 || high > 0) {
            low = Math.floor(((high % 10) * (2 ** 32) + low) / 10)
            high = Math.floor(high / 10)
            digit = ((high % 10) * (2 ** 32) + low) % 10
            digits.unshift(digit)
          }

          if (negative) {
            digits.unshift('-')
          }

          return digits.join('')
        }
        case GRAIN_RATIONAL_BOXED_NUM_TAG: {
          let numerator = view[x + 2]
          let denominator = view[x + 3]
          return `${numerator.toString(10)}/${denominator.toString(10)}`
        }
        case GRAIN_FLOAT32_BOXED_NUM_TAG: {
          return f32view[x + 2].toString(10)
        }
        case GRAIN_FLOAT64_BOXED_NUM_TAG: {
          return float64At(x * 4).toString(10)
        }
      }
    }
    default: {
      return `<unknown heap type: ${view[n / 4]}>`;
    }
  }
}

export function grainToString(runtime, n) {
  const view = managedMemory.view;
  if (!(n & 1)) {
    return (n >> 1).toString();
  } else if ((n & 7) === GRAIN_TUPLE_TAG_TYPE) {
    let tupleIdx = (n ^ 1) / 4;
    let tupleLength = view[tupleIdx];
    if (tupleLength & 0x80000000) {
      return `<cyclic tuple ${n & 0x7FFFFFFF}>`;
    } else {
      view[tupleIdx] |= 0x80000000;
      let elts = [];
      for (let i = 0; i < tupleLength; ++i) {
        elts.push(grainToString(runtime, view[tupleIdx + i + 1]));
      }
      if (elts.length == 1) {
        elts.push("\b");
      }
      view[tupleIdx] = tupleLength;
      return `(${elts.join(", ")})`;
    }
  } else if ((n & 7) === GRAIN_LAMBDA_TAG_TYPE) {
    return "<lambda>";
  } else if ((n & 7) === GRAIN_GENERIC_HEAP_TAG_TYPE) {
    return grainHeapValueToString(runtime, n ^ 3);
  } else if ((n === GRAIN_TRUE)) {
    return "true";
  } else if (n === GRAIN_FALSE) {
    return "false";
  } else if (n === GRAIN_VOID) {
    return "void";
  } else {
    return `<Unknown value: 0x${n}>`;
  }
}

// TODO: Move
class GrainAdtValue {
  constructor(elts) {
    this._elts = elts;
  }

  get elts() {
    return this._elts;
  }
}

GrainAdtValue.prototype.toString = function() {
  if (this._elts.length === 1) {
    return this._elts[0];
  } else {
    return `${this._elts[0]}(${this._elts.slice(1).join(", ")})`;
  }
}

export function grainBoxedNumberToJSVal(runtime, n) {
  const view = managedMemory.view;
  const uview = managedMemory.uview;
  const f32view = managedMemory.f32view;
  let nInt = n / 4
  switch (view[nInt + 1]) {
    case GRAIN_FLOAT32_BOXED_NUM_TAG:
      return f32view[nInt + 2]
    case GRAIN_FLOAT64_BOXED_NUM_TAG:
      return float64At(n + 8)
    case GRAIN_RATIONAL_BOXED_NUM_TAG:
      let numerator = view[nInt + 2]
      let denominator = view[nInt + 3]
      return numerator / denominator
    case GRAIN_INT32_BOXED_NUM_TAG:
      return view[nInt + 2]
    case GRAIN_INT64_BOXED_NUM_TAG:
      let low = uview[nInt + 2]
      let high = view[nInt + 3]
      let negative = high < 0

      if (negative) {
        high = ~high
        low = ~low + 1
      }
      return (high * (2 ** 32) + low) * (negative ? -1 : 1)
  }
}

export function grainHeapValToJSVal(runtime, n) {
  const view = managedMemory.view;
  switch (view[n / 4]) {
  case GRAIN_BOXED_NUM_HEAP_TAG:
    return grainBoxedNumberToJSVal(runtime, n)
  case GRAIN_STRING_HEAP_TAG:
    let byteView = managedMemory.u8view;
    let length = view[(n / 4) + 1];
    let slice = byteView.slice(n + 8, n + 8 + length);
    return decoder.decode(slice);
  case GRAIN_DOM_ELEM_TAG:
    let ref = n / 4;
    return grainDOMRefs[view[ref + 1]];
  case GRAIN_ADT_HEAP_TAG:
    // TODO: Make this reversible
    let x = n / 4;
    //console.log(`<ADT Value: (${view[x + 1]}, ${view[x + 2]}, ${view[x + 3]}, ${view[x + 4]})>`);
    if (runtime) {
      let info = grainAdtInfo(runtime, n);
      //console.log(`\tVariant: ${info}`);
      let [variantName, arity] = info;
      let ret = [variantName];
      for (let i = 0; i < arity; ++i) {
        ret.push(grainToJSVal(runtime, view[x + 5 + i]));
      }
      return new GrainAdtValue(ret);
    }
  default:
    console.warn(`Unknown heap tag at ${n / 4}: ${view[n / 4]}`);
    return undefined;
  }
}

export function grainAdtInfo(runtime, n) {
  const view = managedMemory.view;
  let x = n / 4;
  if (runtime) {
    // In-memory tags are tagged ints
    let moduleId = view[x + 1] >> 1;
    let typeId = view[x + 2] >> 1;
    let variantId = view[x + 3] >> 1;
    let moduleName = runtime.idMap[moduleId];
    //console.log(`\tModule Name: ${moduleName}`);
    let module = runtime.modules[moduleName];
    //console.log(`\tModule: ${module}`);
    let tyinfo = module.types[typeId];
    //console.log(`\tType Info: ${JSON.stringify(tyinfo)}`);
    let info = tyinfo[variantId];
    //console.log(`\tVariant: ${info}`);
    return info;
  }
  return null;
}

export function grainTupleToJSVal(runtime, n) {
  const view = managedMemory.view;
  let tupleIdx = n / 4;
  let tupleLength = view[tupleIdx];
  if (tupleLength & 0x80000000) {
    return Symbol.for('cyclic');
  } else {
    view[tupleIdx] |= 0x80000000;
    let elts = [];
    for (let i = 0; i < tupleLength; ++i) {
      let value = grainToJSVal(runtime, view[tupleIdx + i + 1])
      if (value === Symbol.for('cyclic')) {
        elts.push(elts);
      } else {
        elts.push(value);
      }
    }
    view[tupleIdx] = tupleLength;
    return elts;
  }
}

export function grainToJSVal(runtime, x) {
  if (!(x & 1)) {
    return x >> 1;
  } else if ((x & 7) == 5) {
    return () => {
      throw new GrainError('Computed Grain functions are not callable from JavaScript.')
    }
  } else if ((x & 7) === 3) {
    return grainHeapValToJSVal(runtime, x ^ 3);
  } else if ((x & 7) === GRAIN_TUPLE_TAG_TYPE) {
    return grainTupleToJSVal(runtime, x ^ GRAIN_TUPLE_TAG_TYPE);
  } else if ((x === GRAIN_TRUE)) {
    return true;
  } else if (x === GRAIN_FALSE) {
    return false;
  } else if (x === GRAIN_VOID) {
    return null;
  } else {
    console.warn(`Unknown Grain value: ${x} (0x${x.toString(16)})`);
    return undefined;
  }
}

export function JSToGrainVal(v) {
  const view = managedMemory.view;
  if (typeof v === "number") {
    if (!Number.isInteger(v)) {
      // not an integer, so just pun into a float64
      let userPtr = managedMemory.malloc(4 * 4)
      let ptr = userPtr / 4
      view[ptr] = GRAIN_BOXED_NUM_HEAP_TAG
      view[ptr + 1] = GRAIN_FLOAT64_BOXED_NUM_TAG
      let tmpbuf = new ArrayBuffer(8)
      let f64tmpbuf = new Float64Array(tmpbuf)
      let i32tmpbuf = new Int32Array(tmpbuf)
      f64tmpbuf[0] = v
      view[ptr + 2] = i32tmpbuf[0]
      view[ptr + 3] = i32tmpbuf[1]
      return userPtr | GRAIN_GENERIC_HEAP_TAG_TYPE
    }
    if (v < (1 << 30) && v > (-1 << 30)) {
      // simple int
      return v << 1
    }
    if (v < (1 << 31) && v > (-1 << 31)) {
      // int32
      let userPtr = managedMemory.malloc(4 * 3)
      let ptr = userPtr / 4
      view[ptr] = GRAIN_BOXED_NUM_HEAP_TAG
      view[ptr + 1] = GRAIN_INT32_BOXED_NUM_TAG
      view[ptr + 2] = v
      return userPtr | GRAIN_GENERIC_HEAP_TAG_TYPE
    }
    if (v < (1 << 63) && v > (-1 << 63)) {
      // int64
      let userPtr = managedMemory.malloc(4 * 4)
      let ptr = userPtr / 4
      view[ptr] = GRAIN_BOXED_NUM_HEAP_TAG
      view[ptr + 1] = GRAIN_INT64_BOXED_NUM_TAG
      let tmpbuf = new ArrayBuffer(8)
      let i64tmpbuf = new BigInt64Array(tmpbuf)
      let i32tmpbuf = new Int32Array(tmpbuf)
      i64tmpbuf[0] = v
      view[ptr + 2] = i32tmpbuf[0]
      view[ptr + 3] = i32tmpbuf[1]
      return userPtr | GRAIN_GENERIC_HEAP_TAG_TYPE
    }
    throwGrainError(GRAIN_ERR_OVERFLOW, -1, -1);
    return 0xF00BAE << 1;
  } else if (typeof v === "boolean") {
    if (v) {
      return -1;
    } else {
      return 0x7FFFFFFF;
    }
  } else if (typeof v === "string") {
    let userPtr = managedMemory.malloc((4 * 2) + (((v.length - 1) / 4) + 1));
    let ptr = userPtr / 4;
    view[ptr] = GRAIN_STRING_HEAP_TAG;
    view[ptr + 1] = v.length;
    let byteView = managedMemory.u8view;
    let buf = encoder.encode(v);
    for (let i = 0; i < buf.length; ++i) {
      byteView[i + (ptr * 4) + 8] = buf[i];
    }
    return userPtr | GRAIN_GENERIC_HEAP_TAG_TYPE;
  } else {
    throw new GrainError(-1, "JSToGrainVal not implemented for value with type " + (typeof v));
  }
}
