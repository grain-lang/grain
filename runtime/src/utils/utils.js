const encoder = new TextEncoder("utf-8");
const decoder = new TextDecoder("utf-8");

import {
  GRAIN_STRING_HEAP_TAG,
  GRAIN_ADT_HEAP_TAG,
  GRAIN_LAMBDA_HEAP_TAG,
  GRAIN_BOXED_NUM_HEAP_TAG,
  GRAIN_INT32_BOXED_NUM_TAG,
  GRAIN_INT64_BOXED_NUM_TAG,
  GRAIN_RATIONAL_BOXED_NUM_TAG,
  GRAIN_FLOAT32_BOXED_NUM_TAG,
  GRAIN_FLOAT64_BOXED_NUM_TAG,
} from "../core/tags";

import { GRAIN_TRUE, GRAIN_FALSE, GRAIN_VOID } from "../core/primitives";

/**
 * Formats a number as hex. Useful for debug printing.
 *
 * @param {Number} minWidth [0] - Minimum length of formatted number (will be padded w/ zeros)
 */
export function toHex(n, minWidth) {
  let ret = new Number(n >>> 0).toString(16);
  if (minWidth && ret.length < minWidth) {
    ret = "0".repeat(minWidth - ret.length);
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
    ret = "0".repeat(minWidth - ret.length) + ret;
  }
  return ret;
}

function float64At(runtime, idx) {
  const f64view = runtime.managedMemory.f64view;
  return f64view[idx / 8 + 1];
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

GrainAdtValue.prototype.toString = function () {
  if (this._elts.length === 1) {
    return this._elts[0];
  } else {
    return `${this._elts[0]}(${this._elts.slice(1).join(", ")})`;
  }
};

export function grainBoxedNumberToJSVal(runtime, n) {
  const view = runtime.managedMemory.view;
  const uview = runtime.managedMemory.uview;
  const f32view = runtime.managedMemory.f32view;
  let nInt = n / 4;
  switch (view[nInt + 1]) {
    case GRAIN_FLOAT32_BOXED_NUM_TAG:
      return f32view[nInt + 2];
    case GRAIN_FLOAT64_BOXED_NUM_TAG:
      return float64At(runtime, n + 8);
    case GRAIN_RATIONAL_BOXED_NUM_TAG:
      let numerator = view[nInt + 2];
      let denominator = view[nInt + 3];
      return numerator / denominator;
    case GRAIN_INT32_BOXED_NUM_TAG:
      return view[nInt + 2];
    case GRAIN_INT64_BOXED_NUM_TAG:
      let low = uview[nInt + 2];
      let high = view[nInt + 3];
      let negative = high < 0;

      if (negative) {
        high = ~high;
        low = ~low + 1;
      }
      return (high * 2 ** 32 + low) * (negative ? -1 : 1);
  }
}

export function grainHeapValToJSVal(runtime, n) {
  const view = runtime.managedMemory.view;
  switch (view[n / 4]) {
    case GRAIN_BOXED_NUM_HEAP_TAG:
      return grainBoxedNumberToJSVal(runtime, n);
    case GRAIN_STRING_HEAP_TAG:
      let byteView = runtime.managedMemory.u8view;
      let length = view[n / 4 + 1];
      let slice = byteView.slice(n + 8, n + 8 + length);
      return decoder.decode(slice);
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
    case GRAIN_LAMBDA_HEAP_TAG: {
      return () => {
        throw new Error(
          "Computed Grain functions are not callable from JavaScript."
        );
      };
    }
    case GRAIN_TUPLE_HEAP_TAG: {
      return grainTupleToJSVal(runtime, x);
    }
    default:
      console.warn(`Unknown heap tag at ${n / 4}: ${view[n / 4]}`);
      return undefined;
  }
}

export function grainAdtInfo(runtime, n) {
  const view = runtime.managedMemory.view;
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
  const view = runtime.managedMemory.view;
  let tupleIdx = n / 4;
  let tupleLength = view[tupleIdx + 1];
  if (tupleLength & 0x80000000) {
    return Symbol.for("cyclic");
  } else {
    view[tupleIdx + 1] |= 0x80000000;
    let elts = [];
    for (let i = 0; i < tupleLength; ++i) {
      let value = grainToJSVal(runtime, view[tupleIdx + i + 2]);
      if (value === Symbol.for("cyclic")) {
        elts.push(elts);
      } else {
        elts.push(value);
      }
    }
    view[tupleIdx + 1] = tupleLength;
    return elts;
  }
}

export function grainToJSVal(runtime, x) {
  if (x & 1) {
    return x >> 1;
  } else if ((x & 7) === 0) {
    return grainHeapValToJSVal(runtime, x);
  } else if (x === GRAIN_TRUE) {
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

export function JSToGrainVal(v, runtime) {
  const view = runtime.managedMemory.view;
  if (typeof v === "number") {
    if (!Number.isInteger(v)) {
      // not an integer, so just pun into a float64
      let userPtr = runtime.managedMemory.malloc(4 * 4);
      let ptr = userPtr / 4;
      view[ptr] = GRAIN_BOXED_NUM_HEAP_TAG;
      view[ptr + 1] = GRAIN_FLOAT64_BOXED_NUM_TAG;
      let tmpbuf = new ArrayBuffer(8);
      let f64tmpbuf = new Float64Array(tmpbuf);
      let i32tmpbuf = new Int32Array(tmpbuf);
      f64tmpbuf[0] = v;
      view[ptr + 2] = i32tmpbuf[0];
      view[ptr + 3] = i32tmpbuf[1];
      return userPtr;
    }
    if (v < 1 << 30 && v > -1 << 30) {
      // simple int
      return (v << 1) ^ 1;
    }
    if (v < 1 << 31 && v > -1 << 31) {
      // int32
      let userPtr = runtime.managedMemory.malloc(4 * 3);
      let ptr = userPtr / 4;
      view[ptr] = GRAIN_BOXED_NUM_HEAP_TAG;
      view[ptr + 1] = GRAIN_INT32_BOXED_NUM_TAG;
      view[ptr + 2] = v;
      return userPtr;
    }
    if (v < 1 << 63 && v > -1 << 63) {
      // int64
      let userPtr = runtime.managedMemory.malloc(4 * 4);
      let ptr = userPtr / 4;
      view[ptr] = GRAIN_BOXED_NUM_HEAP_TAG;
      view[ptr + 1] = GRAIN_INT64_BOXED_NUM_TAG;
      let tmpbuf = new ArrayBuffer(8);
      let i64tmpbuf = new BigInt64Array(tmpbuf);
      let i32tmpbuf = new Int32Array(tmpbuf);
      i64tmpbuf[0] = v;
      view[ptr + 2] = i32tmpbuf[0];
      view[ptr + 3] = i32tmpbuf[1];
      return userPtr;
    }
    throw new Error("Can't convert number to Grain value");
  } else if (typeof v === "boolean") {
    if (v) {
      return GRAIN_TRUE;
    } else {
      return GRAIN_FALSE;
    }
  } else if (typeof v === "string") {
    let userPtr = runtime.managedMemory.malloc(
      4 * 2 + ((v.length - 1) / 4 + 1)
    );
    let ptr = userPtr / 4;
    view[ptr] = GRAIN_STRING_HEAP_TAG;
    view[ptr + 1] = v.length;
    let byteView = runtime.managedMemory.u8view;
    let buf = encoder.encode(v);
    for (let i = 0; i < buf.length; ++i) {
      byteView[i + ptr * 4 + 8] = buf[i];
    }
    return userPtr;
  } else {
    throw new Error(
      "JSToGrainVal not implemented for value with type " + typeof v
    );
  }
}
