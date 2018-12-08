import { memory, view, encoder, decoder } from '../runtime';
import { grainHeapAllocate } from '../core/heap';
import { GrainClosure } from '../core/closures';
import { GrainError } from '../errors/errors';
import { grainDOMRefs } from '../lib/DOM';

import {
  GRAIN_TUPLE_TAG_TYPE,
  GRAIN_LAMBDA_TAG_TYPE,
  GRAIN_GENERIC_HEAP_TAG_TYPE,
  GRAIN_DOM_ELEM_TAG,
  GRAIN_STRING_HEAP_TAG,
  GRAIN_ADT_HEAP_TAG
} from '../core/tags';

export function grainHeapValueToString(n, runtime) {
  switch (view[n / 4]) {
  case GRAIN_STRING_HEAP_TAG:
    let byteView = new Uint8Array(memory.buffer);
    let length = view[(n / 4) + 1];
    let slice = byteView.slice(n + 8, n + 8 + length);
    return `"${decoder.decode(slice)}"`;
    break;
  case GRAIN_DOM_ELEM_TAG:
    return grainDOMRefs[view[(n + 4) / 4]].toString();
    break;
  case GRAIN_ADT_HEAP_TAG:
    let x = n / 4;
    //console.log(`<ADT Value: (${view[x + 1]}, ${view[x + 2]}, ${view[x + 3]}, ${view[x + 4]})>`);
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
      let [variantName, arity] = info;
      let printedVals = [];
      for (let i = 0; i < arity; ++i) {
        printedVals.push(grainToString(view[x + 5 + i], runtime));
      }
      if (arity === 0) {
        return variantName;
      } else {
        return `${variantName}(${printedVals})`;
      }
    }
    return "<adt value>";
    break;
  default:
    return `<unknown heap type: ${view[n / 4]}>`;
  }
}

export function grainToString(n, runtime) {
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
        elts.push(grainToString(view[tupleIdx + i + 1], runtime));
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
    return grainHeapValueToString(n ^ 3, runtime);
  } else if ((n === -1)) {
    return "true";
  } else if (n === 0x7FFFFFFF) {
    return "false";
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

  toString() {
    if (this._elts.length === 1) {
      return this._elts[0];
    } else {
      return `${this._elts[0]}(${this._elts.slice(1)})`;
    }
  }
}

export function grainHeapValToJSVal(n, runtime) {
  switch (view[n / 4]) {
  case GRAIN_STRING_HEAP_TAG:
    let byteView = new Uint8Array(memory.buffer);
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
      let [variantName, arity] = info;
      let ret = [variantName];
      for (let i = 0; i < arity; ++i) {
        ret.push(grainToJSVal(view[x + 5 + i], runtime));
      }
      return new GrainAdtValue(ret);
    }
  default:
    console.warn(`Unknown heap tag at ${n / 4}: ${view[n / 4]}`);
    return undefined;
  }
}

export function grainTupleToJSVal(n, runtime) {
  let tupleIdx = n / 4;
  let tupleLength = view[tupleIdx];
  if (tupleLength & 0x80000000) {
    return Symbol.for('cyclic');
  } else {
    view[tupleIdx] |= 0x80000000;
    let elts = [];
    for (let i = 0; i < tupleLength; ++i) {
      let value = grainToJSVal(view[tupleIdx + i + 1], runtime)
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

export function grainToJSVal(x, runtime) {
  if (!(x & 1)) {
    return x >> 1;
  } else if ((x & 7) == 5) {
    return () => {
      throw new GrainError('Computed Grain functions are not callable from JavaScript.')
    }
  } else if ((x & 7) === 3) {
    return grainHeapValToJSVal(x ^ 3, runtime);
  } else if ((x & 7) === GRAIN_TUPLE_TAG_TYPE) {
    return grainTupleToJSVal(x ^ GRAIN_TUPLE_TAG_TYPE, runtime);
  } else if ((x === -1)) {
    return true;
  } else if (x === 0x7FFFFFFF) {
    return false;
  } else {
    console.warn(`Unknown Grain value: ${x} (0x${x.toString(16)})`);
    return undefined;
  }
}

export function JSToGrainVal(v) {
  if (typeof v === "number") {
    // TODO: overflow check
    if (v >= (1 << 30)) {
      throwGrainError(GRAIN_ERR_OVERFLOW, -1, -1);
    }
    return v << 1;
  } else if (typeof v === "boolean") {
    if (v) {
      return -1;
    } else {
      return 0x7FFFFFFF;
    }
  } else if (typeof v === "string") {
    let ptr = grainHeapAllocate(2 + (((v.length - 1) / 4) + 1)) / 4;
    view[ptr] = GRAIN_STRING_HEAP_TAG;
    view[ptr + 1] = v.length;
    let byteView = new Uint8Array(memory.buffer);
    let buf = encoder.encode(v);
    for (let i = 0; i < buf.length; ++i) {
      byteView[i + (ptr * 4) + 8] = buf[i];
    }
    return (ptr * 4) | GRAIN_GENERIC_HEAP_TAG_TYPE;
  } else {
    throw new GrainError(-1, "JSToGrainVal not implemented for value with type " + (typeof v));
  }
}
