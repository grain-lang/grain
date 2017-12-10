import { memory, view, encoder, decoder } from '../runtime';

export function grainHeapValueToString(n) {
  switch (view[n / 4]) {
  case 1:
    let byteView = new Uint8Array(memory.buffer);
    let length = view[(n / 4) + 1];
    let slice = byteView.slice(n + 8, n + 8 + length);
    return `"${decoder.decode(slice)}"`;
    break;
  case 2:
    return grainDOMRefs[view[(n + 4) / 4]].toString();
    break;
  default:
    return `<unknown heap type: ${view[n / 4]}>`;
  }
}

export function grainToString(n) {
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
        elts.push(grainToString(view[tupleIdx + i + 1]));
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
    return grainHeapValueToString(n ^ 3);
  } else if ((n === -1)) {
    return "true";
  } else if (n === 0x7FFFFFFF) {
    return "false";
  } else {
    return `<Unknown value: 0x${n}>`;
  }
}

export function grainHeapValToJSVal(n) {
  switch (view[n / 4]) {
  case GRAIN_STRING_HEAP:
    let byteView = new Uint8Array(memory.buffer);
    let length = view[(n / 4) + 1];
    let slice = byteView.slice(n + 8, n + 8 + length);
    return decoder.decode(slice);
  case GRAIN_DOM_ELEM_TAG:
    let ref = n / 4;
    return grainDOMRefs[view[ref + 1]];
  default:
    console.warn(`Unknown heap tag at ${n / 4}: ${view[n / 4]}`);
    return undefined;
  }
}

export function grainToJSVal(x) {
  if (!(x & 1)) {
    return x >> 1;
  } else if ((x & 7) == 5) {
    if (!grainInitialized) {
      throw new GrainError(-1, "Grain runtime not yet initialized");
    }
    let lambdaLoc = (x ^ 5) / 4;
    return (new GrainClosure(lambdaLoc));
  } else if ((x & 7) === 3) {
    return grainHeapValToJSVal(x ^ 3);
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
    view[ptr] = GRAIN_STRING_HEAP;
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
