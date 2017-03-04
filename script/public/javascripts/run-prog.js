//#!node --expose-wasm
//const fs = require('fs');

function GrainError(code, message) {
  this.name = "GrainError";
  this.code = code;
  this.message = message;
  this.stack = (new Error()).stack;
}
GrainError.prototype = Object.create(Error.prototype);
GrainError.prototype.constructor = GrainError;

const GRAIN_ERR_NOT_NUMBER_COMP = 0;
const GRAIN_ERR_NOT_NUMBER_ARITH = 1;
const GRAIN_ERR_NOT_BOOLEAN_LOGIC = 2;
const GRAIN_ERR_NOT_BOOLEAN_IF = 3;
const GRAIN_ERR_OVERFLOW = 4;
const GRAIN_ERR_GET_NOT_TUP = 5;
const GRAIN_ERR_GET_ITEM_IDX_NOT_NUMBER = 6;
const GRAIN_ERR_GET_ITEM_IDX_TOO_SMALL = 7;
const GRAIN_ERR_GET_ITEM_IDX_TOO_LARGE = 8;
const GRAIN_ERR_CALLED_NON_FUNCTION = 9;
const GRAIN_ERR_ARITY_MISMATCH = 10;
const GRAIN_ERR_OUT_OF_MEMORY = 11;
const GRAIN_ERR_SET_NOT_TUP = 12;
const GRAIN_ERR_SET_ITEM_IDX_NOT_NUMBER = 13;
const GRAIN_ERR_SET_ITEM_IDX_TOO_SMALL = 14;
const GRAIN_ERR_SET_ITEM_IDX_TOO_LARGE = 15;
const GRAIN_ERR_NOT_DOM_ELEMENT_GENERIC = 92;
const GRAIN_ERR_NOT_STRING_GENERIC = 93;
const GRAIN_ERR_NOT_BOOLEAN_GENERIC = 94;
const GRAIN_ERR_NOT_TUPLE_GENERIC = 95;
const GRAIN_ERR_NOT_LAMBDA_GENERIC = 96;
const GRAIN_ERR_BAD_INPUT = 97;
const GRAIN_ERR_NOT_NONNEG = 98;
const GRAIN_ERR_NOT_NUMBER_GENERIC = 99;

const GRAIN_TRUE = 0xFFFFFFFF | 0;
const GRAIN_FALSE = 0x7FFFFFFF | 0;

const GRAIN_NUMBER_TAG_MASK = 0b0001;
const GRAIN_TUPLE_TAG_MASK = 0b0111;

const GRAIN_NUMBER_TAG_TYPE       = 0b0000;
const GRAIN_BOOLEAN_TAG_TYPE      = 0b1111;
const GRAIN_TUPLE_TAG_TYPE        = 0b0001;
const GRAIN_LAMBDA_TAG_TYPE       = 0b0101;
const GRAIN_GENERIC_HEAP_TAG_TYPE = 0b0011;

const GRAIN_STRING_HEAP = 1;
const GRAIN_DOM_ELEM_TAG = 2;

let grainInitialized = false;
let grainModule;
let grainDOMRefs = [];

function getAndMask(tag) {
  switch(tag) {
  case GRAIN_NUMBER_TAG_TYPE:
    return GRAIN_NUMBER_TAG_MASK;
  default:
    return GRAIN_TUPLE_TAG_MASK;
  }
}

function assertGrainTag(tag, n, err) {
  if ((n & getAndMask(tag)) !== tag) {
    throwGrainError(err, n, 0);
  }
}

function assertGrainHeapTag(tag, n, err) {
  assertGrainTag(GRAIN_GENERIC_HEAP_TAG_TYPE, n, err);
  let ptr = n ^ GRAIN_GENERIC_HEAP_TAG_TYPE;
  if (view[ptr / 4] != tag) {
    throwGrainError(err, n, 0);
  }
}

let assertNumber = (n, err) => assertGrainTag(GRAIN_NUMBER_TAG_TYPE, n, err || GRAIN_ERR_NOT_NUMBER_GENERIC);
let assertBoolean = (n, err) => assertGrainTag(GRAIN_BOOLEAN_TAG_TYPE, n, err || GRAIN_ERR_NOT_BOOLEAN_GENERIC);
let assertTuple = (n, err) => assertGrainTag(GRAIN_TUPLE_TAG_TYPE, n, err || GRAIN_ERR_NOT_TUPLE_GENERIC);
let assertLambda = (n, err) => assertGrainTag(GRAIN_LAMBDA_TAG_TYPE, n, err || GRAIN_ERR_NOT_LAMBDA_GENERIC);
let assertString = (n, err) => assertGrainHeapTag(GRAIN_STRING_HEAP, n, err || GRAIN_ERR_NOT_STRING_GENERIC);
let assertDOMElement = (n, err) => assertGrainHeapTag(GRAIN_DOM_ELEM_TAG, n, err || GRAIN_ERR_NOT_DOM_ELEMENT_GENERIC);

let heapAdjust = function(n) {
  throw new GrainError(-1, "Grain runtime is not yet instantiated.");
};

function throwGrainError(errorCode, value1, value2) {
  let message;

  console.error(errorCode);
  console.error(value1);
  console.error(value2);
  let value1AsGrain = grainToString(value1);
  switch (errorCode) {
  case GRAIN_ERR_ARITY_MISMATCH:
    message = `arity mismatch (expected ${value1} arguments, but got ${value2})`;
    break;
  case GRAIN_ERR_NOT_NUMBER_ARITH:
    message = `arithmetic expected a number, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_NUMBER_COMP:
    message = `comparison expected a number, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_NUMBER_GENERIC:
    message = `expected a number, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_BOOLEAN_GENERIC:
    message = `expected a boolean, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_TUPLE_GENERIC:
    message = `expected a tuple, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_LAMBDA_GENERIC:
    message = `expected a lambda, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_OVERFLOW:
    message = `number overflow with value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_BOOLEAN_IF:
    message = `if expected a boolean, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_BOOLEAN_LOGIC:
    message = `logic expected a boolean, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_STRING_GENERIC:
    message = `expected a string, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_DOM_ELEMENT_GENERIC:
    message = `expected a DOM element, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_GET_NOT_TUP:
    message = `tuple access expected tuple, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_SET_NOT_TUP:
    message = `tuple assignment expected tuple, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_GET_ITEM_IDX_NOT_NUMBER:
    message = `tuple access expected number for index, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_SET_ITEM_IDX_NOT_NUMBER:
    message = `tuple assignment expected number for index, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_GET_ITEM_IDX_TOO_SMALL:
  case GRAIN_ERR_SET_ITEM_IDX_TOO_SMALL:
    message = `tuple index too small: ${value1AsGrain} (tuple arity: ${value2})`;
    break;
  case GRAIN_ERR_GET_ITEM_IDX_TOO_LARGE:
  case GRAIN_ERR_SET_ITEM_IDX_TOO_LARGE:
    message = `tuple index too large: ${value1AsGrain} (tuple arity: ${value2})`;
    break;
  case GRAIN_ERR_CALLED_NON_FUNCTION:
    message = `called non-function: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_NONNEG:
    message = `expected a nonnegative number, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_OUT_OF_MEMORY:
    message = `Out of memory`;
    break;
  default:
    message = `Unknown error code: ${errorCode}`;
  }

  throw new GrainError(errorCode, message);
}

function debugPrint(n) {
  // console.log(`0x${n.toString(16)} (0b${n.toString(2)})`);
  return n;
}

let memory = new WebAssembly.Memory({initial: 1});
let view = new Int32Array(memory.buffer);
let encoder = new TextEncoder("utf-8");
let decoder = new TextDecoder("utf-8");
let counter = 0;

var GrainClosure = function(loc) {
  this.loc = loc;
  this.arity = view[loc];
  this.ptr = view[loc + 1];
  this.closureSize = view[loc + 2];
  this.closureElts = view.slice(loc + 3, loc + 3 + this.closureSize);
  this.func = grainModule.instance.exports["GRAIN$LAM_" + this.ptr];
};

GrainClosure.prototype.call = function() {
  if (arguments.length != this.arity) {
    throwGrainError(GRAIN_ERR_ARITY_MISMATCH, this.arity, arguments.length);
    return undefined;
  } else {
    let grainVals = Array.prototype.map.call(arguments, JSToGrainVal);
    grainVals.unshift(this.loc * 4);
    return grainToJSVal(this.func.apply(this.func, grainVals));
  }
};

function printClosure(c) {
  c /= 4;
  let arity = view[c];
  let idx = view[c + 1];
  let closureSize = view[c + 2];
  let closureElts = [];

  for (var i = 0; i < closureSize; ++i) {
    closureElts.push(printNumber(view[c + i + 3]));
  }
  console.log(`<closure@${c}: idx=${idx}, arity=${arity}, size=${closureSize}: ${closureElts}>`);
  console.log(view.slice(0, 32));
  return c;
}

function grainHeapValueToString(n) {
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

function grainToString(n) {
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

function grainHeapValToJSVal(n) {
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

function grainToJSVal(x) {
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

function JSToGrainVal(v) {
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

function grainEqualHelp(x, y, cycles) {
  if ((x & 7) === 1) {
    if ((y & 7) !== 1) {
      return false;
    }
    let xPtr = (x ^ 1) / 4;
    let yPtr = (y ^ 1) / 4;
    if (view[xPtr] !== view[yPtr]) {
      return false;
    }
    if (view[xPtr] & 0x80000000) {
      return true;
    }
    let length = view[xPtr];
    ++cycles;
    view[xPtr] |= 0x80000000;
    view[yPtr] |= 0x80000000;
    let result = true;
    for (let i = 0; i < length; ++i) {
      if (!grainEqualHelp(view[xPtr + i + 1],
                          view[yPtr + i + 1],
                          cycles)) {
        result = false;
        break;
      }
    }
    view[xPtr] = length;
    view[yPtr] = length;
    return result;
  } else {
    return x === y;
  }
}

function grainEqual(x, y) {
  return grainEqualHelp(x, y, 0) ? GRAIN_TRUE : GRAIN_FALSE;
}

function grainCheckMemory(numBytes) {
  if (numBytes === 0) {
    return;
  }
  let curTop = heapAdjust(0);
  if (memory.buffer.byteLength - curTop < numBytes) {
    memory.grow(1);
  }
}

function grainHeapAllocate(numWords) {
  // allocates the number of words
  let curTop = heapAdjust(0);
  let wordsToAllocate = 4 * (Math.ceil((numWords - 1) / 4) + 1);
  heapAdjust(wordsToAllocate * 4);
  return curTop;
}

function grainDOMQuery(n) {
  assertString(n);
  let query = grainToJSVal(n);
  let elem = document.querySelector(query);
  if (elem) {
    grainDOMRefs.push(elem);
    let heapRef = grainHeapAllocate(2) / 4;
    view[heapRef] = GRAIN_DOM_ELEM_TAG;
    view[heapRef+1] = grainDOMRefs.length - 1;
    return (heapRef * 4) ^ 3;
  } else {
    return GRAIN_FALSE;
  }
}

function grainDOMElemSetText(elemRef, textRef) {
  assertDOMElement(elemRef);
  assertString(textRef);
  let elem = grainToJSVal(elemRef);
  elem.innerText = grainToJSVal(textRef);
  return elemRef;
}

function grainDOMDangerouslySetInnerHTML(elemRef, textRef) {
  assertDOMElement(elemRef);
  assertString(textRef);
  let elem = grainToJSVal(elemRef);
  elem.innerHTML = grainToJSVal(textRef);
  return elemRef;
}

function grainDOMAddEventListener(elemRef, eventRef, handlerRef) {
  assertDOMElement(elemRef);
  assertString(eventRef);
  assertLambda(handlerRef);
  let elem = grainToJSVal(elemRef);
  let event = grainToJSVal(eventRef);
  let handler = grainToJSVal(handlerRef);
  elem.addEventListener(event, () => handler.call());
  return elemRef;
}

function displayOnPage(str) {
  document.getElementById('output').innerText = str;
}

function printNumber(n) {
  debugPrint(n);
  let res = grainToString(n);
  displayOnPage(`${res}`);
  console.log(res);
  return n;
}



function stringAppend(s1, s2) {
  assertString(s1);
  assertString(s2);
  s1 = grainToJSVal(s1);
  s2 = grainToJSVal(s2);
  let appended = s1.concat(s2);
  let ret = JSToGrainVal(appended);
  return ret;
}

function stringLength(s) {
  assertString(s);
  return JSToGrainVal(grainToJSVal(s).length);
}

function stringSlice(s, from, to) {
  assertString(s);
  assertNumber(from);
  assertNumber(to);
  s = grainToJSVal(s);
  if (from < 0) {
    throwGrainError(GRAIN_ERR_NOT_NONNEG, from);
  } else if ((to >> 1) > s.length) {
    throw new GrainError(-1, `Index ${to >> 1} greater than string length (${s.length})`);
  }
  from = grainToJSVal(from);
  to = grainToJSVal(to);
  let ret = s.slice(from, to);
  return JSToGrainVal(ret);
}

const importObj = {
  console: {
    log: printNumber,
    debug: debugPrint,
    printClosure: printClosure
  },
  js: {
    mem: memory,
    throwError: throwGrainError,
    checkMemory: grainCheckMemory
  },
  grainBuiltins: {
    print: printNumber,
    equal: grainEqual,
    toString: (n => JSToGrainVal(grainToString(n))),
    stringAppend: stringAppend,
    stringLength: stringLength,
    stringSlice: stringSlice,
    DOMQuery: grainDOMQuery,
    DOMSetText: grainDOMElemSetText,
    DOMDangerouslySetInnerHTML: grainDOMDangerouslySetInnerHTML,
    DOMAddEventListener: grainDOMAddEventListener
  }
};

function fetchSource(url) {
  return fetch(url)
    .then(response => response.text())
    .then(code => {
      document.getElementById('sourceCode').innerText = code;
      Prism.highlightAll();
    });
}

function fetchAndInstantiate(url, importObject) {
  return fetch(url).then(response => response.arrayBuffer())
    .then(bytes => WebAssembly.instantiate(bytes, importObject))
    .then(results => results);
}

function runGrain(module) {
  grainModule = module;
  grainInitialized = true;
  let main = module.instance.exports["GRAIN$MAIN"];
  heapAdjust = module.instance.exports["GRAIN$HEAP_ADJUST"];
  let res = main();
  console.log(`result: ${res}`);
  let resJS = grainToJSVal(res);
  printNumber(res);
  return resJS;
}

function showError(e) {
  displayOnPage(`[[ERROR: ${e.message}]]`);
  console.error(e.message);
  console.error(e.stack);
  throw e;
}

var examples = {
  addition: { source: "adder.gr", wasm: "adder.wasm" },
  lambda: { source: "lambda.gr", wasm: "lambda.wasm" },
  dom: { source: "domSimple.gr", wasm: "domSimple.wasm" },
  domCb: { source: "dom.gr", wasm: "dom.wasm" }
};

function resetPage() {
  document.getElementById('div1').innerHTML = "";
  document.getElementById('div2').innerHTML = "";
  document.getElementById('innerDiv').innerHTML = "";
  document.getElementById('sourceCode').innerHTML = "";
}

function loadExample(e) {
  resetPage();
  fetchSource("examples/".concat(e.source));
  return fetchAndInstantiate("examples/".concat(e.wasm), importObj)
    .then(runGrain)
    .catch(showError);
}

function makeExampleLoader(e) {
  return () => loadExample(e);
}

document.getElementById("navAdd")
  .addEventListener("click", makeExampleLoader(examples.addition));
document.getElementById("navFunc")
  .addEventListener("click", makeExampleLoader(examples.lambda));
document.getElementById("navDOM")
  .addEventListener("click", makeExampleLoader(examples.dom));
document.getElementById("navDOMCallback")
  .addEventListener("click", makeExampleLoader(examples.domCb));

loadExample(examples.addition);
