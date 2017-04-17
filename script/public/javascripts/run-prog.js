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
const GRAIN_ERR_BAD_INPUT = 97;
const GRAIN_ERR_NOT_NONNEG = 98;
const GRAIN_ERR_NOT_NUMBER_GENERIC = 99;

const GRAIN_TRUE = 0xFFFFFFFF | 0;
const GRAIN_FALSE = 0x7FFFFFFF | 0;

let grainInitialized = false;
let grainModule;

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
  case GRAIN_ERR_OVERFLOW:
    message = `number overflow with value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_BOOLEAN_IF:
    message = `if expected a boolean, got value: ${value1AsGrain}`;
    break;
  case GRAIN_ERR_NOT_BOOLEAN_LOGIC:
    message = `logic expected a boolean, got value: ${value1AsGrain}`;
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
  console.log(`0x${n.toString(16)} (0b${n.toString(2)})`);
  return n;
}

let memory = new WebAssembly.Memory({initial: 1});
let view = new Int32Array(memory.buffer);
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
  default:
    return `<unknown heap type: ${view[n / 4]}>`;
  }
}

function grainToString(n) {
  if (!(n & 1)) {
    return (n >> 1).toString();
  } else if ((n & 7) === 1) {
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
  } else if ((n & 7) === 5) {
    return "<lambda>";
  } else if ((n & 7) === 3) {
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
  case 1:
    let byteView = new Uint8Array(memory.buffer);
    let length = view[(n / 4) + 1];
    let slice = byteView.slice(n + 8, n + 8 + length);
    return decoder.decode(slice);
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
    return v << 1;
  } else if (typeof v === "boolean") {
    if (v) {
      return -1;
    } else {
      return 0x7FFFFFFF;
    }
  } else {
    throw new GrainError(-1, "JSToGrainVal not yet implemented for value");
  }
}

function grainEqualHelp(x, y, cycles) {
  if ((x & 7) === 1) {
    if ((y & 7) === -1) {
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

const importObj = {
  console: {
    log: printNumber,
    debug: debugPrint,
    printClosure: printClosure
  },
  js: {
    mem: memory,
    throwError: throwGrainError
  },
  grainBuiltins: {
    print: printNumber,
    equal: grainEqual
  }
};

/*
if (process.argv.length != 3) {
  console.error(`give file pls`);
  process.exit(1);
}*/

function fetchFileAndInstantiate(url, importObject) {

  return new Promise((resolve, reject) => fs.readFile(url, (err, data) => {
    if (err) reject(err);
    else resolve(data);
  })).then(bytes =>
    Wasm.instantiateModule(bytes, importObject)
  ).then(results =>
    results
  );
}

function fetchAndInstantiate(url, importObject) {
  return fetch(url).then(response =>
    response.arrayBuffer()
  ).then(bytes =>
    WebAssembly.instantiate(bytes, importObject)
  ).then(results =>
    results
  );
}

let result = fetchAndInstantiate("t.wasm", importObj).then((module) => {
  grainModule = module;
  grainInitialized = true;
  let main = module.instance.exports["GRAIN$MAIN"];
  heapAdjust = module.instance.exports["GRAIN$HEAP_ADJUST"];
  let res = main();
  let resJS = grainToJSVal(res);
  console.log(resJS.call(4, 5));
  printNumber(res);
  console.log(`result: ${res}`);
}).catch(e => {
  displayOnPage(`[[ERROR: ${e.message}]]`);
  console.error(e.message);
  console.error(e.stack);
  throw e;
});
