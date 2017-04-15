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

function printClosure(c) {
  let view = new Int32Array(importObj.js.mem.buffer);
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

let memory = new WebAssembly.Memory({initial: 1});
let view = new Int32Array(memory.buffer);
let counter = 0;
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
    return `<forwarding pointer: ${n ^ 3}>`;
  } else if ((n === -1)) {
    return "true";
  } else if (n === 0x7FFFFFFF) {
    return "false";
  } else {
    return `<Unknown value: 0x${n}>`;
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

let result = fetchAndInstantiate("t.wasm", importObj).then((instance) => {
  console.log(JSON.stringify(instance, null, 2));
}).catch(e => {
  displayOnPage(`[[ERROR: ${e.message}]]`);
  console.error(e.message);
  console.error(e.stack);
  throw e;
});
