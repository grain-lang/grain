import { heapAdjust } from './core/heap';
import { printClosure } from './core/closures';

import { print, debugPrint } from './lib/print';
import { equal } from './lib/equal';
import toString from './lib/to-string';
import * as libStrings from './lib/strings';
import * as libDOM from './lib/DOM';

let grainInitialized = false;
let grainModule;

export const memory = new WebAssembly.Memory({initial: 1});
export const view = new Int32Array(memory.buffer);
export const encoder = new TextEncoder("utf-8");
export const decoder = new TextDecoder("utf-8");



function displayOnPage(str) {
  document.getElementById('output').innerText = str;
}

const importObj = {
  console: {
    log: print,
    debug: debugPrint,
    printClosure: printClosure
  },
  js: {
    mem: memory,
    throwError: throwGrainError,
    checkMemory: grainCheckMemory
  },
  grainBuiltins: {
    print,
    equal,
    toString,
    ...libStrings,
    ...libDOM
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

export default function loadExample(e) {
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
