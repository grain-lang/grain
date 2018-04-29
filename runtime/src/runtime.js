import 'fast-text-encoding';
import fs from 'fs';

import { heapController, grainCheckMemory } from './core/heap';
import { printClosure } from './core/closures';
import { throwGrainError } from './errors/errors';
import { grainToJSVal } from './utils/utils';

import { print, debugPrint } from './lib/print';
import equal from './lib/equal';
import toString from './lib/to-string';
import * as libStrings from './lib/strings';
import * as libDOM from './lib/DOM';

export let grainModule;

export const memory = new WebAssembly.Memory({initial: 1});
export const view = new Int32Array(memory.buffer);
export const encoder = new TextEncoder("utf-8");
export const decoder = new TextDecoder("utf-8");

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

async function fetchAndInstantiate(url, importObject) {
  let response = await fetch(url);
  if (!response.ok) throw new Error(`[Grain] Could not load ${url} due to a network error.`);
  let bytes = await response.arrayBuffer();
  return WebAssembly.instantiate(bytes, importObject);
}

async function readAndInstantiate(path, importObject) {
  let bytes = fs.readFileSync(path).buffer;
  console.log(bytes)
  return WebAssembly.instantiate(bytes, importObject);
}

function runGrain(module) {
  grainModule = module;
  let main = module.instance.exports["GRAIN$MAIN"];
  heapController.heapAdjust = module.instance.exports["GRAIN$HEAP_ADJUST"];
  let res = main();
  return grainToJSVal(res);
}

export async function GrainNodeRunner(path) {
  let module = await readAndInstantiate(path, importObj);
  return runGrain(module);
}

export default async function GrainRunner(uri) {
  let module = await fetchAndInstantiate(uri, importObj);
  return runGrain(module);
}

