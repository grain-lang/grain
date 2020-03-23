import 'fast-text-encoding';

import { printClosure } from './core/closures';
import { ManagedMemory } from './core/memory';
import { GrainRunner } from './core/runner';
import { throwGrainError } from './errors/errors';
import { grainToString } from './utils/utils';
import { defaultFileLocator } from './utils/locators';

import { print, debugPrint } from './lib/print';
import * as libDOM from './lib/DOM';

export let grainModule;

export const memory = new WebAssembly.Memory({initial: 16});
export const table = new WebAssembly.Table({element: 'anyfunc', initial: 1024});
export const view = new Int32Array(memory.buffer);
export const uview = new Uint32Array(memory.buffer);
export const encoder = new TextEncoder("utf-8");
export const decoder = new TextDecoder("utf-8");

const managedMemory = new ManagedMemory(memory);
export const malloc = managedMemory.malloc.bind(managedMemory)
export const free = managedMemory.free.bind(managedMemory)

const importObj = {
  env: {
    memory
  },
  console: {
    log: print,
    debug: debugPrint,
    printClosure: printClosure
  },
  grainRuntime: {
    mem: memory,
    tbl: table,
    throwError: throwGrainError,
    malloc: managedMemory.malloc.bind(managedMemory),
    free: managedMemory.free.bind(managedMemory),
    incRef: managedMemory.incRef.bind(managedMemory),
    incRefADT: managedMemory.incRefADT.bind(managedMemory),
    incRefTuple: managedMemory.incRefTuple.bind(managedMemory),
    incRefBackpatch: managedMemory.incRefBackpatch.bind(managedMemory),
    incRefSwapBind: managedMemory.incRefSwapBind.bind(managedMemory),
    incRefLocalBind: managedMemory.incRefLocalBind.bind(managedMemory),
    incRefArgBind: managedMemory.incRefArgBind.bind(managedMemory),
    incRef64: managedMemory.incRef64.bind(managedMemory),
    decRef: managedMemory.decRef.bind(managedMemory),
    decRef64: managedMemory.decRef64.bind(managedMemory)
  },
  grainBuiltins: {
    ...libDOM
  }
};

export function buildGrainRunner(locator, opts) {
  let runner = new GrainRunner(locator || ((x) => null), opts);
  runner.addImports(importObj);
  // [TODO] Find something which avoids global state!
  managedMemory.setRuntime(runner);
  return runner;
}

let runner = buildGrainRunner();

// TODO: Migrate API to expose runner object directly

export async function GrainNodeRunner(path) {
  let loaded = await runner.loadFile(path);
  return loaded.run();
}

export default async function GrainRunner(uri) {
  let loaded = await runner.loadURL(uri);
  return loaded.run();
}

export { defaultFileLocator, grainToString };
