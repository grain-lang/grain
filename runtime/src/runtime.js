import 'fast-text-encoding';

import { heapController, grainCheckMemory } from './core/heap';
import { printClosure } from './core/closures';
import { ManagedMemory } from './core/memory';
import { GrainRunner } from './core/runner';
import { throwGrainError } from './errors/errors';
import { grainToString } from './utils/utils';
import { defaultFileLocator } from './utils/locators';

import { print, debugPrint } from './lib/print';
import equal from './lib/equal';
import * as libStrings from './lib/strings';
import * as libDOM from './lib/DOM';

export let grainModule;

export const memory = new WebAssembly.Memory({initial: 1});
export const table = new WebAssembly.Table({element: 'anyfunc', initial: 128});
export const view = new Int32Array(memory.buffer);
export const encoder = new TextEncoder("utf-8");
export const decoder = new TextDecoder("utf-8");

const managedMemory = new ManagedMemory(memory);

const importObj = {
  console: {
    log: print,
    debug: debugPrint,
    printClosure: printClosure
  },
  grainRuntime: {
    mem: memory,
    tbl: table,
    throwError: throwGrainError,
    checkMemory: grainCheckMemory,
    malloc: managedMemory.malloc.bind(managedMemory)
  },
  grainBuiltins: {
    equal,
    ...libStrings,
    ...libDOM
  }
};

export function buildGrainRunner(locator) {
  let runner = new GrainRunner(locator || ((x) => null));
  runner.addImports(importObj);
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
