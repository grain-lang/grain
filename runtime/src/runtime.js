import 'fast-text-encoding';

import { printClosure } from './core/closures';
import { ManagedMemory, TRACE_MEMORY } from './core/memory';
import { GrainRunner } from './core/runner';
import { defaultFileLocator } from './utils/locators';

export let grainModule;

// Workaround for this memory being initialized statically on module load
export const memory = new WebAssembly.Memory({initial: process.env.GRAIN_INIT_MEMORY_PAGES || 64});
export const table = new WebAssembly.Table({element: 'anyfunc', initial: 1024});
export const encoder = new TextEncoder("utf-8");
export const decoder = new TextDecoder("utf-8");
export const managedMemory = new ManagedMemory(memory);

export const malloc = managedMemory.malloc.bind(managedMemory)
export const free = managedMemory.free.bind(managedMemory)

const tracingImports = TRACE_MEMORY ? {
  incRefADT: managedMemory.incRefADT.bind(managedMemory),
  incRefArray: managedMemory.incRefArray.bind(managedMemory),
  incRefTuple: managedMemory.incRefTuple.bind(managedMemory),
  incRefBox: managedMemory.incRefBox.bind(managedMemory),
  incRefBackpatch: managedMemory.incRefBackpatch.bind(managedMemory),
  incRefSwapBind: managedMemory.incRefSwapBind.bind(managedMemory),
  incRefLocalBind: managedMemory.incRefLocalBind.bind(managedMemory),
  incRefArgBind: managedMemory.incRefArgBind.bind(managedMemory),
  incRefGlobalBind: managedMemory.incRefGlobalBind.bind(managedMemory),
  incRefClosureBind: managedMemory.incRefClosureBind.bind(managedMemory),
  incRefCleanupLocals: managedMemory.incRefCleanupLocals.bind(managedMemory),
  decRefArray: managedMemory.decRefArray.bind(managedMemory),
  decRefTuple: managedMemory.decRefTuple.bind(managedMemory),
  decRefBox: managedMemory.decRefBox.bind(managedMemory),
  decRefSwapBind: managedMemory.decRefSwapBind.bind(managedMemory),
  decRefLocalBind: managedMemory.decRefLocalBind.bind(managedMemory),
  decRefArgBind: managedMemory.decRefArgBind.bind(managedMemory),
  decRefGlobalBind: managedMemory.decRefGlobalBind.bind(managedMemory),
  decRefClosureBind: managedMemory.decRefClosureBind.bind(managedMemory),
  decRefCleanupLocals: managedMemory.decRefCleanupLocals.bind(managedMemory),
  decRefCleanupGlobals: managedMemory.decRefCleanupGlobals.bind(managedMemory),
  decRefDrop: managedMemory.decRefDrop.bind(managedMemory),
  decRefIgnoreZeros: managedMemory.decRefIgnoreZeros.bind(managedMemory)
} : {
  decRefIgnoreZeros: managedMemory.decRefIgnoreZeros.bind(managedMemory)
};

function debugPrint(v) {
  console.log(`0x${v.toString(16)} (0b${v.toString(2)})`);
  return v;
}

const importObj = {
  env: {
    memory,
    abort(err) {
      throw new Error(`abort ${err}`)
    },
  },
  console: {
    debug: debugPrint,
    printClosure: printClosure,
    tracepoint: (n) => console.log(`tracepoint ${n} reached`)
  },
  grainRuntime: {
    mem: memory,
    tbl: table,
    malloc,
    free,
    incRef: managedMemory.incRef.bind(managedMemory),
    incRef64: managedMemory.incRef64.bind(managedMemory),
    decRef: managedMemory.decRef.bind(managedMemory),
    decRef64: managedMemory.decRef64.bind(managedMemory),
    ...tracingImports
  },
};

export function buildGrainRunner(locator, opts) {
  // [TODO] Find something which avoids global state!
  let runner = new GrainRunner(locator || ((x) => null), managedMemory, opts);
  runner.addImports(importObj);
  managedMemory.setRuntime(runner);
  return runner;
}

export function dumpMemoryStats() {
  // Only functional when memory.js's TRACE_MEMORY is on
  managedMemory.prepareExit();
}

// TODO: Migrate API to expose runner object directly

export async function GrainNodeRunner(path) {
  let runner = buildGrainRunner();
  let loaded = await runner.loadFile(path);
  return loaded.run();
}

export default async function GrainRunner(uri) {
  let runner = buildGrainRunner();
  let loaded = await runner.loadURL(uri);
  return loaded.run();
}

export { defaultFileLocator };
