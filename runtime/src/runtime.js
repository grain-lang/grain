import "fast-text-encoding";

import { printClosure } from "./core/closures";
import { ManagedMemory } from "./core/memory";
import { GrainRunner } from "./core/runner";
import { defaultFileLocator } from "./utils/locators";

export let grainModule;

// Workaround for this memory being initialized statically on module load
export const memory = new WebAssembly.Memory({
  initial: process.env.GRAIN_INIT_MEMORY_PAGES || 64,
  maximum: process.env.GRAIN_MAX_MEMORY_PAGES,
});
export const table = new WebAssembly.Table({
  element: "anyfunc",
  initial: 1024,
});
export const encoder = new TextEncoder("utf-8");
export const decoder = new TextDecoder("utf-8");
export const managedMemory = new ManagedMemory(memory);

export const malloc = managedMemory.malloc.bind(managedMemory);
export const free = managedMemory.free.bind(managedMemory);

function debugPrint(v) {
  console.log(`0x${v.toString(16)} (0b${v.toString(2)})`);
}

const importObj = {
  env: {
    memory,
    abort(err) {
      throw new Error(`abort ${err}`);
    },
  },
  console: {
    debug: debugPrint,
    printClosure: printClosure,
    tracepoint: (n) => console.log(`tracepoint ${n} reached`),
  },
  grainRuntime: {
    mem: memory,
    tbl: table,
  },
  memoryManager: {
    _growHeap: managedMemory.growHeap.bind(managedMemory),
    _initialHeapSize: managedMemory._memory.buffer.byteLength,
  },
};

export function buildGrainRunner(locator, opts) {
  // [TODO] Find something which avoids global state!
  let runner = new GrainRunner(locator || ((x) => null), managedMemory, opts);
  runner.addImports(importObj);
  managedMemory.setRuntime(runner);
  return runner;
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
