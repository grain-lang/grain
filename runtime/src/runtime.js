import "fast-text-encoding";

import { printClosure } from "./core/closures";
import { ManagedMemory } from "./core/memory";
import { GrainRunner } from "./core/runner";
import { defaultFileLocator } from "./utils/locators";

function debugPrint(v) {
  console.log(`0x${v.toString(16)} (0b${v.toString(2)})`);
}

function buildImportObj(runner) {
  return {
    env: {
      memory: runner.managedMemory._memory,
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
      mem: runner.managedMemory._memory,
      tbl: runner.table,
    },
  };
}

export function buildGrainRunner(locator, opts) {
  const managedMemory = new ManagedMemory(opts);
  const runner = new GrainRunner(locator || (() => null), managedMemory, opts);
  runner.addImports(buildImportObj(runner));
  managedMemory.setRuntime(runner);
  return runner;
}

// TODO: Migrate API to expose runner object directly

export async function GrainNodeRunner(path) {
  let runner = buildGrainRunner();
  let loaded = await runner.loadFile(path);
  return loaded.run();
}

export default async function DefaultGrainRunner(uri) {
  let runner = buildGrainRunner();
  let loaded = await runner.loadURL(uri);
  return loaded.run();
}

export { defaultFileLocator };
