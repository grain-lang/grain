import 'fast-text-encoding';

import { heapController, grainCheckMemory } from './core/heap';
import { printClosure } from './core/closures';
import { readFile, readURL } from './core/grain-module';
import { GrainRunner } from './core/runner';
import { throwGrainError } from './errors/errors';
import { grainToJSVal } from './utils/utils';

import { print, debugPrint } from './lib/print';
import equal from './lib/equal';
import toString from './lib/to-string';
import * as libStrings from './lib/strings';
import * as libDOM from './lib/DOM';

export let grainModule;

export const memory = new WebAssembly.Memory({initial: 1});
export const table = new WebAssembly.Table({element: 'anyfunc', initial: 128});
export const view = new Int32Array(memory.buffer);
export const encoder = new TextEncoder("utf-8");
export const decoder = new TextDecoder("utf-8");

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

function normalizeSlash(s) {
  return s.replace(/\/$/, '');
}

function wrapBase(base) {
  if (!base) {
    return () => base;
  } else if (base instanceof String || typeof base === 'string') {
    let normalized = normalizeSlash(base);
    return () => normalized;
  } else {
    return () => (normalizeSlash(base()));
  }
}

// Default locator definitions. 'base' can either
// be a constant path or a thunk yielding a path.
// If 'base' yields 'null', then 'null' is returned
export function defaultURLLocator(base)  {
  // normalize trailing slash
  let baseFunc = wrapBase(base);
  return async (raw) => {
    let module = raw.replace(/^GRAIN\$MODULE\$/, '');
    let b = baseFunc();
    if (b === null) {
      return null;
    }
    return readURL(b + "/" + module + ".wasm");
  };
}

export function defaultFileLocator(base) {
  // normalize trailing slash
  let baseFunc = wrapBase(base);
  return async (raw) => {
    let module = raw.replace(/^GRAIN\$MODULE\$/, '');
    let b = baseFunc();
    if (b === null) {
      return null;
    }
    let fullpath = b + "/" + module + ".wasm";
    if (!fs.existsSync(fullpath)) {
      return null;
    }
    return readFile(fullpath);
  };
}

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
  let loaded = runner.loadURL(uri);
  return loaded.run();
}

