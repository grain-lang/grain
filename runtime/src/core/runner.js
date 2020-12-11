import { GrainError } from '../errors/errors';
import { wasi, readFile, readURL, readBuffer } from './grain-module';
import { makePrint } from '../lib/print';
import { makeToString } from '../lib/to-string';
import { grainToString } from '../utils/utils';
import { makeMemoryChecker } from './heap';

function roundUp(num, multiple) {
  return multiple * (Math.floor((num - 1) / multiple) + 1);
}

const MALLOC_MODULE = 'stdlib-external/runtime/malloc';

export class GrainRunner {
  constructor(locator, managedMemory, opts) {
    this.modules = {};
    this.imports = {};
    this.idMap = {};
    this.locator = locator;
    this.managedMemory = managedMemory;
    opts = opts || {};
    this.opts = opts;
    this.ptr = 0;
    this.ptrZero = 0;
    this._checkMemory = makeMemoryChecker(this);
    this.limitMemory = opts.limitMemory || -1;
    this.postImports = () => {};
    this.imports['grainRuntime'] = {
      checkMemory: this._checkMemory,
      relocBase: 0,
      moduleRuntimeId: 0
    };
    let boundGrainToString = (v) => grainToString(this, v);
    this.imports['grainBuiltins'] = {
      toString: makeToString(boundGrainToString),
      print: makePrint(boundGrainToString)
    };
    this.loadMemoryManager()
  }

  async loadMemoryManager() {
    const mod = await this.locator(MALLOC_MODULE);
    if (mod) {
      this.memoryManager = mod
      this.memoryManager.instantiate({
        env: {
          memory: this.managedMemory._memory
        },
        memoryManager: {
          _malloc: this.managedMemory._malloc.bind(this.managedMemory),
          _free: this.managedMemory._free.bind(this.managedMemory),
          _growHeap: this.managedMemory.growHeap.bind(this.managedMemory),
          _initialHeapSize: this.managedMemory._memory.buffer.byteLength,
        }
      }, this)
    } else {
      throw new GrainError(-1, 'Failed to locate the memory manager.');
    }
  }

  checkMemory() {
    return this._checkMemory();
  }

  addImport(name, obj) {
    this.imports[name] = obj;
  }

  addImports(importObj) {
    Object.keys(importObj).forEach(m => {
      if (m in this.imports) {
        this.imports[m] = Object.assign(this.imports[m], importObj[m]);
      } else {
        this.imports[m] = importObj[m];
      }
    });
  }

  async load(name, mod) {
    // Currently, we use a "dumb" linking system,
    // in that the compiled object files do not include
    // any URI for locating their dependencies.
    // This will change in the future.
    let moduleImports = mod.importSpecs;
    // First, load any dependencies which need loading
    for (let imp of moduleImports) {
      if (!(imp.module in this.imports)) {
        // Sanity check
        if (imp.module in this.modules) {
          console.warn(`Ignoring possible cyclic dependency: ${imp.module}`);
          continue;
        }
        if (imp.module.startsWith('wasi_')) {
          Object.assign(this.imports, wasi.getImports(mod.wasmModule));
          continue;
        }
        // Should return an instance of GrainModule
        let located = await this.locator(imp.module);
        if (!located) {
          throw new GrainError(-1, `Failed to locate required module: ${imp.module}`);
        }
        this.modules[imp.module] = located;
        // This is a good point to debug when modules are loaded:
        // console.log(`Located module: ${imp.module}`);
        await this.load(imp.module, located);
        if (located.isGrainModule) {
          await located.start();
          this.imports['grainRuntime']['relocBase'] += located.tableSize;
          ++this.imports['grainRuntime']['moduleRuntimeId'];
        }
        this.ptrZero = this.ptr;
        this.imports[imp.module] = located.exports;
      }
    }
    this.postImports();
    // All of the dependencies have been loaded. Now we can instantiate with the import object.
    await mod.instantiate(this.imports, this);
    this.idMap[this.imports['grainRuntime']['moduleRuntimeId']] = name;
    if (!(name in this.modules)) {
      this.modules[name] = mod;
    }
    return mod;
  }

  async loadFile(path) {
    let module = await readFile(path);
    return this.load(module.name, module);
  }

  async runFileUnboxed(path) {
    let module = await this.loadFile(path);
    return module.runUnboxed();
  }

  async runFile(path) {
    let module = await this.loadFile(path);
    return module.start();
  }

  async loadURL(url) {
    let module = await readURL(url);
    return this.load(module.name, module);
  }

  async runURL(path) {
    let module = await this.loadURL(path);
    return module.start();
  }

  async runURLUnboxed(path) {
    let module = await this.loadURL(path);
    return module.runUnboxed();
  }

  async loadBuffer(buffer) {
    let module = await readBuffer(buffer);
    return this.load(module.name, module);
  }

  async runBuffer(buffer) {
    let module = await this.loadBuffer(buffer);
    return module.start();
  }

  async runBufferUnboxed(buffer) {
    let module = await this.loadBuffer(buffer);
    return module.runUnboxed();
  }
}
