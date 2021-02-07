import { GrainError, makeThrowGrainError } from "../errors/errors";
import { wasi, readFile, readURL, readBuffer } from "./grain-module";
import { makeMemoryChecker } from "./heap";
import { managedMemory } from "../runtime";
import { GRAIN_STRING_HEAP_TAG, GRAIN_GENERIC_HEAP_TAG_TYPE } from "./tags";

function roundUp(num, multiple) {
  return multiple * (Math.floor((num - 1) / multiple) + 1);
}

const MALLOC_MODULE = "GRAIN$MODULE$runtime/malloc";

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
    this.encoder = new TextEncoder("utf-8");
    this.decoder = new TextDecoder("utf-8");
    const printNames = {};
    this.imports["grainRuntime"] = {
      checkMemory: this._checkMemory,
      relocBase: 0,
      moduleRuntimeId: 0,
      throwError: makeThrowGrainError(this),
      // Transition functions (to be used until this class is ported to AS; perhaps refactor at that time)
      variantExists: (moduleId, typeId, variantId) => {
        let moduleName = this.idMap[moduleId];
        if (!moduleName) return false;
        let module = this.modules[moduleName];
        if (!module) return false;
        let tyinfo = module.types[typeId];
        if (!tyinfo || Object.keys(tyinfo).length === 0) return false;
        let info = tyinfo[variantId];
        return !!info;
      },
      getVariantName: (moduleId, typeId, variantId) => {
        let moduleName = this.idMap[moduleId];
        let module = this.modules[moduleName];
        let modulePrintNames = printNames[moduleName];
        if (!modulePrintNames) {
          printNames[moduleName] = {};
          modulePrintNames = printNames[moduleName];
        }
        let tyinfo = module.types[typeId];
        let tyPrintNames = modulePrintNames[typeId];
        if (!tyPrintNames) {
          modulePrintNames[typeId] = {};
          tyPrintNames = modulePrintNames[typeId];
        }
        if (typeof tyPrintNames[variantId] === "undefined") {
          tyPrintNames[variantId] = this._makeGrainString(tyinfo[variantId][0]);
        }
        return tyPrintNames[variantId];
      },
      getVariantArity: (moduleId, typeId, variantId) => {
        let moduleName = this.idMap[moduleId];
        let module = this.modules[moduleName];
        let tyinfo = module.types[typeId];
        return tyinfo[variantId][1];
      },
      recordTypeExists: (moduleId, typeId) => {
        let moduleName = this.idMap[moduleId];
        let module = this.modules[moduleName];
        let tyinfo = module.types[typeId];
        return !!tyinfo;
      },
      getRecordArity: (moduleId, typeId) => {
        let moduleName = this.idMap[moduleId];
        let module = this.modules[moduleName];
        let tyinfo = module.types[typeId];
        return Object.keys(tyinfo).length;
      },
      getRecordFieldName: (moduleId, typeId, idx) => {
        let moduleName = this.idMap[moduleId];
        let module = this.modules[moduleName];
        let modulePrintNames = printNames[moduleName];
        if (!modulePrintNames) {
          printNames[moduleName] = {};
          modulePrintNames = printNames[moduleName];
        }
        let tyinfo = module.types[typeId];
        let tyPrintNames = modulePrintNames[typeId];
        if (!tyPrintNames) {
          modulePrintNames[typeId] = {};
          tyPrintNames = modulePrintNames[typeId];
        }
        if (typeof tyPrintNames[idx] === "undefined") {
          tyPrintNames[idx] = this._makeGrainString(Object.keys(tyinfo)[idx]);
        }
        return tyPrintNames[idx];
      },
    };
  }

  get memoryManager() {
    if (!this._memoryManager) {
      this._memoryManager = this.modules[MALLOC_MODULE];
      if (!this._memoryManager)
        throw new GrainError(-1, "Failed to locate the memory manager.");
    }
    return this._memoryManager;
  }

  // [HACK] Temporarily used while we transition to AS-based runtime
  _makeGrainString(v) {
    let buf = this.encoder.encode(v);
    let userPtr = this.managedMemory.malloc(4 * 2 + ((v.length - 1) / 4 + 1));
    let ptr = userPtr / 4;
    let view = this.managedMemory.view;
    view[ptr] = GRAIN_STRING_HEAP_TAG;
    view[ptr + 1] = v.length;
    let byteView = this.managedMemory.u8view;
    for (let i = 0; i < buf.length; ++i) {
      byteView[i + ptr * 4 + 8] = buf[i];
    }
    return userPtr | GRAIN_GENERIC_HEAP_TAG_TYPE;
  }

  // [HACK] Temporarily used while we transition to AS-based runtime
  grainValueToString(v) {
    let grainString = this.imports["stdlib-external/runtime"]["grainToString"](
      v
    );
    let n = grainString ^ GRAIN_GENERIC_HEAP_TAG_TYPE;
    let byteView = this.managedMemory.u8view;
    let length = this.managedMemory.view[n / 4 + 1];
    let slice = byteView.slice(n + 8, n + 8 + length);
    let ret = this.decoder.decode(slice);
    this.managedMemory.free(grainString);
    return ret;
  }

  checkMemory() {
    return this._checkMemory();
  }

  addImport(name, obj) {
    this.imports[name] = obj;
  }

  addImports(importObj) {
    Object.keys(importObj).forEach((m) => {
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
      // useful for debugging:
      // console.log(`processing import ${imp.module} [required by: ${name}] [imported symbol: ${imp.name}]`)
      if (!(imp.module in this.imports)) {
        // Sanity check
        if (imp.module in this.modules) {
          console.warn(`Ignoring possible cyclic dependency: ${imp.module}`);
          continue;
        }
        if (imp.module.startsWith("wasi_")) {
          Object.assign(this.imports, wasi.getImports(mod.wasmModule));
          continue;
        }
        // Should return an instance of GrainModule
        let located = await this.locator(imp.module);
        if (!located) {
          throw new GrainError(
            -1,
            `Failed to locate required module: ${imp.module} [required by: ${name}]`
          );
        }
        this.modules[imp.module] = located;
        // This is a good point to debug when modules are loaded:
        // console.log(`Located module: ${imp.module}`);
        await this.load(imp.module, located);
        if (located.isGrainModule) {
          await located.start();
          this.imports["grainRuntime"]["relocBase"] += located.tableSize;
          ++this.imports["grainRuntime"]["moduleRuntimeId"];
        }
        this.ptrZero = this.ptr;
        this.imports[imp.module] = located.exports;
      }
    }
    this.postImports();
    // All of the dependencies have been loaded. Now we can instantiate with the import object.
    await mod.instantiate(this.imports, this);
    this.idMap[this.imports["grainRuntime"]["moduleRuntimeId"]] = name;
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
