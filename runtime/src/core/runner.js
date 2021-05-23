import { wasi, readFile, readURL, readBuffer } from "./grain-module";
import { GRAIN_STRING_HEAP_TAG, GRAIN_GENERIC_HEAP_TAG_TYPE } from "./tags";

const MALLOC_MODULE = "GRAIN$MODULE$runtime/gc";
const STRING_MODULE = "GRAIN$MODULE$runtime/string";

export class GrainRunner {
  constructor(locator, managedMemory, opts) {
    this.modules = {};
    this.imports = {};
    this.idMap = {};
    this.locator = locator;
    this.managedMemory = managedMemory;
    opts = opts || {};
    this.opts = opts;
    this.postImports = () => {};
    this.encoder = new TextEncoder("utf-8");
    this.decoder = new TextDecoder("utf-8");
    this.imports["grainRuntime"] = {
      relocBase: 0,
      moduleRuntimeId: 0,
    };
    this.table = new WebAssembly.Table({
      element: "anyfunc",
      initial: 1024,
    });
  }

  get memoryManager() {
    if (!this._memoryManager) {
      this._memoryManager = this.modules[MALLOC_MODULE];
      if (!this._memoryManager)
        throw new Error("Failed to locate the memory manager.");
    }
    return this._memoryManager;
  }

  get stringModule() {
    if (!this._stringModule) {
      this._stringModule = this.modules[STRING_MODULE];
      if (!this._stringModule)
        throw new Error("Failed to locate the runtime string module.");
    }
    return this._stringModule;
  }

  async ensureStringModule() {
    if (this.modules[STRING_MODULE]) return;

    let located = await this.locator(STRING_MODULE);
    if (!located) {
      throw new Error(`Failed to ensure string module.`);
    }
    await this.load(STRING_MODULE, located);
    located.start();
  }

  grainValueToString(v) {
    let closure = this.stringModule.requiredExport("GRAIN$EXPORT$toString")
      .value;
    let grainString = this.stringModule.requiredExport("toString")(closure, v);
    let n = grainString;
    this.managedMemory._refreshViews();
    let byteView = this.managedMemory.u8view;
    let length = this.managedMemory.view[n / 4 + 1];
    let slice = byteView.slice(n + 8, n + 8 + length);
    let ret = this.decoder.decode(slice);
    this.managedMemory.free(grainString);
    return ret;
  }

  grainErrorValueToString(v) {
    // Supports basic numbers and strings
    // This is to avoid having all modules depend on all of toString if they
    // don't need it. Error values are only ever simple numbers or strings.
    if (v & 1) return (v >> 1).toString();
    if ((v & 7) === GRAIN_GENERIC_HEAP_TAG_TYPE) {
      if (this.managedMemory.view[v / 4] === GRAIN_STRING_HEAP_TAG) {
        let byteView = this.managedMemory.u8view;
        let length = this.managedMemory.view[v / 4 + 1];
        let slice = byteView.slice(v + 8, v + 8 + length);
        return this.decoder.decode(slice);
      }
    }
    return "<unknown value>";
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
          throw new Error(
            `Failed to locate required module: ${imp.module} [required by: ${name}]`
          );
        }
        this.modules[imp.module] = located;
        // This is a good point to debug when modules are loaded:
        // console.log(`Located module: ${imp.module}`);
        await this.load(imp.module, located);
        if (located.isStartable) {
          located.start();
        }
        this.ptrZero = this.ptr;
        this.imports[imp.module] = located.exports;
      }
    }
    this.postImports();
    // All of the dependencies have been loaded. Now we can instantiate with the import object.
    await mod.instantiate(this.imports, this);
    this.idMap[this.imports["grainRuntime"]["moduleRuntimeId"]] = name;
    if (mod.isStartable) {
      this.imports["grainRuntime"]["relocBase"] += mod.tableSize || 0;

      ++this.imports["grainRuntime"]["moduleRuntimeId"];
    }
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
