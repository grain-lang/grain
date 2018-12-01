import { GrainError } from '../errors/errors';
import { readFile, readURL } from './grain-module';

function roundUp(num, multiple) {
  return multiple * (Math.floor((num - 1) / multiple) + 1);
}

export class GrainRunner {
  constructor(locator, opts) {
    this.modules = {};
    this.imports = {};
    this.locator = locator;
    opts = opts || {};
    this.opts = opts;
    this.ptr = 0;
    this.ptrZero = 0;
    this.imports['grainRuntime'] = {
      malloc: (bytes) => {
        // Basic malloc implementation for now
        let ret = this.ptr;
        this.ptr += roundUp(bytes, 8);
        return ret;
      },
      relocBase: 0,
      moduleRuntimeId: 0
    };
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
        // Should return an instance of GrainModule
        let located = await this.locator(imp.module);
        if (!located) {
          throw new GrainError(-1, `Failed to locate required module: ${imp.module}`);
        }
        this.modules[imp.module] = located;
        await this.load(imp.module, located);
        await located.run();
        this.ptrZero = this.ptr;
        this.imports['grainRuntime']['relocBase'] += located.tableSize;
        this.imports['grainRuntime']['moduleRuntimeId']++;
        this.imports[imp.module] = located.exports;
      }
    }
    // All of the dependencies have been loaded. Now we can instantiate with the import object.
    await mod.instantiate(this.imports);
    if (!(name in this.modules)) {
      this.modules[name] = mod;
    }
    return mod;
  }

  async loadFile(path) {
    let module = await readFile(path);
    return this.load(module.name, module);
  }

  async runFile(path) {
    let module = await this.loadFile(path);
    return module.run();
  }

  async loadURL(url) {
    let module = await readURL(url);
    return this.load(module.name, module);
  }

  async runURL(path) {
    let module = await this.loadURL(path);
    return module.run();
  }
}
