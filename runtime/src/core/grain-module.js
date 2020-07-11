import { GrainError } from '../errors/errors';

import { WASI } from "@wasmer/wasi/lib/index.cjs";
import { WasmFs } from "@wasmer/wasmfs";

let bindings

if (__RUNTIME_BROWSER) {
  const wasmFs = new WasmFs();
  bindings = {
    ...wasiBindings.default,
    fs: wasmFs.fs
  }
} else {
  bindings = wasiBindings.default
}

export const wasi = new WASI({
  args: __RUNTIME_BROWSER ? [] : process.argv,
  env: __RUNTIME_BROWSER ? {} : process.env,
  bindings,
  preopens: {
    '/sandbox': __RUNTIME_BROWSER ? '' : process.cwd()
  }
});

export class GrainModule {
  constructor(wasmModule, name) {
    this.wasmModule = wasmModule;
    this.name = name; // name is optional
    this.runner = null;
    this._cmi = null;
    this._instantiated = null;
  }

  get isInstantiated() {
    return this._instantiated !== null;
  }

  get instantiated() {
    if (!this.isInstantiated) {
      throw new GrainError(-1, `Module${this.name ? (" " + this.name) : ""} must be instantiated before use`);
    }
    return this._instantiated;
  }

  get cmi() {
    if (!this._cmi) {
      let sections = WebAssembly.Module.customSections(this.wasmModule, "cmi");
      if (sections.length === 0) {
        console.warn(`Grain Module${this.name ? (" " + this.name) : ""} missing CMI information`);
        return null;
      }
      let section = sections[0];
      let view = new Uint32Array(section.slice(0, 20));
      // [grain_magic, abi_major, abi_minor, abi_patch, sec_name_length]
      let sectionNameLength = view[4];
      let startOffset = (4 * 5) + sectionNameLength;
      let bytes = section.slice(startOffset, section.byteLength);
      let decoder = new TextDecoder("utf-8");
      let decodedSection = decoder.decode(bytes);
      this._cmi = JSON.parse(decodedSection);
    }
    return this._cmi;
  }

  get importSpecs() {
    return WebAssembly.Module.imports(this.wasmModule);
  }

  get exportSpecs() {
    return WebAssembly.Module.exports(this.wasmModule);
  }

  // TODO: This is a low-level function. There should be convenience
  //       accessors for runtime-required custom binary sections.
  get customSections() {
    return WebAssembly.Module.customSections(this.wasmModule);
  }

  get exports() {
    return this.instantiated.exports;
  }

  get isGrainModule() {
    return !!this.exports["_start"]
  }

  requiredExport(key) {
    let exports = this.exports;
    if (!(key in exports)) {
      throw new Error(`Module ${this.name} missing required export: ${key}`);
    }
    return exports[key];
  }

  start() {
    wasi.start(this.instantiated)
  }

  get tableSize() {
    return this.requiredExport("GRAIN$TABLE_SIZE");
  }

  get cleanupGlobals() {
    return this.requiredExport("GRAIN$CLEANUP_GLOBALS");
  }

  get types() {
    if (!this._types) {
      let cmi = this.cmi;
      if (!cmi) {
        return null;
      }
      this._types = {};
      cmi.cmi_sign.forEach(elt => {
        if (elt[0] !== "TSigType") {
          return;
        }
        let id = elt[2].type_path[1].stamp
        let typ = {};
        this._types[id] = typ;
        let desc = elt[2];
        let kind = desc.type_kind;
        if (!kind) return;
        switch (kind[0]) {
          case "TDataVariant": {
            let variants = kind[1];
            variants.forEach((variant, vidx) => {
              let name = variant.cd_id.name;
              let arity;
              if (variant.cd_args[0] === "TConstrSingleton") {
                arity = 0;
              } else {
                // TConstrTuple
                arity = variant.cd_args[1].length;
              }
              typ[vidx] = [name, arity];
            })
            break;
          }
          case "TDataRecord": {
            let fields = kind[1];
            fields.forEach((field, fidx) => {
              let name = field.rf_name.name;
              typ[name] = fidx;
            })
            break;
          }
          default:
            return
        }
      })
    }
    return this._types;
  }

  async instantiate(importObj, runner) {
    /*console.log(`Instantiating ${this.name}`);
    console.log(`imports:`);
    Object.keys(importObj).forEach(m => {
      console.log(`\timports[${m}]:`);
      let mod = importObj[m];
      Object.keys(mod).forEach(v => {
        let val = mod[v];
        let valstr = (val instanceof Function || typeof val === 'function') ? '<function>' : val;
        console.log(`\t\t${m}.${v}: ${valstr}`);
      });
      console.log('');
    });*/
    this.runner = runner;
    try {
      this._instantiated = await WebAssembly.instantiate(this.wasmModule, importObj);
    } catch (e) {
      console.error(`Exception while instantiating ${this.name}`);
      throw e;
    }
    //console.log(`Instantiated: ${this._instantiated}.`);
    //console.log(`fields: ${Object.keys(this._instantiated)}`);
  }

  async runUnboxed() {
    // This works because we use @wasmer/wasi, but will break in the future.
    // Only the tests currently rely on this.
    wasi.setMemory(this.requiredExport('memory'));
    return await this.requiredExport('_start')();
  }
}

export async function readFile(path) {
  const fs = require('fs');
  let modname = path.replace(/\.gr\(lib\)?$/, '').replace(/.*\/([^/]+)/, '$1');
  //console.log(`Reading module '${modname}' from file: ${path}`);
  let module = await WebAssembly.compile(fs.readFileSync(path));
  return new GrainModule(module, modname);
}

export async function readURL(url) {
  let modname = url; // FIXME
  let response = await fetch(url);
  if (!response.ok) throw new Error(`[Grain] Could not load ${url} due to a network error.`);
  let module = await WebAssembly.compileStreaming(response);
  return new GrainModule(module, modname);
}
