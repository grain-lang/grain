export class ManagedMemory {
  constructor({ initialMemoryPages, maximumMemoryPages }) {
    this._memory = new WebAssembly.Memory({
      initial: initialMemoryPages || 64,
      maximum: maximumMemoryPages,
    });
    this._view = new Int32Array(this._memory.buffer);
    this._uview = new Uint32Array(this._memory.buffer);
    this._u8view = new Uint8Array(this._memory.buffer);
    this._f32view = new Float32Array(this._memory.buffer);
    this._f64view = new Float64Array(this._memory.buffer);
    this._runner = null;
  }

  _refreshViews() {
    this._view = new Int32Array(this._memory.buffer);
    this._uview = new Uint32Array(this._memory.buffer);
    this._u8view = new Uint8Array(this._memory.buffer);
    this._f32view = new Float32Array(this._memory.buffer);
    this._f64view = new Float64Array(this._memory.buffer);
  }

  get view() {
    return this._view;
  }

  get uview() {
    return this._uview;
  }

  get u8view() {
    return this._u8view;
  }

  get f32view() {
    return this._f32view;
  }

  get f64view() {
    return this._f64view;
  }

  setRunner(runner) {
    this._runner = runner;
  }

  incRef(userPtr) {
    let closure = this._runner.memoryManager.requiredExport(
      "GRAIN$EXPORT$incRef"
    ).value;
    return this._runner.memoryManager.requiredExport("incRef")(
      closure,
      userPtr
    );
  }

  decRef(userPtr, src, ignoreZeros) {
    let closure = this._runner.memoryManager.requiredExport(
      "GRAIN$EXPORT$decRef"
    ).value;
    return this._runner.memoryManager.requiredExport("decRef")(
      closure,
      userPtr
    );
  }

  decRefIgnoreZeros(userPtr) {
    let closure = this._runner.memoryManager.requiredExport(
      "GRAIN$EXPORT$decRefIgnoreZeros"
    ).value;
    return this._runner.memoryManager.requiredExport("decRefIgnoreZeros")(
      closure,
      userPtr
    );
  }

  malloc(userPtr) {
    let closure = this._runner.memoryManager.requiredExport(
      "GRAIN$EXPORT$malloc"
    ).value;
    return this._runner.memoryManager.requiredExport("malloc")(
      closure,
      userPtr
    );
  }

  free(userPtr) {
    let closure = this._runner.memoryManager.requiredExport("GRAIN$EXPORT$free")
      .value;
    this._runner.memoryManager.requiredExport("free")(closure, userPtr);
  }
}
