import { mallocJSModule } from './malloc';

export class ManagedMemory {
  constructor(memory) {
    this._memory = memory;
    this._heap = new Int32Array(memory.buffer);
    var globNS;
    if (typeof window === 'undefined') {
      globNS = global;
    } else {
      globNS = window;
    }
    this._mallocModule = mallocJSModule(globNS, {
      initialHeapSize: memory.buffer.byteLength,
      growHeap: () => memory.grow(1)
    }, this._heap);
  }

  malloc(size) {
    return this._mallocModule.malloc(size);
  }
}

class ManagedType {
  constructor(name, initializer, finalizer, to_string, equals, tag) {
    this._name = name;
    this._initializer = initializer;
    this._finalizer = finalizer;
    this._to_string = to_string;
    this._equals = equals;
    this._tag = tag;
  }

  get name() {
    return this._name;
  }

  get tag() {
    return this._tag;
  }

  initialize(memory, address) {
    if (this._initializer) {
      this._initializer(memory, address);
    }
  }

  finalize(memory, address) {
    if (this._finalizer) {
      this._finalizer(memory, address);
    }
  }

  to_string(memory, address) {
    if (this._to_string) {
      return this._to_string(memory, address);
    }
    return `#<Instance: ${this.name}>`;
  }

  equals(memory, address1, address2) {
    if (this._equals) {
      return this._equals(memory, address1, address2);
    }
    return address1 === address2;
  }
}



