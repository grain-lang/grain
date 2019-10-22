import { mallocJSModule } from './malloc';
import { GRAIN_BOOLEAN_TAG_TYPE, GRAIN_NUMBER_TAG_TYPE } from './tags';

/* Notes:
 * 
 * Grain's memory system uses a reference-counted garbage collector.
 * Because `ManagedMemory` is the point of communication between the Grain runtime
 * and the memory management system, we are able to neatly intercept the pointers
 * which are visible to the Grain runtime and tag them with our counts. It is
 * then incumbent upon the compiler (and any Grain plugins which interact with memory)
 * to make sure that reference counting functionality exists at appropriate places
 * in code. Here is the basic idea for how this looks in practice for an n-byte heap object:
 * 
 * [ 0 bit <reserved> ][ 11-bit counter ][ 4-bit value tag ][ n-bit payload ]
 * ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^~~~~~~~~~~~~~~~
 * {start address}                                          {pointer used by grain runtime}
 * 
 * Some notes about this scheme:
 * - The value tag is the same tag used to identify the pointer on the stack
 * - The value tag is admittedly inefficient (since it duplicates the stack value tag), 
 *   but it is a "good enough for now" approach for allowing traversal
 * - If studying this design, one will notice this only allows a maximum of 2048 references to a given 
 *   GC-managed object. In the future, we will ideally come up with a method of making this
 *   work with a greater number of values.
 * 
 *
 * Variable Naming Conventions:
 *   rawPtr  : The pointer returned by the call to malloc()
 *   userPtr : The pointer returned (and referenced by) to the Grain runtime
 */
export class ManagedMemory {
  constructor(memory) {
    this._memory = memory;
    this._headerSize = 4; // 16 bits in bytes
    var globNS;
    if (typeof window === 'undefined') {
      globNS = global;
    } else {
      globNS = window;
    }
    this._mallocModule = mallocJSModule(globNS, {
      initialHeapSize: memory.buffer.byteLength,
      growHeap: () => memory.grow(1)
    }, this._memory.buffer);
  }

  malloc(size, tag) {
    let rawPtr = this._mallocModule.malloc(size + this._headerSize);
    this.populateHeader(rawPtr, tag);
    let userPtr = ret + this._headerSize;
    return userPtr; // offset by headerSize
  }

  populateHeader(rawPtr, tag) {
    var heap = this._memory.buffer;
    for (let i = 0; i < this._headerSize; ++i) {
      heap[rawPtr + i] = 0;
    }
    heap[rawPtr + 2] = 1; // <- init refCount at 1 (from this allocation)
    heap[rawPtr + 3] = tag & 0b1111; // <- 4-bit tag
  }

  // [TODO] These next three methods can probably be made more efficient
  _getRefCount(userPtr) {
    let rawPtr = userPtr - this._headerSize;
    let heap = this._memory.buffer;
    let count = heap[rawPtr] & 0b0111; // reserved bit should always be zero, but let's be safe
    count = count << 4;
    count = count | heap[rawPtr + 1];
    count = count << 4;
    count = count | heap[rawPtr + 2];
    return count;
  }

  _getValueTag(userPtr) {
    let rawPtr = userPtr - this._headerSize;
    let heap = this._memory.buffer;
    return heap[rawPtr + 3];
  }

  incRef(userPtr) {
    let valueTag = this._getValueTag(userPtr);
    if (valueTag === GRAIN_NUMBER_TAG_TYPE || valueTag === GRAIN_BOOLEAN_TAG_TYPE) {
      // no ref-counting for primitives
      // [TODO] The type-checker should make this not ever be called ideally, but it
      //        significantly complicates our codegen
      return;
    }
    let heap = this._memory.buffer;
    let rawPtr = userPtr - this._headerSize;
    let refCount = this._getRefCount(userPtr);
    ++refCount;
    heap[rawPtr] = refCount & (0b0111 << 8);
    heap[rawPtr + 1] = refCount & (0b1111 << 4);
    heap[rawPtr + 2] = refCount & 0b1111;
    return userPtr;
  }

  decRef(userPtr) {
    let valueTag = this._getValueTag(userPtr);
    if (valueTag === GRAIN_NUMBER_TAG_TYPE || valueTag === GRAIN_BOOLEAN_TAG_TYPE) {
      // no ref-counting for primitives
      // [TODO] The type-checker should make this not ever be called ideally, but it
      //        significantly complicates our codegen
      return;
    }
    let heap = this._memory.buffer;
    let rawPtr = userPtr - this._headerSize;
    let refCount = this._getRefCount(userPtr);
    // [TODO] This is a blazing-hot code path. Should we eschew error-checking?
    if (refCount === 0) {
      // [TODO] the formatting on this is busted, but I'm on a plane with no WiFi and can't look up how to do it right
      throw new Error(`decRef called when reference count was zero. Dump: 0x${heap[userPtr-4]} 0x${heap[userPtr-3]} 0x${heap[userPtr-2]} 0x${heap[userPtr-1]} @ ${userPtr} (raw: ${userPtr - self._headerSize})`);
    }
    --refCount;
    if (refCount === 0) {
      // This object is ready to be freed.
      console.warn("Should traverse elements and decref() here!")
      this.free(userPtr);
    } else {
      heap[rawPtr] = refCount & (0b0111 << 8);
      heap[rawPtr + 1] = refCount & (0b1111 << 4);
      heap[rawPtr + 2] = refCount & 0b1111;
    }
    return userPtr;
  }

  free(userPtr) { // [TODO] Do we even need this?
    // stub
    this._mallocModule.malloc(userPtr - this._headerSize);
  }

  free(ptr) {
    return this._mallocModule.free(ptr);
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



