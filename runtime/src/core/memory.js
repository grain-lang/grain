import { mallocJSModule } from './malloc';
import { getTagType, GRAIN_CONST_TAG_TYPE, GRAIN_NUMBER_TAG_TYPE, GRAIN_TUPLE_TAG_TYPE, GRAIN_GENERIC_HEAP_TAG_TYPE, GRAIN_LAMBDA_TAG_TYPE,
         GRAIN_STRING_HEAP_TAG, GRAIN_DOM_ELEM_TAG, GRAIN_ADT_HEAP_TAG } from './tags';
import { grainAdtInfo } from '../utils/utils';

const TRACE_MEMORY = false;

function trace(msg) {
  if (TRACE_MEMORY) {
    console.warn(msg);
  }
}

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
    this._headerSize = 8; // 32 bits in bytes (extra space needed for alignment)
    this._runtime = null;
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

  setRuntime(runtime) {
    this._runtime = runtime;
  }

  malloc(size) {
    trace(`malloc(0x${this._toHex(size)})`);
    let rawPtr = this._mallocModule.malloc(size + this._headerSize);
    trace('\tpopulateHeader')
    this.populateHeader(rawPtr);
    trace('\tend_populateHeader')
    let userPtr = rawPtr + this._headerSize;
    trace(`malloc: ${this._memdump(userPtr)}`);
    return userPtr; // offset by headerSize
  }

  populateHeader(rawPtr) {
    let tag = 0x0; // reserved
    trace('populateHeader');
    var heap = this._memory.buffer;
    for (let i = 0; i < this._headerSize; ++i) {
      heap[rawPtr + i] = 0;
    }
    heap[rawPtr + 2] = 1; // <- init refCount at 1 (from this allocation)
    heap[rawPtr + 3] = tag & 0b1111; // <- 4-bit tag
  }

  // [TODO] These next three methods can probably be made more efficient
  _getRefCount(userPtr) {
    trace('_getRefCount');
    let rawPtr = userPtr - this._headerSize;
    let heap = this._memory.buffer;
    let count = heap[rawPtr] & 0b0111; // reserved bit should always be zero, but let's be safe
    count = count << 4;
    count = count | heap[rawPtr + 1];
    count = count << 4;
    count = count | heap[rawPtr + 2];
    trace(`\t${count}`);
    return count;
  }

  _getValueTag(userPtr) {
    trace('_getValueTag');
    let rawPtr = userPtr - this._headerSize;
    let heap = this._memory.buffer;
    return heap[rawPtr + 3];
  }

  _toHex(n) {
    if (!TRACE_MEMORY) {
      return undefined; // improve performance; this is a very hot path in production
    }
    return (new Number(n)).toString(16);
  }

  _memdump(userPtr) {
    let heap = this._memory.buffer;
    if (!TRACE_MEMORY) {
      return undefined; // improve performance; this is a very hot path in production
    }
    return `Dump: 0x${this._toHex(heap[userPtr-3])} 0x${this._toHex(heap[userPtr-2])} 0x${this._toHex(heap[userPtr-1])} @ 0x${this._toHex(userPtr)} (raw: 0x${this._toHex(userPtr - this._headerSize)})`
  }

  decRef64(userPtr) {
    trace('decRef64 [see next]');
    return this.decRef(userPtr);
  }

  incRef64(userPtr) {
    trace('incRef64 [see next]');
    return this.incRef(userPtr);
  }

  incRefADT(userPtr) {
    trace('incRefADT [see next]');
    return this.incRef(userPtr);
  }

  incRefTuple(userPtr) {
    trace('incRefTuple [see next]');
    return this.incRef(userPtr);
  }

  incRefBackpatch(userPtr) {
    trace('incRefBackpatch [see next]');
    return this.incRef(userPtr);
  }

  incRefSwapBind(userPtr) {
    trace('incRefSwapBind [see next]');
    return this.incRef(userPtr);
  }

  incRefArgBind(userPtr) {
    trace('incRefArgBind [see next]');
    return this.incRef(userPtr);
  }

  incRefLocalBind(userPtr) {
    trace('incRefLocalBind [see next]');
    return this.incRef(userPtr);
  }

  incRef(userPtr) {
    let origInput = userPtr;
    trace(`incRef(0x${this._toHex(userPtr)})`);
    let ptrTagType = getTagType(userPtr);
    if (userPtr === 0 || ptrTagType === GRAIN_NUMBER_TAG_TYPE || ptrTagType === GRAIN_CONST_TAG_TYPE) {
      // no ref-counting for primitives
      // [TODO] The type-checker should make this not ever be called ideally, but it
      //        significantly complicates our codegen
      trace(`\tbailing out (ptrTagType: ${ptrTagType})`);
      return origInput;
    }
    userPtr = userPtr & (~7);
    trace('\tincrementing...');
    let heap = this._memory.buffer;
    let rawPtr = userPtr - this._headerSize;
    let refCount = this._getRefCount(userPtr);
    ++refCount;
    trace(`\tnew count: ${refCount}`);
    heap[rawPtr] = refCount & (0b0111 << 8);
    heap[rawPtr + 1] = refCount & (0b1111 << 4);
    heap[rawPtr + 2] = refCount & 0b1111;
    trace(`\tdump: ${this._memdump(userPtr)}`);
    return origInput;
  }

  decRef(userPtr) {
    trace(`decRef(0x${this._toHex(userPtr)})`);
    let origInput = userPtr;
    let ptrTagType = getTagType(userPtr);
    if (userPtr === 0 || ptrTagType === GRAIN_NUMBER_TAG_TYPE || ptrTagType === GRAIN_CONST_TAG_TYPE) {
      // no ref-counting for primitives
      // [TODO] The type-checker should make this not ever be called ideally, but it
      //        significantly complicates our codegen
      trace(`\tbailing out (ptrTagType: ${ptrTagType})`);
      return origInput;
    }
    userPtr = userPtr & (~7);
    let heap = this._memory.buffer;
    let rawPtr = userPtr - this._headerSize;
    let refCount = this._getRefCount(userPtr);
    // [TODO] This is a blazing-hot code path. Should we eschew error-checking?
    if (refCount === 0) {
      // [TODO] the formatting on this is busted, but I'm on a plane with no WiFi and can't look up how to do it right
      throw new Error(`decRef called when reference count was zero. ${this._memdump(userPtr)}`);
    }
    --refCount;
    trace('\tdecrementing...');
    trace(`\tnew count: ${refCount}`);
    if (refCount === 0) {
      // This object is ready to be freed.
      let view = new Int32Array(heap);
      trace("Should traverse elements and decref() here!")
      switch (ptrTagType) {
        case GRAIN_TUPLE_TAG_TYPE:
          let tupleIdx = userPtr / 4;
          let tupleLength = view[tupleIdx];
          if (tupleLength & 0x80000000) {
            // cyclic. return
            return origInput;
          } else {
            view[tupleIdx] |= 0x80000000;
            for (let i = 0; i < tupleLength; ++i) {
              this.decRef(view[tupleIdx + i + 1])
            }
            view[tupleIdx] = tupleLength;
          }
          break;
        case GRAIN_LAMBDA_TAG_TYPE:
          // 4 * (idx + 3)
          let lambdaIdx = userPtr / 4;
          let numFreeVars = view[lambdaIdx + 2];
          for (let i = 0; i < numFreeVars; ++i) {
            this.decRef(view[lambdaIdx + 3 + i]);
          }
          break;
        case GRAIN_GENERIC_HEAP_TAG_TYPE:
          let genericHeapValUserPtr = userPtr;
          switch (view[genericHeapValUserPtr / 4]) {
            case GRAIN_ADT_HEAP_TAG:
              if (this._runtime) {
                let x = genericHeapValUserPtr / 4;
                let [variantName, arity] = grainAdtInfo(this._runtime, genericHeapValUserPtr);
                for (let i = 0; i < arity; ++i) {
                  this.decRef(view[x + 5 + i]);
                }
              }
            default:
              // No extra traversal needed for Strings and DOM elements
          }
          break;
        default:
          console.warn(`<decRef: Unexpected value tag: 0x${this._toHex(ptrTagType)}> [userPtr=0x${this._toHex(userPtr)}]`)
      }
      this.free(userPtr);
    } else {
      heap[rawPtr] = refCount & (0b0111 << 8);
      heap[rawPtr + 1] = refCount & (0b1111 << 4);
      heap[rawPtr + 2] = refCount & 0b1111;
    }
    return origInput;
  }

  free(userPtr) { // [TODO] Do we even need this?
    // stub
    console.warn(`free 0x${(new Number(userPtr)).toString(16)}`);
    this._mallocModule.free(userPtr - this._headerSize);
    trace('end_free');
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


