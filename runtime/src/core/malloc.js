/*
 * This module implements a memory allocator for JS ArrayBuffers.
 * The algorithm is quite simple, being based on the memory allocator
 * from Pages 185-188 of K&R C (2nd edition). As this is in asm.js,
 * the code is a little opaque (especially considering that we are
 * essentially hand-laying out structs in memory).
 */
import {toHex} from '../utils/utils';

export function mallocJSModule(stdlib, foreign, ext_heap) {
  "use asm";

  /* UNDERSTANDING THE STRUCTURE OF THE FREE LIST
   * The original K&R definition for the free list entry type was the following:
   *
   *     union header {
   *         struct {
   *             union header *ptr;
   *             unsigned size;
   *         } s;
   *         long x; // <- forces 8-byte alignment
   *     };
   *
   * In memory, this is really just two ints (assuming we're working in 32-bit mode).
   * As such, we manually lay out the entries in the heap as follows (note that the
   * array accesses follow asm.js constraints):
   *
   *            JS                   C Equivalent
   *   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   *   var ptr               ===  union header *ptr
   *   heap[ptr >> 2]        ===  ptr->s.ptr
   *   heap[(ptr + 4) >> 2]  ===  ptr->s.size
   */

  /**
   * Minimum amount (in bytes) which the heap
   * may be grown by.
   */
  var MIN_HEAP_REQUEST = 8192;

  /**
   * Pointer to the start of the free list. This is always a multiple of
   * 8, with the exception of its initial value (used as a sentinel).
   */
  var freePtr = 1;

  /**
   * Size (in bytes) of entries in the free list.
   */
  var mallocHeaderSize = 8;

  /**
   * log_2(mallocHeaderSize) (multiplication by the header
   * size is equivalent to left-shifting by this amount)
   */
  var logMallocHeaderSize = 3;

  /**
   * The memory space which this module allocates from.
   * Unsigned 32-bit integers form the address space.
   * To get the index of a given address within this heap,
   * divide the address by four. (in C-speak, think of this
   * as having type (void*))
   */
  var heap = new stdlib.Uint32Array(ext_heap);

  /**
   * FFI binding to a function which requests the heap be grown
   * by a given number of bytes.
   */
  var extGrowHeap = foreign.growHeap;

  /**
   * Initial size of the given heap (in bytes).
   */
  var initialHeapSize = foreign.initialHeapSize | 0;

  /**
   * The current size of the heap (in bytes).
   */
  var heapSize = 0;

  /**
   * Requests that the heap be grown by the given number of bytes.
   *
   * @param {number} nbytes - The number of bytes requested
   * @return {number} - If unsuccessful, -1. Otherwise, the pointer to the beginning of the extended region.
   */
  function growHeap(nbytes) {
    nbytes = nbytes|0;
    var reqSize = 0;
    var reqResult = 0;
    var origSize = 0;
    origSize = heapSize|0;
    // If the size has not been initialized, do so.
    if ((heapSize|0) == 0) {
      heapSize = initialHeapSize|0;
      if ((nbytes|0) > (heapSize|0)) {
        // More bytes requested than the initial heap size,
        // so we need to request more anyway.
        reqSize = (nbytes - heapSize)|0;
        reqResult = extGrowHeap(reqSize|0)|0;
        if ((reqResult|0) == (-1|0)) {
          return -1|0;
        }
        heapSize = ((heapSize|0) + (reqResult|0))|0;
      }
      return origSize|0;
    } else {
      // The size has already been initialized, so call the external function.
      reqResult = extGrowHeap(nbytes|0)|0;
      if ((reqResult|0) == (-1|0)) {
        return -1|0;
      }
      heapSize = ((heapSize|0) + (reqResult|0))|0;
    }
    return origSize|0;
  }

  /**
   * Allocates the requested number of bytes, returning a pointer.
   *
   * @param nbytes {number} - The number of bytes to allocate
   * @return {number} - The pointer to the allocated region (8-byte aligned), or -1 if the allocation failed.
   */
  function malloc(nbytes) {
    nbytes = nbytes|0;
    var p = 0;
    var prevp = 0;
    var nunits = 0;
    var i = 0;
    var newSize = 0;
    var size = 0;
    // Set nbytes to the next multiple of mallocHeaderSize greater
    // than the given size
    nunits = (nbytes + mallocHeaderSize - 1)|0;
    nunits = ((nunits|0) / (mallocHeaderSize|0))|0;
    nunits = (nunits + 1)|0;
    nbytes = (nunits|0) << (logMallocHeaderSize|0); // multiply by header size
    size = size|0;
    p = p|0;

    prevp = freePtr|0;
    // Handle initialization
    if ((heapSize|0) == 0) {
      heap[0] = 0;
      freePtr = 0;
      prevp = 0;
      heap[1] = 0;
    }
    // Search the freelist for any blocks large enough.
    for (p = heap[prevp >> 2]|0; ; prevp = p|0, p = heap[p >> 2]|0) {
      size = heap[(p + 4) >> 2]|0;
      if ((size|0) >= (nbytes|0)) {
        // If this block is big enough, allocate from it.
        if ((size|0) == (nbytes|0)) {
          // It's exactly the right size!
          heap[prevp >> 2] = heap[p >> 2]|0;
        } else {
          // Shrink it as needed
          newSize = (size - nbytes)|0;
          heap[(p + 4) >> 2] = newSize|0;
          p = (p + newSize)|0;
          heap[(p + 4) >> 2] = nbytes|0;
        }
        // Update the pointer to the free list.
        freePtr = prevp|0;
        // Return the address of the region past the header
        return ((p + 8)|0);
      }
      // We've reached the end of the free list. Time to grow the heap.
      if ((p|0) == (freePtr|0)) {
        // Attempt to grow the heap
        p = morecore(nbytes|0)|0;
        // If growing the heap failed, return -1.
        if ((p|0) == (-1|0)) {
          // Error
          return -1|0;
        }
      }
    }
    // Error
    return -1|0;
  }

  /**
   * Asks the runtime for more heap memory.
   * (if you can't tell from the fact that the name is reminiscient
   *  of the 1970s, the name of this function is taken from K&R).
   *
   * @param nbytes {number} - The number of bytes to try to grow the heap by
   * @return {number} - If successful, a pointer to the start of the free list. If not successful, -1.
   */
  function morecore(nbytes) {
    nbytes = nbytes|0;
    var cp = 0;
    var up = 0;
    var origSize = 0;
    var grownAmount = 0;
    origSize = heapSize|0;
    // Don't ask for less than MIN_HEAP_REQUEST bytes
    if ((nbytes|0) < (MIN_HEAP_REQUEST|0)) {
      nbytes = MIN_HEAP_REQUEST|0;
    }
    cp = growHeap(nbytes)|0;
    // If there was an error, return
    if ((cp|0) == (-1|0)) {
      return -1|0;
    }
    // Set the size of the new block to the amount the
    // heap was grown.
    grownAmount = ((heapSize|0) - (origSize|0))|0;
    up = cp;
    heap[(up + 4) >> 2] = grownAmount|0;
    // Call free() with the new block to add it to the free list.
    free(((up|0) + 8)|0);
    // Return the free list pointer.
    return freePtr|0;
  }

  /**
   * Frees the given allocated pointer.
   *
   * @param ap {number} - The pointer to free
   */
  function free(ap) {
    ap = ap|0;
    var blockPtr = 0;
    var p = 0;
    var nextPtr = 0;

    // Get offset of header
    blockPtr = (ap - 8)|0;
    // Edge case: for the first free (called by morecore), the free pointer
    // is actually already pointing to this node, so we don't do anything.
    if ((blockPtr|0) == (freePtr|0)) {
      return;
    }

    // Find the location to insert this block into the free list
    for (p = freePtr|0; !(((blockPtr|0) > (p|0)) & ((blockPtr|0) < (heap[p >> 2]|0))); p = heap[p >> 2]|0) {
      if (((p|0) >= (heap[p >> 2]|0)) & ((blockPtr|0) > (p|0) | (blockPtr|0) < (heap[p >> 2]|0))) {
        break;
      }
    }

    // Merge the block into the adjacent free list entry above, if needed
    if (((blockPtr + (heap[(blockPtr + 4) >> 2]|0))|0) == (heap[p >> 2]|0)) {
      nextPtr = heap[p >> 2]|0;
      heap[(blockPtr + 4) >> 2] = ((heap[(blockPtr + 4) >> 2]|0) + (heap[(nextPtr + 4) >> 2]|0));
      heap[blockPtr >> 2] = heap[nextPtr >> 2]|0;
    } else {
      heap[blockPtr >> 2] = heap[p >> 2]|0;
    }
    // Merge the previous (adjacent) free list entry into this block, if needed
    if ((p + (heap[(p + 4) >> 2]|0)|0) == (blockPtr|0)) {
      heap[(p + 4) >> 2] = ((heap[(p + 4) >> 2]|0) + (heap[(blockPtr + 4) >> 2]|0))|0;
      heap[p >> 2] = heap[blockPtr >> 2]|0;
    } else {
      heap[p >> 2] = blockPtr|0;
    }
    // Set the free list head to this block
    freePtr = p;
  }

  /**
   * Returns the current free list pointer
   * (used for debugging)
   *
   * @return {number} - The free list pointer
   */
  function getFreePtr() {
    return freePtr|0;
  }

  return {
    malloc: malloc,
    free: free,
    getFreePtr: getFreePtr
  };
}

/**
 * Prints the free list for the given allocator to the console.
 *
 * @param malloc {Object} - An initialized malloc module
 * @param buf {ArrayBuffer} - The heap associated with the given module
 */
export function printAllocations(malloc, buf) {
  var freePtr = malloc.getFreePtr() >> 2;
  var ptr = freePtr;
  buf = new Uint32Array(buf);
  var blocks = [];
  do {
    blocks.push({start: `0x${toHex(ptr << 2)}`, next: `0x${toHex(buf[ptr])}`, size: buf[ptr + 1]});
    ptr = buf[ptr] >> 2;
  } while (ptr != freePtr);
  console.log("Blocks:");
  console.log(blocks);
}

// Runs smoke tests for malloc module
// (TODO: make these actual tests)
function testMallocModule() {
  var heapSize = 8196;
  var asmHeap = new ArrayBuffer(heapSize);
  var globNS;
  if (typeof window === 'undefined') {
    globNS = global;
  } else {
    globNS = window;
  }
  var malloc = mallocJSModule(globNS, {initialHeapSize: heapSize, growHeap: () => -1}, asmHeap);
  printAllocations(malloc, asmHeap);
  var first = malloc.malloc(20);
  console.log(`first: ${first}`);
  printAllocations(malloc, asmHeap);
  var second = malloc.malloc(24);
  console.log(`second: ${second}`);
  printAllocations(malloc, asmHeap);
  malloc.free(first);
  console.log(`freed: ${first}`);
  printAllocations(malloc, asmHeap);
  var third = malloc.malloc(4);
  console.log(`third: ${third}`);
  printAllocations(malloc, asmHeap);
  malloc.free(third);
  console.log(`freed: ${third}`);
  printAllocations(malloc, asmHeap);
  malloc.free(second);
  console.log(`freed: ${second}`);
  printAllocations(malloc, asmHeap);
}


