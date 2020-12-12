@external("memoryManager", "_malloc")
export declare function extMalloc(rawPtr: u32, bytes: u32): u32
@external("memoryManager", "_free")
export declare function extFree(ptr: u32): u32
@external("memoryManager", "_growHeap")
export declare function extGrowHeap(numPages: u32): u32
@external("memoryManager", "_initialHeapSize")
export declare const initialHeapSize: u32

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
 * As such, we manually lay out the entries in the heap as follows (note that we
 * use helpers to facilitate accessing and setting these values):
 *
 *            AS                   C Equivalent
 *   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *   let ptr      ===  union header *ptr
 *   getNext(ptr) ===  ptr->s.ptr
 *   getSize(ptr) ===  ptr->s.size
 */

/**
 * Pointer to the start of the free list. This is always a multiple of
 * 8, with the exception of its initial value (used as a sentinel).
 */
let freePtr: u32 = 1;

/**
 * Size (in bytes) of entries in the free list.
 */
let mallocHeaderSize: u32 = 8;

/**
 * log_2(mallocHeaderSize) (multiplication by the header
 * size is equivalent to left-shifting by this amount)
 */
let logMallocHeaderSize: u32 = 3;

/**
 * The current size (in bytes) of the heap.
 */
let heapSize: u32 = 0;

/**
 * Requests that the heap be grown by the given number of bytes.
 *
 * @param {u32} nbytes - The number of bytes requested
 * @return {u32} - If unsuccessful, -1. Otherwise, the pointer to the beginning of the extended region.
 */
export function growHeap(nbytes: u32): u32 {
  nbytes = nbytes;
  let reqSize: u32 = 0;
  let reqResult: u32 = 0;
  let origSize: u32 = 0;
  origSize = heapSize;
  // If the size has not been initialized, do so.
  if (heapSize == 0) {
    heapSize = initialHeapSize;
    if (nbytes > heapSize) {
      // More bytes requested than the initial heap size,
      // so we need to request more anyway.
      reqSize = nbytes - heapSize;
      reqSize = reqSize >> 16;
      reqSize = reqSize + 1;
      reqResult = extGrowHeap(reqSize);
      if (reqResult == -1) {
        return -1;
      }
      heapSize += reqSize << 16;
    }
    return 0;
  } else {
    // The size has already been initialized, so call the external function.
    reqSize = nbytes;
    reqSize = reqSize >> 16;
    reqSize = reqSize + 1;
    reqResult = extGrowHeap(reqSize);
    if (reqResult == -1) {
      return -1;
    }
    heapSize += reqSize << 16;
  }
  return reqResult;
}

@inline
function getNext(ptr: u32): u32 {
  return load<u32>(ptr)
}

@inline
function setNext(ptr: u32, val: u32): void {
  store<u32>(ptr, val)
}

@inline
function getSize(ptr: u32): u32 {
  return load<u32>(ptr, 4)
}

@inline
function setSize(ptr: u32, val: u32): void {
  store<u32>(ptr, val, 4)
}

/**
 * Allocates the requested number of bytes, returning a pointer.
 *
 * @param nbytes {u32} - The number of bytes to allocate
 * @return {u32} - The pointer to the allocated region (8-byte aligned), or -1 if the allocation failed.
 */
export function malloc(nb: u32): u32 {
  // Add space for GC header
  let nbytes: u32 = nb + 8;
  let p: u32 = 0;
  let prevp: u32 = 0;
  let nunits: u32 = 0;

  // Set nbytes to the next multiple of mallocHeaderSize greater
  // than the given size
  nunits = (nbytes + mallocHeaderSize - 1) / mallocHeaderSize + 1;
  nbytes = nunits << logMallocHeaderSize; // multiply by header size

  prevp = freePtr;
  // Handle initialization
  if (heapSize == 0) {
    store<u32>(0, 0)
    freePtr = 0;
    prevp = 0;
    store<u32>(4, 0)
  }
  // Search the freelist for any blocks large enough.
  for (p = getNext(prevp); ; prevp = p, p = getNext(p)) {
    let size = getSize(p);
    if (size >= nbytes) {
      // If this block is big enough, allocate from it.
      if (size == nbytes) {
        // It's exactly the right size!
        setNext(prevp, getNext(p));
      } else {
        // Shrink it as needed
        let newSize = size - nbytes;
        setSize(p, newSize)
        p = p + newSize;
        setSize(p, nbytes)
      }
      // Update the pointer to the free list.
      freePtr = prevp;

      // Let runtime set up reference counting
      // Address of the region past the malloc header
      return extMalloc(p + 8, nb);
    }
    // We've reached the end of the free list. Time to grow the heap.
    if (p == freePtr) {
      // Attempt to grow the heap
      p = morecore(nbytes);
      // If growing the heap failed, return -1.
      if (p == -1) {
        // Error
        return -1;
      }
    }
  }
  // Error
  return -1;
}

/**
 * Asks the runtime for more heap memory.
 * (if you can't tell from the fact that the name is reminiscient
 *  of the 1970s, the name of this function is taken from K&R).
 *
 * @param nbytes {u32} - The number of bytes to try to grow the heap by
 * @return {u32} - If successful, a pointer to the start of the free list. If not successful, -1.
 */
export function morecore(nbytes: u32): u32 {
  let cp: u32 = 0;
  let up: u32 = 0;

  let origSize = heapSize;

  cp = growHeap(nbytes);
  // If there was an error, return
  if (cp == -1) {
    return -1;
  }
  // Set the size of the new block to the amount the
  // heap was grown.
  let grownAmount = heapSize - origSize;
  up = cp;
  setSize(up, grownAmount);
  // Call free() with the new block to add it to the free list.
  // Add an additional 8 for the expected GC header
  free(up + 8 + 8);
  // Return the free list pointer.
  return freePtr;
}

/**
 * Frees the given allocated pointer.
 *
 * @param ap {u32} - The pointer to free
 */
export function free(ap: u32): void {
  // Let memory manager know we're freeing this pointer
  extFree(ap);

  let blockPtr: u32 = ap - 16; // 8 bytes for malloc header + 8 bytes for GC header
  let p: u32 = 0;

  // Edge case: for the first free (called by morecore), the free pointer
  // is actually already pointing to this node, so we don't do anything.
  if (blockPtr == freePtr) {
    return;
  }

  // Find the location to insert this block into the free list
  for (
    p = freePtr;
    !((blockPtr > p) && (blockPtr < getNext(p)));
    p = getNext(p)
  ) {
    if ((p >= getNext(p)) && ((blockPtr > p) || (blockPtr < getNext(p)))) {
      break;
    }
  }

  // Merge the block into the adjacent free list entry above, if needed
  if (blockPtr + getSize(blockPtr) == getNext(p)) {
    let next = getNext(p);
    setSize(blockPtr, getSize(blockPtr) + getSize(next))
    setNext(blockPtr, getNext(next))
  } else {
    setNext(blockPtr, getNext(p))
  }
  // Merge the previous (adjacent) free list entry into this block, if needed
  if (p + getSize(p) == blockPtr) {
    setSize(p, getSize(p) + getSize(blockPtr))
    setNext(p, getNext(blockPtr))
  } else {
    setNext(p, blockPtr)
  }
  // Set the free list head to this block
  freePtr = p;
}

/**
 * Returns the current free list pointer
 * (used for debugging)
 *
 * @return {u32} - The free list pointer
 */
export function getFreePtr(): u32 {
  return freePtr;
}
