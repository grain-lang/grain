import { memory, malloc } from '../runtime';
import { GrainError } from '../errors/errors';

export const heapController = {
  heapAdjust(n) {
    throw new GrainError(-1, "Grain runtime is not yet instantiated.");
  }
};

export function grainCheckMemory(numBytes) {
  if (numBytes === 0) {
    return;
  }
  let curTop = heapController.heapAdjust(0);
  if (memory.buffer.byteLength - curTop < numBytes) {
    memory.grow(1);
  }
}

export function grainHeapAllocate(numWords) {
  // allocates the number of words
  return malloc(numWords * 4)
}
