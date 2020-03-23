import { memory, malloc } from '../runtime';
import { GrainError } from '../errors/errors';

export const heapController = {
  heapAdjust(n) {
    throw new GrainError(-1, "Grain runtime is not yet instantiated.");
  }
};

function grainCheckMemory(runner, numBytes) {
  if (numBytes === 0) {
    return;
  }
  let curTop = heapController.heapAdjust(0);
  if ((runner.opts && runner.opts.limitMemory) > 0 && memory.buffer.byteLength >= runner.opts.limitMemory) {
    return;
  }
  if (memory.buffer.byteLength - curTop < numBytes) {
    memory.grow(1);
  }
}

export function makeMemoryChecker(runner) {
  return (numBytes) => grainCheckMemory(runner, numBytes);
}

export function grainHeapAllocate(numWords) {
  // allocates the number of words
  return malloc(numWords * 4)
}
