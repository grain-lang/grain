#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/queue.h>
#include "gc.h"

extern int our_code_starts_here() asm("our_code_starts_here");
extern void error() asm("error");
extern int print(int val) asm("print");
extern int equal(int val1, int val2) asm("equal");
extern int* try_gc(int* alloc_ptr, int amount_needed, int* first_frame, int* stack_top) asm("try_gc");
extern int* HEAP_END asm("HEAP_END");
extern int* STACK_BOTTOM asm("STACK_BOTTOM");

const int TRUE = 0xFFFFFFFF;
const int FALSE = 0x7FFFFFFF;
size_t HEAP_SIZE;
int* STACK_BOTTOM;
int* HEAP;
int* HEAP_END;

int equal(int val1, int val2) {
  if(val1 == val2) { return TRUE; }
  else { return FALSE; }
}

void print_rec(int val) {
  if(val & 0x00000001 ^ 0x00000001) {
    printf("%d", val >> 1);
  }
  else if((val & 0x00000007) == 5) {
    printf("<function>");
  }
  else if(val == 0xFFFFFFFF) {
    printf("true");
  }
  else if(val == 0x7FFFFFFF) {
    printf("false");
  }
  else if ((val & TUPLE_TAG_MASK) == 5) {
    fprintf(out, "<function>");
  }
  else if ((val & TUPLE_TAG_MASK) != 0) {
    int* addr = (int*)(val - 1);
    // Check whether we've visited this tuple already
    if ((*addr & 0x80000000) != 0) {
      fprintf(out, "<cyclic tuple %d>", (*addr & 0x7FFFFFFFF));
      return;
    }
    // Mark this tuple: save its length locally, then mark it
    int tupleLen = *addr;
    *(addr) = 0x80000000 | (++tupleCounter);
    fprintf(out, "(");
    int len = addr[0];
    for (int i = 1; i <= len; i++) {
      if (i > 1) fprintf(out, ", ");
      printHelp(out, addr[i]);
    }
    fprintf(out, ")");
    // Unmark this tuple: restore its length
    *(addr) = tupleLen;
  }
  else {
    printf("Unknown value: %#010x", val);
  }
}

int print(int val) {
  print_rec(val);
  printf("\n");
  return val;
}

void error(int i) {
  if (i == 0) {
    fprintf(stderr, "Error: comparison operator got non-number");
  }
  else if (i == 1) {
    fprintf(stderr, "Error: arithmetic operator got non-number");
  }
  else if (i == 2) {
    fprintf(stderr, "Error: if condition got non-boolean");
  }
  else if (i == 3) {
    fprintf(stderr, "Error: Integer overflow");
  }
  else if (i == 4) {
    fprintf(stderr, "Error: not a pair");
  }
  else if (i == 5) {
    fprintf(stderr, "Error: index too small");
  }
  else if (i == 6) {
    fprintf(stderr, "Error: index too large");
  }
  else if (i == 7) {
    fprintf(stderr, "Error: arity mismatch");
  }
  else if (i == 8) {
    fprintf(stderr, "Error: application got non-function");
  }
  else {
    fprintf(stderr, "Error: Unknown error code: %d\n", i);
  }
  exit(i);
}

/*
  Try to reserve the desired number of bytes of memory, and free garbage if
  needed.  Fail (and exit the program) if there is insufficient memory.  Does 
  not actually allocate the desired number of bytes of memory; the caller 
  will do that.

  Arguments:

    int* alloc_ptr - the current top of the heap (which we store in ESI), where
                     the next allocation should occur, if possible
    int bytes_needed - the number of bytes of memory we want to allocate
                       (including padding)
    int* cur_frame - the base pointer of the topmost stack frame of our code
                     (i.e., EBP)
    int* cur_stack_top - the stack pointer of the topmost stack frame of our
                         code (i.e., ESP)

  Returns:
    The new top of the heap (i.e. the new value of ESI) after garbage collection.  
    Does not actually allocate bytes_needed space.

  Side effect:
    Also updates HEAP_END to point to the new end of the heap, if it's changed
*/
int* try_gc(int* alloc_ptr, int bytes_needed, int* cur_frame, int* cur_stack_top) {
  int* new_heap = calloc(HEAP_SIZE, sizeof(int));
  int* new_heap_end = new_heap + HEAP_SIZE;
  int* old_heap = HEAP;
  int* old_heap_end = HEAP_END;

  int* new_esi = NULL;

  // Do you trust your new GC?
  bool CONFIDENT = false;

  // Abort early, if we can't allocate a new to-space
  if (new_heap == NULL) {
    fprintf(stderr, "Out of memory: could not allocate a new semispace for garbage collection");
    exit(9);
  }
  
  // When you're confident in your collector, enable the following lines to trigger your GC
  if (confident) {
    new_esi = gc(STACK_BOTTOM, cur_frame, cur_stack_top, HEAP, HEAP_END, new_heap);
    HEAP = new_heap;
    HEAP_END = new_heap_end;
    free(old_heap);
  } else {
    // This just keeps ESI where it is, and cleans up after the unneeded allocation
    free(new_heap);
    new_esi = alloc_ptr;
  }
  
  // Note: strict greater-than is correct here: if new_esi + (bytes_needed / 4) == HEAP_END,
  // that does not mean we're *using* the byte at HEAP_END, but rather that it would be the
  // next free byte, which is still ok and not a heap-overflow.
  if((new_esi + (bytes_needed / 4)) > HEAP_END) {
    fprintf(stderr, "Out of memory: needed %d words, but only %d remain after collection",
            bytes_needed / 4, (HEAP_END - new_esi));
    free(new_heap);
    exit(9);
  }
  else {
    return new_esi;
  }
}

int main(int argc, char** argv) {
  if(argc > 1) {
    HEAP_SIZE = atoi(argv[1]);
  }
  else {
    HEAP_SIZE = 100000;
  }
  HEAP = calloc(HEAP_SIZE, sizeof (int));
  HEAP_END = HEAP + HEAP_SIZE;

  int result = our_code_starts_here(HEAP);

  print(result);
  return 0;
}

