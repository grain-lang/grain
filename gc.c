#include <stdio.h>
#include <setjmp.h>
#include <signal.h>
#include "snek.h"

void naive_print_heap(int* heap, int size) {
  for(int i = 0; i < size; i += 1) {
    printf("  %d/%p: %p (%d)\n", i, (heap + i), (int*)(*(heap + i)), *(heap + i));
  }
}

// Implement the functions below

char *safe_print_str(int val) {
  char *ret;
  if (val == SNEK_TRUE) {
    ret = malloc(6);
    snprintf(ret, 5, "true");
  } else if (val == SNEK_FALSE) {
    ret = malloc(7);
    snprintf(ret, 6, "false");
  } else if (SNEK_IS_NUMBER(val)) {
    ret = malloc(32);
    snprintf(ret, 32, "%d (0x%x)", val / 2, val);
  } else if (SNEK_IS_FWD_PTR(val)) {
    // If you have a cyclic forwarding pointer, God help you.
    ret = malloc(128);
    if (val < 100) {
      snprintf(ret, 128, "<literal: %d> (0x%x)", val, val);
    } else {
      int *ptr = SNEK_GET_FWD_PTR(val);
      snprintf(ret, 128, "<forwarding pointer: %p> (0x%x)", ptr, val);
    }
  } else if (SNEK_IS_TUPLE(val)) {
    ret = malloc(128);
    if (val < 100) {
      snprintf(ret, 128, "<literal: %d> (0x%x)", val, val);
    } else {
      int *tuple = SNEK_GET_TUPLE_PTR(val);
      snprintf(ret, 128, "<tuple: %p> (0x%x)", tuple, val);
    }
  } else if (SNEK_IS_CLOSURE(val)) {
    ret = malloc(128);
    if (val < 100) {
      snprintf(ret, 128, "<literal: %d> (0x%x)", val, val);
    } else {
      int *closure = SNEK_GET_CLOSURE_PTR(val);
      snprintf(ret, 128, "<lambda: %p> (0x%x)", closure, val);
    }
  } else {
    ret = malloc(32);
    snprintf(ret, 31, "<Unknown value: %#010x>", val);
  }
  return ret;
}

void smart_print_value(int *val) {
  char *strval = safe_print_str(*val);
  dbgprintf("  [%p]: %s\n", val, strval);
  free(strval);
}

void smarter_print_heap(int* from_start, int* from_end, int* to_start, int* to_end) {
  dbgprintf("============ FROM HEAP ============\n");
  while (from_start < from_end) {
    smart_print_value(from_start++);
  }
  dbgprintf("============= TO HEAP =============\n");
  while (to_start < to_end) {
    smart_print_value(to_start++);
  }
  dbgprintf("=============== END ===============\n");
}

/*
  Copies a Garter value from the given address to the new heap, 
  but only if the value is heap-allocated and needs copying.

  Arguments:
    garter_val_addr: the *address* of some Garter value, which contains a Garter value,
                     i.e. a tagged word.  
                     It may or may not be a pointer to a heap-allocated value...
    heap_top: the location at which to begin copying, if any copying is needed

  Return value:
    The new top of the heap, at which to continue allocations

  Side effects:
    If the data needed to be copied, then this replaces the value at its old location 
    with a forwarding pointer to its new location
 */
int* copy_if_needed(int* garter_val_addr, int* heap_top, int *to_start, int *to_end) {
  int garter_val = *garter_val_addr;
  dbgprintf("heap_top: %p; garter_val: %p; garter_val_addr: %p; to_start: %p; to_end: %p\n", heap_top, (int*)garter_val, garter_val_addr, to_start, to_end);
  if (SNEK_IS_PRIMITIVE(garter_val)) {
    return heap_top;
  } else if (SNEK_IS_TUPLE(garter_val)) {
    int *heap_thing = SNEK_GET_TUPLE_PTR(garter_val);
    if (SNEK_IS_FWD_PTR(*heap_thing)) {
      int *raw_ptr = SNEK_GET_FWD_PTR(*heap_thing);
      if (to_start <= raw_ptr && raw_ptr <= to_end) {
        *garter_val_addr = (intptr_t)raw_ptr | SNEK_TAG_TUPLE;
        return heap_top;
      }
    }
    int *new_tup = heap_top;
    int arity = SNEK_TUPLE_ARITY(heap_thing);
    int idx;
    *heap_top++ = arity;
    for (idx = 0; idx < arity; ++idx) {
      *heap_top++ = SNEK_TUPLE_ITEM(heap_thing, idx);
    }
    *heap_thing = ((intptr_t)new_tup) | SNEK_TAG_FWD_PTR;
    *garter_val_addr = ((intptr_t)new_tup) | SNEK_TAG_TUPLE;
    int i = 0;
    while ((intptr_t)heap_top % 8) {
      ++i;
      heap_top++;
    }
    dbgprintf("padding for %p: %d\n", new_tup, i);
    for (idx = 0; idx < arity; ++idx) {
      heap_top = copy_if_needed((int*)(&SNEK_TUPLE_ITEM(new_tup, idx)), heap_top, to_start, to_end);
    }
    return heap_top;
  } else if (SNEK_IS_CLOSURE(garter_val)) {
    int *heap_thing = SNEK_GET_CLOSURE_PTR(garter_val);
    if (heap_thing == NULL) {
      return heap_top;
    }
    if (SNEK_IS_FWD_PTR(*heap_thing)) {
      int *raw_ptr = SNEK_GET_FWD_PTR(*heap_thing);
      dbgprintf("Closure: fwd to %p\n", raw_ptr);
      if (to_start <= raw_ptr && raw_ptr <= to_end) {
        dbgprintf("forwarding %p\n", garter_val_addr);
        *garter_val_addr = (intptr_t)raw_ptr | SNEK_TAG_CLOSURE;
        return heap_top;
      }
    }
    int *new_closure = heap_top;
    int size = SNEK_CLOSURE_SIZE(heap_thing);
    int idx;
    heap_top[0] = (int)SNEK_CLOSURE_ARITY(heap_thing);
    heap_top++;
    *(heap_top++) = (intptr_t)SNEK_CLOSURE_PTR(heap_thing);
    *(heap_top++) = size;
    for (idx = 0; idx < size; ++idx) {
      *heap_top++ = SNEK_CLOSURE_ITEM(heap_thing, idx);
    }
    *heap_thing = ((intptr_t)new_closure) | SNEK_TAG_FWD_PTR;
    *garter_val_addr = ((intptr_t)new_closure) | SNEK_TAG_CLOSURE;
    int i = 0;
    while ((intptr_t)heap_top % 8) {
      ++i;
      heap_top++;
    }
    dbgprintf("padding for %p: %d\n", new_closure, i);
    for (idx = 0; idx < size; ++idx) {
      dbgprintf("loop: idx=%d; size=%d\n", idx, size);
      heap_top = copy_if_needed((int*)(SNEK_CLOSURE_ITEM_REF(new_closure, idx)), heap_top, to_start, to_end);
    }
    return heap_top;
  } else if (SNEK_IS_FWD_PTR(garter_val)) {
    *garter_val_addr = (intptr_t)SNEK_GET_FWD_PTR(*garter_val_addr);
    return heap_top;
  } else {
    char *valstr = print_str(garter_val);
    dbgprintf("Unknown value on heap: %s\n", valstr);
    free(valstr);
    exit(1);
  }
}

/*
  Implements Cheney's garbage collection algorithm.

  Arguments:
    bottom_frame: the base pointer of our_code_starts_here, i.e. the bottommost Garter frame
    top_frame: the base pointer of the topmost Garter stack frame
    top_stack: the current stack pointer of the topmost Garter stack frame
    from_start and from_end: bookend the from-space of memory that is being compacted
    to_start: the beginning of the to-space of memory

  Returns:
    The new location within to_start at which to allocate new data
 */
int* gc(int* bottom_frame, int* top_frame, int* top_stack, int* from_start, int* from_end, int* to_start, int *heap_start, int *heap_end) {
  dbgprintf("gc(bottom_frame=%p, top_frame=%p, top_stack=%p, from_start=%p, from_end=%p, to_start=%p)\n", bottom_frame, top_frame, top_stack, from_start, from_end, to_start);
  //dbgprintf("top o da stak: 0x%x\n", *top_stack);
  dbgprintf("STACK:\n");
  for (int* cur_word = top_stack; cur_word < top_frame; cur_word++) {
    char *s = safe_print_str(*cur_word);
    dbgprintf("  %p: %s\n", cur_word, s);
    free(s);
  }
  for (int* cur_word = top_stack; cur_word < top_frame; cur_word++) {
    
    if (SNEK_IS_PTR(*cur_word)) {
      int *raw_ptr = SNEK_GET_PTR(*cur_word);
      dbgprintf("GC-ing raw pointer: %p\n", raw_ptr);
      if (from_start <= raw_ptr && raw_ptr <= from_end) {
        to_start = copy_if_needed(cur_word, to_start, heap_start, heap_end);
      }
    }
    
  }
  if (top_frame < bottom_frame) {
    /**
     * Our Garter Stack Layout:
     *
     * |   XXXXXXX   |
     * |   OLD EBP   | <- top_frame 
     * |   OLD EDI   | <- top_frame + 1
     * |   RET ADD   | <- top_frame + 2
     * | next frame  | <- top_frame + 3
     */
    // Update old EDI
    to_start = copy_if_needed(top_frame + 1, to_start, heap_start, heap_end);
    // GC next frame
    to_start = gc(bottom_frame,
                  (int*)(*top_frame), // [top_frame] points to the saved EBP, which is the next stack frame
                  top_frame + 3,      // [top_frame+4] points to the return address
                                      // so [top_frame+8] is the next frame's stack-top
                  from_start,
                  from_end,
                  to_start,
                  heap_start,
                  heap_end); // to_start has been changed to include any newly copied data
  }
  // after copying the remaining stack frames, return the new allocation starting point
  dbgprintf("STACK post-gc:\n");
  for (int* cur_word = top_stack; cur_word < top_frame; cur_word++) {
    char *s = safe_print_str(*cur_word);
    dbgprintf("  %p: %s\n", cur_word, s);
    free(s);
  }
  return to_start;       
}

