#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "snek.h"
#include "gc.h"

extern int our_code_starts_here(int* HEAP) asm("our_code_starts_here");
extern void error(int err_code, int val) asm("error");
extern int print(void *self, int val) asm("print");
extern int print_stack(int val, void *addr, int *top) asm("print_stack");
extern int input(void *self) asm("input");
extern int equal(void *self, int val1, int val2) asm("equal");
extern int* try_gc(int* alloc_ptr, int amount_needed, int* first_frame, int* stack_top) asm("try_gc");
extern int* HEAP_END asm("HEAP_END");
extern int* STACK_BOTTOM asm("STACK_BOTTOM");
extern int* CORE_HEAP asm("CORE_HEAP");

#define SNEK_ERR_NOT_NUMBER_COMP 0
#define SNEK_ERR_NOT_NUMBER_ARITH 1
#define SNEK_ERR_NOT_BOOLEAN_LOGIC 2
#define SNEK_ERR_NOT_BOOLEAN_IF 3
#define SNEK_ERR_OVERFLOW 4
#define SNEK_ERR_GET_NOT_TUP 5
#define SNEK_ERR_GET_ITEM_IDX_NOT_NUMBER 6
#define SNEK_ERR_GET_ITEM_IDX_TOO_SMALL 7
#define SNEK_ERR_GET_ITEM_IDX_TOO_LARGE 8
#define SNEK_ERR_CALLED_NON_FUNCTION 9
#define SNEK_ERR_ARITY_MISMATCH 10
#define SNEK_ERR_OUT_OF_MEMORY 11
#define SNEK_ERR_SET_NOT_TUP 12
#define SNEK_ERR_SET_ITEM_IDX_NOT_NUMBER 13
#define SNEK_ERR_SET_ITEM_IDX_TOO_SMALL 14
#define SNEK_ERR_SET_ITEM_IDX_TOO_LARGE 15
#define SNEK_ERR_BAD_INPUT 97
#define SNEK_ERR_NOT_NONNEG 98
#define SNEK_ERR_NOT_NUMBER_GENERIC 99


size_t HEAP_SIZE;
int* STACK_BOTTOM;
int* HEAP;
int* HEAP_END;

int tupleCounter = 0;

int equal_help(int x, int y, int cycles) {
  if (SNEK_IS_TUPLE(x)) {
    if (!SNEK_IS_TUPLE(y)) {
      return SNEK_FALSE;
    }
    // Check tuples are equal
    int *x_contents = SNEK_GET_TUPLE_PTR(x);
    int *y_contents = SNEK_GET_TUPLE_PTR(y);
    if (SNEK_TUPLE_ARITY(x_contents) != SNEK_TUPLE_ARITY(y_contents)) {
      return SNEK_FALSE;
    }
    if (*x_contents & 0x80000000) {
      return SNEK_TRUE;
    }
    int i;
    int length = x_contents[0];
    ++cycles;
    *x_contents = 0x80000000 | cycles;
    *y_contents = 0x80000000 | cycles;
    int result = SNEK_TRUE;
    for (i = 0; i < length; ++i) {
      if (equal_help(SNEK_TUPLE_ITEM(x_contents, i),
                     SNEK_TUPLE_ITEM(y_contents, i),
                     cycles) == SNEK_FALSE) {
        result = SNEK_FALSE;
        break;
      }
    }
    *x_contents = length;
    *y_contents = length;
    return SNEK_TRUE;
  } else if (x == y) {
    return SNEK_TRUE;
  } else {
    return SNEK_FALSE;
  }
}

int equal(void *self, int val1, int val2) {
  return equal_help(val1, val2, 0);
}

int input(void *self) {
  char buf[50];
  int ret;
 scan:
  printf("Please input a value: ");
  while (fgets(buf, 50, stdin) == NULL || buf[0] == '\n') {
    printf("Please input a value: ");
    
  }
  if (buf[0] != 'f' && buf[0] != 't' &&
      !('0' <= buf[0] && buf[0] <= '9')) {
    fprintf(stderr, "bad input (must be a number between -1073741824 and 1073741823 (inclusive) or a boolean (i.e. true, false))\n");
    goto scan;
  } else if (buf[0] == 'f') {
    if (strcmp(buf, "false\n")) {
      fprintf(stderr, "bad input (must be a number between -1073741824 and 1073741823 (inclusive) or a boolean (i.e. true, false))\n");
      goto scan;
    }
    ret = SNEK_FALSE;
  } else if (buf[0] == 't') {
    if (strcmp(buf, "true\n")) {
      fprintf(stderr, "bad input (must be a number between -1073741824 and 1073741823 (inclusive) or a boolean (i.e. true, false))\n");
      goto scan;
    }
    ret = SNEK_TRUE;
  } else {
    if (!sscanf(buf, "%d", &ret)
        || (ret > 1073741823)
        || (ret < -1073741824)) {
      fprintf(stderr, "bad input (must be a number between -1073741824 and 1073741823 (inclusive) or a boolean (i.e. true, false))\n");
      goto scan;
    }
    ret *= 2;
  }
  return ret;
}

char *print_str_help(int val) {
  char *ret;
  if (val == SNEK_TRUE) {
    ret = malloc(6);
    snprintf(ret, 5, "true");
  } else if (val == SNEK_FALSE) {
    ret = malloc(7);
    snprintf(ret, 6, "false");
  } else if (SNEK_IS_NUMBER(val)) {
    ret = malloc(16);
    snprintf(ret, 15, "%d", val / 2);
  } else if (SNEK_IS_FWD_PTR(val)) {
    ret = malloc(64);
    int *ptr = SNEK_GET_FWD_PTR(val);
    snprintf(ret, 64, "<forwarding pointer: %p>", ptr);
  } else if (SNEK_IS_TUPLE(val)) {
    // Tuple
    int *tuple_contents = SNEK_GET_TUPLE_PTR(val);
    if ((*tuple_contents & 0x80000000) != 0) {
      ret = malloc(64);
      snprintf(ret, 64, "<cyclic tuple %d>", (int)(*tuple_contents & 0x7FFFFFFF));
    } else {
      ret = malloc(2);
      ret[0] = '(';
      ret[1] = '\0';
      int i;
      int length;
      char *tmp;
      int len = *tuple_contents;
      *tuple_contents = 0x80000000 | (++tupleCounter);
      for (i = 0; i < len; ++i) {
        char *component = print_str(SNEK_TUPLE_ITEM(tuple_contents, i));
        int retlen = strlen(ret);
        length = retlen + strlen(component);
        tmp = ret;
        ret = malloc(length + 4);
        strcpy(ret, tmp);
        strcat(ret, component);
        if (i < (len - 1)) {
          ret[length] = ',';
          ret[length + 1] = ' ';
          ret[length + 2] = '\0';
        } else {
          ret[length] = ')';
          ret[length + 1] = '\0';
        }
        free(tmp);
        free(component);
      }
      *tuple_contents = len;
    }
  } else if (SNEK_IS_CLOSURE(val)) {
    ret = malloc(64);
    snprintf(ret, 63, "<lambda>");
  } else {
    ret = malloc(32);
    snprintf(ret, 31, "<Unknown value: %#010x>", val);
  }
  return ret;
}

char *print_str(int val) {
  tupleCounter = 0;
  return print_str_help(val);
}

int print(void *self, int val) {
  char *to_print = print_str(val);
  printf("%s\n", to_print);
  free(to_print);
  return val;
}

void error(int err_code, int val) {
  int *val_as_ptr = (int*)val;
  int expected_arity;
  switch(err_code) {
    case SNEK_ERR_ARITY_MISMATCH:
      // Wishlist: actually print arity
      expected_arity = SNEK_CLOSURE_ARITY(val_as_ptr);
      fprintf(stderr, "arity mismatch (expected: %d (0x%x)) <val: %p>\n", expected_arity, expected_arity, (int*)(intptr_t)val);
      // Need to not run print_str in this case
      goto quit;
  default:
    break;
  }
  char *buf = print_str(val);
  switch (err_code) {
    // These would go to STDERR, but runner.ml only looks
    // at what goes to STDOUT.
  case SNEK_ERR_NOT_NUMBER_ARITH:
    fprintf(stderr, "arithmetic expected a number, got value: %s\n", buf);
    break;
  case SNEK_ERR_NOT_NUMBER_COMP:
    fprintf(stderr, "comparison expected a number, got value: %s\n", buf);
    break;
  case SNEK_ERR_NOT_NUMBER_GENERIC:
    fprintf(stderr, "expected a number, got value: %s\n", buf);
    break;
  case SNEK_ERR_OVERFLOW:
    fprintf(stderr, "number overflow with value: %s\n", buf);
    break;
  case SNEK_ERR_NOT_BOOLEAN_IF:
    fprintf(stderr, "if expected a boolean, got value: %s\n", buf);
    break;
  case SNEK_ERR_NOT_BOOLEAN_LOGIC:
    fprintf(stderr, "logic expected a boolean, got value: %s\n", buf);
    break;
  case SNEK_ERR_GET_NOT_TUP:
    fprintf(stderr, "tuple access expected tuple, got value: %s\n", buf);
    break;
  case SNEK_ERR_SET_NOT_TUP:
    fprintf(stderr, "tuple assignment expected tuple, got value: %s\n", buf);
    break;
  case SNEK_ERR_GET_ITEM_IDX_NOT_NUMBER:
    fprintf(stderr, "tuple access expected number for index, got value: %s\n", buf);
    break;
  case SNEK_ERR_SET_ITEM_IDX_NOT_NUMBER:
    fprintf(stderr, "tuple assignment expected number for index, got value: %s\n", buf);
    break;
  case SNEK_ERR_GET_ITEM_IDX_TOO_SMALL:
  case SNEK_ERR_SET_ITEM_IDX_TOO_SMALL:
    fprintf(stderr, "tuple index too small: %s\n", buf);
    break;
  case SNEK_ERR_GET_ITEM_IDX_TOO_LARGE:
  case SNEK_ERR_SET_ITEM_IDX_TOO_LARGE:
    // Wishlist: show maximum value allowed
    fprintf(stderr, "tuple index too large: %s\n", buf);
    break;
  case SNEK_ERR_CALLED_NON_FUNCTION:
    fprintf(stderr, "called non-function: %s\n", buf);
    break;
  case SNEK_ERR_NOT_NONNEG:
    fprintf(stderr, "printStack expected a nonnegative number, got value: %s\n", buf);
    break;
  case SNEK_ERR_OUT_OF_MEMORY:
    fprintf(stderr, "Out of memory\n");
    break;
  default:
    fprintf(stderr, "Unknown error code: %d (error called with value: %s)\n", err_code, buf);
  }
  free(buf);
 quit:
  exit(1);
}

int print_stack(int val, void *addr, int *top) {
  if (val < 0) {
    error(SNEK_ERR_NOT_NONNEG, val);
  }
  int n = val >> 1;
  int i;
  int *stack_base = NULL;
  int *stack;
  char *buf;
  fprintf(stderr, "called\n");
  while (stack_base != STACK_BOTTOM) {
    printf("%p\n", addr);
    stack_base = *(int**)addr;
    stack = (int*)addr;
    // Prints the local variables in the current stack frame
    i = 0;
    while (stack >= top) {
      if ((unsigned int)*stack == 0xDEADBEEF) {
        buf = "----";
      } else {
        buf = print_str(*stack);
      }
      printf("\t%p: %20s\t[0x%-8x] [ebp-%-2d]\n", stack, buf, *stack, 4 * i++);
      if ((unsigned int)*stack != 0xDEADBEEF) {
        free(buf);
      }
      --stack;
    }
    // Set ESP to EBP
    top = addr;
    // Set EBP to old EBP
    addr = stack_base;
    // repeat
  }
  fprintf(stderr, "return\n");
  return val;
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
  int* new_heap = (int*)calloc(HEAP_SIZE, sizeof(int));
  int* new_heap_end = new_heap + HEAP_SIZE;
  int* old_heap = HEAP;
  int* old_heap_end = HEAP_END;

  int* new_esi = NULL;

  // Do you trust your new GC?
  int CONFIDENT = 1;

  // Abort early, if we can't allocate a new to-space
  if (new_heap == NULL) {
    fprintf(stderr, "Out of memory: could not allocate a new semispace for garbage collection");
    exit(SNEK_ERR_OUT_OF_MEMORY);
  }
  
  // When you're confident in your collector, enable the following lines to trigger your GC
  if (CONFIDENT) {
    smarter_print_heap(old_heap, old_heap_end, new_heap, new_heap_end);
    dbgprintf("VALID HEAP BOUNDS (old: %p-%p) (new: %p-%p)\n", HEAP, HEAP_END, new_heap, new_heap_end);
    new_esi = gc(STACK_BOTTOM, cur_frame, cur_stack_top, HEAP, HEAP_END, new_heap, new_heap, new_heap_end);
    smarter_print_heap(old_heap, old_heap_end, new_heap, new_heap_end);
    HEAP = new_heap;
    HEAP_END = new_heap_end;
    free(old_heap);
  } else {
    // This just keeps ESI where it is, and cleans up after the unneeded allocation
    free(new_heap);
    new_heap = NULL;
    new_esi = alloc_ptr;
  }
  
  // Note: strict greater-than is correct here: if new_esi + (bytes_needed / 4) == HEAP_END,
  // that does not mean we're *using* the byte at HEAP_END, but rather that it would be the
  // next free byte, which is still ok and not a heap-overflow.
  if (bytes_needed / 4 > HEAP_SIZE) {
    fprintf(stderr, "Allocation error: needed %d words, but the heap is only %d words",
            bytes_needed / 4, HEAP_SIZE);
    exit(SNEK_ERR_OUT_OF_MEMORY);
  } else if((new_esi + (bytes_needed / 4)) > HEAP_END) {
    fprintf(stderr, "Out of memory: needed %d words, but only %d remain after collection",
            bytes_needed / 4, (HEAP_END - new_esi));
    if (new_heap != NULL) free(new_heap);
    exit(SNEK_ERR_OUT_OF_MEMORY);
  }
  else {
    return new_esi;
  }
}

int *CORE_HEAP;

int main(int argc, char** argv) {
  if(argc > 1) {
    HEAP_SIZE = atoi(argv[1]);
  }
  else {
    HEAP_SIZE = 100000;
  }
  HEAP = (int*)calloc(HEAP_SIZE, sizeof (int));
  HEAP_END = HEAP + HEAP_SIZE;

  CORE_HEAP = (int*)calloc(48, sizeof(int));
  int result = our_code_starts_here(HEAP);

  print(NULL, result);
  return 0;
}

