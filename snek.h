#ifndef __SNEK_H
#define __SNEK_H

#include <stdlib.h>
#include <stdint.h>

#define SNEK_NUM_TAG_MASK 0x00000001
#define SNEK_TUPLE_TAG_MASK 0x00000007

// Cast to ints since uints don't really appear in the code
#define SNEK_TRUE  ((int)0xFFFFFFFF)
#define SNEK_FALSE ((int)0x7FFFFFFF)

//#define SNEK_DBG_PRINT

// So that register allocation doesn't bite us later...
#ifdef __x86_64__
#pragma message "WARNING: Compiling for 64-bit"
typedef uint64_t word_t;
#else
typedef int word_t;
#endif

#define SNEK_TAG_BOOLEAN 0x7FFFFFFF
#define SNEK_TAG_NUMBER  0x0
#define SNEK_TAG_TUPLE   0x1
#define SNEK_TAG_FWD_PTR 0x3
#define SNEK_TAG_CLOSURE 0x5

#define SNEK_IS_BOOLEAN(x) ((((int)(x)) & SNEK_FALSE) == SNEK_FALSE) // 0b111
#define SNEK_IS_NUMBER(x)  (~((int)(x)) & 1) // 0b0
#define SNEK_IS_TUPLE(x)   ((((int)(x)) & SNEK_TUPLE_TAG_MASK) == 1) // 0b001
#define SNEK_IS_CLOSURE(x) ((((int)(x)) & SNEK_TUPLE_TAG_MASK) == 5) // 0b101
#define SNEK_IS_FWD_PTR(x) ((((int)(x)) & SNEK_TUPLE_TAG_MASK) == 3) // 0b011

#define SNEK_IS_PRIMITIVE(x) ({                     \
      int __x = (int)x;                             \
      SNEK_IS_BOOLEAN(__x) || SNEK_IS_NUMBER(__x);  \
    })

#define SNEK_TUPLE_ARITY(x)     ((int)(((word_t*)(x))[0]))
#define SNEK_TUPLE_ITEM(tup, i)       (((word_t*)tup)[(i) + 1])

#define SNEK_CLOSURE_ARITY(x)      ((int) (((word_t*)(x))[0]))
#define SNEK_CLOSURE_PTR(x)        ((int*) ((word_t*)(x))[1])
#define SNEK_CLOSURE_SIZE(x)       ((int) (((word_t*)(x))[2]))
#define SNEK_CLOSURE_ITEM(c, i)           (((word_t*)(c))[(i) + 3])
#define SNEK_CLOSURE_ITEM_REF(c, i)       (((word_t*)(c)) + (i) + 3)

#define SNEK_GET_TUPLE_PTR(t)   ((int*)(((intptr_t)(t)) & ~1))
#define SNEK_GET_CLOSURE_PTR(c) ((int*)(((intptr_t)(c)) & ~5))
#define SNEK_GET_FWD_PTR(x)     ((int*)((x) & ~3))

#define SNEK_IS_PTR(x) ({\
  int __x = (int)(x);    \
  SNEK_IS_TUPLE(__x) || SNEK_IS_CLOSURE(__x) || SNEK_IS_FWD_PTR(__x); \
    })

#define SNEK_GET_PTR(x) ({\
      int __x = (int)(x);  \
      (SNEK_IS_TUPLE(__x)) ? \
      SNEK_GET_TUPLE_PTR(__x) : \
      ((SNEK_IS_CLOSURE(__x)) ? \
       SNEK_GET_CLOSURE_PTR(__x) : \
       SNEK_GET_FWD_PTR(__x));     \
    })

#ifdef SNEK_DBG_PRINT
#define dbgprintf(fmt, ...) fprintf(stderr, "[DEBUG] " fmt, ##__VA_ARGS__)
#else
#define dbgprintf(...) do {} while (0)
#endif

char *print_str(int val);

#endif
