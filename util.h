#ifndef UTIL_INCLUDED
#define UTIL_INCLUDED

#include <stdlib.h>

/* function pointer typedefs for generic containers */
typedef int (*CompareFunction) (const void*, const void*);
typedef void (*DestroyFunction) (void*);
typedef void (*PrintFunction) (const void*);

/* null functions for generic containers */
void NullDestroyFunction(void*);
void NullPrintFunction(const void*);

/* RGB color */
typedef struct RGB {
  unsigned char r, g, b;
} RGB;

/* randomDouble() returns a random number between 0 and 1 */
double randomDouble();

/* MIN and MAX */
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

/* alloc functions */
void Assert(int assertion, char* error);
void *SafeMalloc(size_t size);
void *SafeCalloc(size_t count, size_t size);
#define SafeFree(PTR) free(PTR)

#endif /* UTIL_INCLUDED */
