#ifndef UTIL_INCLUDED
#define UTIL_INCLUDED

#include <stdlib.h>

/* function pointer typedefs for generic containers */
typedef int (*CompareFunction) (void*, void*);
typedef void (*DestroyFunction) (void*);
typedef void* (*CopyFunction) (void*);
typedef void (*PrintFunction) (void*);

/* null functions for generic containers */
void NullDestroyFunction(void*);  /* does nothing */
void* NullCopyFunction(void*);  /* returns the supplied parameter without doing anything */
void NullPrintFunction(void*);  /* does nothing */

/* container functions for ints */
/* These are actually a bit wasteful, because we could just use the (void*) pointer to store the int value... hmm */
/* functions for ints */
void* IntNew(int a);
void* IntCopy(void* a);
void IntDelete(void* a);
int IntCompare(void* a, void* b);
void IntPrint(void* a);

/* randomDouble() returns a random number between 0 and 1 */
double randomDouble();

/* MIN, MAX, ABS */
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define ABS(X)   ((X) >= 0 ? (X) : -(X))

/* alloc functions */
void Assert(int assertion, char* error);
void *SafeMalloc(size_t size);
void *SafeCalloc(size_t count, size_t size);
#define SafeFree(PTR) free(PTR)
#define SafeFreeOrNull(PTR) if (PTR) SafeFree(PTR);

/* stupid quoting bollix */
#define QUOTEME_(x) #x
#define QUOTEME(x) QUOTEME_(x)

#endif /* UTIL_INCLUDED */
