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

/* abort functions for when null functions should never be called */
void AbortDestroyFunction(void*);
void* AbortCopyFunction(void*);

/* container functions for ints */

/* Functions for ints
   (It's tempting to think that rather than allocating space, one could just use the (void*) pointer to store the int value;
   however, this risks platform-specific errors/warnings due to differences in bytesize/signedness between void* and int)
*/
void* IntNew(int a);
void* IntCopy(void* a);
void IntDelete(void* a);
int IntCompare(void* a, void* b);
void IntPrint(void* a);

/* randomDouble() returns a uniformly-distributed real number between 0 and 1 */
double randomDouble();

/* randomInt(N) returns a uniformly-distributed integer from 0 to N-1 */
int randomInt(int);

/* randomExp() returns an exponentially-distributed real number between 0 and infinity */
double randomExp();

/* DUMP */
#undef DUMP
#define DUMP(x, fmt) printf("%s:%u: %s=" fmt, __FILE__, __LINE__, #x, x)

/* MIN, MAX, ABS */
#undef MIN
#undef MAX
#undef ABS
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define ABS(X)   ((X) >= 0 ? (X) : -(X))

/* alloc functions */
void Abort(char* error);
void Assert(int assertion, char* error);
void *SafeMalloc(size_t size);
void *SafeCalloc(size_t count, size_t size);
#define SafeFree(PTR) free(PTR)
#define SafeFreeOrNull(PTR) if (PTR) SafeFree(PTR);

/* stupid quoting bollix */
#define QUOTEME_(x) #x
#define QUOTEME(x) QUOTEME_(x)

#endif /* UTIL_INCLUDED */
