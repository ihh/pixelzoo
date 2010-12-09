#ifndef UTIL_INCLUDED
#define UTIL_INCLUDED

#include <stdlib.h>

/* RGB color */
typedef struct RGB {
  unsigned char r, g, b;
} RGB;

/* randomDouble() returns a random number between 0 and 1 */
double randomDouble();

/* MIN and MAX */
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

/* misc functions */
void Assert(int assertion, char* error);
void *SafeMalloc(size_t size);
void *SafeCalloc(size_t count, size_t size);

#endif /* UTIL_INCLUDED */
