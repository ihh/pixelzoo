#ifndef UTIL_INCLUDED
#define UTIL_INCLUDED

#include <stdlib.h>

/* RGB color */
typedef struct RGB {
  unsigned char r, g, b;
} RGB;

/* (x,y) co-ordinates */
typedef struct XYCoord {
  int x, y;
} XYCoord;

/* sort relation on (x,y) coords: orders by increasing y, then increasing x */
int XYCoordComp (const void* av, const void* bv);

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
