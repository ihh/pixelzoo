#include <stdio.h>
#include <stdlib.h>
#include "util.h"

int XYCoordComp (const void* av, const void* bv) {
  XYCoord *a, *b;
  a = (XYCoord*) av;
  b = (XYCoord*) bv;
  if (a->y > b->y)
    return +1;
  else if (a->y == b->y)
    return a->x > b->x ? +1 : (a->x == b->x ? 0 : -1);
  /* a->y < b->y */
  return -1;
}

double randomDouble() {
  return (double) rand() / (double) RAND_MAX;
}

void Assert(int assertion, char* error) {
  if(!assertion) {
    printf("Assertion Failed: %s\n",error);
    exit(-1);
  }
}

void * SafeMalloc(size_t size) {
  void * result;

  if ( (result = malloc(size)) ) { /* assignment intentional */
    return(result);
  } else {
    printf("memory overflow: malloc failed in SafeMalloc.");
    printf("  Exiting Program.\n");
    exit(-1);
  }
  return(0);
}

void *SafeCalloc(size_t count, size_t size) {
  void * result;

  if ( (result = calloc(count,size)) ) { /* assignment intentional */
    return(result);
  } else {
    printf("memory overflow: calloc failed in SafeCalloc.");
    printf("  Exiting Program.\n");
    exit(-1);
  }
  return(0);
}
