#include <stdio.h>
#include <stdlib.h>
#include "util.h"

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

/*  NullFunction does nothing; it is included so that it can be passed */
/*  as a function to newRBTree, etc, when no other suitable function has */
/*  been defined */

void NullFunction(void * junk) { ; }
