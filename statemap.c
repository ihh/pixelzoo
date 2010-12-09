#include <stdio.h>
#include "statemap.h"

State* newState(State s) {
  State* sPtr;
  sPtr = SafeMalloc (sizeof (State));
  *sPtr = s;
  return sPtr;
}

void deleteState(void* a) {
  SafeFree((State*)a);
}

int compareState(const void* a,const void* b) {
  if( *(State*)a > *(State*)b) return(1);
  if( *(State*)a < *(State*)b) return(-1);
  return(0);
}

void printState(const void* a) {
  printf("%lu",*(State*)a);
}
