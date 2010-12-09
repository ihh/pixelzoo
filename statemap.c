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

int compareState(void* a,void* b) {
  if( *(State*)a > *(State*)b) return(1);
  if( *(State*)a < *(State*)b) return(-1);
  return(0);
}

void printState(void* a) {
  printf("%lu",*(State*)a);
}
