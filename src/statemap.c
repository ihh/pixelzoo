#include <stdio.h>
#include "statemap.h"

State* newState(State s) {
  State* sPtr;
  sPtr = SafeMalloc (sizeof (State));
  *sPtr = s;
  return sPtr;
}

void* copyState(void* sPtr) {
  return (void*) newState (*(State*)sPtr);
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
  printf("%llu",*(State*)a);
}

Message* newMessage(Message s) {
  Message* sPtr;
  sPtr = SafeMalloc (sizeof (Message));
  *sPtr = s;
  return sPtr;
}

void* copyMessage(void* sPtr) {
  return (void*) newMessage (*(Message*)sPtr);
}

void deleteMessage(void* a) {
  SafeFree((Message*)a);
}

int compareMessage(void* a,void* b) {
  if( *(Message*)a > *(Message*)b) return(1);
  if( *(Message*)a < *(Message*)b) return(-1);
  return(0);
}

void printMessage(void* a) {
  printf("%llu",*(Message*)a);
}
