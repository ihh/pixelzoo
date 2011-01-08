#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "util.h"

int randomInt (int N) {
  return (int) (randomDouble() * (double) N);
}

double randomDouble() {
  return (double) rand() / ((double) RAND_MAX + 1.);   /* the +1 ensures that this function never returns 1 */
}

double randomExp() {
  double r;
  r = randomDouble();
  return r > 0. ? -log(r) : 0.;
}

void Abort(char* error) {
  printf("Abort: %s\n",error);
  exit(-1);
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

/*  NullDestroyFunction & NullPrintFunction do nothing; they are included so that they can be passed
    as a function to newRBTree, etc, when no other suitable function has been defined */

void* NullCopyFunction(void * item) { return item; }
void NullDestroyFunction(void * junk) { ; }
void NullPrintFunction(void * junk) { ; }

/* Abort null functions */
void AbortDestroyFunction(void* junk) { Abort ("Unimplemented destroy function called"); }
void* AbortCopyFunction(void* junk) { Abort ("Unimplemented copy function called"); return NULL; }

/* Int functions */
void* IntNew(Int64 a) {
  Int64 *ptr;
  ptr = (Int64*) SafeMalloc (sizeof (Int64));
  *ptr = a;
  return (void*) ptr;
}

void* IntCopy(void* a) {
  return IntNew (*(Int64*)a);
}

void IntDelete(void* a) {
  SafeFree((Int64*)a);
}

int IntCompare(void* a, void* b) {
  if( *(Int64*)a > *(Int64*)b) return 1;
  if( *(Int64*)a < *(Int64*)b) return -1;
  return 0;
}

void IntPrint(void* a) {
  printf("%lli",*(Int64*)a);
}

/* Double functions */
void* DoubleNew(double a) {
  double *ptr;
  ptr = (double*) SafeMalloc (sizeof (double));
  *ptr = a;
  return (void*) ptr;
}

void* DoubleCopy(void* a) {
  return DoubleNew (*(double*)a);
}

void DoubleDelete(void* a) {
  SafeFree((double*)a);
}

int DoubleCompare(void* a, void* b) {
  if( *(double*)a > *(double*)b) return(1);
  if( *(double*)a < *(double*)b) return(-1);
  return(0);
}

void DoublePrint(void* a) {
  printf("%g",*(double*)a);
}
