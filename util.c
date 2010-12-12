#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

/*  NullDestroyFunction & NullPrintFunction do nothing; they are included so that they can be passed
    as a function to newRBTree, etc, when no other suitable function has been defined */

void* NullCopyFunction(void * item) { return item; }
void NullDestroyFunction(void * junk) { ; }
void NullPrintFunction(void * junk) { ; }


/* Int* functions */
void* IntNew(int a) {
  int *ptr;
  ptr = (int*) SafeMalloc (sizeof (int));
  *ptr = a;
  return (void*) ptr;
}

void* IntCopy(void* a) {
  return IntNew (*(int*)a);
}

void IntDestroy(void* a) {
  SafeFree((int*)a);
}

int IntCompare(void* a, void* b) {
  if( *(int*)a > *(int*)b) return(1);
  if( *(int*)a < *(int*)b) return(-1);
  return(0);
}

void IntPrint(void* a) {
  printf("%i",*(int*)a);
}

/* String* functions */
void* StringNew(char *a) {
  char *ptr;
  ptr = (char*) SafeMalloc ((strlen(a) + 1) * sizeof(char));
  (void) strcpy (ptr, a);
  return (void*) ptr;
}

void* StringCopy(void* a) {
  return StringNew ((char*) a);
}

void StringDestroy(void* a) {
  SafeFree((int*)a);
}

int StringCompare(void* a, void* b) {
  int cmp = strcmp ((char*)a, (char*)b);
  return cmp > 0 ? +1 : (cmp < 0 ? -1 : 0);
}

void StringPrint(void* a) {
  printf("%i",*(int*)a);
}
