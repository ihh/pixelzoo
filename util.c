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

void convertHSVtoRGB (double H, double S, double V, RGB* rgb) {
  /* From http://en.wikipedia.org/wiki/HSL_and_HSV#From_HSV */
  int sextant;
  double C, X, M, Hdash;
  unsigned char c, x, m;
  Assert (H >= 0 && H < 360, "convertHSVtoRGB: H out of range");
  Assert (S >= 0 && S <= 1, "convertHSVtoRGB: S out of range");
  Assert (V >= 0 && V <= 1, "convertHSVtoRGB: V out of range");
  C = V * S;
  Hdash = H / 60;
  sextant = (int) Hdash;
  X = C * (1 - ABS((Hdash - (sextant - (sextant % 2))) - 1));   /* Hdash % 2 = Hdash - (sextant - (sextant % 2)) */
  M = V - C;
  m = (unsigned char) (.5 + M * 255);
  c = m + (unsigned char) (.5 + C * 255);
  x = m + (unsigned char) (.5 + X * 255);
  switch (sextant) {
  case 0:
    rgb->r = c;
    rgb->g = x;
    rgb->b = m;
    break;

  case 1:
    rgb->r = x;
    rgb->g = c;
    rgb->b = m;
    break;

  case 2:
    rgb->r = m;
    rgb->g = c;
    rgb->b = x;
    break;

  case 3:
    rgb->r = m;
    rgb->g = x;
    rgb->b = c;
    break;

  case 4:
    rgb->r = x;
    rgb->g = m;
    rgb->b = c;
    break;

  case 5:
    rgb->r = c;
    rgb->g = 0;
    rgb->b = x;
    break;

  default:
    /* should never get here */
    rgb->r = rgb->g = rgb->b = 0xff;
    break;
  }
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

void IntDelete(void* a) {
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
