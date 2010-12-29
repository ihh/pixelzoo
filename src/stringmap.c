#include <string.h>
#include <stdio.h>
#include "stringmap.h"

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

void StringDelete(void* a) {
  SafeFree((int*)a);
}

int StringCompare(void* a, void* b) {
  int cmp = strcmp ((char*)a, (char*)b);
  return cmp > 0 ? +1 : (cmp < 0 ? -1 : 0);
}

void StringPrint(void* a) {
  printf("%i",*(int*)a);
}
