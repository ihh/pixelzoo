#include "vector.h"

Vector* newVector (void (*DestroyFunc)(void*), void (*PrintFunc)(void*)) {
  Vector* vec;
  vec = SafeMalloc (sizeof (Vector));
  vec->begin = vec->end = vec->endAlloc = NULL;
  return vec;
}

void deleteVector (Vector* vec) {
  void** ptr;
  for (ptr = vec->begin; ptr != vec->end; ++ptr)
    (*vec->Destroy) (*ptr);
  if (vec->begin != NULL)
    free (vec->begin);
  free (vec);
}

void VectorReserve (Vector* vec, size_t n) {
  void **newBegin, **srcPtr, **destPtr;
  if (n > VectorCapacity(vec)) {
    newBegin = SafeMalloc (n * sizeof(void*));
    for (srcPtr = vec->begin, destPtr = newBegin; srcPtr != vec->end; ++srcPtr, ++destPtr)
      *destPtr = *srcPtr;
    if (vec->begin != NULL)
      free (vec->begin);
    vec->begin = newBegin;
    vec->end = destPtr;
    vec->endAlloc = newBegin + n;
  }
}

void VectorPushBack (Vector* vec, void* value) {
  if (vec->end == vec->endAlloc)
    VectorReserve (vec, 2 * VectorSize(vec));  /* double the capacity each time, to avoid O(N^2) behavior */
  *(vec->end++) = value;
}

void VectorPrint (Vector* vec) {
  void** ptr;
  for (ptr = vec->begin; ptr != vec->end; ++ptr)
    (*vec->Print) (*ptr);
}

void* VectorGet (Vector* vec, size_t n) {
  Assert (VectorInBounds(vec,n), "VectorGet: index out of bounds");
  return vec->begin[n];
}

void VectorSet (Vector* vec, size_t n, void* value) {
  Assert (VectorInBounds(vec,n), "VectorGet: index out of bounds");
  vec->begin[n] = value;
}
