#include "vector.h"

Vector* newVector (void* (*CopyFunc)(void*),
		   void (*DestroyFunc)(void*),
		   void (*PrintFunc)(void*)) {
  Vector* vec;
  vec = SafeMalloc (sizeof (Vector));
  vec->Copy = CopyFunc;
  vec->Destroy = DestroyFunc;
  vec->Print = PrintFunc;
  vec->begin = vec->end = vec->endAlloc = NULL;
  return vec;
}

void deleteVector (Vector* vec) {
  void** ptr;
  for (ptr = vec->begin; ptr != vec->end; ++ptr)
    (*vec->Destroy) (*ptr);
  if (vec->begin != NULL)
    SafeFree(vec->begin);
  SafeFree(vec);
}

Vector* VectorDeepCopy (Vector* vec) {
  Vector* copyVec;
  void** iter;
  copyVec = newVector (vec->Copy, vec->Destroy, vec->Print);
  VectorReserve (copyVec, VectorCapacity(vec));
  for (iter = vec->begin; iter != vec->end; ++iter)
    VectorPushBack (copyVec, (*vec->Copy) (*iter));
  return copyVec;
}

void VectorReserve (Vector* vec, size_t n) {
  void **newBegin, **srcPtr, **destPtr;
  if (n > VectorCapacity(vec)) {
    newBegin = SafeMalloc (n * sizeof(void*));
    for (srcPtr = vec->begin, destPtr = newBegin; srcPtr != vec->end; ++srcPtr, ++destPtr)
      *destPtr = *srcPtr;
    if (vec->begin != NULL)
      SafeFree(vec->begin);
    vec->begin = newBegin;
    vec->end = destPtr;
    vec->endAlloc = newBegin + n;
  }
}

void VectorPushBack (Vector* vec, void* value) {
  if (vec->end == vec->endAlloc)
    VectorReserve (vec, vec->end == NULL ? DefaultInitialVectorCapacity : (2 * VectorCapacity(vec)));  /* double the capacity each time, to avoid O(N^2) behavior */
  *(vec->end++) = value;
}

void* VectorPop (Vector* vec) {
  void *val;
  Assert (VectorSize(vec) > 0, "Popped empty vector");
  val = VectorBack (vec);
  --vec->end;
  return val;
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
  if (vec->begin[n])
    (*vec->Destroy) (vec->begin[n]);  /* delete old value */
  vec->begin[n] = value;
}

void* VectorDeepCopyVoid(void* vector) { return (void*) VectorDeepCopy ((Vector*) vector); }
void VectorPrintVoid(void* vector) { VectorPrint ((Vector*) vector); }
void VectorDeleteVoid(void* vector) { deleteVector ((Vector*) vector); }
