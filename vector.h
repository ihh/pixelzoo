#ifndef VECTOR_INCLUDED
#define VECTOR_INCLUDED

#include <stdlib.h>
#include "util.h"

/* Destroy(a) takes a pointer to whatever 'a' might be and frees it accordingly */
typedef struct Vector {
  void (*Destroy)(void* a);
  void (*Print)(const void* a);
  void **begin, **end, **endAlloc;
} Vector;

Vector* newVector (void (*DestroyFunc)(void*),
		   void (*PrintFunc)(void*));
void deleteVector (Vector* vec);
void* VectorGet (Vector* vec, size_t n);
void VectorSet (Vector* vec, size_t n, void* value);
void VectorReserve (Vector* vec, size_t n);
void VectorPushBack (Vector* vec, void* value);
void VectorPrint (Vector* vec);  /* debug */

#define VectorSize(VEC) ((size_t) ((VEC)->end - (VEC)->begin))
#define VectorCapacity(VEC) ((size_t) ((VEC)->endAlloc - (VEC)->begin))
#define VectorInBounds(VEC,N) ((N) >= 0 && (N) < VectorSize(VEC))

#endif /* VECTOR_INCLUDED */
