#ifndef VECTOR_INCLUDED
#define VECTOR_INCLUDED

#include <stdlib.h>
#include "util.h"

typedef struct Vector {
  DestroyFunction Destroy;
  PrintFunction Print;
  void **begin, **end, **endAlloc;
} Vector;

Vector* newVector(DestroyFunction DestroyFunc,
		  PrintFunction PrintFunc);
void deleteVector (Vector* vec);
void* VectorGet (Vector* vec, size_t n);
void VectorSet (Vector* vec, size_t n, void* value);
void VectorReserve (Vector* vec, size_t n);
void VectorPushBack (Vector* vec, void* value);
void VectorPrint (Vector* vec);  /* debug */

#define VectorSize(VEC) ((size_t) ((VEC)->end - (VEC)->begin))
#define VectorCapacity(VEC) ((size_t) ((VEC)->endAlloc - (VEC)->begin))
#define VectorInBounds(VEC,N) ((N) >= 0 && (N) < VectorSize(VEC))

/* void versions of print & delete */
void VectorPrintVoid(const void*);
void VectorDeleteVoid(void*);

#endif /* VECTOR_INCLUDED */
