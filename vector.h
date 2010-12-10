#ifndef VECTOR_INCLUDED
#define VECTOR_INCLUDED

#include <stdlib.h>
#include "util.h"

typedef struct Vector {
  CopyFunction Copy;
  DestroyFunction Destroy;
  PrintFunction Print;
  void **begin, **end, **endAlloc;
} Vector;

Vector* newVector(CopyFunction CopyFunc,
		  DestroyFunction DestroyFunc,
		  PrintFunction PrintFunc);
void deleteVector (Vector* vec);
Vector* VectorDeepCopy (Vector* vec);  /* uses CopyFunction to copy values */
void* VectorGet (Vector* vec, size_t n);
void VectorSet (Vector* vec, size_t n, void* value);
void VectorReserve (Vector* vec, size_t n);
void VectorPushBack (Vector* vec, void* value);
void VectorPrint (Vector* vec);  /* debug */

#define VectorSize(VEC) ((size_t) ((VEC)->end - (VEC)->begin))
#define VectorCapacity(VEC) ((size_t) ((VEC)->endAlloc - (VEC)->begin))
#define VectorInBounds(VEC,N) ((N) >= 0 && (N) < VectorSize(VEC))

/* void versions of copy, print & delete */
void* VectorDeepCopyVoid(void*);
void VectorPrintVoid(void*);
void VectorDeleteVoid(void*);

#endif /* VECTOR_INCLUDED */
