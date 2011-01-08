#ifndef VECTOR_INCLUDED
#define VECTOR_INCLUDED

#include <stdlib.h>
#include "util.h"

/* number of blocks that will be allocated the first time you push stuff onto an empty vector */
#define DefaultInitialVectorCapacity ((size_t) 10)

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
void VectorSet (Vector* vec, size_t n, void* value);  /* uses DestroyFunction to delete previous value, if non-NULL */
void VectorReserve (Vector* vec, size_t n);
void VectorPushBack (Vector* vec, void* value);
void* VectorPop (Vector* vec);   /* caller assumes responsibility for deleting val */
void VectorPrint (Vector* vec);  /* debug */

#define VectorSize(VEC) ((size_t) ((VEC)->end - (VEC)->begin))
#define VectorCapacity(VEC) ((size_t) ((VEC)->endAlloc - (VEC)->begin))
#define VectorInBounds(VEC,N) ((N) >= 0 && (N) < VectorSize(VEC))

#define VectorFront(VEC) (*(VEC)->begin)
#define VectorBack(VEC) (*((VEC)->end - 1))

/* void versions of copy, print & delete */
void* VectorDeepCopyVoid(void*);
void VectorPrintVoid(void*);
void VectorDeleteVoid(void*);

#endif /* VECTOR_INCLUDED */
