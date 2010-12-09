#ifndef XYMAP_INCLUDED
#define XYMAP_INCLUDED

#include "rbtree.h"
#include "list.h"
#include "vector.h"

/* (x,y) co-ordinates */
typedef struct XYCoord {
  int x, y;
} XYCoord;

/* various methods on (x,y) coords */
XYCoord* newXYCoord (int x, int y);
void deleteXYCoord (void* cv);
int compareXYCoord (const void* av, const void* bv);
void printXYCoord (const void* cv);

/* mappings from (x,y) coords to values - basically wrappers for RBTree functions */
typedef RBTree XYMap;
typedef RBNode XYMapNode;
#define newXYMap(ValueDestroyFunc,ValuePrintFunc) ((XYMap*) newRBTree (compareXYCoord, deleteXYCoord, ValueDestroyFunc, printXYCoord, ValuePrintFunc))
#define deleteXYMap(XYMAPPTR) deleteRBTree ((RBTree*) XYMAPPTR)
#define XYMapInsert(XYMAPPTR,X,Y,VALUE) ((XYMapNode*) RBTreeInsert ((RBTree*) XYMAPPTR, (void*) newXYCoord(X,Y), VALUE))
#define XYMapErase(XYMAPPTR,X,Y) RBTreeErase ((RBTree*) XYMAPPTR, (void*) newXYCoord(X,Y))
#define XYMapFind(XYMAPPTR,X,Y) ((XYMapNode*) RBTreeFind ((RBTree*) XYMAPPTR, (void*) newXYCoord(X,Y)))

/* typedefs & macros for XYSet, a value-less XYMap */
typedef XYMap XYSet;
typedef XYMapNode XYSetNode;
#define newXYSet() ((XYSet*) newXYMap (NullFunction, NullFunction))
#define deleteXYSet(XYSETPTR) deleteXYMap((XYMap*)XYSETPTR)
#define XYSetInsert(XYSETPTR,X,Y) ((XYSetNode*) XYMapInsert((XYMap*)XYSETPTR,X,Y,NULL)
#define XYSetErase(XYSETPTR,X,Y) XYMapErase((XYMap*)XYSETPTR,X,Y)
#define XYSetFind(XYSETPTR,X,Y) ((XYSetNode*) XYMapFind((XYMap*)XYSETPTR,X,Y))

/* XYList */
typedef List XYList;
typedef ListNode XYListNode;
#define newXYList() ((XYList*) newList (deleteXYCoord, printXYCoord))
#define deleteXYList(XYLISTPTR) deleteList ((List*) XYLISTPTR)
#define XYListInsertBefore(XYLISTPTR,XYLISTNODEPTR,X,Y) ListInsertBefore ((List*) XYLISTPTR, (ListNode*) XYLISTNODEPTR, (void*) newXYCoord(X,Y))
#define XYListErase(XYLISTPTR,XYLISTNODEPTR) ListErase ((List*) XYLISTPTR, (ListNode*) XYLISTNODEPTR)
#define XYListAppend(XYLISTPTR,X,Y) XYListInsertBefore(XYLISTPTR,NULL,X,Y)
#define XYListPrepend(XYLISTPTR,X,Y) XYListInsertBefore(XYLISTPTR,XYLISTPTR->head,X,Y)

/* XYVector */
typedef Vector XYVector;
#define newXYVector() ((XYVector*) newVector (deleteXYCoord, printXYCoord))
#define deleteXYVector(XYVECPTR) deleteVector ((Vector*) XYVECPTR)
#define XYVectorGet(XYVECPTR,N) VectorGet ((Vector*) XYVECPTR, N)
#define XYVectorSet(XYVECPTR,N,VALUE) VectorSet ((Vector*) XYVECPTR, N, VALUE)
#define XYVectorReserve(XYVECPTR,N) VectorReserve ((Vector*) XYVECPTR, N)
#define XYVectorPushBack(XYVECPTR,VALUE) VectorPushBack ((Vector*) XYVECPTR, VALUE)
#define XYVectorSize(XYVECPTR) VectorSize ((Vector*) XYVECPTR)

#endif /* XYMAP_INCLUDED */
