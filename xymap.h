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
int compareXYCoord (void* av, void* bv);
void printXYCoord (void* cv);

/* mappings from (x,y) coords to values - basically wrappers for RBTree functions */
typedef RBTree XYMap;
typedef RBNode XYMapNode;
#define newXYMap(ValueDestroyFunc,ValuePrintFunc) ((XYMap*) newRBTree (compareXYCoord, deleteXYCoord, ValueDestroyFunc, printXYCoord, ValuePrintFunc))
#define deleteXYMap(XYMAPPTR) deleteRBTree ((RBTree*) XYMAPPTR)
#define XYMapInsert(XYMAPPTR,X,Y,VALUE) ((XYMapNode*) RBTreeInsert ((RBTree*) XYMAPPTR, (void*) newXYCoord(X,Y), VALUE))
#define XYMapErase(XYMAPPTR,X,Y,TEMPXYCOORD) ( (TEMPXYCOORD).x = X, (TEMPXYCOORD).y = Y, RBTreeErase ((RBTree*) XYMAPPTR, (void*) &TEMPXYCOORD))
#define XYMapFind(XYMAPPTR,X,Y,TEMPXYCOORD) ( (TEMPXYCOORD).x = X, (TEMPXYCOORD).y = Y, ((XYMapNode*) RBTreeFind ((RBTree*) XYMAPPTR, (void*) &TEMPXYCOORD)))

/* typedefs & macros for XYSet, a value-less XYMap */
typedef XYMap XYSet;
typedef XYMapNode XYSetNode;
#define newXYSet() ((XYSet*) newXYMap (NullDestroyFunction, NullPrintFunction))
#define deleteXYSet(XYSETPTR) deleteXYMap((XYMap*)XYSETPTR)
#define XYSetInsert(XYSETPTR,X,Y) ((XYSetNode*) XYMapInsert((XYMap*)XYSETPTR,X,Y,NULL)
#define XYSetErase(XYSETPTR,X,Y,TEMPXYCOORD) XYMapErase((XYMap*)XYSETPTR,X,Y,TEMPXYCOORD)
#define XYSetFind(XYSETPTR,X,Y,TEMPXYCOORD) ((XYSetNode*) XYMapFind((XYMap*)XYSETPTR,X,Y,TEMPXYCOORD))

/* XYList */
typedef List XYList;
typedef ListNode XYListNode;
#define newXYList() ((XYList*) newList (deleteXYCoord, printXYCoord))
#define deleteXYList(XYLISTPTR) deleteList ((List*) XYLISTPTR)
#define XYListEmpty(XYLISTPTR) ListEmpty ((List*) XYLISTPTR)
#define XYListSize(XYLISTPTR) ListSize ((List*) XYLISTPTR)
#define XYListInsertBefore(XYLISTPTR,XYLISTNODEPTR,X,Y) ListInsertBefore ((List*) XYLISTPTR, (ListNode*) XYLISTNODEPTR, (void*) newXYCoord(X,Y))
#define XYListErase(XYLISTPTR,XYLISTNODEPTR) ListErase ((List*) XYLISTPTR, (ListNode*) XYLISTNODEPTR)
#define XYListAppend(XYLISTPTR,X,Y) XYListInsertBefore(XYLISTPTR,NULL,X,Y)
#define XYListPrepend(XYLISTPTR,X,Y) XYListInsertBefore(XYLISTPTR,XYLISTPTR->head,X,Y)
#define XYListPop(XYLISTPTR,X,Y) (X) = ((XYCoord*) XYLISTPTR->tail->value)->x; (Y) = ((XYCoord*) XYLISTPTR->tail->value)->y; XYListErase(XYLISTPTR,XYLISTPTR->tail);

/* XYVector */
typedef Vector XYVector;
#define newXYVector() ((XYVector*) newVector (deleteXYCoord, printXYCoord))
#define deleteXYVector(XYVECPTR) deleteVector ((Vector*) XYVECPTR)
#define XYVectorGet(XYVECPTR,N) VectorGet ((Vector*) XYVECPTR, N)
#define XYVectorSet(XYVECPTR,N,X,Y) VectorSet ((Vector*) XYVECPTR, N, (void*) newXYCoord(X,Y))
#define XYVectorReserve(XYVECPTR,N) VectorReserve ((Vector*) XYVECPTR, N)
#define XYVectorPushBack(XYVECPTR,X,Y) VectorPushBack ((Vector*) XYVECPTR, (void*) newXYCoord(X,Y))
#define XYVectorSize(XYVECPTR) VectorSize ((Vector*) XYVECPTR)

#endif /* XYMAP_INCLUDED */
