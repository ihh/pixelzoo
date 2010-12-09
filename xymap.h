#ifndef XYMAP_INCLUDED
#define XYMAP_INCLUDED

#include "rbtree.h"

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
XYMap* newXYMap (void (*ValueDestroyFunc)(void*), 
		 void (*ValuePrintFunc)(void*));
void deleteXYMap (XYMap* map);
XYMapNode* XYMapInsert (XYMap* map, int x, int y, void* value);
void XYMapErase (XYMap* map, int x, int y);
XYMapNode* XYMapFind (XYMap* map, int x, int y);

/* typedefs & macros for XYSet, a value-less XYMap */
typedef XYMap XYSet;
typedef XYMapNode XYSetNode;
XYSet* newXYSet();
#define deleteXYSet(XYSET) deleteXYMap((XYMap*)XYSET)
#define XYSetInsert(XYSET,X,Y) ((XYSetNode*) XYMapInsert((XYMap*)XYSET,X,Y,NULL)
#define XYSetErase(XYSET,X,Y) XYMapErase((XYMap*)XYSET,X,Y)
#define XYSetFind(XYSET,X,Y) ((XYSetNode*) XYMapFind((XYMap*)XYSET,X,Y))

#endif /* XYMAP_INCLUDED */
