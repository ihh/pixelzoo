#include <stdio.h>
#include "XYmap.h"

XYCoord* newXYCoord (int x, int y) {
  XYCoord* c;
  c = SafeMalloc (sizeof (XYCoord));
  c->x = x;
  c->y = y;
  return c;
}

int compareXYCoord (const void* av, const void* bv) {
  XYCoord *a, *b;
  a = (XYCoord*) av;
  b = (XYCoord*) bv;
  if (a->y > b->y)
    return +1;
  else if (a->y == b->y)
    return a->x > b->x ? +1 : (a->x == b->x ? 0 : -1);
  /* a->y < b->y */
  return -1;
}

void deleteXYCoord (void* cv) {
  XYCoord *c;
  c = (XYCoord*) cv;
  free(c);
}

void printXYCoord (const void* cv) {
  XYCoord *c;
  c = (XYCoord*) cv;
  printf ("(%d,%d)", c->x, c->y);
}

XYMap* newXYMap (void (*ValueDestroyFunc)(void*), 
		    void (*ValuePrintFunc)(void*)) {
  return (XYMap*) newRBTree (compareXYCoord, deleteXYCoord, ValueDestroyFunc, printXYCoord, ValuePrintFunc);
}

void deleteXYMap (XYMap* map) {
  deleteRBTree ((RBTree*) map);
}

XYMapNode* XYMapInsert (XYMap* map, int x, int y, void* value) {
  return (XYMapNode*) RBTreeInsert ((RBTree*) map, (void*) newXYCoord(x,y), value);
}

void XYMapErase (XYMap* map, int x, int y) {
  RBTreeErase ((RBTree*) map, (void*) newXYCoord(x,y));
}

XYMapNode* XYMapFind (XYMap* map, int x, int y) {
  return (XYMapNode*) RBTreeFind ((RBTree*) map, (void*) newXYCoord(x,y));
}

XYSet* newXYSet() {
  return (XYSet*) newXYMap (NullFunction, NullFunction);
}
