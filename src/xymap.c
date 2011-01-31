#include <stdio.h>
#include "xymap.h"

XYCoord* newXYCoord (int x, int y) {
  XYCoord* c;
  c = SafeMalloc (sizeof (XYCoord));
  c->x = x;
  c->y = y;
  return c;
}

void* copyXYCoord (void* xyPtr) {
  return (void*) newXYCoord (((XYCoord*)xyPtr)->x, ((XYCoord*)xyPtr)->y);
}

int compareXYCoord (void* av, void* bv) {
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
  SafeFree(c);
}

void printXYCoord (void* cv) {
  XYCoord *c;
  c = (XYCoord*) cv;
  printf ("(%d,%d)", c->x, c->y);
}
