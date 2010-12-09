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
  SafeFree(c);
}

void printXYCoord (const void* cv) {
  XYCoord *c;
  c = (XYCoord*) cv;
  printf ("(%d,%d)", c->x, c->y);
}
