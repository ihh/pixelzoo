#ifndef TOOL_INCLUDED
#define TOOL_INCLUDED

#include "quadtree.h"
#include "rule.h"

typedef struct Tool {
  char *name;  /* name of this tool */
  QuadTree *brush;
  State state;
  double paintRate, rechargeRate;  /* mean number of particles deposited/refilled per second */
  int reserve;  /* number of particles left to be deposited */
} Tool;

#endif /* TOOL_INCLUDED */
