#ifndef TOOL_INCLUDED
#define TOOL_INCLUDED

#include "quadtree.h"
#include "rule.h"

typedef struct Tool {
  char *name;  /* name of this tool */
  QuadTree *brush;
  State state;
  double paintRate, rechargeRate;  /* mean number of particles deposited/refilled per second */
  double reserve;  /* number of particles left to be deposited */
} Tool;

Tool* newTool();
void deleteTool (Tool *tool);

void useTool (Tool *tool, Board *board, int x, int y, double duration);
void rechargeTool (Tool *tool, double duration);

#endif /* TOOL_INCLUDED */
