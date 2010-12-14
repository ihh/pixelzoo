#ifndef TOOL_INCLUDED
#define TOOL_INCLUDED

#include "quadtree.h"
#include "rule.h"
#include "board.h"
#include "statemap.h"
#include "xymap.h"

typedef struct Tool {
  char *name;  /* name of this tool */
  QuadTree *brush;  /* brush shape */
  State state;  /* state this tool paints */
  XYSet *overwriteLoc;  /* board locations this tool can overwrite */
  StateSet *overwrite;  /* (masked) states this tool can overwrite */
  State overwriteMask;  /* mask for overwrite */
  double paintRate, rechargeRate;  /* mean number of particles deposited/refilled per second */
  double reserve, maxReserve;  /* reserve = number of particles left that can be deposited */
} Tool;

Tool* newTool (char *name, int size);
void deleteTool (Tool *tool);

void useTool (Tool *tool, Board *board, int x, int y, double duration);
void rechargeTool (Tool *tool, double duration);

#endif /* TOOL_INCLUDED */
