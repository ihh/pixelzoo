#ifndef TOOL_INCLUDED
#define TOOL_INCLUDED

#include "quadtree.h"
#include "rule.h"
#include "board.h"
#include "statemap.h"
#include "xymap.h"

typedef struct Tool {
  char *name;  /* name of this tool */
  QuadTree *brushIntensity;  /* brush probability distribution */
  XYMap *brushState;  /* optional XYCoord->State map for brush */
  State defaultBrushState;  /* if brushState==NULL, use this state for entire brush */
  XYSet *overwriteLoc;  /* board locations this tool is allowed to overwrite */
  StateSet *overwriteStates;  /* (masked) states this tool is allowed to overwrite */
  State overwriteMask;  /* mask for overwriteStates */
  double paintRate;  /* mean number of chunks deposited per second */
  double reserve;  /* number of particles left that can be deposited */
  double rechargeRate;  /* particle recharge rate, i.e. rate per second at which reserve is replenished */
  double maxReserve;  /* max value of reserve */
  int particlesPerChunk;  /* particles per chunk */
  int singleUse;  /* if true, brushIntensity[x][y] will be reset to zero after particle (x,y) deposited */
} Tool;

Tool* newTool (char *name, int size);
void deleteTool (Tool *tool);

void useTool (Tool *tool, Board *board, int x, int y, double duration);
void rechargeTool (Tool *tool, double duration);

#endif /* TOOL_INCLUDED */
