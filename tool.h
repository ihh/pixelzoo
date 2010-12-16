#ifndef TOOL_INCLUDED
#define TOOL_INCLUDED

#include "quadtree.h"
#include "rule.h"
#include "board.h"
#include "statemap.h"
#include "xymap.h"

typedef struct Tool {
  /* toolbox appearance */
  char *name;  /* name of this tool */

  /* brush */
  QuadTree *brushIntensity;  /* brush probability distribution */
  XYMap *brushState;  /* optional XYCoord->State map for brush */
  State defaultBrushState;  /* if brushState==NULL, use this state for entire brush */

  /* permissions */
  XYSet *overwriteLoc;  /* board locations this tool is allowed to overwrite */
  StateSet *overwriteStates;  /* (masked) states this tool is allowed to overwrite */
  State overwriteMask;  /* mask for overwriteStates */

  /* spray rate */
  double sprayRate;  /* mean number of particles deposited per second */
  double reserve;  /* number of particles left that can be deposited */
  double rechargeRate;  /* particle recharge rate, i.e. rate per second at which reserve is replenished */
  double maxReserve;  /* max value of reserve */

} Tool;

Tool* newTool (char *name, int size);
void deleteTool (Tool *tool);

void useTool (Tool *tool, Board *board, int x, int y, double duration);
void rechargeTool (Tool *tool, double duration);

#endif /* TOOL_INCLUDED */
