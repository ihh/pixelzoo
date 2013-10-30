#ifndef TOOL_INCLUDED
#define TOOL_INCLUDED

#include "quadtree.h"
#include "rule.h"
#include "board.h"
#include "statemap.h"
#include "xymap.h"

/* Tools are designed to be used in an overhead view, so the z-coordinate is absolute */
typedef struct Tool {
  /* toolbox appearance */
  char *name;  /* name of this tool */
  int z;  /* absolute z-coord specifying layer that Tool operates on */

  /* brush */
  LocalOffset brushCenter;  /* delta(x,y) of central cell in brush */
  QuadTree *brushIntensity;  /* brush probability distribution */
  XYMap *brushState;  /* optional XYCoord->State map for brush */
  State defaultBrushState;  /* if brushState==NULL, use this state for entire brush */

  /* permissions */
  XYSet *overwriteDisallowLoc;  /* board locations this tool is NOT allowed to overwrite */
  StateSet *overwriteStates;  /* (masked) states this tool is allowed to overwrite */
  State overwriteMask;  /* mask for overwriteStates */

  /* spray rate */
  double sprayRate;  /* mean number of particles deposited per second */
  double reserve;  /* number of particles left that can be deposited */
  double rechargeRate;  /* particle recharge rate, i.e. rate per second at which reserve is replenished */
  double maxReserve;  /* max value of reserve */

} Tool;

Tool* newTool (char *name, int size);  /* copies name */
void deleteTool (void *tool);
void printTool (void *tool);

void useTool (Tool *tool, void *voidGame, int xStart, int yStart, int xEnd, int yEnd, double duration);
void rechargeTool (Tool *tool, double duration);

#endif /* TOOL_INCLUDED */
