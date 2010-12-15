#include "tool.h"
#include "util.h"
#include "stringmap.h"

/* by default, tool paints empty space onto empty space, i.e. does nothing */
#define DefaultToolOldState 0
#define DefaultToolNewState 0

Tool* newTool (char *name, int size) {
  Tool *tool;
  int x, y;
  tool = SafeMalloc (sizeof(Tool));
  tool->name = StringCopy (name);

  tool->brushIntensity = newQuadTree (size);
  for (x = 0; x < size; ++x)
    for (y = 0; y < size; ++y)
      updateQuadTree (tool->brushIntensity, x, y, 1.);
  tool->brushState = NULL;
  tool->defaultBrushState = DefaultToolNewState;

  tool->overwriteLoc = newXYSet();
  tool->overwriteStates = newStateSet();
  (void) StateSetInsert (tool->overwriteStates, DefaultToolOldState);
  tool->overwriteMask = TypeMask;

  tool->paintRate = tool->rechargeRate = tool->reserve = tool->maxReserve = 1.;

  return tool;
}

void deleteTool (Tool *tool) {
  deleteQuadTree (tool->brushIntensity);
  if (tool->brushState)
    deleteXYMap (tool->brushState);
  deleteXYSet (tool->overwriteLoc);
  deleteStateSet (tool->overwriteStates);
  StringDelete (tool->name);
  SafeFree (tool);
}

void useTool (Tool *tool, Board *board, int x, int y, double duration) {
  int particles, xOffset, yOffset, xPaint, yPaint;
  State maskedOldState, newState;
  XYCoord xyTmp;
  XYMapNode *xyNode;
  particles = (int) (.5 + tool->paintRate * duration);
  while (particles-- > 0 && topQuadRate(tool->brushIntensity) > 0. && tool->reserve > 0.) {
    sampleQuadLeaf (tool->brushIntensity, &xOffset, &yOffset);
    newState = tool->defaultBrushState;
    if (tool->brushState)
      if ((xyNode = XYMapFind(tool->brushState,x,y,xyTmp)))
	newState = *(State*)xyNode->value;
    xPaint = x + xOffset;
    yPaint = y + yOffset;
    maskedOldState = readBoardState(board,xPaint,yPaint) & tool->overwriteMask;
    if (StateSetFind (tool->overwriteStates, maskedOldState)) {
      writeBoardState (board, xPaint, yPaint, newState);
      tool->reserve = MAX (tool->reserve - 1, 0.);
    }
  }
}

void rechargeTool (Tool *tool, double duration) {
    tool->reserve = MIN (tool->reserve + tool->rechargeRate * duration, tool->maxReserve);
}
