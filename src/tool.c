#include <stdio.h>
#include "tool.h"
#include "util.h"
#include "stringmap.h"

/* by default, tool paints empty space onto empty space, i.e. does nothing */
#define DefaultToolOldState EmptyState
#define DefaultToolNewState EmptyState

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
  tool->brushCenter.x = tool->brushCenter.y = size / 2;

  tool->overwriteDisallowLoc = NULL;
  tool->overwriteStates = NULL;
  tool->overwriteMask = TypeMask;

  tool->hidden = 0;
  tool->sprayRate = tool->rechargeRate = tool->reserve = tool->maxReserve = 1.;

  return tool;
}

void deleteTool (void *voidTool) {
  Tool *tool;
  tool = (Tool*) voidTool;
  deleteQuadTree (tool->brushIntensity);
  if (tool->brushState)
    deleteXYMap (tool->brushState);
  if (tool->overwriteDisallowLoc)
    deleteXYSet (tool->overwriteDisallowLoc);
  if (tool->overwriteStates)
    deleteStateSet (tool->overwriteStates);
  StringDelete (tool->name);
  SafeFree (tool);
}

void useTool (Tool *tool, Board *board, int xStart, int yStart, int xEnd, int yEnd, double duration) {
  int particles, xOffset, yOffset, xPaint, yPaint;
  State oldState, maskedOldState, newState;
  XYCoord xyTmp;
  XYMapNode *xyNode;
  double linePos, linePosDelta, xDelta, yDelta;
  particles = (int) (.5 + tool->sprayRate * duration);
  linePos = 0.;
  linePosDelta = 1. / (double) particles;
  xDelta = xEnd - xStart;
  yDelta = yEnd - yStart;
  while (particles-- > 0 && topQuadRate(tool->brushIntensity) > 0. && tool->reserve > 0.) {
    sampleQuadLeaf (tool->brushIntensity, board->rng, &xOffset, &yOffset);
    newState = tool->defaultBrushState;
    if (tool->brushState)
      if ((xyNode = XYMapFind(tool->brushState,xOffset,yOffset,xyTmp)))
	newState = *(State*)xyNode->value;
    xPaint = xStart + xOffset - tool->brushCenter.x + (int) (.5 + linePos * xDelta);
    yPaint = yStart + yOffset - tool->brushCenter.y + (int) (.5 + linePos * yDelta);
    linePos += linePosDelta;
    if (onBoard (board, xPaint, yPaint)) {
      if (tool->overwriteDisallowLoc == NULL || XYSetFind (tool->overwriteDisallowLoc, xPaint, yPaint, xyTmp) == NULL) {
	oldState = readBoardStateUnguarded(board,xPaint,yPaint);
	maskedOldState = oldState & tool->overwriteMask;
	if (tool->overwriteStates == NULL || StateSetFind (tool->overwriteStates, maskedOldState)) {
	  writeBoardStateUnguardedFunction (board, xPaint, yPaint, newState);
	  if (board->moveLog)
	    (void) MoveListAppend (board->moveLog, board->microticks, xPaint, yPaint, newState);
	  if (oldState != newState)
	    tool->reserve = MAX (tool->reserve - 1, 0.);
	}
      }
    }
  }
}

void rechargeTool (Tool *tool, double duration) {
    tool->reserve = MIN (tool->reserve + tool->rechargeRate * duration, tool->maxReserve);
}

void printTool (void *voidTool) {
  Tool *tool;
  tool = (Tool*) voidTool;
  printf ("Tool %s : reserve %g / %g\n", tool->name, tool->reserve, tool->maxReserve);
}
