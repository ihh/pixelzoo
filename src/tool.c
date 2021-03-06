#include <stdio.h>
#include "tool.h"
#include "game.h"
#include "util.h"
#include "stringmap.h"

/* by default, tool paints empty space onto empty space, i.e. does nothing */
#define DefaultToolOldState EmptyState
#define DefaultToolNewState EmptyState

int toolPosAllowed (Tool*, Board*, int, int, int, State);

Tool* newTool (char *name, int size) {
  Tool *tool;
  int x, y;
  tool = SafeMalloc (sizeof(Tool));
  tool->name = StringCopy (name);
  tool->icon = NULL;

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

  tool->sprayRate = tool->rechargeRate = tool->reserve = tool->maxReserve = 1.;
  tool->isStamp = 0;

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
  SafeFreeOrNull (tool->icon);
  SafeFree (tool);
}

int toolPosAllowed (Tool *tool, Board *board, int xPaint, int yPaint, int zPaint, State newState) {
  State oldState, maskedOldState;
  XYCoord xyTmp;
  if (onBoard (board, xPaint, yPaint, zPaint)) {
    if (tool->overwriteDisallowLoc == NULL || XYSetFind (tool->overwriteDisallowLoc, xPaint, yPaint, xyTmp) == NULL) {
      oldState = readBoardStateUnguarded(board,xPaint,yPaint,zPaint);
      if (oldState != newState) {
	maskedOldState = oldState & tool->overwriteMask;
	if (tool->overwriteStates == NULL || StateSetFind (tool->overwriteStates, maskedOldState))
	  return 1;
      }
    }
  }
  return 0;
}

void useTool (Tool *tool, void *voidGame, int xStart, int yStart, int xEnd, int yEnd, double duration) {
  Game *game;
  Board *board;
  int particles, xOffset, yOffset, xPaint, yPaint, zPaint, allClear;
  State newState;
  XYCoord xyTmp;
  XYMapNode *xyNode;
  double linePos, linePosDelta, xDelta, yDelta;

  game = (Game*) voidGame;
  board = game->board;
  zPaint = tool->z;  /* always paint to top layer */
  particles = (int) (.5 + tool->sprayRate * duration);
  linePos = 0.;
  linePosDelta = 1. / (double) particles;
  xDelta = xEnd - xStart;
  yDelta = yEnd - yStart;

  if (tool->isStamp) {
    if (tool->reserve >= RBTreeSize(tool->brushState)) {
	allClear = 1;
	xStart += -tool->brushCenter.x + (int) (.5 + .5 * xDelta);
	yStart += -tool->brushCenter.y + (int) (.5 + .5 * yDelta);

	for (xyNode = RBTreeFirst(tool->brushState);
	     allClear && !RBTreeIteratorFinished(tool->brushState,xyNode);
	     xyNode = RBTreeSuccessor(tool->brushState,xyNode)) {
	  xPaint = xStart + ((XYCoord*) xyNode->key)->x;
	  yPaint = yStart + ((XYCoord*) xyNode->key)->y;
	  newState = *(State*) xyNode->value;
	  if (!toolPosAllowed (tool, board, xPaint, yPaint, zPaint, newState))
	    allClear = 0;
	}

	if (allClear)
	  for (xyNode = RBTreeFirst(tool->brushState);
	       !RBTreeIteratorFinished(tool->brushState,xyNode);
	       xyNode = RBTreeSuccessor(tool->brushState,xyNode)) {
	    xPaint = xStart + ((XYCoord*) xyNode->key)->x;
	    yPaint = yStart + ((XYCoord*) xyNode->key)->y;
	    newState = *(State*) xyNode->value;
	    writeBoardMove (board, xPaint, yPaint, zPaint, newState);
	    tool->reserve = MAX (tool->reserve - 1, 0.);
	  }
      }

  } else
    while (particles-- > 0 && topQuadRate(tool->brushIntensity) > 0. && tool->reserve > 0.) {
      sampleQuadLeaf (tool->brushIntensity, game->rng, &xOffset, &yOffset);   /* NB this must *not* use the Board's RNG, as it is a user event, not a simulation event */
      newState = tool->defaultBrushState;
      if (tool->brushState)
	if ((xyNode = XYMapFind(tool->brushState,xOffset,yOffset,xyTmp)))
	  newState = *(State*)xyNode->value;
      xPaint = xStart + xOffset - tool->brushCenter.x + (int) (.5 + linePos * xDelta);
      yPaint = yStart + yOffset - tool->brushCenter.y + (int) (.5 + linePos * yDelta);
      linePos += linePosDelta;
      if (toolPosAllowed (tool, board, xPaint, yPaint, zPaint, newState)) {
	writeBoardMove (board, xPaint, yPaint, zPaint, newState);
	tool->reserve = MAX (tool->reserve - 1, 0.);
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
