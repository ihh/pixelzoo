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
  tool->brush = newQuadTree (size);
  for (x = 0; x < size; ++x)
    for (y = 0; y < size; ++y)
      updateQuadTree (tool->brush, x, y, 1.);
  tool->overwrite = newStateSet();
  (void) StateSetInsert (tool->overwrite, DefaultToolOldState);
  tool->overwriteMask = TypeMask;
  tool->state = DefaultToolNewState;
  tool->paintRate = tool->rechargeRate = tool->reserve = tool->maxReserve = 1.;
  return tool;
}

void deleteTool (Tool *tool) {
  deleteStateSet (tool->overwrite);
  deleteQuadTree (tool->brush);
  StringDelete (tool->name);
  SafeFree (tool);
}

void useTool (Tool *tool, Board *board, int x, int y, double duration) {
  int particles, xOffset, yOffset, xPaint, yPaint;
  State maskedOldState;
  particles = (int) (tool->paintRate * duration);
  while (particles > 0 && tool->reserve > 0.) {
    --particles;
    sampleQuadLeaf (tool->brush, &xOffset, &yOffset);
    xPaint = x + xOffset;
    yPaint = y + yOffset;
    maskedOldState = readBoardState(board,xPaint,yPaint) & tool->overwriteMask;
    if (StateSetFind (tool->overwrite, maskedOldState)) {
      writeBoardState (board, xPaint, yPaint, tool->state);
      tool->reserve = MAX (tool->reserve - 1, 0.);
    }
  }
}

void rechargeTool (Tool *tool, double duration) {
    tool->reserve = MIN (tool->reserve + tool->rechargeRate * duration, tool->maxReserve);
}
