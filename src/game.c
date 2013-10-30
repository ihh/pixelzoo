#include <stdio.h>
#include <math.h>

#include "game.h"
#include "notify.h"
#include "tool.h"

/* uncomment the #define to log all board writes to stderr */
/*
#define PIXELZOO_DEBUG
*/

/* buffer size for log messages */
#define LOG_BUFFER_SIZE 256

Game* newGame() {
  Game *game;
  int n;

  game = SafeCalloc (1, sizeof (Game));

  game->board = NULL;
  game->rng = newRNG();

  game->ticksPerSecond = DefaultTicksPerSecond;

  game->toolByName = newStringMap (AbortCopyFunction, deleteTool, printTool);
  game->toolOrder = newList (AbortCopyFunction, NullDestroyFunction, printTool);
  game->selectedTool = NULL;
  game->toolActive = 0;

  game->writeProtectWatcher = newCellWatcher (writeProtectIntercept, (void*) game, NULL, NullDestroyFunction);

  for (n = 0; n < ConsoleLines; ++n)
    game->consoleText[n] = NULL;
  game->consoleLastLineIndex = ConsoleLines - 1;

  return game;
}

void deleteGame (Game *game) {
  int n;
  for (n = 0; n < ConsoleLines; ++n)
    if (game->consoleText[n])
      StringDelete (game->consoleText[n]);
  deleteStringMap (game->toolByName);
  deleteList (game->toolOrder);
  deleteCellWatcher (game->writeProtectWatcher);
  deleteRNG (game->rng);
  deleteBoard (game->board);
  SafeFree (game);
}

void addToolToGame (Game *game, Tool *tool) {
  (void) StringMapInsert (game->toolByName, tool->name, tool);
  (void) ListInsertBefore (game->toolOrder, NULL, tool);
}

void gameStart (Game *game) {
  /* called once at start of game */
}

void gameQuit (Game *game) {
  /* called once at end of game */
}

void gameLoop (Game *game, double targetTicks, double maxFractionOfTimeInterval, int64_Microticks *actualMicroticks_ret, double *actualTicks_ret, int *actualUpdates, double *evolveTime) {
  double maxUpdateTimeInSeconds;
  int64_Microticks targetMicroticks;

  maxUpdateTimeInSeconds = maxFractionOfTimeInterval * targetTicks / game->ticksPerSecond;
  targetMicroticks = FloatToIntMillionths (targetTicks);

  innerGameLoop (game, targetMicroticks, maxUpdateTimeInSeconds, actualMicroticks_ret, actualTicks_ret, actualUpdates, evolveTime);
}

void innerGameLoop (Game *game, int64_Microticks targetMicroticks, double maxUpdateTimeInSeconds, int64_Microticks *actualMicroticks_ret, double *actualTicks_ret, int *actualUpdates, double *evolveTime) {
  double actualTicks;
  int64_Microticks actualMicroticks;

  evolveBoard (game->board, targetMicroticks, maxUpdateTimeInSeconds, &actualMicroticks, actualUpdates, evolveTime);
  actualTicks = IntMillionthsToFloat (actualMicroticks);

  useTools (game, actualTicks);
  updateBalloons (game->board, actualTicks / game->ticksPerSecond);

  if (actualMicroticks_ret)
    *actualMicroticks_ret = actualMicroticks;

  if (actualTicks_ret)
    *actualTicks_ret = actualTicks;
}

void useTools (Game *game, double duration) {
  RBNode *node;
  Tool *tool;
  Stack *enumResult;
  enumResult = RBTreeEnumerate (game->toolByName, NULL, NULL);	  
  while ((node = (RBNode*) StackPop (enumResult))) {
    tool = (Tool*) node->value;
    if (tool == game->selectedTool && game->toolActive) {
      useTool (tool, game, game->toolPos.x, game->toolPos.y, game->lastToolPos.x, game->lastToolPos.y, duration);
      game->lastToolPos = game->toolPos;
    } else
      rechargeTool (tool, duration);
  }
  deleteStack (enumResult);
}

int numberOfToolsVisible (Game *game) {
  ListNode *toolNode;
  int nTools;
  nTools = 0;
  for (toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next)
    ++nTools;
  return nTools;
}

State writeProtectIntercept (CellWatcher *watcher, Board *board, int x, int y, int z, State state) {
  return readBoardStateUnguarded(board,x,y,z);
}

void printToGameConsole (Game *game, char *text, PaletteIndex color, double size) {
  char *copy, *ptr, *start, newLine, atEnd;
  copy = (char*) StringCopy (text);
  ptr = start = copy;
  do {
    newLine = (*ptr == '\n');
    atEnd = (*ptr == '\0');
    if (atEnd || newLine) {
      *ptr = '\0';
      game->consoleLastLineIndex = (game->consoleLastLineIndex + 1) % ConsoleLines;
      if (game->consoleText[game->consoleLastLineIndex])
	StringDelete (game->consoleText[game->consoleLastLineIndex]);
      game->consoleText[game->consoleLastLineIndex] = (char*) StringCopy (start);
      game->consoleColor[game->consoleLastLineIndex] = color;
      game->consoleSize[game->consoleLastLineIndex] = size;
      start = ptr + 1;
    }
    ++ptr;
  } while (!atEnd);
  StringDelete (copy);
}
