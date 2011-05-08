#include <stdio.h>
#include <math.h>

#include "game.h"
#include "notify.h"
#include "tool.h"

/* buffer size for log messages */
#define LOG_BUFFER_SIZE 256

Game* newGame() {
  Game *game;
  int n;

  game = SafeMalloc (sizeof (Game));

  game->board = NULL;

  game->ticksPerSecond = DefaultTicksPerSecond;
  game->goalTestsPerSecond = DefaultGoalTestsPerSecond;
  game->lastGoalTestTime = 0;

  game->toolByName = newStringMap (AbortCopyFunction, deleteTool, printTool);
  game->toolOrder = newList (AbortCopyFunction, NullDestroyFunction, printTool);
  game->selectedTool = NULL;
  game->toolActive = 0;

  game->theExit.portalState = PortalWaiting;
  game->theExit.type = EmptyType;
  game->theExit.soFar = 0;
  game->theExit.watcher = newCellWatcher (exitPortalIntercept, (void*) game, NULL, NullDestroyFunction);

  game->gameState = GameOn;
  game->goal = NULL;

  game->trigger = newList (AbortCopyFunction, deleteGoalTrigger, NullPrintFunction);
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
  deleteCellWatcher (game->theExit.watcher);
  deleteCellWatcher (game->writeProtectWatcher);
  deleteList (game->trigger);
  if (game->goal)
    deleteGoal (game->goal);
  SafeFree (game);
}

void addToolToGame (Game *game, Tool *tool) {
  (void) StringMapInsert (game->toolByName, tool->name, tool);
  (void) ListInsertBefore (game->toolOrder, NULL, tool);
}

void gameStart (Game *game) {
  testGameGoal (game, 1);
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

  if (game->gameState == GameOn || game->gameState == GameWon)   /* tools working? */
    useTools (game, actualTicks);
  testGameGoal (game, 0);
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
      useTool (tool, game->board, game->toolPos.x, game->toolPos.y, game->lastToolPos.x, game->lastToolPos.y, duration);
      game->lastToolPos = game->toolPos;
    } else
      rechargeTool (tool, duration);
  }
  deleteStack (enumResult);
}

void testGameGoal (Game *game, int forceTest) {
  double currentBoardTime, elapsedBoardTime;

  /* check the clock - time for a goal test? */
  currentBoardTime = IntMillionthsToFloat (game->board->microticks);
  elapsedBoardTime = currentBoardTime - game->lastGoalTestTime;
  if (!forceTest && elapsedBoardTime < game->ticksPerSecond / game->goalTestsPerSecond)
    return;
  game->lastGoalTestTime = currentBoardTime;

  /* delegate game logic to Goal */
  if (game->gameState == GameOn && game->goal != NULL)
    (void) testGoalMet (game->goal, (void*) game);
}

int numberOfToolsVisible (Game *game) {
  ListNode *toolNode;
  int nTools;
  nTools = 0;
  for (toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next) {
    Tool *tool = toolNode->value;
    if (!tool->hidden)
      ++nTools;
  }
  return nTools;
}

GoalTrigger* newGoalTrigger (Game *game, Goal *goal) {
  GoalTrigger* trigger;
  trigger = SafeMalloc (sizeof (GoalTrigger));
  trigger->overwriteType = EmptyType;
  trigger->goal = goal;
  trigger->watcher = newCellWatcher (goalTriggerIntercept, (void*) game, (void*) trigger, NullDestroyFunction);
  return trigger;
}

void deleteGoalTrigger (void* voidTrigger) {
  GoalTrigger* trigger;
  trigger = (GoalTrigger*) voidTrigger;
  deleteCellWatcher (trigger->watcher);
  SafeFree (trigger);
}

State exitPortalIntercept (CellWatcher *watcher, Board *board, int x, int y, State state) {
  Game *game;
  char exitPortalLogBuf[LOG_BUFFER_SIZE];
  game = (Game*) watcher->game;
  if (game->gameState == GameOn && game->theExit.portalState == PortalCounting) {
    if (StateType(state) == game->theExit.type) {
      ++game->theExit.soFar;
      sprintf (exitPortalLogBuf, "%d %s%s reached the exit!", game->theExit.soFar, game->board->byType[game->theExit.type]->name, game->theExit.soFar > 1 ? "s have" : " has");
      printToGameConsole (game, exitPortalLogBuf, PaletteWhite, 1.);
      printf ("%s\n", exitPortalLogBuf);
    }
  }
  return readBoardStateUnguarded(board,x,y);
}

State goalTriggerIntercept (CellWatcher *watcher, Board *board, int x, int y, State state) {
  GoalTrigger *trigger;
  Game *game;
  game = (Game*) watcher->game;
  trigger = (GoalTrigger*) watcher->context;
  if (StateType(state) == trigger->overwriteType)
    (void) testGoalMet (trigger->goal, game);
  unregisterCellWatcher (board, watcher);
  return state;
}

State writeProtectIntercept (CellWatcher *watcher, Board *board, int x, int y, State state) {
  return readBoardStateUnguarded(board,x,y);
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
