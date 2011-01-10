#include <stdio.h>
#include <math.h>

#include "game.h"
#include "notify.h"
#include "tool.h"

Game* newGame() {
  Game *game;
  int n;

  game = SafeMalloc (sizeof (Game));

  game->board = NULL;

  game->updatesPerSecond = DefaultUpdatesPerSecond;
  game->goalTestsPerSecond = DefaultGoalTestsPerSecond;
  game->boardOverloadCreep = DefaultOverloadCreep;
  game->boardMinOverload = DefaultMinOverload;
  game->lastGoalTestTime = 0;

  game->toolByName = newStringMap (AbortCopyFunction, deleteTool, printTool);
  game->selectedTool = NULL;
  game->toolActive = 0;

  game->theEntrance.total = game->theEntrance.soFar = 0;
  game->theEntrance.pos.x = game->theEntrance.pos.y = 0;
  game->theEntrance.state = EmptyState;
  game->theEntrance.rate = 1.;

  game->theExit.portalState = PortalWaiting;
  game->theExit.type = EmptyType;
  game->theExit.soFar = 0;
  game->theExit.watcher = newCellWatcher (exitPortalIntercept, (void*) game, NullDestroyFunction);

  game->gameState = GameOn;
  game->goal = NULL;

  game->charger = newList (AbortCopyFunction, deleteToolCharger, NullPrintFunction);
  game->writeProtectWatcher = newCellWatcher (writeProtectIntercept, (void*) NULL, NullDestroyFunction);

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
  deleteCellWatcher (game->theExit.watcher);
  deleteCellWatcher (game->writeProtectWatcher);
  deleteList (game->charger);
  if (game->goal)
    deleteGoal (game->goal);
  SafeFree (game);
}

void gameLoop (Game *game, double targetUpdatesPerCell, double maxFractionOfTimeInterval, double *actualUpdatesPerCell_ret, int *actualUpdates, double *evolveTime) {
  double maxUpdateTimeInSeconds, actualUpdatesPerCell, currentOverloadThreshold, newOverloadThreshold, overloadScaleFactor;
  maxUpdateTimeInSeconds = maxFractionOfTimeInterval * targetUpdatesPerCell / game->updatesPerSecond;

  evolveBoard (game->board, targetUpdatesPerCell, maxUpdateTimeInSeconds, &actualUpdatesPerCell, actualUpdates, evolveTime);
  if (game->gameState == GameOn || game->gameState == GameWon)   /* tools working? */
    useTools (game, actualUpdatesPerCell);
  makeEntrances (game);
  testGameGoal (game);
  updateBalloons (game->board, actualUpdatesPerCell / game->updatesPerSecond);

  if (actualUpdatesPerCell_ret)
    *actualUpdatesPerCell_ret = actualUpdatesPerCell;

  /* update overload threshold */
  currentOverloadThreshold = boardTopOverloadThreshold(game->board);
  overloadScaleFactor = pow (game->boardOverloadCreep, targetUpdatesPerCell);
  if (actualUpdatesPerCell < targetUpdatesPerCell) {
    newOverloadThreshold = boardFiringRate(game->board) / overloadScaleFactor;
    boardSetOverloadThreshold (game->board, MAX (game->boardMinOverload, newOverloadThreshold));
  } else if (currentOverloadThreshold < 1.) {
    newOverloadThreshold = currentOverloadThreshold * overloadScaleFactor;
    boardSetOverloadThreshold (game->board, MIN (1., newOverloadThreshold));
  }
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

void makeEntrances (Game *game) {
  double entrancePeriod, nextEntranceTime;
  if (game->theEntrance.soFar < game->theEntrance.total) {
    entrancePeriod = 1. / game->theEntrance.rate;
    nextEntranceTime = (double) game->theEntrance.soFar * entrancePeriod;
    if (game->board->updatesPerCell > nextEntranceTime
	&& readBoardState(game->board,game->theEntrance.pos.x,game->theEntrance.pos.y) != game->theEntrance.state) {
      writeBoardState (game->board,game->theEntrance.pos.x,game->theEntrance.pos.y,game->theEntrance.state);
      ++game->theEntrance.soFar;
    }
  }
}

void testGameGoal (Game *game) {
  double elapsedBoardTime;

  /* check the clock - time for a goal test? */
  elapsedBoardTime = game->board->updatesPerCell - game->lastGoalTestTime;
  if (elapsedBoardTime < game->updatesPerSecond / game->goalTestsPerSecond)
    return;
  game->lastGoalTestTime = game->board->updatesPerCell;

  /* delegate game logic to Goal */
  if (game->gameState == GameOn && game->goal != NULL)
    (void) testGoalMet (game->goal, (void*) game);
}

int numberOfToolsVisible (Game *game) {
  Stack *toolStack;
  StringMapNode *toolNode;
  int nTools;
  nTools = 0;
  toolStack = RBTreeEnumerate (game->toolByName, NULL, NULL);
  while ((toolNode = StackPop(toolStack)) != NULL) {
    Tool *tool = toolNode->value;
    if (!tool->hidden)
      ++nTools;
  }
  return nTools;
}

ToolCharger* newToolCharger() {
  ToolCharger* charger;
  charger = SafeMalloc (sizeof (ToolCharger));
  charger->overwriteType = EmptyType;
  charger->tool = NULL;
  charger->watcher = newCellWatcher (toolChargerIntercept, (void*) charger, NullDestroyFunction);
  return charger;
}

void deleteToolCharger (void* voidCharger) {
  ToolCharger* charger;
  charger = (ToolCharger*) voidCharger;
  deleteCellWatcher (charger->watcher);
  SafeFree (charger);
}

State exitPortalIntercept (CellWatcher *watcher, Board *board, int x, int y, State state) {
  Game *game;
  game = (Game*) watcher->context;
  if (game->gameState == GameOn && game->theExit.portalState == PortalCounting) {
    if (StateType(state) == game->theExit.type) {
      ++game->theExit.soFar;
      printf ("Evacuated %d %s%s\n", game->theExit.soFar, game->board->byType[game->theExit.type]->name, game->theExit.soFar > 1 ? "s" : "");
    }
  }
  return readBoardStateUnguarded(board,x,y);
}

State toolChargerIntercept (CellWatcher *watcher, Board *board, int x, int y, State state) {
  ToolCharger *charger;
  charger = (ToolCharger*) watcher->context;
  if (StateType(state) == charger->overwriteType) {
    printf ("%s tool %s\n", charger->tool->hidden ? "Unlocked" : "Recharged", charger->tool->name);
    charger->tool->reserve = charger->tool->maxReserve;
    charger->tool->hidden = 0;
  }
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
