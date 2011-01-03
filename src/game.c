#include <stdio.h>

#include "game.h"
#include "notify.h"
#include "tool.h"

Game* newGame() {
  Game *game;
  game = SafeMalloc (sizeof (Game));
  game->board = NULL;
  game->gameState = GameOn;
  game->updatesPerSecond = DefaultUpdatesPerSecond;
  game->goalTestsPerSecond = DefaultGoalTestsPerSecond;
  game->lastGoalTestTime = clock();
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
  game->goal = NULL;
  game->charger = newList (AbortCopyFunction, deleteToolCharger, NullPrintFunction);
  game->writeProtectWatcher = newCellWatcher (writeProtectIntercept, (void*) NULL, NullDestroyFunction);
  return game;
}

void deleteGame (Game *game) {
  deleteStringMap (game->toolByName);
  deleteCellWatcher (game->theExit.watcher);
  deleteCellWatcher (game->writeProtectWatcher);
  deleteList (game->charger);
  if (game->goal)
    deleteGoal (game->goal);
  SafeFree (game);
}

void gameLoop (Game *game, double targetUpdatesPerCell, double maxFractionOfTimeInterval, double *actualUpdatesPerCell_ret, int *actualUpdates, double *evolveTime) {
  double maxUpdateTimeInSeconds, actualUpdatesPerCell;
  maxUpdateTimeInSeconds = maxFractionOfTimeInterval * targetUpdatesPerCell / game->updatesPerSecond;

  evolveBoard (game->board, targetUpdatesPerCell, maxUpdateTimeInSeconds, &actualUpdatesPerCell, actualUpdates, evolveTime);
  if (game->gameState == GameOn || game->gameState == GameWon)   /* tools working? */
    useTools (game, actualUpdatesPerCell);
  makeEntrances (game);
  updateGameState (game);

  if (actualUpdatesPerCell_ret)
    *actualUpdatesPerCell_ret = actualUpdatesPerCell;
}

void useTools (Game *game, double duration) {
  RBNode *node;
  Tool *tool;
  Stack *enumResult;
  enumResult = RBTreeEnumerate (game->toolByName, NULL, NULL);	  
  while ((node = (RBNode*) StackPop (enumResult))) {
    tool = (Tool*) node->value;
    if (tool == game->selectedTool && game->toolActive)
      useTool (tool, game->board, game->toolPos.x, game->toolPos.y, duration);
    else
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

void updateGameState (Game *game) {
  clock_t now;

  /* check the clock - time for a goal test? */
  now = clock();
  if ((now - game->lastGoalTestTime) / CLOCKS_PER_SEC < 1. / game->goalTestsPerSecond)
    return;
  game->lastGoalTestTime = now;

  /* delegate game logic to Goal */
  if (game->gameState == GameOn && game->goal != NULL)
    (void) testGoalMet (game->goal, game->board);
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
    charger->tool->reserve = charger->tool->maxReserve;
    printf ("Recharged tool %s\n", charger->tool->name);
  }
  unregisterCellWatcher (board, watcher);
  return state;
}

State writeProtectIntercept (CellWatcher *watcher, Board *board, int x, int y, State state) {
  return readBoardStateUnguarded(board,x,y);
}
