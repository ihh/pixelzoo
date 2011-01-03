#include <stdio.h>

#include "game.h"
#include "notify.h"
#include "tool.h"

Game* newGame() {
  Game *game;
  game = SafeMalloc (sizeof (Game));
  game->board = NULL;
  game->updatesPerSecond = DefaultUpdatesPerSecond;
  game->gameState = GameOn;
  game->toolByName = newStringMap (AbortCopyFunction, deleteTool, printTool);
  game->selectedTool = NULL;
  game->toolActive = 0;
  game->theEntrance.total = game->theEntrance.soFar = 0;
  game->theEntrance.pos.x = game->theEntrance.pos.y = 0;
  game->theEntrance.state = EmptyState;
  game->theEntrance.rate = 1.;
  game->theExit.portalState = PortalWaiting;
  game->theExit.type = EmptyType;
  game->theExit.toWin = game->theExit.soFar = 0;
  game->theExit.watcher = newCellWatcher (exitPortalIntercept, (void*) game, NullDestroyFunction);
  game->theExit.liveGoal = game->theExit.openGoal = NULL;
  game->timeLimit = 0.;
  game->charger = newList (AbortCopyFunction, deleteToolCharger, NullPrintFunction);
  game->writeProtectWatcher = newCellWatcher (writeProtectIntercept, (void*) NULL, NullDestroyFunction);
  return game;
}

void deleteGame (Game *game) {
  deleteStringMap (game->toolByName);
  deleteCellWatcher (game->theExit.watcher);
  deleteCellWatcher (game->writeProtectWatcher);
  deleteList (game->charger);
  if (game->theExit.liveGoal)
    deleteGoal (game->theExit.liveGoal);
  if (game->theExit.openGoal)
    deleteGoal (game->theExit.openGoal);
  SafeFree (game);
}

void gameLoop (Game *game, double targetUpdatesPerCell, double maxFractionOfTimeInterval, double *actualUpdatesPerCell_ret, int *actualUpdates, double *evolveTime) {
  double maxUpdateTimeInSeconds, actualUpdatesPerCell;
  maxUpdateTimeInSeconds = maxFractionOfTimeInterval * targetUpdatesPerCell / game->updatesPerSecond;

  evolveBoard (game->board, targetUpdatesPerCell, maxUpdateTimeInSeconds, &actualUpdatesPerCell, actualUpdates, evolveTime);
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
  /* define macros for the transitions between states */
#define GameOpeningComplete (game->theExit.openGoal == NULL || testGoalMet (game->theExit.openGoal, game->board))
#define GameExitComplete (game->theExit.soFar >= game->theExit.toWin)
#define GameNotAlive (game->theExit.liveGoal != NULL && !testGoalMet (game->theExit.liveGoal, game->board))
#define GameOutOfTime (game->timeLimit > 0. && (game->board->updatesPerCell / game->updatesPerSecond) >= game->timeLimit)

  /* switch on current state */
  if (game->gameState == GameOn) {
    if (GameOutOfTime) {
      printf ("Out of time - you lose!\n");
      game->gameState = GameTimeUp;

    } else {
      switch (game->theExit.portalState) {

      case PortalWaiting:
	if (GameOpeningComplete) {
	  printf ("The exit portal has opened: the evacuation can begin...\n");
	  game->theExit.portalState = PortalCounting;
	} else if (GameNotAlive) {
	  printf ("Population extinct - you lose!\n");
	  game->theExit.portalState = PortalDestroyed;
	}
	break;

      case PortalCounting:
	if (GameExitComplete) {
	  printf ("Successful evacuation - you win! You may continue to the next level.\n");
	  game->theExit.portalState = PortalUnlocked;
	} else if (GameNotAlive) {
	  printf ("Population extinct - you lose!\n");
	  game->theExit.portalState = PortalDestroyed;
	}

	/* all other states are final */
      default:
	break;
      }
    }
  }
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
      if (game->theExit.soFar <= game->theExit.toWin)
	printf ("Evacuated %d %s%s, need %d to win\n", game->theExit.soFar, game->board->byType[game->theExit.type]->name, game->theExit.soFar > 1 ? "s" : "", game->theExit.toWin);
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
