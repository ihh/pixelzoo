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
  game->allTools = newList (AbortCopyFunction, deleteTool, printTool);
  game->selectedTool = NULL;
  game->toolActive = 0;
  game->theEntrance.total = game->theEntrance.soFar = 0;
  game->theEntrance.pos.x = game->theEntrance.pos.y = 0;
  game->theEntrance.state = EmptyState;
  game->theEntrance.rate = 1.;
  game->theExit.type = EmptyType;
  game->theExit.toWin = game->theExit.soFar = 0;
  game->theExit.watcher = newCellWatcher (exitPortalIntercept, (void*) game, NullDestroyFunction);
  game->timeLimit = 0.;
  game->charger = newList (AbortCopyFunction, deleteToolCharger, NullPrintFunction);
  game->writeProtectWatcher = newCellWatcher (writeProtectIntercept, (void*) NULL, NullDestroyFunction);
  game->aliveGoal = game->exitOpenGoal = NULL;
  return game;
}

void deleteGame (Game *game) {
  deleteList (game->allTools);
  deleteCellWatcher (game->theExit.watcher);
  deleteCellWatcher (game->writeProtectWatcher);
  deleteList (game->charger);
  if (game->aliveGoal)
    deleteGoal (game->aliveGoal);
  if (game->exitOpenGoal)
    deleteGoal (game->exitOpenGoal);
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
  ListNode *node;
  Tool *tool;
  for (node = game->allTools->head; node != NULL; node = node->next) {
    tool = (Tool*) node->value;
    if (tool == game->selectedTool && game->toolActive)
      useTool (tool, game->board, game->toolPos.x, game->toolPos.y, duration);
    else
      rechargeTool (tool, duration);
  }
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
#define GameOpeningComplete (game->exitOpenGoal == NULL || testGoalMet (game->exitOpenGoal, game->board))
#define GameExitComplete (game->theExit.soFar >= game->theExit.toWin)
#define GameNotAlive (game->aliveGoal != NULL && !testGoalMet (game->aliveGoal, game->board))
#define GameOutOfTime (game->timeLimit > 0. && (game->board->updatesPerCell / game->updatesPerSecond) >= game->timeLimit)

  /* switch on current state */
  switch (game->gameState) {
  case GameOn:
    if (GameOpeningComplete) {
      printf ("The exit portal is now open!\n");
      game->gameState = GameExitOpen;
    } else if (GameNotAlive) {
      printf ("Population extinct - you lose!\n");
      game->gameState = GameLost;
    } else if (GameOutOfTime) {
      printf ("Out of time - you lose!\n");
      game->gameState = GameLost;
    }
    break;

 case GameExitOpen:
    if (GameExitComplete) {
      game->gameState = GameWon;
      printf ("Successful evacuation - you win! You may exit to the next level.\n");
    } else if (GameNotAlive) {
      printf ("Population extinct - you lose!\n");
      game->gameState = GameLost;
    } else if (GameOutOfTime) {
      printf ("Out of time - you lose!\n");
      game->gameState = GameLost;
    }
    break;

    /* All other states are final */
  default:
    break;
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
  switch (game->gameState) {
  case GameExitOpen:
  case GameWon:
    if (StateType(state) == game->theExit.type) {
      ++game->theExit.soFar;
      if (game->theExit.soFar <= game->theExit.toWin)
	printf ("Evacuated %d %s%s, need %d to win\n", game->theExit.soFar, game->board->byType[game->theExit.type]->name, game->theExit.soFar > 1 ? "s" : "", game->theExit.toWin);
      else if (game->theExit.soFar % game->theExit.toWin == 0)
	printf ("Evacuated %d %s%s, turned %d away\n", game->theExit.toWin, game->board->byType[game->theExit.type]->name, game->theExit.soFar > 1 ? "s" : "", game->theExit.soFar - game->theExit.toWin);
    }
    break;

  default:
    break;
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
