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
  game->totalEntrants = game->entrantsSoFar = 0;
  game->entrancePos.x = game->entrancePos.y = 0;
  game->entryState = EmptyState;
  game->entranceRate = 1.;
  game->exitType = EmptyType;
  game->exitsToWin = game->exitsSoFar = 0;
  game->exitPortalWatcher = newCellWatcher (exitPortalIntercept, (void*) game, NullDestroyFunction);
  game->timeLimit = 0.;
  game->charger = newList (AbortCopyFunction, deleteToolCharger, NullPrintFunction);
  return game;
}

void deleteGame (Game *game) {
  deleteList (game->allTools);
  deleteCellWatcher (game->exitPortalWatcher);
  deleteList (game->charger);
  SafeFree (game);
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
  if (game->entrantsSoFar < game->totalEntrants) {
    entrancePeriod = 1. / game->entranceRate;
    nextEntranceTime = (double) game->entrantsSoFar * entrancePeriod;
    if (game->board->updatesPerCell > nextEntranceTime
	&& readBoardState(game->board,game->entrancePos.x,game->entrancePos.y) != game->entryState) {
      writeBoardState (game->board,game->entrancePos.x,game->entrancePos.y,game->entryState);
      ++game->entrantsSoFar;
    }
  }
}

void updateGameState (Game *game) {
  /* test win/lose conditions */
  if (game->gameState == GameOn) {
    if (game->exitsToWin > 0 && game->exitsSoFar >= game->exitsToWin) {
      game->gameState = GameWon;
      printf ("Successful evacuation - you win!\n");
    } else if (game->exitType != EmptyType && game->board->byType[game->exitType]->count == 0 && game->entrantsSoFar >= game->totalEntrants) {
      printf ("Population extinct - you lose!\n");
      game->gameState = GameLost;
    } else if (game->timeLimit > 0. && (game->board->updatesPerCell / game->updatesPerSecond) >= game->timeLimit) {
      printf ("Out of time - you lose!\n");
      game->gameState = GameLost;
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
  if (StateType(state) == game->exitType) {
    ++game->exitsSoFar;
    printf ("Evacuated %d %s(s), need %d to win\n", game->exitsSoFar, game->board->byType[game->exitType]->name, game->exitsToWin);
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
