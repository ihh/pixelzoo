#include <stdio.h>

#include "game.h"
#include "notify.h"
#include "tool.h"

Game* newGame() {
  Game *game;
  game = SafeMalloc (sizeof (Game));
  game->board = NULL;
  game->updatesPerSecond = 100;
  game->gameState = GameOn;
  game->allTools = newList (AbortCopyFunction, deleteTool, printTool);
  game->selectedTool = NULL;
  game->toolActive = 0;
  game->totalEntrants = game->remainingEntrants = 0;
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

void updateGameState (Game *game) {
  /* test win/lose conditions */
  if (game->gameState == GameOn) {
    if (game->exitsToWin > 0 && game->exitsSoFar >= game->exitsToWin) {
      game->gameState = GameWon;
      printf ("Successful evacuation - you win!\n");
    } else if (game->exitType != EmptyType && game->board->byType[game->exitType]->count == 0 && game->remainingEntrants == 0) {
      printf ("Population extinct - you lose!\n");
      game->gameState = GameLost;
    } else if (game->timeLimit > 0. && game->board->updatesPerCell >= game->timeLimit) {
      printf ("Time out - you lose!\n");
      game->gameState = GameLost;
    }
  }
}

ToolCharger* newToolCharger() {
  ToolCharger* charger;
  charger = SafeMalloc (sizeof (ToolCharger));
  charger->overwriteType = EmptyType;
  charger->tool = NULL;
  return charger;
}

void deleteToolCharger (void* voidCharger) {
  ToolCharger* charger;
  charger = (ToolCharger*) voidCharger;
  SafeFree (charger);
}

State exitPortalIntercept (CellWatcher *watcher, Board *board, int x, int y, State state) {
  Game *game;
  game = (Game*) watcher->context;
  if (StateType(state) == game->exitType)
    ++game->exitsSoFar;
  return readBoardStateUnguarded(board,x,y);
}
