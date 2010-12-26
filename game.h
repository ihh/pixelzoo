#ifndef GAME_INCLUDED
#define GAME_INCLUDED

#include "board.h"
#include "tool.h"
#include "stringmap.h"
#include "xymap.h"

typedef struct ToolCharger ToolCharger;

/* state of play */
typedef struct Game {
  /* board */
  Board *board;
  double updatesPerSecond;  /* rate at which to run the Board */
  enum GameState { GameOn, GameWon, GameLost, GameQuit } gameState;

  /* toolbox */
  List *allTools;     /* all Tool's, including empty/locked */
  Tool *selectedTool;
  XYCoord toolPos;
  int toolActive;

  /* entrance */
  XYCoord entrancePos;
  State entryState;
  int entrants;  /* number of entryState's remaining to place at entrancePos */
  double entranceRate;

  /* exit */
  Type exitType;  /* if this type's Particle count reaches zero, and entrants==0, then game is lost */
  int exitsToWin;  /* number of exitType's that must still exit the board; if this reaches zero, the game is won */
  CellWatcher *exitPortalWatcher;  /* (Game*) context */

  /* time limit */
  double timeLimit;  /* when board->updatesPerCell exceeds this, game is lost */

  /* power-ups */
  List *charger;  /* all ToolCharger's */

} Game;

/* Game methods */
Game* newGame();
void deleteGame (Game *game);
void updateGameState (Game *game);  /* tests win/lose conditions */

/* Two types of CellWatcher: ExitPortal and ToolCharger */
State exitPortalIntercept (CellWatcher *watcher, Board *board, int x, int y, State state);
State toolChargerIntercept (CellWatcher *watcher, Board *board, int x, int y, State state);

struct ToolCharger {
  Type overwriteType;  /* cell must be overwritten with this Type to get Tool bonus */
  Tool *tool;
};

typedef Game* ExitPortalContext;
typedef ToolCharger* ToolChargerContext;

/*
  Game threads (all on timers):
  Judge thread: test win/lose conditions, end game or sleep
  Evolve thread: if not paused, use current selected tool (if active), recharge inactive tools, evolve board, recalculate overload, sleep
  Redraw thread: redraw board, sleep

  UI events:
  Key press (or toolbar touch): select current tool
  Mouse down (or board touch): set current tool active flag
  Mouse move (or board touch): set current tool x, y
  Mouse up (or board touch release): clear tool active flag
 */


#endif /* GAME_INCLUDED */
