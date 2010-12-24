#ifndef GAME_INCLUDED
#define GAME_INCLUDED

#include "board.h"
#include "tool.h"
#include "stringmap.h"
#include "xymap.h"

/* state of play */
typedef struct Game {
  /* board */
  Board *board;

  /* toolbox */
  List *allTools;     /* all Tool's, including empty/locked */
  Tool *selectedTool;
  XYCoord toolPos;

  /* entrance */
  XYCoord entrancePos;
  State entryState;
  int entrants;  /* number of entryState's remaining to place at entrancePos */
  double entranceRate;

  /* exit */
  Type exitType;  /* if this type's Particle count reaches zero, and entrants==0, then game is lost */
  int exitsToWin;  /* number of exitType's that must still exit the board before the game is won */
  CellWatcher *exitPortalWatcher;

  /* time limit */
  double timeLimit;  /* when board->updatesPerCell exceeds this, game is lost */

  /* power-ups */
  /* TODO: write me (use toolChargerIntercept in "notify.h"; may need a new struct to specify overwrite Type, the Tool, once-only, etc) */


} Game;

/*
  Game threads (all on timers):
  Evolve thread: if not paused, evolve board, recalculate overload, sleep
  Redraw thread: redraw board, sleep
  NPC thread: use tools, test goals -> award rewards, sleep
  Tool thread: use current selected tool (if active), recharge inactive tools, sleep

  UI events:
  Key press (or toolbar touch): select current tool
  Mouse down (or board touch): set current tool active flag
  Mouse move (or board touch): set current tool x, y
  Mouse up (or board touch release): clear tool active flag
 */


#endif /* GAME_INCLUDED */
