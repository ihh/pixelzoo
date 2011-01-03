#ifndef GAME_INCLUDED
#define GAME_INCLUDED

#include "board.h"
#include "tool.h"
#include "stringmap.h"
#include "xymap.h"
#include "goal.h"

#define DefaultUpdatesPerSecond 100

/* power-up */
typedef struct ToolCharger ToolCharger;

/* entrance portal */
typedef struct EntrancePortal {
  XYCoord pos;
  State state;
  int total, soFar;  /* number of entryState's to place at entrancePos */
  double rate;
} EntrancePortal;

/* exit portal */
typedef struct ExitPortal {
  enum PortalState { PortalWaiting,    /* the startgame: exit portal closed, i.e. ignoring incoming Particle's. Player must meet openGoal */
		     PortalCounting,   /* the endgame: exit portal is "open" and counting incoming Particle's */
		     PortalUnlocked,   /* the "win" outcome: exit portal count reached */
		     PortalDestroyed   /* the "fail" outcome: aliveGoal not met */
  } portalState;

  Goal *liveGoal;  /* if this goal fails, portal's population is extinct */
  Goal *openGoal;  /* player must meet this goal for the exit to open and the portal to move into the PortalCounting state */

  Type type;
  int toWin, soFar;  /* number of type's that must exit / have exited the board */
  CellWatcher *watcher;  /* (Game*) context */
} ExitPortal;

/* state of play */
typedef struct Game {
  /* board */
  Board *board;
  double updatesPerSecond;  /* rate at which to run the Board */
  enum GameState { GameOn,       /* keep playing */
		   GameTimeUp,   /* the time limit has expired */
		   GameQuit      /* player quit */
  } gameState;

  /* toolbox */
  List *allTools;     /* all Tool's, including empty/locked */
  Tool *selectedTool;
  XYCoord toolPos;
  int toolActive;

  /* entrance */
  EntrancePortal theEntrance;

  /* exit portal */
  ExitPortal theExit;

  /* time limit */
  double timeLimit;  /* when (board->updatesPerCell / game->updatesPerSecond) exceeds this, game  */

  /* power-ups */
  List *charger;  /* all ToolCharger's */

  /* dummy CellWatcher for write protects */
  CellWatcher *writeProtectWatcher;

} Game;

/* Game methods */
Game* newGame();
void deleteGame (Game *game);
void gameLoop (Game *game, double targetUpdatesPerCell, double maxFractionOfTimeInterval, double *actualUpdatesPerCell, int *actualUpdates, double *evolveTime);

#define gameRunning(GAME_PTR) ((GAME_PTR)->gameState == GameOn)
#define quitGame(GAME_PTR) { (GAME_PTR)->gameState = GameQuit; }

/* helpers */
void makeEntrances (Game *game);
void useTools (Game *game, double duration);  /* duration is measured in board time, i.e. updates per cell */
void updateGameState (Game *game);  /* tests win/lose conditions */

/* Types of CellWatcher: ExitPortal, ToolCharger and WriteProtect */
State exitPortalIntercept (CellWatcher *watcher, Board *board, int x, int y, State state);
State toolChargerIntercept (CellWatcher *watcher, Board *board, int x, int y, State state);
State writeProtectIntercept (CellWatcher *watcher, Board *board, int x, int y, State state);

struct ToolCharger {
  CellWatcher *watcher;
  Type overwriteType;  /* cell must be overwritten with this Type to get Tool bonus */
  Tool *tool;
};

ToolCharger* newToolCharger();
void deleteToolCharger (void* charger);

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
