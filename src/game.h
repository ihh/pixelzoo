#ifndef GAME_INCLUDED
#define GAME_INCLUDED

#include <time.h>
#include "board.h"
#include "tool.h"
#include "stringmap.h"
#include "xymap.h"
#include "goal.h"

/* default rates */
#define DefaultUpdatesPerSecond   100
#define DefaultGoalTestsPerSecond 1

/* console */
#define ConsoleLines 100

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
  enum PortalState { PortalWaiting = 0,    /* the startgame: exit portal closed, i.e. ignoring incoming Particle's. Player must meet openGoal */
		     PortalCounting = 1,   /* the endgame: exit portal is "open" and counting incoming Particle's */
		     PortalUnlocked = 2,   /* the "win" outcome: exit portal count reached */
		     PortalDestroyed = 3   /* the "fail" outcome: aliveGoal not met */
  } portalState;

  Type type;
  int soFar;  /* number of type's that have exited the board */
  CellWatcher *watcher;  /* (Game*) context */
} ExitPortal;

/* state of play */
typedef struct Game {
  /* board */
  Board *board;

  /* timing */
  double updatesPerSecond;     /* rate at which to run the Board. DO NOT MODIFY WHILE RUNNING - conversions to "Board time" depend on this being constant! */
  double goalTestsPerSecond;   /* rate at which to test Goal */
  double lastGoalTestTime;     /* time is "Board time", i.e. measured in updates/cell/second */

  /* toolbox */
  StringMap *toolByName;     /* all Tool's, including empty/locked; this is the owning container for Tool's */
  Tool *selectedTool;
  XYCoord toolPos;
  int toolActive;

  /* Game logic */
  /* game state */
  enum GameState { GameOn = 0,       /* board is evolving, player can use tools */
		   GameWon = 1,      /* board is evolving, player can use tools, they've won (exit portal opened, etc) */
		   GameLost = 2,     /* board is evolving, player can't use tools because they've lost (the time limit has expired, etc) */
		   GamePaused = 3,   /* board not evolving, player can't use tools, can return to GameOn state (currently unimplemented) */
		   GameQuit = 4      /* game over, no way out of this state */
  } gameState;

  /* main Goal */
  Goal *goal;    /* results of testing this Goal are discarded; use PseudoGoal's to drive game state */

  /* entrance portal */
  EntrancePortal theEntrance;

  /* exit portal */
  ExitPortal theExit;

  /* power-ups */
  List *charger;  /* all ToolCharger's */

  /* dummy CellWatcher for write protects */
  CellWatcher *writeProtectWatcher;

  /* text output */
  /* console */
  char *consoleText[ConsoleLines];
  PaletteIndex consoleColor[ConsoleLines];
  double consoleSize[ConsoleLines];
  int consoleLastLineIndex;

} Game;

/* Game methods */
Game* newGame();
void deleteGame (Game *game);
void gameLoop (Game *game, double targetUpdatesPerCell, double maxFractionOfTimeInterval, double *actualUpdatesPerCell, int *actualUpdates, double *evolveTime);

#define gameRunning(GAME_PTR) ((GAME_PTR)->gameState == GameOn || (GAME_PTR)->gameState == GameWon || (GAME_PTR)->gameState == GameLost)
#define quitGame(GAME_PTR) { (GAME_PTR)->gameState = GameQuit; }

void printToGameConsole (Game *game, char *text, PaletteIndex color, double size);   /* copies text */

/* helpers */
void makeEntrances (Game *game);
void useTools (Game *game, double duration);  /* duration is measured in board time, i.e. updates per cell */
void testGameGoal (Game *game);

int numberOfToolsVisible (Game *game);

/* Balloons */

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

#endif /* GAME_INCLUDED */
