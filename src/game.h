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
#define DefaultOverloadCreep      1.1
#define DefaultMinOverload        .001

/* console */
#define ConsoleLines 100

/* power-up */
typedef struct GoalTrigger GoalTrigger;

/* exit portal */
typedef struct ExitPortal {
  enum PortalState { PortalWaiting,    /* the startgame: exit portal closed, i.e. ignoring incoming Particle's. Player must meet openGoal */
		     PortalCounting,   /* the endgame: exit portal is "open" and counting incoming Particle's */
		     PortalUnlocked,   /* the "win" outcome: exit portal count reached */
		     PortalDestroyed   /* the "fail" outcome: aliveGoal not met */
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
  double lastGoalTestTime;     /* measured in "Board time", i.e. updates/cell/second */
  double boardMinOverload;     /* minimum value of board overload threshold */
  double boardOverloadCreep;   /* factor by which board overload threshold is multiplied every update that the board is NOT overloaded */

  /* toolbox */
  StringMap *toolByName;     /* all Tool's, including empty/locked; this is the owning container for Tool's */
  List *toolOrder;           /* this does not own Tool's, just specifies what order they should be displayed in */
  Tool *selectedTool;
  XYCoord toolPos, lastToolPos;
  int toolActive;

  /* Game logic */
  /* game state */
  enum GameState { GameOn,       /* board is evolving, player can use tools */
		   GameWon,      /* board is evolving, player can use tools, they've won (exit portal opened, etc) */
		   GameLost,     /* board is evolving, player can't use tools because they've lost (the time limit has expired, etc) */
		   GamePaused,   /* board not evolving, player can't use tools, can return to GameOn state (currently unimplemented) */
		   GameQuit      /* game over, no way out of this state */
  } gameState;

  /* main Goal */
  Goal *goal;    /* results of testing this Goal are discarded; use PseudoGoal's to drive game state */

  /* exit portal */
  ExitPortal theExit;

  /* power-ups, etc. */
  List *trigger;  /* all GoalTrigger's */

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
void gameStart (Game *game);
void gameLoop (Game *game, double targetUpdatesPerCell, double maxFractionOfTimeInterval, double *actualUpdatesPerCell, int *actualUpdates, double *evolveTime);

#define gameRunning(GAME_PTR) ((GAME_PTR)->gameState == GameOn || (GAME_PTR)->gameState == GameWon || (GAME_PTR)->gameState == GameLost)
#define quitGame(GAME_PTR) { (GAME_PTR)->gameState = GameQuit; }

void printToGameConsole (Game *game, char *text, PaletteIndex color, double size);   /* copies text */

/* helpers */
void useTools (Game *game, double duration);  /* duration is measured in board time, i.e. updates per cell */
void testGameGoal (Game *game, int forceTest);   /* game->goal will only be tested every (1/goalTestsPerSecond) secs, unless forceTest is true */

int numberOfToolsVisible (Game *game);

/* builders */
void addToolToGame (Game *game, Tool *tool);

/* Types of CellWatcher: ExitPortal, GoalTrigger and WriteProtect */
State exitPortalIntercept (CellWatcher *watcher, Board *board, int x, int y, State state);
State writeProtectIntercept (CellWatcher *watcher, Board *board, int x, int y, State state);
State goalTriggerIntercept (CellWatcher *watcher, Board *board, int x, int y, State state);

struct GoalTrigger {
  CellWatcher *watcher;
  State overwriteType;
  Goal *goal;
};

GoalTrigger* newGoalTrigger (Game *game, Goal *goal);
void deleteGoalTrigger (void* trigger);

typedef Game* ExitPortalContext;
typedef GoalTrigger* GoalTriggerContext;

#endif /* GAME_INCLUDED */
