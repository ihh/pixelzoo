#ifndef GAME_INCLUDED
#define GAME_INCLUDED

#include <time.h>
#include "board.h"
#include "tool.h"
#include "stringmap.h"
#include "xymap.h"

/* default rates */
#define DefaultTicksPerSecond     100

/* console */
#define ConsoleLines 100

/* state of play */
typedef struct Game {
  /* board */
  Board *board;

  /* Non-critical timing variables (i.e. used only by solo-player mode or UI).
     Being non-critical to distributed consensus, these can be floating-point variables and subject to local FPU peculiarities.
  */
  double ticksPerSecond;     /* rate at which to run the Board, in Ticks/second. DO NOT MODIFY WHILE RUNNING - conversions to "Board time" depend on this being constant! */

  /* Non-reproducible random numbers */
  RandomNumberGenerator *rng;

  /* toolbox */
  StringMap *toolByName;     /* all Tool's, including empty/locked; this is the owning container for Tool's */
  List *toolOrder;           /* this does not own Tool's, just specifies what order they should be displayed in */
  Tool *selectedTool;
  XYCoord toolPos, lastToolPos;
  int toolActive;

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
void gameQuit (Game *game);
void gameLoop (Game *game, double targetTicks, double maxFractionOfTimeInterval, int64_Microticks *actualMicroticks_ret, double *actualTicks_ret, int *actualUpdates, double *evolveTime);
void innerGameLoop (Game *game, int64_Microticks targetMicroticks, double maxUpdateTimeInSeconds, int64_Microticks *actualMicroticks_ret, double *actualTicks_ret, int *actualUpdates, double *evolveTime);

void printToGameConsole (Game *game, char *text, PaletteIndex color, double size);   /* copies text */

/* helpers */
void useTools (Game *game, double duration);  /* duration is measured in board time, i.e. updates per cell */

int numberOfToolsVisible (Game *game);

/* builders */
void addToolToGame (Game *game, Tool *tool);

/* Types of CellWatcher: WriteProtect */
State writeProtectIntercept (CellWatcher *watcher, Board *board, int x, int y, int z, State state);

#endif /* GAME_INCLUDED */
