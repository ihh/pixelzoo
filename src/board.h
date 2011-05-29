#ifndef BOARD_INCLUDED
#define BOARD_INCLUDED

#include "rule.h"
#include "particle.h"
#include "bintree.h"
#include "move.h"
#include "util.h"
#include "vector.h"
#include "stringmap.h"

typedef struct CellWatcher CellWatcher;

typedef struct Board {
  Particle** byType;  /* Type t; byType[t] */
  StringMap* subRule;  /* subroutine ParticleRule's */
  int size;  /* board is a square, this is the length of each side in cells */
  State *cell, *sync;   /* cell[boardIndex(size,x,y)] is the current state at (x,y); sync[boardIndex(size,x,y)] is the state pending the next board synchronization */
  unsigned char *syncWrite; /* syncWrite[boardIndex(size,x,y)] is true if sync[boardIndex(size,x,y)] should be written to cell[boardIndex(size,x,y)] at next board sync */
  CellWatcher **watcher;  /* notify[boardIndex(size,x,y)] is pointer to CellWatcher object that intercepts & potentially modifies writes to cell (x,y) */
  BinTree *asyncBin, *syncBin, *syncUpdateBin;  /* asyncBin = stochastic update rates AND queue, syncBin = sync update rates, syncUpdateBin = sync update queue */
  int syncParticles, lastSyncParticles;  /* number of synchronous particles on the board now, and after last board sync */
  Palette palette;  /* cell color scheme used by this Board */
  int64_Microticks microticks;  /* time elapsed on this board, in units of (expected updates per cell / 2^{20}) */
  char sampledNextAsyncEventTime, sampledNextSyncEventTime;  /* if true, then the next async/sync event time has been predetermined by a call to the random number generator */
  int64_Microticks microticksAtNextAsyncEvent, microticksAtNextSyncEvent;  /* time of upcoming async/sync events */
  int64_Microticks microticksAtNextBoardSync;  /* time of next board sync */
  signed long long int updateCount;  /* total number of updates (calls to evolveBoardCell, evolveBoardCellSync, syncBoard, or replayBoardMove) */
  int syncUpdates;  /* number of board synchronizations */
  Vector *balloon;  /* container & owner of Balloon's */
  void *game;  /* passed to rule-triggered Goal's. Set this to NULL in "playback" mode to prevent Rule's from triggering Goal's */
  RandomNumberGenerator *rng;  /* the Board's random number generator. Drives all random simulation events */
  char rngReleased;  /* if true, then the Board has no cached random samples from the random number generator: the RNG's state can be saved or restored. Call boardReleaseRandomNumbers() to set this flag */
  MoveList *moveLog, *moveQueue; /* log of past user moves, and queue of simulated upcoming user moves */
} Board;

/* public methods */
Board* newBoard (int size);
void deleteBoard (Board* board);
void addParticleToBoard (Particle* p, Board* board);  /* turns over responsibility for deleting the Particle to the Board */
PaletteIndex readBoardColor (Board* board, int x, int y);

void logBoardMoves (Board* board);
void writeBoardMove (Board* board, int x, int y, State state);
void replayBoardMove (Board* board);

void boardReleaseRandomNumbers (Board* board);   /* causes the Board to forget any random numbers (i.e. upcoming event times) that it has sampled */

void updateBalloons (Board *board, double duration);  /* duration is measured in real time, i.e. seconds */
#define addBalloon(BOARD_PTR,BALLOON_PTR,X,Y) { VectorPushBack ((BOARD_PTR)->balloon, newPlacedBalloon (BALLOON_PTR, X, Y, (1. - IntMillionthsToFloat((BOARD_PTR)->microticks % PowerOfTwoClosestToOneMillion)))); }

/* macros to access board without bounds overrun errors.
   Note: to ensure moves are logged, use writeBoardMove function, rather than writeBoardState macro.
*/
#define onBoard(BOARD_PTR,X,Y) ((X) >= 0 && (X) < (BOARD_PTR)->size && (Y) >= 0 && (Y) < (BOARD_PTR)->size)
#define readBoardState(BOARD_PTR,X,Y) (onBoard(BOARD_PTR,X,Y) ? (State) readBoardStateUnguarded(BOARD_PTR,X,Y) : (State) 0)
#define writeBoardState(BOARD_PTR,X,Y,STATE) { if (onBoard(BOARD_PTR,X,Y)) writeBoardStateUnguardedFunction(BOARD_PTR,X,Y,STATE); }
#define readBoardParticle(BOARD_PTR,X,Y) (BOARD_PTR)->byType[StateType(readBoardState(BOARD_PTR,X,Y))]
#define readBoardParticleUnguarded(BOARD_PTR,X,Y) (BOARD_PTR)->byType[StateType(readBoardStateUnguarded(BOARD_PTR,X,Y))]

/* number of cells on board */
#define boardCells(BOARD_PTR) (((double) (BOARD_PTR)->size) * ((double) (BOARD_PTR)->size))

/* board firing rate = mean rate at which rules are firing. ranges from 0 (empty) to 1 (full) */
#define boardFiringRate(BOARD_PTR) (boardAsyncFiringRate(BOARD_PTR) + boardSyncFiringRate(BOARD_PTR))
#define boardAsyncFiringRate(BOARD_PTR) (topBinRate((BOARD_PTR)->asyncBin) / boardCells(BOARD_PTR))
#define boardSyncFiringRate(BOARD_PTR) (topBinRate((BOARD_PTR)->syncBin) / boardCells(BOARD_PTR))
#define boardAsyncParticles(BOARD_PTR) (boardCells(BOARD_PTR) - (BOARD_PTR)->syncParticles)

/* evolveBoard
   attempts to update each cell targetUpdatesPerCell times, terminating if this limit is reached or maxElapsedTimeInSeconds has elapsed (whichever occurs first).
   return values:
   *elapsedMicroticks_ret = elapsed time on board clock
   *cellUpdates_ret = the actual number of cells updated
   *elapsedTimeInSeconds_ret = the actual elapsed time in seconds
*/
void evolveBoard (Board* board, int64_Microticks targetElapsedMicroticks, double maxElapsedTimeInSeconds, int64_Microticks *elapsedMicroticks_ret, int *cellUpdates_ret, double *elapsedTimeInSeconds_ret);


/* Board read accessors.
   These "unguarded" methods do not check for off-board co-ordinates. Use readBoardState macro instead.
*/

/* Board read accessor for asynchronous updates.
 */
State readBoardStateUnguardedFunction (Board* board, int x, int y);

/* Board read accessor for synchronous updates.
 */
State readSyncBoardStateUnguardedFunction (Board* board, int x, int y);

/* Board write accessors.
   These "unguarded" methods do not check for off-board co-ordinates, or log the move. Use writeBoardMove function instead.
*/

/* Board write accessor for asynchronous updates.
 */
void writeBoardStateUnguardedFunction (Board* board, int x, int y, State state);

/* Board write accessor for synchronous updates.
 */
void writeSyncBoardStateUnguardedFunction (Board* board, int x, int y, State state);

/* Dummy Board write accessor
 */
void dummyWriteBoardStateFunction (Board* board, int x, int y, State state);


/* Private helper methods & macros */

/* private board index conversion macros */
#define boardIndex(SIZE,X,Y) ((X) + (SIZE) * (Y))
#define boardIndexToX(SIZE,IDX) ((IDX) % (SIZE))
#define boardIndexToY(SIZE,IDX) ((int) ((IDX) / (SIZE)))

/* private board read macros */
#define readBoardStateUnguarded(BOARD_PTR,X,Y) (BOARD_PTR)->cell[boardIndex((BOARD_PTR)->size,X,Y)]
#define readSyncBoardStateUnguarded(BOARD_PTR,X,Y) (BOARD_PTR)->sync[boardIndex((BOARD_PTR)->size,X,Y)]

/* Function pointers for board read & write.
 */
typedef State (*BoardReadFunction) (Board*, int, int);
typedef void (*BoardWriteFunction) (Board*, int, int, State);

/* Other helper methods */
void attemptRule (Particle *ruleOwner, ParticleRule *rule, Board *board, int x, int y, BoardReadFunction readUnguarded, BoardWriteFunction writeUnguarded);

void evolveBoardCell (Board *board, int x, int y);
void evolveBoardCellSync (Board *board, int x, int y);

void freezeBoard (Board* board);
void syncBoard (Board* board);

#endif /* BOARD_INCLUDED */
