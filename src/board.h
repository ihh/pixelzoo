#ifndef BOARD_INCLUDED
#define BOARD_INCLUDED

#include "rule.h"
#include "particle.h"
#include "bintree.h"
#include "move.h"
#include "util.h"
#include "vector.h"
#include "stringmap.h"
#include "proto.h"

typedef struct CellWatcher CellWatcher;

typedef struct Board {
  Particle** byType;  /* Type t; byType[t] */
  ProtoTable* protoTable;
  StringMap* subRule;  /* global subroutine ParticleRule's */
  int size;  /* board is a square, this is the length of each side in cells */
  int depth;  /* number of layers */
  State *cell, *sync;   /* cell[boardIndex(size,x,y,z)] is the current state at (x,y,z); sync[boardIndex(size,x,y,z)] is the state pending the next board synchronization */
  unsigned char *syncWrite; /* syncWrite[boardIndex(size,x,y,z)] is true if sync[boardIndex(size,x,y,z)] should be written to cell[boardIndex(size,x,y,z)] at next board sync */
  CellWatcher **watcher;  /* notify[boardIndex(size,x,y,z)] is pointer to CellWatcher object that intercepts & potentially modifies writes to cell (x,y,z) */
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
  RandomNumberGenerator *rng;  /* the Board's random number generator. Drives all random simulation events */
  char rngReleased;  /* if true, then the Board has no cached random samples from the random number generator: the RNG's state can be saved or restored. Call boardReleaseRandomNumbers() to set this flag */
  MoveList *moveLog, *moveQueue; /* log of past user moves, and queue of simulated upcoming user moves */
} Board;

/* public methods */
Board* newBoard (int size, int depth);
void deleteBoard (Board* board);
void addParticleToBoard (Particle* p, Board* board);  /* turns over responsibility for deleting the Particle to the Board */
PaletteIndex readBoardColor (Board* board, int x, int y, int z);

void logBoardMoves (Board* board);
void writeBoardMove (Board* board, int x, int y, int z, State state);
void replayBoardMove (Board* board);

void boardReleaseRandomNumbers (Board* board);   /* causes the Board to forget any random numbers (i.e. upcoming event times) that it has sampled */

void updateBalloons (Board *board, double duration);  /* duration is measured in real time, i.e. seconds */
#define addBalloon(BOARD_PTR,BALLOON_PTR,X,Y) { VectorPushBack ((BOARD_PTR)->balloon, newPlacedBalloon (BALLOON_PTR, X, Y, (1. - IntMillionthsToFloat((BOARD_PTR)->microticks % PowerOfTwoClosestToOneMillion)))); }

/* macros to access board without bounds overrun errors.
   Note: to ensure moves are logged, use writeBoardMove function, rather than writeBoardState macro.
*/
#define onBoard(BOARD_PTR,X,Y,Z) ((X) >= 0 && (X) < (BOARD_PTR)->size && (Y) >= 0 && (Y) < (BOARD_PTR)->size && (Z) >= 0 && (Z) < (BOARD_PTR)->depth)
#define readBoardState(BOARD_PTR,X,Y,Z) (onBoard(BOARD_PTR,X,Y,Z) ? (State) readBoardStateUnguarded(BOARD_PTR,X,Y,Z) : (State) 0)
#define writeBoardState(BOARD_PTR,X,Y,Z,STATE) { if (onBoard(BOARD_PTR,X,Y,Z)) writeBoardStateUnguardedFunction(BOARD_PTR,X,Y,Z,STATE); }
#define readBoardParticle(BOARD_PTR,X,Y,Z) (BOARD_PTR)->byType[StateType(readBoardState(BOARD_PTR,X,Y,Z))]
#define readBoardParticleUnguarded(BOARD_PTR,X,Y,Z) (BOARD_PTR)->byType[StateType(readBoardStateUnguarded(BOARD_PTR,X,Y,Z))]

/* number of cells on board */
#define boardCells(BOARD_PTR) (((double) (BOARD_PTR)->size) * ((double) (BOARD_PTR)->size) * ((double) (BOARD_PTR)->depth))

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
State readBoardStateUnguardedFunction (Board* board, int x, int y, int z);

/* Board read accessor for synchronous updates.
 */
State readSyncBoardStateUnguardedFunction (Board* board, int x, int y, int z);

/* Board write accessors.
   These "unguarded" methods do not check for off-board co-ordinates, or log the move. Use writeBoardMove function instead.
*/

/* Board write accessor for asynchronous updates.
 */
void writeBoardStateUnguardedFunction (Board* board, int x, int y, int z, State state);

/* Board write accessor for synchronous updates.
 */
void writeSyncBoardStateUnguardedFunction (Board* board, int x, int y, int z, State state);

/* Dummy Board write accessor
 */
void dummyWriteBoardStateFunction (Board* board, int x, int y, int z, State state);


/* Private helper methods & macros */

/* private board index conversion macros */
#define boardIndex(SIZE,X,Y,Z) ((X) + (SIZE) * ((Y) + (SIZE) * (Z)))
#define boardIndexToX(SIZE,IDX) ((IDX) % (SIZE))
#define boardIndexToY(SIZE,IDX) ((int) (((IDX) / (SIZE)) % (SIZE)))
#define boardIndexToZ(SIZE,IDX) ((int) ((IDX) / ((SIZE) * (SIZE))))

/* private board read macros */
#define readBoardStateUnguarded(BOARD_PTR,X,Y,Z) (BOARD_PTR)->cell[boardIndex((BOARD_PTR)->size,X,Y,Z)]
#define readSyncBoardStateUnguarded(BOARD_PTR,X,Y,Z) (BOARD_PTR)->sync[boardIndex((BOARD_PTR)->size,X,Y,Z)]

/* Function pointers for board read & write.
 */
typedef State (*BoardReadFunction) (Board*, int, int, int);
typedef void (*BoardWriteFunction) (Board*, int, int, int, State);

/* Other helper methods */
void attemptRule (Particle *ruleOwner, ParticleRule *rule, Board *board, int x, int y, int z, BoardReadFunction readUnguarded, BoardWriteFunction writeUnguarded);

void evolveBoardCell (Board *board, int x, int y, int z);
void evolveBoardCellSync (Board *board, int x, int y, int z);

void freezeBoard (Board* board);
void syncBoard (Board* board);

#endif /* BOARD_INCLUDED */
