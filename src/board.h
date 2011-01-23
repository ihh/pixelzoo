#ifndef BOARD_INCLUDED
#define BOARD_INCLUDED

#include "rule.h"
#include "particle.h"
#include "bintree.h"
#include "util.h"
#include "vector.h"

typedef struct CellWatcher CellWatcher;

typedef struct Board {
  Particle** byType;  /* Type t; byType[t] */
  int size;  /* board is a square, this is the length of each side in cells */
  State *cell, *sync;   /* cell[boardIndex(size,x,y)] is the current state at (x,y); sync[boardIndex(size,x,y)] is the state pending the next board synchronization */
  unsigned char *syncWrite; /* syncWrite[boardIndex(size,x,y)] is true if sync[boardIndex(size,x,y)] should be written to cell[boardIndex(size,x,y)] at next board sync */
  CellWatcher **watcher;  /* notify[boardIndex(size,x,y)] is pointer to CellWatcher object that intercepts & potentially modifies writes to cell (x,y) */
  BinTree *asyncBin, *syncBin, *syncUpdateBin;  /* asyncBin = stochastic update rates AND queue, syncBin = sync update rates, syncUpdateBin = sync update queue */
  int syncParticles, lastSyncParticles;  /* number of synchronous particles on the board now, and after last board sync */
  double overloadThreshold;  /* overload rules will be used at (x,y) if boardFiringRate(board) > overloadThreshold */
  Palette palette;  /* cell color scheme used by this Board */
  double updatesPerCell;  /* time elapsed on this board, in units of expected updates per cell */
  double updatesPerCellAfterLastBoardSync;  /* time elapsed on this board at time of last board sync */
  int syncUpdates;  /* number of board synchronizations */
  Vector *balloon;  /* container & owner of Balloon's */
  void *game;  /* passed to rule-triggered Goal's */
} Board;

/* public methods */
Board* newBoard (int size);
void deleteBoard (Board* board);
void addParticleToBoard (Particle* p, Board* board);  /* turns over responsibility for deleting the Particle to the Board */
PaletteIndex readBoardColor (Board* board, int x, int y);

void updateBalloons (Board *board, double duration);  /* duration is measured in real time, i.e. seconds */
#define addBalloon(BOARD_PTR,BALLOON_PTR,X,Y) { VectorPushBack ((BOARD_PTR)->balloon, newPlacedBalloon (BALLOON_PTR, X, Y, (1. - (((BOARD_PTR)->updatesPerCell) - (double) (int) ((BOARD_PTR)->updatesPerCell))))); }

/* macros to access board without bounds overrun errors */
#define onBoard(BOARD_PTR,X,Y) ((X) >= 0 && (X) < (BOARD_PTR)->size && (Y) >= 0 && (Y) < (BOARD_PTR)->size)
#define readBoardState(BOARD_PTR,X,Y) (onBoard(BOARD_PTR,X,Y) ? (State) readBoardStateUnguarded(BOARD_PTR,X,Y) : (State) 0)
#define writeBoardState(BOARD_PTR,X,Y,STATE) { if (onBoard(BOARD_PTR,X,Y)) writeBoardStateUnguarded(BOARD_PTR,X,Y,STATE); }
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
   attempts to update each cell targetUpdatesPerCell times, terminating if this limit is reached or maxTimeInSeconds has elapsed (whichever occurs first).
   return values:
   *updatesPerCell_ret = elapsed time on board clock
   *actualUpdates_ret = the actual number of cells updated
   *elapsedTimeInSeconds_ret = the actual elapsed time in seconds
*/
void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds, double *updatesPerCell_ret, int *actualUpdates_ret, double *elapsedTimeInSeconds_ret);

/* Private helper methods & macros */

/* private board index conversion macros */
#define boardIndex(SIZE,X,Y) ((X) + (SIZE) * (Y))
#define boardIndexToX(SIZE,IDX) ((IDX) % (SIZE))
#define boardIndexToY(SIZE,IDX) ((int) ((IDX) / (SIZE)))

/* private board read macros */
#define readBoardStateUnguarded(BOARD_PTR,X,Y) (BOARD_PTR)->cell[boardIndex((BOARD_PTR)->size,X,Y)]
#define readSyncBoardStateUnguarded(BOARD_PTR,X,Y) (BOARD_PTR)->sync[boardIndex((BOARD_PTR)->size,X,Y)]

/* Board read accessors.
   These "unguarded" methods do not check for off-board co-ordinates. Use readBoardState macro instead.
*/

/* Board write accessor for asynchronous updates.
 */
State readBoardStateUnguardedFunction (Board* board, int x, int y);

/* Board write accessor for synchronous updates.
 */
State readSyncBoardStateUnguardedFunction (Board* board, int x, int y);

/* Board write accessors.
   These "unguarded" methods do not check for off-board co-ordinates. Use writeBoardState macro instead.
*/

/* Board write accessor for asynchronous updates.
 */
void writeBoardStateUnguarded (Board* board, int x, int y, State state);

/* Board write accessor for synchronous updates.
 */
void writeSyncBoardStateUnguarded (Board* board, int x, int y, State state);

/* Dummy Board write accessor
 */
void dummyWriteBoardState (Board* board, int x, int y, State state);

/* Function pointers for board read & write.
 */
typedef State (*BoardReadFunction) (Board*, int, int);
typedef void (*BoardWriteFunction) (Board*, int, int, State);

/* Other helper methods */
void attemptRule (Particle *ruleOwner, ParticleRule *rule, Board *board, int x, int y, BoardReadFunction readUnguarded, BoardWriteFunction writeUnguarded);
#define boardOverloaded(BOARD_PTR) (boardFiringRate(BOARD_PTR) >= (BOARD_PTR)->overloadThreshold)

void evolveBoardCell (Board *board, int x, int y);
void evolveBoardCellSync (Board *board, int x, int y);

void freezeBoard (Board* board);
void syncBoard (Board* board);

#endif /* BOARD_INCLUDED */
