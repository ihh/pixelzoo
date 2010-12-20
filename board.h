#ifndef BOARD_INCLUDED
#define BOARD_INCLUDED

#include "rule.h"
#include "particle.h"
#include "quadtree.h"
#include "util.h"

typedef struct Board {
  Particle** byType;  /* Type t; byType[t] */
  int size;
  State **cell, **sync;   /* int x, y; cell[x][y] */
  QuadTree *asyncQuad, *syncQuad;  /* private: asyncQuad = stochastic update rates, syncQuad = sync update rates */
  int syncParticles;  /* number of synchronous particles currently on the board */
  double* overloadThreshold;  /* overload rules will be used at (x,y) if boardLocalFiringRate(board,x,y,lev) > overloadThreshold[lev] for any value of lev */
  Palette palette;
  double updatesPerCell;  /* time elapsed on this board, in units of expected updates per cell */
  int syncUpdates;  /* number of synchronous update cycles */
} Board;

/* public methods */
Board* newBoard (int size);
void deleteBoard (Board* board);
void addParticleToBoard (Particle* p, Board* board);  /* turns over responsibility for deleting the Particle to the Board */
PaletteIndex readBoardColor (Board* board, int x, int y);

/* macros to access board without bounds overrun errors */
#define onBoard(BOARD_PTR,X,Y) ((X) >= 0 && (X) < (BOARD_PTR)->size && (Y) >= 0 && (Y) < (BOARD_PTR)->size)
#define readBoardState(BOARD_PTR,X,Y) (onBoard(BOARD_PTR,X,Y) ? (State) readBoardStateUnguarded(BOARD_PTR,X,Y) : (State) 0)
#define writeBoardState(BOARD_PTR,X,Y,STATE) { if (onBoard(BOARD_PTR,X,Y)) writeBoardStateUnguarded(BOARD_PTR,X,Y,STATE); }
#define readBoardParticle(BOARD_PTR,X,Y) (BOARD_PTR)->byType[StateType(readBoardState(BOARD_PTR,X,Y))]
#define readBoardParticleUnguarded(BOARD_PTR,X,Y) (BOARD_PTR)->byType[StateType(readBoardStateUnguarded(BOARD_PTR,X,Y))]

/* number of cells on board */
#define boardCells(BOARD_PTR) (((double) (BOARD_PTR)->size) * ((double) (BOARD_PTR)->size))

/* board firing rate = mean rate at which rules are firing. ranges from 0 (empty) to 1 (full) */
#define boardFiringRate(BOARD_PTR) ((topQuadRate((BOARD_PTR)->syncQuad) + topQuadRate((BOARD_PTR)->asyncQuad)) / boardCells(BOARD_PTR))
#define boardAsyncFiringRate(BOARD_PTR) (topQuadRate((BOARD_PTR)->asyncQuad) / boardCells(BOARD_PTR))
#define boardSyncFiringRate(BOARD_PTR) (topQuadRate((BOARD_PTR)->syncQuad) / boardCells(BOARD_PTR))
#define boardLocalFiringRate(BOARD_PTR,X,Y,LEVEL) \
  ((getQuadRate((BOARD_PTR)->syncQuad,X,Y,LEVEL) + getQuadRate((BOARD_PTR)->asyncQuad,X,Y,LEVEL)) / (double) quadCells((BOARD_PTR)->syncQuad,LEVEL))
#define boardAsyncParticles(BOARD_PTR) (boardCells(BOARD_PTR) - (BOARD_PTR)->syncParticles)

/* evolveBoard
   attempts to update each cell targetUpdatesPerCell times, terminating if this limit is reached or maxTimeInSeconds has elapsed (whichever occurs first).
   returns the update rate (cells updated/second) & the estimated lower bound (i.e. what the update rate would be if all rules were firing).
*/
void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds, double* updateRate_ret, double* minUpdateRate_ret);

/* Private helper methods & macros */

/* Board accessors.
   The "unguarded" methods do not check for off-board co-ordinates. Use writeBoardState macro instead.
*/
#define readBoardStateUnguarded(BOARD_PTR,X,Y) (BOARD_PTR)->cell[X][Y]
void writeBoardStateUnguarded (Board* board, int x, int y, State state);

/* Board accessor for synchronous updates.
 */
void writeSyncBoardStateUnguarded (Board* board, int x, int y, State state);

/* Function pointer for board write.
 */
typedef void (*BoardWriteFunction) (Board*, int, int, State);

/* Other helper methods */
int testRuleCondition (RuleCondition* cond, Board* board, int x, int y, int overloaded);
State execRuleOperation (RuleOperation* op, Board* board, int x, int y, State oldSrcState, State oldDestState, int overloaded, BoardWriteFunction write);  /* returns the newly-written State */
#define getRuleOperationOldSrcState(OP_PTR,BOARD_PTR,X,Y) readBoardState(BOARD_PTR,X+(OP_PTR)->src.x,Y+(OP_PTR)->src.y)
#define getRuleOperationOldDestState(OP_PTR,BOARD_PTR,X,Y) readBoardState(BOARD_PTR,X+(OP_PTR)->dest.x,Y+(OP_PTR)->dest.y)

int attemptRule (StochasticRule* rule, Board* board, int x, int y, int overloaded, BoardWriteFunction write);
int boardOverloaded (Board* board, int x, int y);

void evolveBoardCell (Board* board, int x, int y);
void evolveBoardSync (Board* board);

#endif /* BOARD_INCLUDED */
