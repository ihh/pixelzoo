#ifndef BOARD_INCLUDED
#define BOARD_INCLUDED

#include "rule.h"
#include "particle.h"
#include "quadtree.h"

typedef struct Board {
  Particle** by_type;  /* Type t; by_type[t] */
  int size;
  State** cell;   /* int x, y; cell[x][y] */
  QuadTree* quad;  /* private */
  double overloadThreshold;  /* when boardFiringRate>overloadThreshold, rules use overloadRate's instead of rate's */
} Board;

/* public methods */
Board* newBoard (int size);
void deleteBoard (Board* board);
Particle* newBoardParticle (Board* board, char* name, Type type, int nRules);
void finalizeBoardRules (Board* board);  /* call after setting up all particles & rules */

/* macros to access board without bounds overrun errors */
#define onBoard(BOARD_PTR,X,Y) ((X) >= 0 && (X) <= (BOARD_PTR)->size && (Y) >= 0 && (Y) <= (BOARD_PTR)->size)
#define readBoardState(BOARD_PTR,X,Y) (onBoard(BOARD_PTR,X,Y) ? readBoardStateUnguarded(BOARD_PTR,X,Y) : 0)
#define writeBoardState(BOARD_PTR,X,Y,STATE) { if (onBoard(BOARD_PTR,X,Y)) writeBoardStateUnguarded(BOARD_PTR,X,Y,STATE); }
#define readBoardParticle(BOARD_PTR,X,Y) (BOARD_PTR)->by_type[readBoardState(BOARD_PTR,X,Y) & TypeMask]

/* number of cells on board */
#define boardCells(BOARD_PTR) (((double) (BOARD_PTR)->size) * ((double) (BOARD_PTR)->size))

/* board firing rate = mean rate at which rules are firing. ranges from 0 (empty) to 1 (full) */
#define boardFiringRate(BOARD_PTR) (topQuadRate((BOARD_PTR)->quad) / boardCells(BOARD_PTR))

/* evolveBoard
   attempts to update each cell targetUpdatesPerCell times, terminating if this limit is reached or maxTimeInSeconds has elapsed (whichever occurs first).
   returns the update rate (cells updated/second) & the estimated lower bound (i.e. what the update rate would be if all rules were firing).
*/
void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds, double* updateRate_ret, double* minUpdateRate_ret);

/* private helper methods & macros */
#define readBoardStateUnguarded(BOARD_PTR,X,Y) (BOARD_PTR)->cell[X][Y]   /* does not check for off-board co-ordinates. Use readBoardState macro instead */
void writeBoardStateUnguarded (Board* board, int x, int y, State state);  /* does not check for off-board co-ordinates. Use writeBoardState macro instead */

int testRuleCondition (RuleCondition* cond, Board* board, int x, int y);
void execRuleOperation (RuleOperation* op, Board* board, int x, int y);

void evolveBoardCell (Board* board, int x, int y, int overloaded);

#endif /* BOARD_INCLUDED */
