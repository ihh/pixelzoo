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
} Board;

/* public methods */
Board* newBoard (int size);
void deleteBoard (Board* board);
Particle* newBoardParticle (Board* board, char* name, Type type, int nRules);
Particle* readBoardParticle (Board* board, int x, int y);  /* safe to use off-board co-ordinates (no bounds overrun errors) */

/* evolveBoard returns the update rate (updates/second) & the estimated minimum rate if all rules were firing */
void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds, double* updateRate_ret, double* minUpdateRate_ret);

/* macros to access board without bounds overrun errors */
#define onBoard(BOARD_PTR,X,Y) ((X) >= 0 && (X) <= (BOARD_PTR)->size && (Y) >= 0 && (Y) <= (BOARD_PTR)->size)
#define readBoardState(BOARD_PTR,X,Y) (onBoard(BOARD_PTR,X,Y) ? readBoardStateUnguarded(BOARD_PTR,X,Y) : 0)
#define writeBoardState(BOARD_PTR,X,Y,STATE) { if (onBoard(BOARD_PTR,X,Y)) writeBoardStateUnguarded(BOARD_PTR,X,Y,STATE); }

/* private helper methods & macros */
#define readBoardStateUnguarded(BOARD_PTR,X,Y) (BOARD_PTR)->cell[X][Y]   /* does not check for off-board co-ordinates. Use readBoardState macro instead */
void writeBoardStateUnguarded (Board* board, int x, int y, State state);  /* does not check for off-board co-ordinates. Use writeBoardState macro instead */

int testRuleCondition (RuleCondition* cond, Board* board, int x, int y);
void execRuleOperation (RuleOperation* op, Board* board, int x, int y);

void evolveBoardCell (Board* board, int x, int y);

#endif /* BOARD_INCLUDED */
