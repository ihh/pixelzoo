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

Board* newBoard (int size);
void deleteBoard (Board* board);
Particle* newBoardParticle (Board* board, char* name, Type type, int nRules);
void writeBoard (Board* board, int x, int y, State state);  /* does not check for off-board co-ordinates. use safeWriteBoard(board,x,y,state) instead */
Particle* readBoard (Board* board, int x, int y);  /* safe to use off-board co-ordinates */
void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds);

/* macros to access board without bounds overrun errors */
#define onBoard(BOARD_PTR,X,Y) ((X) >= 0 && (X) <= (BOARD_PTR)->size && (Y) >= 0 && (Y) <= (BOARD_PTR)->size)
#define safeReadBoardState(BOARD_PTR,X,Y) (onBoard(BOARD_PTR,X,Y) ? (BOARD_PTR)->cell[(X)][(Y)] : 0)
#define safeWriteBoard(BOARD_PTR,X,Y,STATE) { if (onBoard(BOARD_PTR,X,Y,STATE)) writeBoard(BOARD_PTR,X,Y,STATE); }

/* private helper methods */
int testRuleCondition (RuleCondition* cond, Board* board, int x, int y);
void execRuleOperation (RuleOperation* op, Board* board, int x, int y);

#endif /* BOARD_INCLUDED */
