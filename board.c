#include <stdlib.h>
#include <stdio.h>
#include "board.h"

Board* newBoard (int size) {
  Board* board;
  int x;
  board = malloc (sizeof (Board));
  board->by_type = calloc (NumTypes, sizeof(Particle*));
  board->size = size;
  board->cell = malloc (size * sizeof(State*));
  for (x = 0; x < size; ++x)
    board->cell[x] = calloc (size, sizeof(State));
  board->quad = newQuadTree (size);
  return board;
}

void deleteBoard (Board* board) {
  unsigned long t;
  int x;
  deleteQuadTree (board->quad);
  for (x = 0; x < board->size; ++x)
    free (board->cell[x]);
  free (board->cell);
  for (t = 0; t < NumTypes; ++t)
    if (board->by_type[(Type) t])
      deleteParticle (board->by_type[(Type) t]);
  free (board->by_type);
  free (board);
}

Particle* newBoardParticle (Board* board, char* name, Type type, int nRules) {
  Particle* p;
  p = newParticle (name, nRules);
  p->type = type;
  board->by_type[type] = p;
  return p;
}

void writeBoard (Board* board, int x, int y, State state) {
  Type t;
  Particle* p;
  t = state & TypeMask;
  p = board->by_type[t];
  if (p) {
    board->cell[x][y] = state;
    updateQuadTree (board->quad, x, y, p->totalRate);
  } else {
    board->cell[x][y] = 0;
    updateQuadTree (board->quad, x, y, 0.);
  }
}

Particle* readBoard (Board* board, int x, int y) {
  return board->by_type[safeReadBoardState(board,x,y) & TypeMask];
}

int testRuleCondition (RuleCondition* cond, Board* board, int x, int y) {
  State lhs, rhs;
  x += cond->loc.x;
  y += cond->loc.y;
  lhs = safeReadBoardState(board,x,y) & cond->mask;
  rhs = cond->rhs & cond->mask;
  switch (cond->opcode) {
  case EQ: return lhs == rhs;
  case NEQ: return lhs != rhs;
  case GT: return lhs > rhs;
  case LT: return lhs < rhs;
  case GEQ: return lhs >= rhs;
  case LEQ: return lhs <= rhs;
  case TRUE: return 1;
  default: break;
  }
  return 0;
}

void execRuleOperation (RuleOperation* op, Board* board, int x, int y) {
  int xSrc, ySrc;
  xSrc = x + op->src.x;
  ySrc = y + op->src.y;
  x += op->dest.x;
  y += op->dest.y;
  if (onBoard(board,x,y) && onBoard(board,xSrc,ySrc))  /* only check once */
    writeBoard (board, x, y, 
		(board->cell[x][y] & (StateMask ^ (op->mask << op->left_shift)))
		| ((((board->cell[xSrc][ySrc] >> op->right_shift) + op->offset) & op->mask) << op->left_shift));
}
