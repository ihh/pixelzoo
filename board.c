#include <stdlib.h>
#include <stdio.h>
#include <time.h>
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

void writeBoardStateUnguarded (Board* board, int x, int y, State state) {
  Type t;
  Particle* p;
  t = state & TypeMask;
  p = board->by_type[t];
  if (p) {
    board->cell[x][y] = state;
    updateQuadTree (board->quad, x, y, p->normalizedRate);
  } else {
    board->cell[x][y] = 0;
    updateQuadTree (board->quad, x, y, 0.);
  }
}

void finalizeBoardRules (Board* board) {
  unsigned long t;
  Particle* p;
  int r;
  for (t = 0; t < NumTypes; ++t) {
    p = board->by_type[t];
    if (p) {
      p->totalRate = 0.;
      for (r = 0; r < p->nRules; ++r)
	p->totalRate += p->rule[r].rate;
      p->normalizedRate = min (p->totalRate, 1.);
    }
  }
}

int testRuleCondition (RuleCondition* cond, Board* board, int x, int y) {
  State lhs, rhs;
  if (randomDouble() < cond->ignoreProb)
    return 1;
  x += cond->loc.x;
  y += cond->loc.y;
  lhs = readBoardState(board,x,y) & cond->mask;
  rhs = cond->rhs & cond->mask;
  switch (cond->opcode) {
  case EQ: return lhs == rhs;
  case NEQ: return lhs != rhs;
  case GT: return lhs > rhs;
  case LT: return lhs < rhs;
  case GEQ: return lhs >= rhs;
  case LEQ: return lhs <= rhs;
  case TRUE: return 1;
  case FALSE: default: break;
  }
  return 0;
}

void execRuleOperation (RuleOperation* op, Board* board, int x, int y) {
  int xSrc, ySrc;
  if (randomDouble() < op->failProb)
    return;
  xSrc = x + op->src.x;
  ySrc = y + op->src.y;
  x += op->dest.x;
  y += op->dest.y;
  if (onBoard(board,x,y))  /* only check once */
    writeBoardStateUnguarded (board, x, y, 
			      (readBoardStateUnguarded(board,x,y) & (StateMask ^ (op->mask << op->left_shift)))
			      | ((((readBoardState(board,xSrc,ySrc) >> op->right_shift) + op->offset) & op->mask) << op->left_shift));
}

void evolveBoardCell (Board* board, int x, int y) {
  Particle* p;
  int n, k;
  double rand;
  StochasticRule* rule;
  p = readBoardParticle (board, x, y);
  if (p) {
    rand = randomDouble() * p->totalRate;
    for (n = 0; n < p->nRules; ++n) {
      rule = &p->rule[n];
      if ((rand -= rule->rate) <= 0) {
	for (k = 0; k < NumRuleConditions; ++k)
	  if (!testRuleCondition (&rule->cond[k], board, x, y))
	    return;  /* bail out of loops over k & n */
	for (k = 0; k < NumRuleOperations; ++k)
	  execRuleOperation (&rule->op[k], board, x, y);
	return;  /* bail out of loop over n */
      }
    }
  }
}

void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds, double* updateRate_ret, double* minUpdateRate_ret) {
  int actualUpdates, x, y;
  double effectiveUpdates, targetUpdates, elapsedTime;
  clock_t start, now;
  actualUpdates = 0;
  effectiveUpdates = elapsedTime = 0.;
  targetUpdates = targetUpdatesPerCell * boardCells(board);
  start = clock();
  while (topQuadRate(board->quad) > 0.) {
    now = clock();
    elapsedTime = (now - start) / CLOCKS_PER_SEC;
    if (elapsedTime > maxTimeInSeconds || effectiveUpdates >= targetUpdates)
      break;
    /* estimate expected number of rejected moves per accepted move as follows:
       rejections = \sum_{n=0}^{\infty} n*(p^n)*(1-p)   where p=rejectProb
                  = (1-p) * p * d/dp \sum_{n=0}^{\infty} p^n
                  = (1-p) * p * d/dp 1/(1-p)
                  = (1-p) * p * 1/(1-p)^2
                  = p / (1-p)
                  = (1-q) / q   where q = acceptProb = topQuadRate/boardCells
       accepted + rejected = 1 + (1-q)/q = 1/q = boardCells/topQuadRate
    */
    effectiveUpdates += boardCells(board) / topQuadRate(board->quad);
    ++actualUpdates;

    sampleQuadLeaf (board->quad, &x, &y);
    evolveBoardCell (board, x, y);
  }
  if (updateRate_ret)
    *updateRate_ret = (elapsedTime > 0) ? (effectiveUpdates / elapsedTime) : 0.;
  if (minUpdateRate_ret)
    *minUpdateRate_ret = (elapsedTime > 0) ? (actualUpdates / elapsedTime) : 0.;
}
