#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "board.h"

Board* newBoard (int size) {
  Board *board;
  int x, gray, hue, sat, bright, colFlag[SceneryStates];
  RGB *col;
  board = SafeMalloc (sizeof (Board));
  board->byType = SafeCalloc (NumTypes, sizeof(Particle*));
  board->size = size;
  board->cell = SafeMalloc (size * sizeof(State*));
  for (x = 0; x < size; ++x)
    board->cell[x] = SafeCalloc (size, sizeof(State));
  board->quad = newQuadTree (size);
  board->overloadThreshold = SafeMalloc ((board->quad->K + 1) * sizeof(double));
  for (x = 0; x <= board->quad->K; ++x)
    board->overloadThreshold[x] = 1.;

  /* scenery colors */
  for (x = 0; x < SceneryStates; ++x)
    colFlag[x] = 1;  /* prime factors of this debugging flag indicate number of ways/times it's been initialized */

  for (gray = 0; gray < SceneryGrayLevels; ++gray) {
    colFlag[GrayScenery(gray)] *= 3;
    col = &board->sceneryColor[GrayScenery(gray)];
    col->r = col->g = col->b = 255 * gray / (SceneryGrayLevels - 1);
  }

  for (hue = 0; hue < SceneryColorHues; ++hue)
    for (bright = 0; bright < SceneryColorBrights; ++bright) {
      colFlag[ColorScenery(hue,bright)] *= 5;
      col = &board->sceneryColor[ColorScenery(hue,bright)];
      convertHSVtoRGB (((double) hue) * 360 / SceneryColorHues,
		       1.,
		       ((double) bright + 1) / SceneryColorBrights, col);
    }

  for (hue = 0; hue < SceneryPaleColorHues; ++hue)
    for (bright = 0; bright < SceneryPaleColorBrights; ++bright)
      for (sat = 0; sat < SceneryPaleColorSaturations; ++sat) {
	colFlag[PaleColorScenery(hue,sat,bright)] *= 7;
	col = &board->sceneryColor[PaleColorScenery(hue,sat,bright)];
	convertHSVtoRGB (((double) hue) * 360 / SceneryPaleColorHues,
			 ((double) sat / (SceneryPaleColorSaturations + 1)),
			 ((double) bright + 1) / SceneryPaleColorBrights, col);
      }

  for (x = 0; x < SceneryStates; ++x) {
    Assert (colFlag[x] <= 7, "color multiply initialized");
    Assert (colFlag[x] > 1, "color uninitialized");
  }

  return board;
}

void deleteBoard (Board* board) {
  unsigned long t;
  int x;
  SafeFree(board->overloadThreshold);
  deleteQuadTree (board->quad);
  for (x = 0; x < board->size; ++x)
    SafeFree(board->cell[x]);
  SafeFree(board->cell);
  for (t = 0; t < NumTypes; ++t)
    if (board->byType[(Type) t])
      deleteParticle (board->byType[(Type) t]);
  SafeFree(board->byType);
  SafeFree(board);
}

void writeBoardStateUnguarded (Board* board, int x, int y, State state) {
  Type t;
  Particle* p;
  t = (state & TypeMask) >> TypeShift;
  if (t == EmptyType) {
    board->cell[x][y] = state;
    updateQuadTree (board->quad, x, y, 0.);
  } else {
    p = board->byType[t];
    if (p == NULL) {
      board->cell[x][y] = EmptyState;
      updateQuadTree (board->quad, x, y, 0.);
    } else {
      board->cell[x][y] = state;
      updateQuadTree (board->quad, x, y, p->normalizedRate);
    }
  }
}

RGB* readBoardColor (Board* board, int x, int y) {
  Particle *p;
  State s;
  Type t;
  RGB *c;
  c = &board->sceneryColor[0];  /* default to black */
  s = readBoardState (board, x, y);
  t = StateType(s);
  if (t == EmptyType)
    c = &board->sceneryColor[s % SceneryMax];
  else {
    p = board->byType[t];
    if (p)
      c = &p->color;
  }
  return c;
}

Particle* newBoardParticle (Board* board, char* name, Type type, int nRules) {
  Particle* p;
  p = newParticle (name, nRules);
  p->type = type;
  board->byType[type] = p;
  return p;
}

void addParticleToBoard (Particle* p, Board* board) {
  int r;
  board->byType[p->type] = p;
  p->totalRate = p->totalOverloadRate = 0.;
  for (r = 0; r < p->nRules; ++r) {
    p->totalRate += p->rule[r].rate;
    p->totalOverloadRate += p->rule[r].overloadRate;
  }
  p->normalizedRate = MIN (p->totalRate, 1.);
}

int testRuleCondition (RuleCondition* cond, Board* board, int x, int y, int overloaded) {
  State lhs, rhs;
  if (randomDouble() < (overloaded ? cond->overloadIgnoreProb : cond->ignoreProb))
    return 1;
  x += cond->loc.x;
  y += cond->loc.y;
  lhs = readBoardState(board,x,y) & cond->mask;
  rhs = cond->rhs;
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

void execRuleOperation (RuleOperation* op, Board* board, int x, int y, int overloaded) {
  int xSrc, ySrc;
  if (randomDouble() < (overloaded ? op->overloadFailProb : op->failProb))
    return;
  xSrc = x + op->src.x;
  ySrc = y + op->src.y;
  x += op->dest.x;
  y += op->dest.y;
  if (onBoard(board,x,y))  /* only check once */
      writeBoardStateUnguarded (board, x, y, 
				(readBoardStateUnguarded(board,x,y) & (StateMask ^ (op->mask << op->leftShift)))
				| (((((readBoardState(board,xSrc,ySrc) & op->preMask) >> op->rightShift) + op->offset) & op->mask) << op->leftShift));
}

void evolveBoardCell (Board* board, int x, int y) {
  Particle* p;
  int n, k, overloaded;
  double rand;
  StochasticRule* rule;
  p = readBoardParticle (board, x, y);
  if (p) {
    overloaded = 0;
    for (n = 0; n <= board->quad->K && !overloaded; ++n)
      if (boardLocalFiringRate(board,x,y,n) > board->overloadThreshold[n])
	overloaded = 1;
    rand = randomDouble() * (overloaded ? p->totalOverloadRate : p->totalRate);
    for (n = 0; n < p->nRules; ++n) {
      rule = &p->rule[n];
      if ((rand -= (overloaded ? rule->overloadRate : rule->rate)) <= 0) {
	for (k = 0; k < NumRuleConditions; ++k)
	  if (!testRuleCondition (&rule->cond[k], board, x, y, overloaded))
	    return;  /* bail out of loops over k & n */
	for (k = 0; k < NumRuleOperations; ++k)
	  execRuleOperation (&rule->op[k], board, x, y, overloaded);
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
    elapsedTime = ((double) now - start) / (double) CLOCKS_PER_SEC;
    if (elapsedTime > maxTimeInSeconds || effectiveUpdates >= targetUpdates)
      break;
    /* estimate expected number of rejected moves per accepted move as follows:
       rejections = \sum_{n=0}^{\infty} n*(p^n)*(1-p)   where p = rejectProb
                  = (1-p) * p * d/dp \sum_{n=0}^{\infty} p^n
                  = (1-p) * p * d/dp 1/(1-p)
                  = (1-p) * p * 1/(1-p)^2
                  = p / (1-p)
                  = (1-q) / q   where q = acceptProb = topQuadRate/boardCells
       accepted + rejected = 1 + (1-q)/q = 1/q = boardCells/topQuadRate
    */
    effectiveUpdates += 1. / boardFiringRate(board);
    ++actualUpdates;

    sampleQuadLeaf (board->quad, &x, &y);
    evolveBoardCell (board, x, y);
  }
  if (updateRate_ret)
    *updateRate_ret = (elapsedTime > 0) ? (effectiveUpdates / elapsedTime) : 0.;
  if (minUpdateRate_ret)
    *minUpdateRate_ret = (elapsedTime > 0) ? (actualUpdates / elapsedTime) : 0.;
}
