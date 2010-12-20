#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "board.h"
#include "notify.h"

Board* newBoard (int size) {
  Board *board;
  int x;
  board = SafeMalloc (sizeof (Board));
  board->byType = SafeCalloc (NumTypes, sizeof(Particle*));
  board->size = size;
  board->cell = SafeMalloc (size * sizeof(State*));
  board->sync = SafeMalloc (size * sizeof(State*));
  for (x = 0; x < size; ++x) {
    board->cell[x] = SafeCalloc (size, sizeof(State));
    board->sync[x] = SafeCalloc (size, sizeof(State));
  }
  board->quad = newQuadTree (size);
  board->async = newQuadTree (size);
  board->syncParticles = 0;
  board->overloadThreshold = SafeMalloc ((board->quad->K + 1) * sizeof(double));
  for (x = 0; x <= board->quad->K; ++x)
    board->overloadThreshold[x] = 1.;
  board->updatesPerCell = 0.;
  board->syncUpdates = 0;

  initializePalette (&board->palette);

  return board;
}

void deleteBoard (Board* board) {
  unsigned long t;
  int x;
  SafeFree(board->overloadThreshold);
  deleteQuadTree (board->quad);
  deleteQuadTree (board->async);
  for (x = 0; x < board->size; ++x) {
    SafeFree(board->cell[x]);
    SafeFree(board->sync[x]);
  }
  SafeFree(board->cell);
  SafeFree(board->sync);
  for (t = 0; t < NumTypes; ++t)
    if (board->byType[(Type) t])
      deleteParticle (board->byType[(Type) t]);
  SafeFree(board->byType);
  SafeFree(board);
}

void writeBoardStateUnguarded (Board* board, int x, int y, State state) {
  Type t;
  Particle *p, *pOld;
  t = StateType(state);
  pOld = readBoardParticleUnguarded(board,x,y);
  if (t == EmptyType) {
    board->cell[x][y] = state;
    updateQuadTree (board->quad, x, y, 0.);
    updateQuadTree (board->async, x, y, 0.);
    if (pOld && pOld->synchronous)
      --board->syncParticles;
  } else {
    p = board->byType[t];
    if (p == NULL) {
      board->cell[x][y] = EmptyState;
      updateQuadTree (board->quad, x, y, 0.);
      updateQuadTree (board->async, x, y, 0.);
      if (pOld && pOld->synchronous)
	--board->syncParticles;
    } else {
      board->cell[x][y] = state;
      updateQuadTree (board->quad, x, y, p->firingRate);
      updateQuadTree (board->async, x, y, p->asyncFiringRate);
      if (p->synchronous) {
	if (pOld == NULL || !pOld->synchronous)
	  ++board->syncParticles;
      } else
	if (pOld && pOld->synchronous)
	  --board->syncParticles;
    }
  }
  /* TODO: check for BoardWatcher's, call appropriate ParticleNotifyFunction(s) */
}

void writeSyncBoardStateUnguarded (Board* board, int x, int y, State state) {
  board->sync[x][y] = state;
}

PaletteIndex readBoardColor (Board* board, int x, int y) {
  Particle *p;
  State s;
  Type t;
  PaletteIndex c;
  c = 0;  /* default to black */
  s = readBoardState (board, x, y);
  t = StateType(s);
  if (t == EmptyType)
    c = s & PaletteMask;  /* hard-wired shortcut for empties */
  else {
    p = board->byType[t];
    if (p)
      c = getParticleColor (p, s);
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
  int r, n, m;
  StochasticRule *rule;
  board->byType[p->type] = p;
  p->totalRate = p->totalOverloadRate = 0.;
  for (r = 0; r < p->nRules; ++r) {
    rule = &p->rule[r];
    p->totalRate += rule->rate;
    p->totalOverloadRate += rule->overloadRate;
    for (n = 1; n < NumRuleOperations; ++n)
      for (m = n; m > 0; --m) {
	if (rule->op[n].src.x == rule->op[n-m].dest.x
	    && rule->op[n].src.y == rule->op[n-m].dest.y)
	  rule->cumulativeOpSrcIndex[n] = m;
	if (rule->op[n].dest.x == rule->op[n-m].dest.x
	    && rule->op[n].dest.y == rule->op[n-m].dest.y)
	  rule->cumulativeOpDestIndex[n] = m;
      }
  }
  p->firingRate = MIN (p->totalRate, 1.);
  p->asyncFiringRate = p->synchronous ? 0. : p->firingRate;
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

State execRuleOperation (RuleOperation* op, Board* board, int x, int y, State oldSrcState, State oldDestState, int overloaded, BoardWriteFunction write) {
  State newState;
  if (randomDouble() < (overloaded ? op->overloadFailProb : op->failProb))
    return oldDestState;
  x += op->dest.x;
  y += op->dest.y;
  if (onBoard(board,x,y)) {  /* only check once */
    newState = (oldDestState & (StateMask ^ (op->mask << op->leftShift)))
      | (((((oldSrcState & op->preMask) >> op->rightShift) + op->offset) & op->mask) << op->leftShift);
    (*write) (board, x, y, newState);
    return newState;
  }
  return oldDestState;
}

void evolveBoardCell (Board* board, int x, int y) {
  Particle* p;
  int n, overloaded;
  double rand;
  StochasticRule* rule;
  p = readBoardParticle (board, x, y);
  if (p) {
    /*
      Assert (!p->synchronous, "evolveBoardCell called on async particle");
    */
    overloaded = boardOverloaded (board, x, y);
    /* sample a rule at random */
    rand = randomDouble() * (overloaded ? p->totalOverloadRate : p->totalRate);
    for (n = 0; n < p->nRules; ++n) {
      rule = &p->rule[n];
      if ((rand -= (overloaded ? rule->overloadRate : rule->rate)) <= 0) {
	(void) attemptRule (rule, board, x, y, overloaded, writeBoardStateUnguarded);
	return;
      }
    }
  }
}

void evolveBoardSync (Board* board) {
  Particle* p;
  int x, y, size, n, swap, overloaded, *ruleOrder;
  State *cellCol, *syncCol;
  StochasticRule* rule;
  size = board->size;
  /* copy board */
  for (x = 0; x < size; ++x)
    memcpy ((void*) board->sync[x], (void*) board->cell[x], size * sizeof(State));
  /* do updates */
  for (x = 0; x < size; ++x)
    for (y = 0; y < size; ++y) {
      p = readBoardParticle (board, x, y);
      if (p && p->synchronous) {
	overloaded = boardOverloaded (board, x, y);
	/* attempt each rule in random sequence, stopping when one succeeds */
	ruleOrder = SafeMalloc (p->nRules * sizeof(int));  /* Fisher-Yates shuffle */
	for (n = 0; n < p->nRules; ++n)
	  ruleOrder[n] = n;
	for (n = 0; n < p->attempts; ++n) {
	  if (p->shuffle) {
	    swap = n + (int) (randomDouble() * (double) (p->nRules - n));
	    rule = &p->rule[ruleOrder[swap]];
	    ruleOrder[swap] = ruleOrder[n];
	  } else
	    rule = &p->rule[n];
	  if (randomDouble() < (overloaded ? rule->overloadRate : rule->rate))
	    if (attemptRule (rule, board, x, y, overloaded, writeSyncBoardStateUnguarded))
	      break;
	}
	SafeFree (ruleOrder);
      }
    }
  /* update only the cells that changed */
  for (x = 0; x < size; ++x) {
    cellCol = board->cell[x];
    syncCol = board->sync[x];
    for (y = 0; y < size; ++y)
      if (syncCol[y] != cellCol[y])
	writeBoardStateUnguarded (board, x, y, syncCol[y]);
  }
}

int boardOverloaded (Board* board, int x, int y) {
  int n;
  for (n = 0; n <= board->quad->K; ++n)
    if (boardLocalFiringRate(board,x,y,n) > board->overloadThreshold[n])
      return 1;
  return 0;
}

int attemptRule (StochasticRule* rule, Board* board, int x, int y, int overloaded, BoardWriteFunction write) {
  int k, mSrc, mDest;
  RuleCondition *cond;
  RuleOperation *op;
  State intermediateState[NumRuleOperations], oldSrcState, oldDestState;
  for (k = 0; k < NumRuleConditions; ++k) {
    cond = &rule->cond[k];
    if (!testRuleCondition (cond, board, x, y, overloaded))
      return 0;
  }
  for (k = 0; k < NumRuleOperations; ++k) {
    op = &rule->op[k];
    mSrc = rule->cumulativeOpSrcIndex[k];
    mDest = rule->cumulativeOpDestIndex[k];
    oldSrcState = mSrc ? intermediateState[k - mSrc] : getRuleOperationOldSrcState(op,board,x,y);
    oldDestState = mDest ? intermediateState[k - mDest] : getRuleOperationOldDestState(op,board,x,y);
    intermediateState[k] = execRuleOperation (op, board, x, y, oldSrcState, oldDestState, overloaded, write);
  }
  return 1;
}

void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds, double* updateRate_ret, double* minUpdateRate_ret) {
  int actualUpdates, x, y;
  double effectiveUpdates, targetUpdates, elapsedTime, effectiveUpdatesPerCell;
  clock_t start, now;
  /* start the clock */
  start = clock();
  actualUpdates = 0;
  /* handle stochastic updates */
  effectiveUpdates = elapsedTime = effectiveUpdatesPerCell = 0.;
  targetUpdates = targetUpdatesPerCell * boardCells(board);
  while (1) {
    /* check if realtime clock deadline reached */
    now = clock();
    elapsedTime = ((double) now - start) / (double) CLOCKS_PER_SEC;
    if (elapsedTime > maxTimeInSeconds)
      break;
    /* handle outstanding synchronous updates */
    while (board->syncUpdates < (int) board->updatesPerCell) {
      actualUpdates += board->syncParticles;
      effectiveUpdates += board->syncParticles;
      ++board->syncUpdates;
      evolveBoardSync (board);
    }
    /* check if target update count reached */
    if (effectiveUpdates >= targetUpdates) {
      effectiveUpdates = targetUpdates;
      break;
    }
    /* check if async quad tree empty */
    if (topQuadRate(board->async) == 0.) {
      effectiveUpdates += boardAsyncParticles(board);
      continue;
    }
    /* handle stochastic updates */
    /* estimate expected number of rejected moves per accepted move as follows:
       rejections = \sum_{n=0}^{\infty} n*(p^n)*(1-p)   where p = rejectProb
                  = (1-p) * p * d/dp \sum_{n=0}^{\infty} p^n
                  = (1-p) * p * d/dp 1/(1-p)
                  = (1-p) * p * 1/(1-p)^2
                  = p / (1-p)
                  = (1-q) / q   where q = acceptProb = topQuadRate/boardCells
       accepted + rejected = 1 + (1-q)/q = 1/q = boardCells/topQuadRate
    */
    effectiveUpdates += 1. / boardAsyncFiringRate(board);
    ++actualUpdates;

    sampleQuadLeaf (board->async, &x, &y);
    evolveBoardCell (board, x, y);
  }
  effectiveUpdatesPerCell = effectiveUpdates / boardCells(board);
  board->updatesPerCell += effectiveUpdatesPerCell;
  /* calculate rates */
  if (updateRate_ret)
    *updateRate_ret = (elapsedTime > 0) ? (effectiveUpdates / elapsedTime) : 0.;
  if (minUpdateRate_ret)
    *minUpdateRate_ret = (elapsedTime > 0) ? (actualUpdates / elapsedTime) : 0.;
}
