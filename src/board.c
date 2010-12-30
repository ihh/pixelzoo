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
	board->cell = SafeCalloc (size * size, sizeof(State));
	board->sync = SafeCalloc (size * size, sizeof(State));
	board->watcher = SafeCalloc (size * size, sizeof(CellWatcher*));
	board->syncWrite = SafeCalloc (size * size, sizeof(unsigned char));
	board->syncQuad = newQuadTree (size);
	board->asyncQuad = newQuadTree (size);
	board->syncUpdateQuad = newQuadTree (size);
	board->syncParticles = 0;
	board->lastSyncParticles = 0.;
	board->overloadThreshold = SafeMalloc ((board->syncQuad->K + 1) * sizeof(double));
	for (x = 0; x <= board->syncQuad->K; ++x)
		board->overloadThreshold[x] = 1.;
	board->updatesPerCell = 0.;
	board->syncUpdates = 0;
	
	initializePalette (&board->palette);
	
	return board;
}

void deleteBoard (Board* board) {
	unsigned long t;
	SafeFree(board->overloadThreshold);
	deleteQuadTree (board->syncUpdateQuad);
	deleteQuadTree (board->syncQuad);
	deleteQuadTree (board->asyncQuad);
	SafeFree(board->cell);
	SafeFree(board->sync);
	SafeFree(board->syncWrite);
	SafeFree(board->watcher);
	for (t = 0; t < NumTypes; ++t)
		if (board->byType[(Type) t])
			deleteParticle (board->byType[(Type) t]);
	SafeFree(board->byType);
	SafeFree(board);
}

void writeBoardStateUnguarded (Board* board, int x, int y, State state) {
	int i;
	Type t;
	Particle *p, *pOld;
	CellWatcher *watcher;
	/* if there's a CellWatcher watching this cell, allow it to intercept & modify the write */
	i = boardIndex(board->size,x,y);
	watcher = board->watcher[i];
	if (watcher)
		state = (*watcher->intercept) (watcher, board, x, y, state);
	/* get new Type & Particle, and old Particle */
	t = StateType(state);
	p = board->byType[t];
	pOld = board->byType[StateType(board->cell[i])];
	/* decrement old count */
	if (pOld) {
		--pOld->count;
		if (pOld->synchronous)
			--board->syncParticles;
	}
	/* update cell array & quad trees */
	if (p == NULL) {
		if (t != EmptyType)  /* handle the EmptyType specially: allow it to keep its color state */
			state = EmptyState;
		updateQuadTree (board->asyncQuad, x, y, 0.);
		updateQuadTree (board->syncQuad, x, y, 0.);
	} else {
		updateQuadTree (board->asyncQuad, x, y, p->asyncFiringRate);
		updateQuadTree (board->syncQuad, x, y, p->syncFiringRate);
		/* update new count */
		++p->count;
		if (p->synchronous)
			++board->syncParticles;
	}
	board->cell[i] = state;
	if (!board->syncWrite[i])
		board->sync[i] = state;
}

void writeSyncBoardStateUnguarded (Board* board, int x, int y, State state) {
	int i;
	i = boardIndex(board->size,x,y);
	board->sync[i] = state;
	board->syncWrite[i] = 1;
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
	p->asyncFiringRate = p->synchronous ? 0. : MIN (p->totalRate, 1.);
	p->syncFiringRate = p->synchronous ? 1. : 0.;
	if (p->synchronous && p->syncPeriod == 0)
		p->syncPeriod = MAX ((int) (1./p->totalRate), 1);  /* guess a sensible default for period */
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
		case TestEQ: return lhs == rhs;
		case TestNEQ: return lhs != rhs;
		case TestGT: return lhs > rhs;
		case TestLT: return lhs < rhs;
		case TestGEQ: return lhs >= rhs;
		case TestLEQ: return lhs <= rhs;
		case TestTRUE: return 1;
		case TestFALSE: default: break;
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
				(void) attemptRule (p, rule, board, x, y, overloaded, writeBoardStateUnguarded);
				return;
			}
		}
	}
}

void evolveBoardCellSync (Board* board, int x, int y) {
	Particle* p;
	int n, wins, fails, swap, overloaded, *ruleOrder;
	double rand, remainingRate, ruleRate;
	StochasticRule* rule;
	/* do an update */
	p = readBoardParticle (board, x, y);
	if (p && p->synchronous && board->syncUpdates % p->syncPeriod == p->syncPhase) {
		overloaded = boardOverloaded (board, x, y);
		/* attempt each rule in random sequence, stopping when one succeeds */
		ruleOrder = SafeMalloc (p->nRules * sizeof(int));  /* weighted Fisher-Yates shuffle */
		for (n = 0; n < p->nRules; ++n)
			ruleOrder[n] = n;
		remainingRate = (overloaded ? p->totalOverloadRate : p->totalRate);
		for (n = wins = fails = 0; n < p->nRules && wins < p->successes && fails < p->failures; ++n) {
			if (p->shuffle) {
				rand = randomDouble() * remainingRate;
				for (swap = n; 1; ++swap) {
					rule = &p->rule[ruleOrder[swap]];
					ruleRate = (overloaded ? rule->overloadRate : rule->rate);
					rand -= ruleRate;
					if (rand < 0. || swap == p->nRules - 1)
						break;
				}
				ruleOrder[swap] = ruleOrder[n];
				remainingRate -= ruleRate;
			} else {
				rule = &p->rule[n];
				ruleRate = (overloaded ? rule->overloadRate : rule->rate);
				if (randomDouble() > ruleRate)
					continue;
			}
			if (attemptRule (p, rule, board, x, y, overloaded, writeSyncBoardStateUnguarded))
				++wins;
			else
				++fails;
		}
		SafeFree (ruleOrder);
	}
}

void syncBoard (Board* board) {
	int x, y, size, i;
	State *cell, *sync;
	unsigned char *syncWrite;
	cell = board->cell;
	sync = board->sync;
	syncWrite = board->syncWrite;
	size = board->size;
	/* update only the cells that changed */
	if (board->lastSyncParticles > 0)
		for (x = 0; x < size; ++x)
			for (y = 0; y < size; ++y) {
				i = boardIndex(size,x,y);
				if (syncWrite[i]) {
					writeBoardStateUnguarded (board, x, y, sync[i]);
					syncWrite[i] = 0;
				}
			}
	/* freeze the update queue */
	copyQuadTree (board->syncQuad, board->syncUpdateQuad);
	board->lastSyncParticles = board->syncParticles;
	board->updatesPerCellAfterLastBoardSync = board->updatesPerCell;
	board->syncUpdates++;
}

int boardOverloaded (Board* board, int x, int y) {
	int n;
	for (n = 0; n <= board->syncQuad->K; ++n)
		if (boardLocalFiringRate(board,x,y,n) > board->overloadThreshold[n])
			return 1;
	return 0;
}

int attemptRule (Particle* ruleOwner, StochasticRule* rule, Board* board, int x, int y, int overloaded, BoardWriteFunction write) {
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

void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds, double *updatesPerCell_ret, int *actualUpdates_ret, double *elapsedTimeInSeconds_ret) {
	int actualUpdates, x, y;
	double startingBoardTime, targetBoardTime, elapsedClockTime, asyncEventRate, timeToTarget, timeToNextBoardSync, pendingSyncEventsToService, timeToNextSyncEvent, timeToNextAsyncEvent;
	clock_t start, now;
	
	/* start the clocks */
	start = clock();
	actualUpdates = 0;
	elapsedClockTime = 0.;
	startingBoardTime = board->updatesPerCell;
	targetBoardTime = startingBoardTime + targetUpdatesPerCell;
	
	/* main loop */
	while (1) {
		
		/* check if realtime clock deadline reached */
		now = clock();
		elapsedClockTime = ((double) now - start) / (double) CLOCKS_PER_SEC;
		if (elapsedClockTime > maxTimeInSeconds)
			break;
		
		/* check if board clock target reached */
		timeToTarget = targetBoardTime - board->updatesPerCell;
		if (timeToTarget <= 0) {
			board->updatesPerCell = targetBoardTime;
			break;
		}
		
		/* if it's past time for an update, and the sync queue is empty, flush all synchronized updates to board */
		while (1) {
			timeToNextBoardSync = board->updatesPerCellAfterLastBoardSync + 1. - board->updatesPerCell;
			pendingSyncEventsToService = topQuadRate (board->syncUpdateQuad);
			if (pendingSyncEventsToService <= 0 && timeToNextBoardSync <= 0)
				syncBoard (board);
			else
				break;
		}
		
		/* calculate the time to the next sync event & the next async event */
		timeToNextSyncEvent = pendingSyncEventsToService > 0 ? (MAX(timeToNextBoardSync,0.) / pendingSyncEventsToService) : (2*timeToTarget);
		
		asyncEventRate = topQuadRate (board->asyncQuad);
		timeToNextAsyncEvent = asyncEventRate > 0 ? (randomExp() / asyncEventRate) : (2*timeToTarget);
		
		/* decide: sync or async? */
		if (timeToNextSyncEvent < MIN(timeToNextAsyncEvent,timeToTarget)) {
			
			board->updatesPerCell += timeToNextSyncEvent;
			
			/* sync: randomly process a pending synchronized cell update */
			sampleQuadLeaf (board->syncUpdateQuad, &x, &y);
			updateQuadTree (board->syncUpdateQuad, x, y, 0.);
			
			evolveBoardCellSync (board, x, y);
			++actualUpdates;
			
		} else if (timeToNextAsyncEvent < timeToTarget) {
			
			board->updatesPerCell += timeToNextAsyncEvent;
			
			/* async: evolve a random cell */
			sampleQuadLeaf (board->asyncQuad, &x, &y);
			
			evolveBoardCell (board, x, y);
			++actualUpdates;
			
		} else {
			/* reached target time */
			board->updatesPerCell = targetBoardTime;
			break;  /* this 'break' should actually be redundant, but this depends on an FPU equality, so... */
		}
		
	}
	/* calculate update rates */
	if (updatesPerCell_ret)
		*updatesPerCell_ret = board->updatesPerCell - startingBoardTime;
	if (actualUpdates_ret)
		*actualUpdates_ret = actualUpdates;
	if (elapsedTimeInSeconds_ret)
		*elapsedTimeInSeconds_ret = elapsedClockTime;
}
