#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "board.h"
#include "notify.h"
#include "goal.h"

Board* newBoard (int size) {
	Board *board;
	board = SafeMalloc (sizeof (Board));
	board->byType = SafeCalloc (NumTypes, sizeof(Particle*));
	board->size = size;
	board->cell = SafeCalloc (size * size, sizeof(State));
	board->sync = SafeCalloc (size * size, sizeof(State));
	board->watcher = SafeCalloc (size * size, sizeof(CellWatcher*));
	board->syncWrite = SafeCalloc (size * size, sizeof(unsigned char));
	board->syncBin = newBinTree (size * size);
	board->asyncBin = newBinTree (size * size);
	board->syncUpdateBin = newBinTree (size * size);
	board->syncParticles = 0;
	board->lastSyncParticles = 0.;
	board->overloadThreshold = 1.;
	board->updatesPerCell = 0.;
	board->syncUpdates = 0;
	board->balloon = newVector (AbortCopyFunction, deleteBalloon, NullPrintFunction);
	board->game = NULL;

	initializePalette (&board->palette);

	return board;
}

void deleteBoard (Board* board) {
	State t;
	deleteVector (board->balloon);
	deleteBinTree (board->syncUpdateBin);
	deleteBinTree (board->syncBin);
	deleteBinTree (board->asyncBin);
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

State readBoardStateUnguardedFunction (Board* board, int x, int y) {
  return readBoardStateUnguarded(board,x,y);
}

State readSyncBoardStateUnguardedFunction (Board* board, int x, int y) {
  return readSyncBoardStateUnguarded(board,x,y);
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
	/* update cell array & bin trees */
	if (p == NULL) {
		if (t != EmptyType)  /* handle the EmptyType specially: allow it to keep its color state */
			state = EmptyState;
		updateBinTree (board->asyncBin, i, 0.);
		updateBinTree (board->syncBin, i, 0.);
	} else {
		updateBinTree (board->asyncBin, i, p->asyncFiringRate);
		updateBinTree (board->syncBin, i, p->syncFiringRate);
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

void dummyWriteBoardState (Board* board, int x, int y, State state) {
  return;
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
	p = newParticle (name);
	p->type = type;
	board->byType[type] = p;
	return p;
}

void addParticleToBoard (Particle* p, Board* board) {
	board->byType[p->type] = p;
	p->asyncFiringRate = p->synchronous ? 0. : MIN (p->rate, 1.);
	p->syncFiringRate = p->synchronous ? 1. : 0.;
	if (p->synchronous && p->syncPeriod == 0)
		p->syncPeriod = MAX ((int) (1./p->rate), 1);  /* guess a sensible default for period */
}

void evolveBoardCell (Board* board, int x, int y) {
	Particle* p;
	p = readBoardParticle (board, x, y);
	if (p) {
		/*
		 Assert (!p->synchronous, "evolveBoardCell called on async particle");
		 */
	  attemptRule (p, p->rule, board, x, y, readBoardStateUnguardedFunction, writeBoardStateUnguarded);
	}
}

void evolveBoardCellSync (Board* board, int x, int y) {
	Particle* p;
	/* do an update */
	p = readBoardParticle (board, x, y);
	if (p && p->synchronous && board->syncUpdates % p->syncPeriod == p->syncPhase) {
	  attemptRule (p, p->rule, board, x, y, readSyncBoardStateUnguardedFunction, writeSyncBoardStateUnguarded);
	}
}

void syncBoard (Board* board) {
	int x, y, size, i;
	State *sync;
	unsigned char *syncWrite;
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
	copyBinTree (board->syncBin, board->syncUpdateBin);
	board->lastSyncParticles = board->syncParticles;
	board->updatesPerCellAfterLastBoardSync = board->updatesPerCell;
	board->syncUpdates++;
}

void attemptRule (Particle* ruleOwner, ParticleRule* rule, Board* board, int x, int y, BoardReadFunction read, BoardWriteFunction write) {
  int xSrc, ySrc, xDest, yDest;
  State oldSrcState, oldDestState, state;
  LookupRuleParams *lookup;
  ModifyRuleParams *modify;
  RandomRuleParams *random;
  OverloadRuleParams *overload;
  Goal *goal;
  StateMapNode *node;

  while (rule != NULL) {
    switch (rule->type) {
    case LookupRule:
      lookup = &rule->param.lookup;
      rule = lookup->defaultRule;
      xSrc = x + lookup->loc.x;
      ySrc = y + lookup->loc.y;
      if (onBoard (board, xSrc, ySrc)) {
	state = (*read) (board,xSrc,ySrc) & lookup->mask;
	node = StateMapFind (lookup->matchRule, state);
	rule = node ? (ParticleRule*) node->value : lookup->defaultRule;
      }
      break;

    case ModifyRule:
      modify = &rule->param.modify;
      xSrc = x + modify->src.x;
      ySrc = y + modify->src.y;
      if (onBoard (board, xSrc, ySrc)) {
	xDest = x + modify->dest.x;
	yDest = y + modify->dest.y;
	if (onBoard (board, xDest, yDest)) {
	  oldSrcState = (*read) (board, xSrc, ySrc);
	  oldDestState = (*read) (board, xDest, yDest);
	  state = (oldDestState & (StateMask ^ modify->destMask))
	    | (((((oldSrcState & modify->srcMask) >> modify->rightShift) + modify->offset) << modify->leftShift) & modify->destMask);
	  (*write) (board, xDest, yDest, state);
	}
      }
      rule = modify->nextRule;
      break;

    case RandomRule:
      random = &rule->param.random;
      rule = randomDouble() < random->prob ? random->passRule : random->failRule;
      break;

    case OverloadRule:
      overload = &rule->param.overload;
      rule = boardOverloaded(board) ? overload->slowRule : overload->fastRule;
      break;

    case GoalRule:
      goal = (Goal*) rule->param.goal;
      if (goal)
	if (testGoalAtPos (goal, board->game, x, y)) {
	  deleteGoal (goal);
	  rule->param.goal = NULL;
	}
      rule = NULL;
      break;

    default:
      Abort ("Unknown rule type");
      break;
    }
  }
}

void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds, double *updatesPerCell_ret, int *actualUpdates_ret, double *elapsedTimeInSeconds_ret) {
  int actualUpdates, boardIdx, x, y;
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
			pendingSyncEventsToService = topBinRate (board->syncUpdateBin);
			if (pendingSyncEventsToService <= 0 && timeToNextBoardSync <= 0)
				syncBoard (board);
			else
				break;
		}
		
		/* calculate the time to the next sync event & the next async event */
		timeToNextSyncEvent = pendingSyncEventsToService > 0 ? (MAX(timeToNextBoardSync,0.) / pendingSyncEventsToService) : (2*timeToTarget);
		
		asyncEventRate = topBinRate (board->asyncBin);
		timeToNextAsyncEvent = asyncEventRate > 0 ? (randomExp() / asyncEventRate) : (2*timeToTarget);
		
		/* decide: sync or async? */
		if (timeToNextSyncEvent < MIN(timeToNextAsyncEvent,timeToTarget)) {
			
			board->updatesPerCell += timeToNextSyncEvent;
			
			/* sync: randomly process a pending synchronized cell update */
			sampleBinLeaf (board->syncUpdateBin, &boardIdx);
			updateBinTree (board->syncUpdateBin, boardIdx, 0.);

			x = boardIndexToX (board->size, boardIdx);
			y = boardIndexToY (board->size, boardIdx);
			
			evolveBoardCellSync (board, x, y);
			++actualUpdates;
			
		} else if (timeToNextAsyncEvent < timeToTarget) {
			
			board->updatesPerCell += timeToNextAsyncEvent;
			
			/* async: evolve a random cell */
			sampleBinLeaf (board->asyncBin, &boardIdx);

			x = boardIndexToX (board->size, boardIdx);
			y = boardIndexToY (board->size, boardIdx);
			
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

void updateBalloons (Board *board, double duration) {
  void **ptr, **write;
  Balloon *b;
  for (write = ptr = board->balloon->begin; ptr != board->balloon->end; ++ptr) {
    b = (Balloon*) *ptr;
    if ((b->timeToLive -= duration) > 0) {
      *(write++) = b;
      b->z += b->zInc * duration;
      b->size *= pow (b->sizeMul, duration);
      b->opacity *= pow (b->opacityMul, duration);
    } else if (resetBalloon (b))
      *(write++) = b;
    else
      deleteBalloon (b);
  }
  board->balloon->end = write;
  qsort (board->balloon->begin, VectorSize(board->balloon), sizeof (void*), BalloonCompare);
}
