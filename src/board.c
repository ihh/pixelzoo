#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "board.h"
#include "notify.h"
#include "goal.h"
#include "mersenne.h"

/* uncomment the #define to log all board writes to stderr */
/*
#define PIXELZOO_DEBUG
*/

/* for attemptRule() debugging: max rule depth, and rule trace */
#define MaxRuleDepth 100

Board* newBoard (int size) {
  Board *board;
  board = SafeMalloc (sizeof (Board));
  board->byType = SafeCalloc (NumTypes, sizeof(Particle*));
  board->protoTable = NULL;
  board->subRule = newStringMap (AbortCopyFunction, deleteParticleRule, NullPrintFunction);
  board->size = size;
  board->cell = SafeCalloc (size * size, sizeof(State));
  board->sync = SafeCalloc (size * size, sizeof(State));
  board->watcher = SafeCalloc (size * size, sizeof(CellWatcher*));
  board->syncWrite = SafeCalloc (size * size, sizeof(unsigned char));
  board->syncBin = newBinTree (size * size);
  board->asyncBin = newBinTree (size * size);
  board->syncUpdateBin = newBinTree (size * size);
  board->syncParticles = 0;
  board->lastSyncParticles = 0;
  board->microticks = 0;
  board->microticksAtNextBoardSync = 0;
  board->updateCount = 0;
  board->syncUpdates = 0;
  board->balloon = newVector (AbortCopyFunction, deleteBalloon, NullPrintFunction);
  board->game = NULL;
  board->rng = newRNG();
  board->rngReleased = 1;
  board->sampledNextAsyncEventTime = board->sampledNextSyncEventTime = 0;
  board->moveLog = board->moveQueue = NULL;

  initializePalette (&board->palette);

  return board;
}

void deleteBoard (Board* board) {
  State t;
  if (board->moveQueue)
    deleteMoveList (board->moveQueue);
  if (board->moveLog)
    deleteMoveList (board->moveLog);
  deleteRNG (board->rng);
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
  deleteStringMap (board->subRule);
  if (board->protoTable)
    deleteProtoTable (board->protoTable);
  SafeFree(board->byType);
  SafeFree(board);
}

State readBoardStateUnguardedFunction (Board* board, int x, int y) {
  return readBoardStateUnguarded(board,x,y);
}

State readSyncBoardStateUnguardedFunction (Board* board, int x, int y) {
  return readSyncBoardStateUnguarded(board,x,y);
}

void writeBoardMove (Board* board, int x, int y, State state) {
  if (onBoard(board,x,y)) {
    writeBoardStateUnguardedFunction (board, x, y, state);
    if (board->moveLog)
      (void) MoveListAppend (board->moveLog, board->microticks, board->updateCount, x, y, state);
    ++board->updateCount;
    board->sampledNextAsyncEventTime = board->sampledNextSyncEventTime = 0;
  }
}

void replayBoardMove (Board* board) {
  Move *nextMove;

  nextMove = MoveListFront (board->moveQueue);

  board->microticks = nextMove->t;
  board->sampledNextAsyncEventTime = 0;
  board->sampledNextSyncEventTime = 0;

#ifdef PIXELZOO_DEBUG
  fprintf (stderr, "Servicing move at (%d,%d)\n", nextMove->x, nextMove->y);
#endif /* PIXELZOO_DEBUG */

  writeBoardMove (board, nextMove->x, nextMove->y, nextMove->state);  /* auto-increments updateCount */
  (void) MoveListShift (board->moveQueue);
  deleteMove (nextMove);
}

void logBoardMoves (Board* board) {
  if (board->moveLog == NULL)
    board->moveLog = newMoveList();
}

void writeBoardStateUnguardedFunction (Board* board, int x, int y, State state) {
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
    updateBinTree (board->asyncBin, i, 0);
    updateBinTree (board->syncBin, i, 0);
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

#ifdef PIXELZOO_DEBUG
  fprintf (stderr, "writeBoardStateUnguardedFunction: %d %d %llx\n", x, y, state);
#endif /* PIXELZOO_DEBUG */

}

void writeSyncBoardStateUnguardedFunction (Board* board, int x, int y, State state) {
  int i;
  i = boardIndex(board->size,x,y);
  board->sync[i] = state;
  board->syncWrite[i] = 1;

#ifdef PIXELZOO_DEBUG
  fprintf (stderr, "writeSyncBoardStateUnguardedFunction: %d %d %llx\n", x, y, state);
#endif /* PIXELZOO_DEBUG */

}

void dummyWriteBoardStateFunction (Board* board, int x, int y, State state) {
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
  p->asyncFiringRate = p->synchronous ? 0 : MIN (p->rate, PowerOfTwoClosestToOneMillion);
  p->syncFiringRate = p->synchronous ? PowerOfTwoClosestToOneMillion : 0;
  if (p->synchronous && p->syncPeriod == 0) {
    p->syncPeriod = (int) (.5 + (1. / IntMillionthsToFloat(p->rate)));  /* use a sensible default for period */
    p->syncPeriod = MAX (p->syncPeriod, 1);  /* period must not be zero */
  }
}

void evolveBoardCell (Board* board, int x, int y) {
  Particle* p;
  p = readBoardParticle (board, x, y);
  if (p) {
    /*
      Assert (!p->synchronous, "evolveBoardCell called on async particle");
    */
    attemptRule (p, p->rule, board, x, y, readBoardStateUnguardedFunction, writeBoardStateUnguardedFunction);
  }
}

void evolveBoardCellSync (Board* board, int x, int y) {
  Particle* p;
  /* do an update */
  p = readBoardParticle (board, x, y);
  if (p && p->synchronous && board->syncUpdates % p->syncPeriod == p->syncPhase) {
    /* change "readBoardStateUnguardedFunction" to "readSyncBoardStateUnguardedFunction"
       to allow synchronous Particle's to preview the upcoming sync state of the Board.
       This allows sync Particle's modify operations to be cumulative
       (useful e.g. for accumulator rules that count the neighborhood;
       note however that Conway's Life - an obvious application for accumulator rules -
       requires simultaneity, so Conway modifies *can't* be cumulative - they must be instantaneous/atomic)
    */
    attemptRule (p, p->rule, board, x, y, readBoardStateUnguardedFunction, writeSyncBoardStateUnguardedFunction);
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
	  writeBoardStateUnguardedFunction (board, x, y, sync[i]);
	  syncWrite[i] = 0;
	}
      }
  /* freeze the update queue */
  copyBinTree (board->syncBin, board->syncUpdateBin);
  board->lastSyncParticles = board->syncParticles;
  board->syncUpdates++;
}

void attemptRule (Particle* ruleOwner, ParticleRule* rule, Board* board, int x, int y, BoardReadFunction read, BoardWriteFunction write) {
  int xSrc, ySrc, xDest, yDest, tracePos, r;
  State currentSrcState, currentDestState, newDestState, offset, var;
  Type type;
  Particle *particle;
  unsigned int shift;
  LookupRuleParams *lookup;
  ModifyRuleParams *modify;
  DeliverRuleParams *deliver;
  RandomRuleParams *random;
  LoadRuleParams *load;
  Goal *goal;
  StateMapNode *lookupNode;
  RBNode *dispatchNode;
  ParticleRule *ruleTrace[MaxRuleDepth];
  State reg[NumberOfRegisters];

  tracePos = 0;
  while (rule != NULL) {

    Assert (tracePos < MaxRuleDepth, "Rules nested too deep");
    ruleTrace[tracePos++] = rule;

    switch (rule->type) {
    case LookupRule:
      lookup = &rule->param.lookup;
      if (lookup->loc.xyAreRegisters) {
	xSrc = x + (Int64) reg[lookup->loc.x];
	ySrc = y + (Int64) reg[lookup->loc.y];
      } else {
	xSrc = x + lookup->loc.x;
	ySrc = y + lookup->loc.y;
      }
      if (onBoard (board, xSrc, ySrc) && lookup->matchRule != NULL) {

	currentSrcState = (*read) (board, xSrc, ySrc);
	shift = lookup->shift;
	if (shift < BitsPerState) {
	  /* read-write var */
	  var = (currentSrcState & lookup->mask) >> shift;
	} else {
	  /* read-only var */
	  type = StateType (currentSrcState);
	  particle = board->byType[type];
	  shift -= BitsPerState;  /* convert shift into an offset into the read-only bitvector */
	  var = particle
	    ? ((particle->readOnly[shift / BitsPerState] & lookup->mask) >> (shift % BitsPerState))
	    : 0;
	}

	lookupNode = StateMapFind (lookup->matchRule, var);
	rule = lookupNode
	  ? (ParticleRule*) lookupNode->value
	  : ((lookup->lowRule && StateMapIsBeforeFirst (lookup->matchRule, var))
	     ? lookup->lowRule
	     : (lookup->highRule && (StateMapIsAfterLast (lookup->matchRule, var))
		? lookup->highRule
		: lookup->defaultRule));
      } else
	rule = lookup->defaultRule;
      break;

    case ModifyRule:
      modify = &rule->param.modify;
      if (modify->src.xyAreRegisters) {
	xSrc = x + (Int64) reg[modify->src.x];
	ySrc = y + (Int64) reg[modify->src.y];
      } else {
	xSrc = x + modify->src.x;
	ySrc = y + modify->src.y;
      }

      if (onBoard (board, xSrc, ySrc)) {
	if (modify->dest.xyAreRegisters) {
	  xDest = x + (Int64) reg[modify->dest.x];
	  yDest = y + (Int64) reg[modify->dest.y];
	} else {
	  xDest = x + modify->dest.x;
	  yDest = y + modify->dest.y;
	}

	if (onBoard (board, xDest, yDest)) {

	  currentSrcState = (*read) (board, xSrc, ySrc);
	  shift = modify->rightShift;
	  if (shift < BitsPerState) {
	    /* read-write var */
	    var = (currentSrcState & modify->srcMask) >> modify->rightShift;
	  } else {
	    /* read-only var */
	    type = StateType (currentSrcState);
	    particle = board->byType[type];
	    shift -= BitsPerState;  /* convert shift into an offset into the read-only bitvector */
	    var = particle
	      ? ((particle->readOnly[shift / BitsPerState] & modify->srcMask) >> (shift % BitsPerState))
	      : 0;
	  }

	  offset = modify->offsetIsRegister ? reg[modify->offset] : modify->offset;

	  currentDestState = (*read) (board, xDest, yDest);
	  newDestState = (currentDestState & (StateMask ^ modify->destMask))
	    | (((var + offset) << modify->leftShift) & modify->destMask);
	  (*write) (board, xDest, yDest, newDestState);
	}
      }
      rule = modify->nextRule;
      break;

    case DeliverRule:
      deliver = &rule->param.deliver;
      /* DeliverRule hands over control: update x, y, ruleOwner and rule */
      x += deliver->recipient.x;
      y += deliver->recipient.y;
      if (onBoard (board, x, y)) {
	currentSrcState = (*read) (board, x, y);
	type = StateType (currentSrcState);
	ruleOwner = board->byType[type];
	dispatchNode = ruleOwner ? RBTreeFind (ruleOwner->dispatch, &deliver->message) : (RBNode*) NULL;
	rule = (ruleOwner && dispatchNode) ? ((ParticleRule*) dispatchNode->value) : ((ParticleRule*) NULL);
      } else {
	ruleOwner = NULL;
	rule = NULL;
      }
      break;

    case RandomRule:
      random = &rule->param.random;
      rule = rngRandomProb(board->rng) < random->prob ? random->passRule : random->failRule;
      break;

    case GoalRule:
      goal = (Goal*) rule->param.goal;
      if (goal && board->game)
	if (testGoalAtPos (goal, board->game, x, y)) {
	  deleteGoal (goal);
	  rule->param.goal = NULL;
	}
      rule = NULL;
      break;

    case GotoRule:
      rule = rule->param.gotoLabel;
      break;

    case LoadRule:
      load = &rule->param.load;
      for (r = 0; r < load->n; ++r)
	reg[load->reg[r]] = load->state[r];
      rule = load->nextRule;
      break;

    default:
      Abort ("Unknown rule type");
      break;
    }
  }
}

void evolveBoard (Board* board, int64_Microticks targetElapsedMicroticks, double maxElapsedTimeInSeconds, int64_Microticks *elapsedMicroticks_ret, int *cellUpdates_ret, double *elapsedTimeInSeconds_ret) {
  int cellUpdates, boardIdx, x, y;
  int64_Microticks microticksAtStart, microticksAtTarget, microticksAtNextMove, microticksToNextBoardSync;
  int64_Microticks deadline0, deadline1, deadline2, deadline3;
  double elapsedClockTime;
  int64_Microhurtz asyncEventRate, pendingSyncEvents;
  clock_t start, now;
  Move *nextMove;

  /* check parameters */
  Assert (targetElapsedMicroticks > 0, "The board clock must advance during the simulation");

  /* start the clocks */
  start = clock();
  elapsedClockTime = 0.;

  cellUpdates = 0;
  microticksAtStart = board->microticks;
  microticksAtTarget = microticksAtStart + targetElapsedMicroticks;

  /* main loop */
  while (1) {

    /* check if realtime clock deadline reached; if so, bail out as soon as the minimum one microtick of Board time has elapsed */
    now = clock();
    elapsedClockTime = ((double) now - start) / (double) CLOCKS_PER_SEC;
    if (board->microticks > microticksAtStart && elapsedClockTime > maxElapsedTimeInSeconds && maxElapsedTimeInSeconds >= 0)
      break;
		
    /* check if board clock target already passed */
    if (board->microticks > microticksAtTarget) {
      board->microticks = microticksAtTarget;
      break;
    }

    /* calculate the Board time to the next sync event & the next async event */
    pendingSyncEvents = topBinRate(board->syncUpdateBin) / PowerOfTwoClosestToOneMillion;
    if (!board->sampledNextSyncEventTime) {
      board->microticksAtNextSyncEvent = microticksAtTarget + 1;  /* by default, postpone the next event to some indefinite point in the future */
      if (pendingSyncEvents > 0) {
	microticksToNextBoardSync = board->microticksAtNextBoardSync - board->microticks;
	board->microticksAtNextSyncEvent = board->microticks + (MAX (microticksToNextBoardSync - 1, 0) / pendingSyncEvents);  /* try to evenly spread sync events in time */
	board->sampledNextSyncEventTime = 1;
      }
    }

    if (!board->sampledNextAsyncEventTime) {
      board->microticksAtNextAsyncEvent = microticksAtTarget + 1;  /* by default, postpone the next event to some indefinite point in the future */
      asyncEventRate = topBinRate (board->asyncBin);
      if (asyncEventRate > 0) {
	board->microticksAtNextAsyncEvent = board->microticks + rngRandomWait(board->rng,asyncEventRate);
	board->sampledNextAsyncEventTime = 1;
	board->rngReleased = 0;
      }
    }

    /* poll move queue */
    microticksAtNextMove = microticksAtTarget + 1;  /* by default, assume the next move is at some indefinite point in the future */
    nextMove = NULL;
    if (board->moveQueue)
      if (!MoveListEmpty (board->moveQueue)) {
	nextMove = MoveListFront (board->moveQueue);
	microticksAtNextMove = nextMove->t;
      }

    /* Figure out what the next event is, and service it.
       In the event that multiple events coincide at the same discrete timepoint, the precedence of event service is as follows:

          BoardSynchronization > SynchronousParticleUpdates > AsynchronousParticleUpdates > UpdatesFromMoveQueue > TargetTimeReached

       Reaching the target time corresponds to ceding control to the Game loop, at which point Game events (UI moves & Goal triggers) occur.
       This cession of control is the lowest-priority event to be serviced.
       The second lowest-priority is the move queue, which is used to play back Game events.
    */
    deadline0 = microticksAtTarget;  /* deadline for popping moves from move queue */
    deadline1 = MIN (deadline0, microticksAtNextMove);   /* deadline for async particle updates */
    deadline2 = MIN (deadline1, board->microticksAtNextAsyncEvent);  /* deadline for sync particle updates */
    deadline3 = MIN (deadline2, board->microticksAtNextSyncEvent);  /* deadline for board syncs */

    /* First check the update count of the next move */
    if (nextMove && nextMove->u == board->updateCount) {
 
      Assert (nextMove->t > microticksAtTarget || nextMove->t == deadline3, "Move with update count pre-empted scheduled Board events");
      replayBoardMove (board);
      continue;

    } else if (board->microticksAtNextBoardSync <= deadline3 && pendingSyncEvents <= 0) {  /* do a board sync? (won't happen until all pending sync events are processed) */

      board->microticks = board->microticksAtNextBoardSync;
      board->sampledNextAsyncEventTime = 0;
      board->sampledNextSyncEventTime = 0;

      board->microticksAtNextBoardSync += PowerOfTwoClosestToOneMillion;

#ifdef PIXELZOO_DEBUG
      fprintf (stderr, "Synchronizing board\n");
#endif /* PIXELZOO_DEBUG */

      syncBoard (board);
      ++board->updateCount;
      continue;

    } else if (board->microticksAtNextSyncEvent <= deadline2) {  /* do a synchronized cell update? */
			
      board->microticks = board->microticksAtNextSyncEvent;
      board->sampledNextAsyncEventTime = 0;
      board->sampledNextSyncEventTime = 0;
			
      sampleBinLeaf (board->syncUpdateBin, board->rng, &boardIdx);
      updateBinTree (board->syncUpdateBin, boardIdx, 0);

      x = boardIndexToX (board->size, boardIdx);
      y = boardIndexToY (board->size, boardIdx);

#ifdef PIXELZOO_DEBUG
      fprintf (stderr, "Updating synchronized cell at (%d,%d)\n", x, y);
#endif /* PIXELZOO_DEBUG */
			
      evolveBoardCellSync (board, x, y);
      ++cellUpdates;
      ++board->updateCount;
      continue;

    } else if (board->microticksAtNextAsyncEvent <= deadline1) {  /* do an asynchronous cell update? */
			
      board->microticks = board->microticksAtNextAsyncEvent;
      board->sampledNextAsyncEventTime = 0;
      board->sampledNextSyncEventTime = 0;
			
      sampleBinLeaf (board->asyncBin, board->rng, &boardIdx);

      x = boardIndexToX (board->size, boardIdx);
      y = boardIndexToY (board->size, boardIdx);

#ifdef PIXELZOO_DEBUG
      fprintf (stderr, "Updating asynchronous cell at (%d,%d)\n", x, y);
#endif /* PIXELZOO_DEBUG */
			
      evolveBoardCell (board, x, y);
      ++cellUpdates;
      ++board->updateCount;
      continue;

    } else if (microticksAtNextMove <= deadline0) {  /* take a move from the queue? */

      if (nextMove->u >= 0)
	Assert (nextMove->u == board->updateCount, "Move count does not match Board update count");
      replayBoardMove (board);
      continue;
			
    } else {  /* reached target time */

#ifdef PIXELZOO_DEBUG
      fprintf (stderr, "Reached target time\n");
#endif /* PIXELZOO_DEBUG */

      board->microticks = microticksAtTarget;
      break;
    }

  }
  /* calculate update rates */
  if (elapsedMicroticks_ret)
    *elapsedMicroticks_ret = board->microticks - microticksAtStart;
  if (cellUpdates_ret)
    *cellUpdates_ret = cellUpdates;
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

void boardReleaseRandomNumbers (Board *board) {
  board->sampledNextAsyncEventTime = 0;
  board->sampledNextSyncEventTime = 0;
  board->rngReleased = 1;
}
