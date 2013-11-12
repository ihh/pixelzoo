#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "board.h"
#include "notify.h"
#include "mersenne.h"
#include "balloon.h"

/* helpers */
State readVarFromLoc (Board *board, BoardReadFunction read, const LocalOffset *loc, int xOrigin, int yOrigin, int zOrigin, unsigned int shift, State mask, State *reg, int *isOnBoard, int *boardIndex_ret);

/* for attemptRule() debugging: max rule depth, and rule trace */
#define MaxRuleDepth 100

Board* newBoard (int size, int depth) {
  Board *board;
  board = SafeMalloc (sizeof (Board));
  board->byType = SafeCalloc (NumTypes, sizeof(Particle*));
  board->protoTable = NULL;
  board->subRule = newStringMap (AbortCopyFunction, deleteParticleRule, NullPrintFunction);
  board->size = size;
  board->depth = depth;
  board->cell = SafeCalloc (size * size * depth, sizeof(State));
  board->sync = SafeCalloc (size * size * depth, sizeof(State));
  board->watcher = SafeCalloc (size * size * depth, sizeof(CellWatcher*));
  board->syncWrite = SafeCalloc (size * size * depth, sizeof(unsigned char));
  board->meta = SafeCalloc (size * size * depth, sizeof(char*));
  board->syncBin = newBinTree (size * size * depth);
  board->asyncBin = newBinTree (size * size * depth);
  board->syncUpdateBin = newBinTree (size * size * depth);
  board->syncParticles = 0;
  board->lastSyncParticles = 0;
  board->microticks = 0;
  board->microticksAtNextBoardSync = 0;
  board->updateCount = 0;
#ifdef PIXELZOO_DEBUG
  board->targetUpdateCount = -1;
  board->logRules = 0;
#endif /* PIXELZOO_DEBUG */
  board->syncUpdates = 0;
  board->balloon = newVector (AbortCopyFunction, deleteBalloon, NullPrintFunction);
  board->rng = newRNG();
  board->rngReleased = 1;
  board->sampledNextAsyncEventTime = board->sampledNextSyncEventTime = 0;
  board->moveLog = board->moveQueue = NULL;
  board->winType = MaxType;
  board->winVarOffset = 0;
  board->winVarWidth = BitsPerVars;
  board->incumbentWinVar = -1;
  board->challengerWinVar = -1;

  initializePalette (&board->palette);

  return board;
}

void deleteBoard (Board* board) {
  long i;
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
  for (i = 0; i < board->size * board->size * board->depth; ++i)
    SafeFreeOrNull (board->meta[i]);
  SafeFree(board->meta);
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

State readBoardStateUnguardedFunction (Board* board, int x, int y, int z) {
  return readBoardStateUnguarded(board,x,y,z);
}

State readSyncBoardStateUnguardedFunction (Board* board, int x, int y, int z) {
  return readSyncBoardStateUnguarded(board,x,y,z);
}

void writeBoardMove (Board* board, int x, int y, int z, State state) {
  if (onBoard(board,x,y,z)) {
    writeBoardStateUnguardedFunction (board, x, y, z, state);
    if (board->moveLog)
      (void) MoveListAppend (board->moveLog, board->microticks, board->updateCount, x, y, z, state);
    board->sampledNextAsyncEventTime = board->sampledNextSyncEventTime = 0;
  }
}

void replayBoardMove (Board* board) {
  Move *nextMove;

#ifdef PIXELZOO_DEBUG
  if (board->targetUpdateCount >= 0 && board->targetUpdateCount <= board->updateCount)
    return;
#endif /* PIXELZOO_DEBUG */

  nextMove = MoveListFront (board->moveQueue);

  board->microticks = nextMove->t;
  board->sampledNextAsyncEventTime = 0;
  board->sampledNextSyncEventTime = 0;

#ifdef PIXELZOO_DEBUG
  Warn ("Servicing move at (%d,%d,%d)", nextMove->x, nextMove->y, nextMove->z);
#endif /* PIXELZOO_DEBUG */

  writeBoardMove (board, nextMove->x, nextMove->y, nextMove->z, nextMove->state);  /* auto-increments updateCount */
  (void) MoveListShift (board->moveQueue);
  deleteMove (nextMove);
  ++board->updateCount;
}

void logBoardMoves (Board* board) {
  if (board->moveLog == NULL)
    board->moveLog = newMoveList();
}

void writeBoardStateUnguardedFunction (Board* board, int x, int y, int z, State state) {
  int i;
  Type t;
  Particle *p, *pOld;
  CellWatcher *watcher;
  /* if there's a CellWatcher watching this cell, allow it to intercept & modify the write */
  i = boardIndex(board->size,x,y,z);
  watcher = board->watcher[i];
  if (watcher)
    state = (*watcher->intercept) (watcher, board, x, y, z, state);
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
  if (board->logRules) {
    const char* debugStr = boardTypeVarsDebugString(board,state);
    Warn ("Write: %d %d %d %llx  %s", x, y, z, state, debugStr);
    SafeFree ((char*) debugStr);
  }
#endif /* PIXELZOO_DEBUG */

}

void writeSyncBoardStateUnguardedFunction (Board* board, int x, int y, int z, State state) {
  int i;
  i = boardIndex(board->size,x,y,z);
  board->sync[i] = state;
  board->syncWrite[i] = 1;

#ifdef PIXELZOO_DEBUG
  Warn ("writeSyncBoardStateUnguardedFunction: %d %d %d %llx", x, y, z, state);
#endif /* PIXELZOO_DEBUG */

}

void dummyWriteBoardStateFunction (Board* board, int x, int y, int z, State state) {
  return;
}

PaletteIndex readBoardColor (Board* board, int x, int y, int z) {
  Particle *p;
  State s;
  Type t;
  PaletteIndex c;
  c = 0;  /* default to black */
  s = readBoardState (board, x, y, z);
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

void evolveBoardCell (Board* board, int x, int y, int z) {
  Particle* p;
#ifdef PIXELZOO_DEBUG
  if (board->targetUpdateCount >= 0 && board->targetUpdateCount <= board->updateCount)
    return;
#endif /* PIXELZOO_DEBUG */
  p = readBoardParticle (board, x, y, z);
  if (p) {
    /*
      Assert (!p->synchronous, "evolveBoardCell called on async particle");
    */
    attemptRule (p, p->rule, board, x, y, z, readBoardStateUnguardedFunction, writeBoardStateUnguardedFunction);
  }
  ++board->updateCount;
}

void evolveBoardCellSync (Board* board, int x, int y, int z) {
  Particle* p;
#ifdef PIXELZOO_DEBUG
  if (board->targetUpdateCount >= 0 && board->targetUpdateCount <= board->updateCount)
    return;
#endif /* PIXELZOO_DEBUG */
  /* do an update */
  p = readBoardParticle (board, x, y, z);
  if (p && p->synchronous && board->syncUpdates % p->syncPeriod == p->syncPhase) {
    /* change "readBoardStateUnguardedFunction" to "readSyncBoardStateUnguardedFunction"
       to allow synchronous Particle's to preview the upcoming sync state of the Board.
       This allows sync Particle's modify operations to be cumulative
       (useful e.g. for accumulator rules that count the neighborhood;
       note however that Conway's Life - an obvious application for accumulator rules -
       requires simultaneity, so Conway modifies *can't* be cumulative - they must be instantaneous/atomic)
    */
    attemptRule (p, p->rule, board, x, y, z, readBoardStateUnguardedFunction, writeSyncBoardStateUnguardedFunction);
  }
  ++board->updateCount;
}

void syncBoard (Board* board) {
  int x, y, z, size, depth, i;
  State *sync;
  unsigned char *syncWrite;
#ifdef PIXELZOO_DEBUG
  if (board->targetUpdateCount >= 0 && board->targetUpdateCount <= board->updateCount)
    return;
#endif /* PIXELZOO_DEBUG */
  sync = board->sync;
  syncWrite = board->syncWrite;
  size = board->size;
  depth = board->depth;
  /* update only the cells that changed */
  if (board->lastSyncParticles > 0)
    for (x = 0; x < size; ++x)
      for (y = 0; y < size; ++y)
	for (z = 0; z < depth; ++z) {
	  i = boardIndex(size,x,y,z);
	  if (syncWrite[i]) {
	    writeBoardStateUnguardedFunction (board, x, y, z, sync[i]);
	    syncWrite[i] = 0;
	  }
	}
  /* freeze the update queue */
  copyBinTree (board->syncBin, board->syncUpdateBin);
  board->lastSyncParticles = board->syncParticles;
  ++board->syncUpdates;
  ++board->updateCount;
}

State readVarFromLoc (Board *board, BoardReadFunction read, const LocalOffset *loc, int x, int y, int z, unsigned int shift, State mask, State *reg, int *isOnBoard, int *boardIndex_ret) {
  State state, var;
  Type type;
  int xLoc, yLoc, zLoc;
  Particle *particle;

  var = 0;
  if (loc->xyzAreRegisters) {
    xLoc = x + (Int64) reg[loc->x];
    yLoc = y + (Int64) reg[loc->y];
    zLoc = z + (Int64) reg[loc->z];
  } else {
    xLoc = x + loc->x;
    yLoc = y + loc->y;
    zLoc = z + loc->z;
  }

  if (onBoard (board, xLoc, yLoc, zLoc)) {
    *isOnBoard = 1;
    if (boardIndex_ret)
      *boardIndex_ret = boardIndex (board->size, xLoc, yLoc, zLoc);
    state = (*read) (board, xLoc, yLoc, zLoc);
    if (shift < BitsPerState) {
      /* read-write var */
      var = (state & mask) >> shift;
    } else {
      /* read-only var */
      type = StateType (state);
      particle = board->byType[type];
      shift -= BitsPerState;  /* convert shift into an offset into the read-only bitvector */
      if (particle)
	var = (particle->readOnly[shift / BitsPerState] & mask) >> (shift % BitsPerState);
    }
  } else
    *isOnBoard = 0;

  return var;
}

void attemptRule (Particle* ruleOwner, ParticleRule* rule, Board* board, int x, int y, int z, BoardReadFunction read, BoardWriteFunction write) {
  int xDest, yDest, zDest, tracePos, r, isOnBoard, srcBoardIndex, destBoardIndex;
  State currentSrcState, currentDestState, newDestState, offset, var;
  Type type;
  LookupRuleParams *lookup;
  CompareRuleParams *compare;
  ModifyRuleParams *modify;
  DeliverRuleParams *deliver;
  RandomRuleParams *random;
  LoadRuleParams *load;
  StateMapNode *lookupNode;
  MessageRuleMapNode *dispatchNode;
  ParticleRule *ruleTrace[MaxRuleDepth];
  State reg[NumberOfRegisters + 1], regVal;

#ifdef PIXELZOO_DEBUG
  if (board->logRules)
    Warn ("attemptRule @(%d,%d,%d)", x, y, z);
#endif

  reg[NumberOfRegisters] = 0;  /* this allows us to use register -1 as a default zero */
  tracePos = 0;
  while (rule != NULL) {

    Assert (tracePos < MaxRuleDepth, "Rules nested too deep");
    ruleTrace[tracePos++] = rule;

    switch (rule->type) {
    case LookupRule:

      lookup = &rule->param.lookup;
      var = readVarFromLoc (board, read, &lookup->loc, x, y, z, lookup->shift, lookup->mask, reg, &isOnBoard, NULL);
#ifdef PIXELZOO_DEBUG
      if (board->logRules)
	Warn ("Lookup @(%d,%d,%d) %s(%d,%d,%d) &%llx >>%d = %d",
	      x, y, z,
	      lookup->loc.xyzAreRegisters ? "*" : "",
	      lookup->loc.x, lookup->loc.y, lookup->loc.z,
	      lookup->mask, lookup->shift, var);
#endif /* PIXELZOO_DEBUG */
      if (isOnBoard) {
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

    case CompareRule:
      compare = &rule->param.compare;
      var = readVarFromLoc (board, read, &compare->loc, x, y, z, compare->shift, compare->mask, reg, &isOnBoard, NULL);
      regVal = reg[compare->registerIndex];
#ifdef PIXELZOO_DEBUG
      if (board->logRules)
        Warn ("Compare @(%d,%d,%d) %s(%d,%d,%d) &%llx >>%d = %d <=> reg[%d] = %d",
	      x, y, z,
	      compare->loc.xyzAreRegisters ? "*" : "",
	      compare->loc.x, compare->loc.y, compare->loc.z,
	      compare->mask, compare->shift, var,
	      compare->registerIndex, regVal);
#endif /* PIXELZOO_DEBUG */
      rule = var < regVal
	? compare->ltRule
	: (var > regVal
	   ? compare->gtRule
	   : compare->eqRule);
      break;

    case ModifyRule:
      modify = &rule->param.modify;
      if (modify->dest.xyzAreRegisters) {
	xDest = x + (Int64) reg[modify->dest.x];
	yDest = y + (Int64) reg[modify->dest.y];
	zDest = z + (Int64) reg[modify->dest.z];
      } else {
	xDest = x + modify->dest.x;
	yDest = y + modify->dest.y;
	zDest = z + modify->dest.z;
      }

      if (onBoard (board, xDest, yDest, zDest)) {
	var = readVarFromLoc (board, read, &modify->src, x, y, z, modify->rightShift, modify->srcMask, reg, &isOnBoard, &srcBoardIndex);
	if (isOnBoard) {
	  offset = modify->offsetIsRegister ? reg[modify->offset] : modify->offset;

#ifdef PIXELZOO_DEBUG
	  if (board->logRules) {
	    if (modify->offsetIsRegister)
	      Warn ("Modify @(%d,%d,%d) %s(%d,%d,%d) &%llx >>%d +reg[%d]=%llx <<%d &%llx %s(%d,%d,%d)",
		    x, y, z,
		    modify->src.xyzAreRegisters ? "*" : "",
		    modify->src.x, modify->src.y, modify->src.z,
		    modify->srcMask, modify->rightShift,
		    modify->offset, offset,
		    modify->leftShift, modify->destMask,
		    modify->dest.xyzAreRegisters ? "*" : "",
		    modify->dest.x, modify->dest.y, modify->dest.z);
	    else
	      Warn ("Modify @(%d,%d,%d) %s(%d,%d,%d) &%llx >>%d +%llx <<%d &%llx %s(%d,%d,%d)",
		    x, y, z,
		    modify->src.xyzAreRegisters ? "*" : "",
		    modify->src.x, modify->src.y, modify->src.z,
		    modify->srcMask, modify->rightShift,
		    offset,
		    modify->leftShift, modify->destMask,
		    modify->dest.xyzAreRegisters ? "*" : "",
		    modify->dest.x, modify->dest.y, modify->dest.z);
	  }
#endif /* PIXELZOO_DEBUG */

	  currentDestState = (*read) (board, xDest, yDest, zDest);
	  newDestState = (currentDestState & (StateMask ^ modify->destMask))
	    | (((var + offset) << modify->leftShift) & modify->destMask);
	  (*write) (board, xDest, yDest, zDest, newDestState);

	  destBoardIndex = boardIndex (board->size, xDest, yDest, zDest);
	  switch (modify->modifyType) {
	  case EatModify:
	    SafeFreeOrNull (board->meta[srcBoardIndex]);
	    board->meta[srcBoardIndex] = board->meta[destBoardIndex];
	    board->meta[srcBoardIndex] = NULL;
	    break;
	  case KillModify:
	    SafeFreeOrNull (board->meta[destBoardIndex]);
	    board->meta[destBoardIndex] = NULL;
	    break;
	  case ConserveModify:
	  default:
	    break;
	  }
	}
      }
      rule = modify->nextRule;
      break;

    case DeliverRule:
      deliver = &rule->param.deliver;
      /* DeliverRule hands over control: update x, y, z, ruleOwner and rule */
      if (deliver->recipient.xyzAreRegisters) {
	x += (Int64) reg[deliver->recipient.x];
	y += (Int64) reg[deliver->recipient.y];
	z += (Int64) reg[deliver->recipient.z];
      } else {
	xDest = x + deliver->recipient.x;
	yDest = y + deliver->recipient.y;
	zDest = z + deliver->recipient.z;
      }

      if (onBoard (board, x, y, z)) {
	currentSrcState = (*read) (board, x, y, z);
	type = StateType (currentSrcState);
	ruleOwner = board->byType[type];
	dispatchNode = ruleOwner ? MessageRuleMapFind (ruleOwner->dispatch, deliver->message) : (MessageRuleMapNode*) NULL;
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

    case GotoRule:
      rule = rule->param.gotoLabel;
#ifdef PIXELZOO_DEBUG
      if (board->logRules)
	if (rule->label)
	  Warn ("Goto %s", rule->label);
#endif
      break;

    case LoadRule:
      load = &rule->param.load;
      for (r = 0; r < load->n; ++r) {
	reg[load->reg[r]] = load->state[r];
#ifdef PIXELZOO_DEBUG
	if (board->logRules)
	  Warn ("Load reg[%d] = %llx", load->reg[r], load->state[r]);
#endif /* PIXELZOO_DEBUG */
      }
      rule = load->nextRule;
      break;

    default:
      Abort ("Unknown rule type");
      break;
    }
  }
}

void evolveBoard (Board* board, int64_Microticks targetElapsedMicroticks, double maxElapsedTimeInSeconds, int64_Microticks *elapsedMicroticks_ret, int *cellUpdates_ret, double *elapsedTimeInSeconds_ret) {
  int cellUpdates, boardIdx, x, y, z;
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

       Reaching the target time corresponds to ceding control to the Game loop, at which point Game events (UI moves) occur.
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

      syncBoard (board);
      continue;

    } else if (board->microticksAtNextSyncEvent <= deadline2) {  /* do a synchronized cell update? */
			
      board->microticks = board->microticksAtNextSyncEvent;
      board->sampledNextAsyncEventTime = 0;
      board->sampledNextSyncEventTime = 0;
			
      sampleBinLeaf (board->syncUpdateBin, board->rng, &boardIdx);
      updateBinTree (board->syncUpdateBin, boardIdx, 0);

      x = boardIndexToX (board->size, boardIdx);
      y = boardIndexToY (board->size, boardIdx);
      z = boardIndexToZ (board->size, boardIdx);

      evolveBoardCellSync (board, x, y, z);
      ++cellUpdates;
      continue;

    } else if (board->microticksAtNextAsyncEvent <= deadline1) {  /* do an asynchronous cell update? */
			
      board->microticks = board->microticksAtNextAsyncEvent;
      board->sampledNextAsyncEventTime = 0;
      board->sampledNextSyncEventTime = 0;
			
      sampleBinLeaf (board->asyncBin, board->rng, &boardIdx);

      x = boardIndexToX (board->size, boardIdx);
      y = boardIndexToY (board->size, boardIdx);
      z = boardIndexToZ (board->size, boardIdx);
			
      evolveBoardCell (board, x, y, z);
      ++cellUpdates;
      continue;

    } else if (microticksAtNextMove <= deadline0) {  /* take a move from the queue? */

      if (nextMove->u >= 0)
	Assert (nextMove->u == board->updateCount, "Move count does not match Board update count");
      replayBoardMove (board);
      continue;
			
    } else {  /* reached target time */

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

const char* boardTypeVarsDebugString (Board *board, State state) {
  char *str;
  Type type;
  Particle *particle;
  int size, nvars;
  ListNode *node;
  VarsDescriptor *vars;
  type = StateType(state);
  particle = board->byType[type];
  size = strlen(particle->name) + 2;
  for (node = particle->vars->head; node; node = node->next) {
    vars = (VarsDescriptor*) node->value;
    size += strlen(vars->name) + 18;  /* 16 is enough characters to display 2^48 in decimal, plus a couple padding characters */
  }
  str = SafeMalloc(size);
  sprintf(str,"%s",particle->name);
  nvars = 0;
  for (node = particle->vars->head; node; node = node->next) {
    vars = (VarsDescriptor*) node->value;
    sprintf(str+strlen(str),"%s%s=%lld",nvars ? "," : "(",vars->name,StateVar(state,vars->offset,vars->width));
    ++nvars;
  }
  if (nvars)
    sprintf(str+strlen(str),")");
  return str;
}

int boardWinner (Board *board) {
  RBTree *rb;
  int x, y, z, var, bestVar, bestCount, incumbentExtinct;
  State s;
  RBNode *node;
  bestVar = -1;
  bestCount = -1;
  rb = newRBTree (IntCompare, IntCopy, IntCopy, IntDelete, IntDelete, NullPrintFunction, NullPrintFunction);
  for (x = 0; x < board->size; ++x)
    for (y = 0; y < board->size; ++y)
      for (z = 0; z < board->depth; ++z) {
	s = readBoardState (board, x, y, z);
	if (StateType(s) == board->winType) {
	  var = (int) StateVar(s,board->winVarOffset,board->winVarWidth);
	  node = RBTreeFind (rb, &var);
	  if (node == NULL)
	    node = RBTreeInsert (rb, IntNew(var), IntNew(0));
	  if (++*(int*)node->value > bestCount) {
	    bestCount = *(int*)node->value;
	    bestVar = var;
	  }
	}
      }
  incumbentExtinct = RBTreeFind(rb,&board->incumbentWinVar) == NULL;
  deleteRBTree (rb);
  return (incumbentExtinct && bestVar == board->challengerWinVar)
    ? board->challengerWinVar
    : board->incumbentWinVar;
}

int boardContestParticleCount (Board *board, int id) {
  int x, y, z, var, count;
  State s;
  count = 0;
  for (x = 0; x < board->size; ++x)
    for (y = 0; y < board->size; ++y)
      for (z = 0; z < board->depth; ++z) {
	s = readBoardState (board, x, y, z);
	if (StateType(s) == board->winType) {
	  var = (int) StateVar(s,board->winVarOffset,board->winVarWidth);
	  if (var == id)
	    ++count;
	  }
      }
  return count;
}
