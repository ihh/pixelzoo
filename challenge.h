#ifndef CHALLENGE_INCLUDED
#define CHALLENGE_INCLUDED

#include <stdlib.h>
#include <time.h>
#include "goal.h"
#include "stringmap.h"
#include "xymap.h"

/* Challenge */
typedef struct Challenge {
  /* source stages */
  StringSet* stage;
  /* goal */
  Goal *goal;
  double goalTestRate;  /* mean number of tests per second */
  double delayBetweenAwards;  /* time, in seconds, to wait after awarding before re-testing */
  int timesAwarded, maxTimesAwarded;
  clock_t lastAwardTime;
  /* rewards. Any of the strings can be null, signifying no effect */
  StringIntMap *scoreDelta;  /* deltas to coins, xp, alignment, etc (can be positive, negative or zero) */
  char *rewardText;  /* print this text */
  char *tool;        /* award this tool (or, top it up) */
  char *achievement; /* unlock this achievement */
  char *npcSleep, *npcWake;  /* names of NPC's to inactivate/activate */
  char *nextStage;   /* destination stage */
} Challenge;

Challenge* newChallenge();
void deleteChallenge (Challenge *challenge);

#endif /* CHALLENGE_INCLUDED */
