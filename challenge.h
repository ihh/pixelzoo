#ifndef CHALLENGE_INCLUDED
#define CHALLENGE_INCLUDED

#include <stdlib.h>
#include "goal.h"
#include "list.h"

/* Challenge */
typedef struct Challenge {
  /* source stage */
  char *stage;
  /* goal */
  Goal *goal;
  double goalTestRate;  /* mean number of tests per second */
  double delayBetweenAwards;  /* time, in seconds, to wait after awarding before re-testing */
  int timesAwarded, maxTimesAwarded;
  clock_t lastAwardTime;
  /* rewards */
  char *rewardText;  /* print this text */
  char *tool;        /* award this tool (or, top it up) */
  char *nextStage;   /* destination stage */
} Challenge;

#endif /* CHALLENGE_INCLUDED */
