#include "challenge.h"

Challenge* newChallenge() {
  Challenge* chal;
  chal = SafeMalloc (sizeof (Challenge));
  chal->goal = NULL;
  chal->stage = chal->nextStage = NULL;
  chal->tool = chal->rewardText = NULL;
  chal->goalTestRate = 1.;
  chal->delayBetweenAwards = 0.;
  chal->timesAwarded = 0;
  chal->maxTimesAwarded = 1;
  return chal;
}

void deleteChallenge (Challenge *challenge) {
  SafeFreeOrNull (challenge->stage);
  SafeFreeOrNull (challenge->rewardText);
  SafeFreeOrNull (challenge->tool);
  SafeFreeOrNull (challenge->nextStage);
  if (challenge->goal)
    deleteGoal (challenge->goal);
  SafeFree (challenge);
}
