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
  /* rewards (or more generally, effects) of completing the goal.
     Any of the pointers can be NULL, signifying no effect of that type
  */
  StringIntMap *scoreDelta, *toolReserveDelta;  /* deltas to scores (coins, xp, alignment, etc) or tool reserves. Can be positive, negative or zero. Tools with positive reserve deltas will be auto-added */
  char *cutSceneText, *commentText;  /* print text, in the following order
					1. If cutSceneText!=NULL, it is printed and the game is paused until the player reads it (i.e. a cut-scene, duh). The game then resumes.
					2. If commentText!=NULL, it is printed in a speech balloon for a few seconds while the game continues.
				     */
  StringVector *achievments; /* unlock these achievments */
  Dictionary *npcSendMessage, *npcAddChoice, *npcRemoveChoice;  /* map from names of NPC's to messages they should be sent, or choices that should be made available (or unavailable) to the player */
  enum NpcTransition { NoTransition, Goto, Gosub, Return, Sleep } npcTransition;  /* type of transition this NPC should make */
  char *nextStage;   /* destination stage (for Goto/Gosub transitions) */
} Challenge;

Challenge* newChallenge();
void deleteChallenge (Challenge *challenge);

#endif /* CHALLENGE_INCLUDED */
