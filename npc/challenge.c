#include "challenge.h"
#include "npc.h"

Challenge* newChallenge() {
  Challenge* chal;
  chal = SafeMalloc (sizeof (Challenge));

  /* preconditions */
  chal->stage = newStringSet();
  chal->goal = NULL;
  chal->goalTestRate = 1.;
  chal->delayBetweenAwards = 0.;
  chal->timesAwarded = 0;
  chal->maxTimesAwarded = 1;

  /* rewards */
  chal->scoreDelta = chal->toolReserveDelta = NULL;
  chal->cutSceneText = chal->commentText = chal->nextStage = NULL;
  chal->achievments = NULL;
  chal->npcSendMessage = chal->npcAddChoice = chal->npcRemoveChoice = NULL;
  chal->npcTransition = NoTransition;

  return chal;
}

void deleteChallenge (Challenge *challenge) {
  SafeFreeOrNull (challenge->cutSceneText);
  SafeFreeOrNull (challenge->commentText);
  SafeFreeOrNull (challenge->nextStage);

  deleteStringSet (challenge->stage);

  if (challenge->goal)
    deleteGoal (challenge->goal);

  if (challenge->scoreDelta)
    deleteStringIntMap (challenge->scoreDelta);
  if (challenge->toolReserveDelta)
    deleteStringIntMap (challenge->toolReserveDelta);

  if (challenge->achievments)
    deleteStringVector (challenge->achievments);

  if (challenge->npcSendMessage)
    deleteDictionary (challenge->npcSendMessage);
  if (challenge->npcAddChoice)
    deleteDictionary (challenge->npcAddChoice);
  if (challenge->npcRemoveChoice);
  deleteDictionary (challenge->npcRemoveChoice);

  SafeFree (challenge);
}
