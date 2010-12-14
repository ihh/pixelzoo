#ifndef NPC_INCLUDED
#define NPC_INCLUDED

#include "stringmap.h"

typedef struct NPC {
  char *name, *currentStage;
  Dictionary *stageDescription;
  StringMap *stageChallenges;  /* map from String's to Challenge's */
  int invisible;  /* if true, NPC will not appear in lists visible to player */
} NPC;

#endif /* NPC_INCLUDED */
