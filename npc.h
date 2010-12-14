#ifndef NPC_INCLUDED
#define NPC_INCLUDED

typedef struct NPC {
  Dictionary *stageDescription;
  StringMap *stageChallenges;
  char *currentStage;
} NPC;

#endif /* NPC_INCLUDED */
