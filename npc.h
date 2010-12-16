#ifndef NPC_INCLUDED
#define NPC_INCLUDED

#include "stringmap.h"
#include "xymap.h"

typedef struct NPC {
  char *name, *currentStage;
  Dictionary *stageDescription;
  StringMap *stageChallenges;  /* multimap from String's (stage names) to Challenge's (ways out of that stage).
				  Since it is a multimap, string keys can appear multiple times, and RBTreeEnumerate should be used to search.
				  Also, different keys can point to same value, so values must be deleted by NPC, not StringMap. */
  StringMap *stageTool;        /* map from String (stage name) to the Tool (if any) that the NPC will use during that stage */
  int invisible;  /* if true, NPC will not appear in lists visible to player */
  int sleeping;  /* if true, NPC is sleeping & inactive */
  XYCoord focus;  /* board location at which NPC is "focused" */
} NPC;

#endif /* NPC_INCLUDED */
