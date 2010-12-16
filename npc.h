#ifndef NPC_INCLUDED
#define NPC_INCLUDED

#include "stringmap.h"
#include "xymap.h"

typedef struct NPC {
  char *name, *currentStage;
  Dictionary *stageDescription;
  StringMap *myTool;  /* map from tool names to Tool's used by this NPC (unavailable to player). This is the container that "owns" Tool's */
  List *myChallenge;  /* list of Challenge's belonging to this NPC. This is the container that "owns" Challenge's */

  StringMap *stageChallenges;  /* multimap from String's (stage names) to Challenge's (ways out of that stage).
				  Since it is a multimap, string keys can appear multiple times, and RBTreeEnumerate should be used to search.
				  Also, different keys can point to same value, so values must be deleted by NPC (via myChallenge), not this StringMap. */

  StringMap *stageTool;        /* multimap from String (stage name) to Tool's (if any) that the NPC will use during that stage.
				  Since it is a multimap, string keys can appear multiple times, and RBTreeEnumerate should be used to search.
				  Also, different keys can point to same value, so values must be deleted by NPC (via myTool), not this StringMap. */

  StringVector *stageStack;    /* for Gosub/Return transitions */

  StringSet *playerChoices;      /* messages the player can send to this NPC */
  StringSet *incomingMessages;   /* messages to be processed (including player choices & messages from NPCs) */
  Dictionary *messageChoiceText; /* text the player sees (as a prompt from the NPC) when a given message is available */

  int invisible;  /* if true, NPC will not appear in lists visible to player */
  int awake;  /* if true, NPC is sleeping & inactive */

  XYCoord focus;  /* board location at which NPC is "focused", e.g. placed by player.
		     TODO: add other ways to (re)focus NPCs, e.g. related to the way player completed a goal
		  */

} NPC;

#endif /* NPC_INCLUDED */
