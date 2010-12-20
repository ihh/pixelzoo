#ifndef NPC_INCLUDED
#define NPC_INCLUDED

#include "stringmap.h"
#include "xymap.h"

/*
  NPC's are basically pushdown state machines with Tool's (and status messages) assocated with the nodes,
  and Challenge's associated with the edges.
  To avoid overloading the word "State" (also used to indicate cellular automaton state),
  and since NPC's are effectively quest-givers,
  we speak of an NPC being at a particular "stage" (c.f. level) instead.
  One way that NPC's can transition between stages is by being sent messages,
  either via explicit player choices (selecting an option from a list of available choices),
  or by being sent messages from other NPC's (as those NPC's make particular transitions).
 */
typedef struct NPC {
  /* Tool's and Challenge's */
  StringMap *myTool;  /* map from tool names to Tool's used by this NPC (unavailable to player). This is the container that "owns" Tool's */
  List *myChallenge;  /* list of Challenge's belonging to this NPC. This is the container that "owns" Challenge's */

  /* Name lookups for Tool's and Challenge's */
  StringMap *stageChallenges;  /* multimap from String's (stage names) to Challenge's (ways out of that stage).
				  Since it is a multimap, string keys can appear multiple times, and RBTreeEnumerate should be used to search.
				  Also, different keys can point to same value, so values must be deleted by NPC (via myChallenge), not this StringMap. */

  StringMap *stageTool;        /* multimap from String (stage name) to Tool's (if any) that the NPC will use during that stage.
				  Since it is a multimap, string keys can appear multiple times, and RBTreeEnumerate should be used to search.
				  Also, different keys can point to same value, so values must be deleted by NPC (via myTool), not this StringMap. */

  /* textual descriptions presented to player */
  Dictionary *stageDescription;  /* text the player sees (as the NPC's status message) when the NPC is at a particular stage */
  Dictionary *messageChoiceText; /* text the player sees (as a prompt from the NPC) when a given message is available */

  int invisible;  /* if true, NPC will not appear in lists visible to player */

  /* state (OK, "stage") of this NPC */
  char *name, *currentStage;
  StringVector *stageStack;    /* for Gosub/Return transitions */

  int awake;  /* if true, NPC is sleeping & inactive */

  XYCoord focus;  /* board location at which NPC is "focused", e.g. placed by player.
		     TODO: develop other ways to (re)focus NPCs, e.g. related to the way player completed a goal
		  */

  /* available & incoming messages */
  StringSet *playerChoices;      /* messages the player can send to this NPC */
  StringSet *incomingMessages;   /* messages to be processed (including player choices & messages from NPCs) */

} NPC;

#endif /* NPC_INCLUDED */
