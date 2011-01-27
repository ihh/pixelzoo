#include <string.h>
#include "xmlgoal.h"
#include "xmlboard.h"
#include "xmlutil.h"
#include "xmlgame.h"

/* private builder method prototypes */
Balloon* newBalloonFromXmlNode (xmlNode* node);

/* method defs */
Goal* newGoalFromXmlNode (xmlNode *goalParentNode, Game *game) {
  Goal *goal, *subGoal[2];
  const char *enumText;
  xmlNode *goalNode, *node, *subGoalNode, *countNode, *areaNode, *entropyNode, *reserveNode, *balloonNode;
  XYSet *area;
  StateSet *wallSet, *typeSet;
  int n, lazy, cached, enumState;
  Tool *tool;

  goal = NULL;

  Assert (goalParentNode != NULL, "newGoalFromXmlNode: null goal parent node");

  goalNode = goalParentNode->children;
  while (goalNode != NULL && goalNode->type != XML_ELEMENT_NODE)
    goalNode = goalNode->next;
  Assert (goalNode != NULL && goalNode->type == XML_ELEMENT_NODE, "newGoalFromXmlNode: null goal node");

  if (MATCHES (goalNode, AREA_GOAL)) {
    area = newXYSet();
    for (node = goalNode->children; node; node = node->next)
      if (MATCHES (node, POS_GPARAM))
	(void) XYSetInsert (area, CHILDINT(node,X), CHILDINT(node,Y));
    subGoalNode = CHILD (goalNode, GOAL_GPARAM);
    goal = newAreaGoal (area,
			subGoalNode ? newGoalFromXmlNode (subGoalNode, game) : NULL);

  } else if (MATCHES (goalNode, CAGE_GOAL)) {
    /* currently the XML adapter sets up the EnclosuresGoal to determine whether a given state is a wall by examining Type only, ignoring Vars */
    wallSet = newStateSet();
    for (node = goalNode->children; node; node = node->next)
      if (MATCHES (node, WALL_GPARAM))
	(void) StateSetInsert (wallSet, OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE)) << TypeShift);  /* hardwired to match Type bits only */
    subGoalNode = CHILD (goalNode, GOAL_GPARAM);
    countNode = CHILD (goalNode, COUNT_GPARAM);
    areaNode = CHILD (goalNode, POINTS_GPARAM);
    goal = newEnclosuresGoal (TypeMask,  /* hardwired to match Type bits only */
			      wallSet,
			      countNode ? OPTCHILDINT(countNode,MIN_GPARAM,0) : 0,
			      countNode ? OPTCHILDINT(countNode,MAX_GPARAM,0) : 0,
			      areaNode ? OPTCHILDINT(areaNode,MIN_GPARAM,0) : 0,
			      areaNode ? OPTCHILDINT(areaNode,MAX_GPARAM,0) : 0,
			      CHILD(goalNode,MOORE_GPARAM) == NULL,  /* if using Moore (as opposed to von Neumann) topology, diagonal connections are not allowed */
			      subGoalNode ? newGoalFromXmlNode (subGoalNode, game) : NULL);

  } else if (MATCHES (goalNode, POPULATION_GOAL)) {
    typeSet = newStateSet();
    for (node = goalNode->children; node; node = node->next)
      if (MATCHES (node, DECTYPE_GPARAM))
	(void) StateSetInsert (typeSet, decToSignedLongLong ((const char*) getNodeContent(node)));
      else if (MATCHES (node, HEXTYPE_GPARAM))
	(void) StateSetInsert (typeSet, hexToUnsignedLongLong ((const char*) getNodeContent(node)));
    Assert (RBTreeSize(typeSet) > 0, "In " XMLPREFIX(POPULATION_GOAL) " goal: no specified types to count");
    countNode = CHILD (goalNode, COUNT_GPARAM);
    entropyNode = CHILD (goalNode, ENTROPY_GPARAM);
    goal = newEntropyGoal (typeSet,
			   OPTCHILDHEX(goalNode,MASK_GPARAM,StateMask),
			   countNode ? OPTCHILDINT(countNode,MIN_GPARAM,0) : 0,
			   countNode ? OPTCHILDINT(countNode,MAX_GPARAM,0) : 0,
			   entropyNode ? OPTCHILDFLOAT(entropyNode,MIN_GPARAM,0) : 0,
			   entropyNode ? OPTCHILDFLOAT(entropyNode,MAX_GPARAM,-1) : -1);

  } else if (MATCHES (goalNode, CACHED_GOAL)) {
    goal = newCachedGoal (newGoalFromXmlNode (CHILD (goalNode, GOAL_GPARAM), game),
			  OPTCHILDINT (goalNode, REPS_GPARAM, 1));

  } else if (MATCHES (goalNode, AND_GOAL)) {
    lazy = CHILD (goalNode, LAZY_GPARAM) != NULL;
    cached = CHILD (goalNode, CACHE_GPARAM) != NULL;
    for (n = 0, node = goalNode->children; node; node = node->next)
      if (MATCHES (node, GOAL_GPARAM)) {
	if (n == 2) {
	  subGoal[0] = newAndGoal (subGoal[0], subGoal[1], lazy);
	  setSubgoalParents (subGoal[0]);   /* we must do this here since setSubgoalParents is not recursive, so these subgoals we've shunted down a level will otherwise not get a call */
	  --n;
	}
	subGoal[n] = newGoalFromXmlNode (node, game);
	if (cached)
	  subGoal[n] = newCachedGoal (subGoal[n], 1);
	++n;
      }
    if (n < 2)
      subGoal[1] = newTrueGoal();
    goal = newAndGoal (subGoal[0], subGoal[1], lazy);

  } else if (MATCHES (goalNode, OR_GOAL)) {
    lazy = CHILD (goalNode, LAZY_GPARAM) != NULL;
    cached = CHILD (goalNode, CACHE_GPARAM) != NULL;
    for (n = 0, node = goalNode->children; node; node = node->next)
      if (MATCHES (node, GOAL_GPARAM)) {
	if (n == 2) {
	  subGoal[0] = newOrGoal (subGoal[0], subGoal[1], lazy);
	  setSubgoalParents (subGoal[0]);   /* we must do this here since setSubgoalParents is not recursive, so these subgoals we've shunted down a level will otherwise not get a call */
	  --n;
	}
	subGoal[n] = newGoalFromXmlNode (node, game);
	if (cached)
	  subGoal[n] = newCachedGoal (subGoal[n], 1);
	++n;
      }
    if (n < 2)
      subGoal[1] = newFalseGoal();
    goal = newOrGoal (subGoal[0], subGoal[1], lazy);

  } else if (MATCHES (goalNode, NOT_GOAL)) {
    goal = newNotGoal (newGoalFromXmlNode (CHILD (goalNode, GOAL_GPARAM), game));

  } else if (MATCHES (goalNode, REPEAT_GOAL)) {
    goal = newRepeatGoal (newGoalFromXmlNode (CHILD (goalNode, GOAL_GPARAM), game),
			  CHILDINT (goalNode, REPS_GPARAM));

  } else if (MATCHES (goalNode, TIME_GOAL)) {
    goal = newBoardTimeGoal (OPTCHILDFLOAT(goalNode,MIN_GPARAM,0.) * game->updatesPerSecond,
			     OPTCHILDFLOAT(goalNode,MAX_GPARAM,-1.) * game->updatesPerSecond);

  } else if (MATCHES (goalNode, TESTTOOL_GOAL)) {
    tool = (Tool*) StringMapFind (game->toolByName, (const char*) CHILDSTRING (goalNode, TOOLNAME_GPARAM))->value;
    reserveNode = CHILD (goalNode, RESERVE_GPARAM);
    goal = newCheckToolGoal ((void*) tool,
			     reserveNode ? OPTCHILDFLOAT(reserveNode,MIN_GPARAM,0.) : 0.,
			     reserveNode ? OPTCHILDFLOAT(reserveNode,MAX_GPARAM,tool->maxReserve) : tool->maxReserve);

  } else if (MATCHES (goalNode, TESTEXIT_GOAL)) {
    countNode = CHILD (goalNode, COUNT_GPARAM);
    enumText = (const char*) CHILDSTRING (goalNode, EXSTATE_GPARAM);
    enumState = -1;
    MATCHENUM (enumState, enumText, PortalWaiting);
    MATCHENUM (enumState, enumText, PortalCounting);
    MATCHENUM (enumState, enumText, PortalUnlocked);
    MATCHENUM (enumState, enumText, PortalDestroyed);
    Assert (enumState >= 0, "Attempt to find unknown portal state");
    goal = newCheckPortalGoal ((void*) &game->theExit,
			       enumState,
			       countNode ? OPTCHILDINT(countNode,MIN_GPARAM,0.) : 0.,
			       countNode ? OPTCHILDINT(countNode,MAX_GPARAM,-1.) : -1.);

  } else if (MATCHES (goalNode, TESTGAME_GOAL)) {
    enumText = (const char*) CHILDSTRING (goalNode, GSTATE_GPARAM);
    enumState = -1;
    MATCHENUM (enumState, enumText, GameOn);
    MATCHENUM (enumState, enumText, GameWon);
    MATCHENUM (enumState, enumText, GameLost);
    Assert (enumState >= 0, "Attempt to find unknown game state");
    goal = newCheckGameStateGoal (enumState);

  } else if (MATCHES (goalNode, CHARGE_GOAL)) {
    tool = (Tool*) StringMapFind (game->toolByName, (const char*) CHILDSTRING (goalNode, TOOLNAME_GPARAM))->value;
    goal = newChargeToolPseudoGoal (tool,
				    OPTCHILDFLOAT(goalNode,RESERVE_GPARAM,tool->maxReserve));

  } else if (MATCHES (goalNode, SETEXIT_GOAL)) {
    enumText = (const char*) CHILDSTRING (goalNode, EXSTATE_GPARAM);
    enumState = -1;
    MATCHENUM (enumState, enumText, PortalCounting);
    MATCHENUM (enumState, enumText, PortalUnlocked);
    MATCHENUM (enumState, enumText, PortalDestroyed);
    Assert (enumState >= 0, "Attempt to set unknown portal state");
    goal = newSetPortalStatePseudoGoal ((void*) &game->theExit,
					enumState);

  } else if (MATCHES (goalNode, SETGAME_GOAL)) {
    enumText = (const char*) CHILDSTRING (goalNode, GSTATE_GPARAM);
    enumState = -1;
    MATCHENUM (enumState, enumText, GameWon);
    MATCHENUM (enumState, enumText, GameLost);
    Assert (enumState >= 0, "Attempt to set unknown game state");
    goal = newSetGameStatePseudoGoal (enumState);

  } else if (MATCHES (goalNode, USETOOL_GOAL)) {
    tool = newToolFromXmlNode (CHILD (goalNode, TOOL_GPARAM));
    goal = newUseToolPseudoGoal (tool, OPTCHILDFLOAT (goalNode, DURATION_GPARAM, game->updatesPerSecond / game->goalTestsPerSecond));

  } else if (MATCHES (goalNode, PRINT_GOAL)) {
    goal = newPrintMessagePseudoGoal ((const char*) CHILDSTRING (goalNode, MESSAGE_GPARAM));

  } else if (MATCHES (goalNode, BALLOON_GOAL)) {
    balloonNode = CHILD (goalNode, BALLOON_GPARAM);
    goal = newPlaceBalloonPseudoGoal (balloonNode ? newBalloonFromXmlNode (balloonNode) : NULL);

  } else if (MATCHES (goalNode, TRUE_GOAL)) {
    goal = newTrueGoal();

  } else if (MATCHES (goalNode, FALSE_GOAL)) {
    goal = newFalseGoal();

  } else if (MATCHES (goalNode, MAYBE_GOAL)) {
    goal = newMaybeGoal (CHILDFLOAT (goalNode, PROB_GPARAM));

  } else {
    Abort ("Unknown goal type");
  }

  if (goal)
    setSubgoalParents (goal);

  return goal;
}

Balloon* newBalloonFromXmlNode (xmlNode* balloonNode) {
  xmlNode *locNode;
  HSB24 color;
  Balloon *balloon;
  locNode = CHILD (balloonNode, POS);
  color = OPTCHILDINT (balloonNode, COLOR, OPTCHILDHEX (balloonNode, HEXCOLOR, HSB24White));
  balloon = newProtoBalloon ((char*) CHILDSTRING (balloonNode, TEXT),
			     locNode ? OPTCHILDINT(locNode,X,0) : 0,
			     locNode ? OPTCHILDINT(locNode,Y,0) : 0,
			     ConvertHsb24ToPaletteIndex (color),
			     OPTCHILDFLOAT (balloonNode, SIZE, 1.),
			     OPTCHILDFLOAT (balloonNode, TTL, DefaultBalloonTTL),
			     OPTCHILDFLOAT (balloonNode, RISE, DefaultBalloonRise),
			     OPTCHILDFLOAT (balloonNode, ZOOM, DefaultBalloonZoom),
			     OPTCHILDFLOAT (balloonNode, FADE, DefaultBalloonFade),
			     OPTCHILDFLOAT (balloonNode, RATE, 1.));
  if (CHILD (balloonNode, PERSIST) != NULL)
    balloon->reset = balloon;
  return balloon;
}
