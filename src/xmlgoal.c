#include <string.h>
#include "xmlgoal.h"
#include "xmlboard.h"
#include "xmlutil.h"

Goal* newGoalFromXmlNode (xmlNode *goalNode, Game *game) {
  Goal *goal, *subGoal[2];
  const char *goalTypeAttr, *enumText;
  xmlNode *node, *subGoalNode, *countNode, *areaNode, *entropyNode, *reserveNode;
  XYSet *area;
  StateSet *wallSet, *typeSet;
  int n, lazy, cached, enumState;
  Tool *tool;

  goal = NULL;
  Assert (goalNode != NULL, "newGoalFromXmlNode: null goal node");

  goalTypeAttr = ATTR(goalNode,GOALTYPE);
  if (ATTRMATCHES (goalTypeAttr, AREA_GOAL)) {
    area = newXYSet();
    for (node = goalNode->children; node; node = node->next)
      if (MATCHES (node, POS_GPARAM))
	(void) XYSetInsert (area, CHILDINT(node,X), CHILDINT(node,Y));
    subGoalNode = CHILD (goalNode, GOAL_GPARAM);
    goal = newAreaGoal (area,
			subGoalNode ? newGoalFromXmlNode (subGoalNode, game) : NULL);

  } else if (ATTRMATCHES (goalTypeAttr, CAGE_GOAL)) {
    /* currently the XML adapter sets up the EnclosuresGoal to determine whether a given state is a wall by examining Type only, ignoring Vars */
    wallSet = newStateSet();
    for (node = goalNode->children; node; node = node->next)
      if (MATCHES (node, WALL_GPARAM))
	(void) StateSetInsert (wallSet, OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE)) << TypeShift);  /* hardwired to match Type bits only */
    subGoalNode = CHILD (goalNode, GOAL_GPARAM);
    countNode = CHILD (goalNode, COUNT_GPARAM);
    areaNode = CHILD (goalNode, AREA_GPARAM);
    goal = newEnclosuresGoal (TypeMask,  /* hardwired to match Type bits only */
			      wallSet,
			      countNode ? OPTCHILDINT(countNode,MIN_GPARAM,0) : 0,
			      countNode ? OPTCHILDINT(countNode,MAX_GPARAM,0) : 0,
			      areaNode ? OPTCHILDINT(areaNode,MIN_GPARAM,0) : 0,
			      areaNode ? OPTCHILDINT(areaNode,MAX_GPARAM,0) : 0,
			      CHILD(goalNode,MOORE_GPARAM) == NULL,  /* if using Moore (as opposed to von Neumann) topology, diagonal connections are not allowed */
			      subGoalNode ? newGoalFromXmlNode (subGoalNode, game) : NULL);

  } else if (ATTRMATCHES (goalTypeAttr, POPULATION_GOAL)) {
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

  } else if (ATTRMATCHES (goalTypeAttr, CACHED_GOAL)) {
    goal = newCachedGoal (newGoalFromXmlNode (CHILD (goalNode, GOAL_GPARAM), game),
			  OPTCHILDINT (goalNode, REPS_GPARAM, 1));

  } else if (ATTRMATCHES (goalTypeAttr, AND_GOAL)) {
    lazy = CHILD (goalNode, LAZY_GPARAM) != NULL;
    cached = CHILD (goalNode, CACHED_GPARAM) != NULL;
    for (n = 0, node = goalNode->children; node; node = node->next)
      if (MATCHES (node, GOAL_GPARAM)) {
	if (n == 2) {
	  subGoal[0] = newAndGoal (subGoal[0], subGoal[1], lazy);
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

  } else if (ATTRMATCHES (goalTypeAttr, OR_GOAL)) {
    lazy = CHILD (goalNode, LAZY_GPARAM) != NULL;
    cached = CHILD (goalNode, CACHED_GPARAM) != NULL;
    for (n = 0, node = goalNode->children; node; node = node->next)
      if (MATCHES (node, GOAL_GPARAM)) {
	if (n == 2) {
	  subGoal[0] = newOrGoal (subGoal[0], subGoal[1], lazy);
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

  } else if (ATTRMATCHES (goalTypeAttr, NOT_GOAL)) {
    goal = newNotGoal (newGoalFromXmlNode (CHILD (goalNode, GOAL_GPARAM), game));

  } else if (ATTRMATCHES (goalTypeAttr, REPEAT_GOAL)) {
    goal = newRepeatGoal (newGoalFromXmlNode (CHILD (goalNode, GOAL_GPARAM), game),
			  CHILDINT (goalNode, REPS_GPARAM));

  } else if (ATTRMATCHES (goalTypeAttr, TIME_GOAL)) {
    goal = newBoardTimeGoal (OPTCHILDFLOAT(goalNode,MIN_GPARAM,0.) * game->updatesPerSecond,
			     OPTCHILDFLOAT(goalNode,MAX_GPARAM,-1.) * game->updatesPerSecond);

  } else if (ATTRMATCHES (goalTypeAttr, TESTTOOL_GOAL)) {
    tool = (Tool*) StringMapFind (game->toolByName, (const char*) CHILDSTRING (goalNode, TOOLNAME_GPARAM))->value;
    reserveNode = CHILD (goalNode, RESERVE_GPARAM);
    goal = newCheckToolGoal ((void*) tool,
			     reserveNode ? OPTCHILDFLOAT(reserveNode,MIN_GPARAM,0.) : 0.,
			     reserveNode ? OPTCHILDFLOAT(reserveNode,MAX_GPARAM,tool->maxReserve) : tool->maxReserve);

  } else if (ATTRMATCHES (goalTypeAttr, TESTEXIT_GOAL)) {
    countNode = CHILD (goalNode, COUNT_GPARAM);
    enumText = (const char*) CHILDSTRING (goalNode, STATE_GPARAM);
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

  } else if (ATTRMATCHES (goalTypeAttr, TESTGAME_GOAL)) {
    enumText = (const char*) CHILDSTRING (goalNode, STATE_GPARAM);
    enumState = -1;
    MATCHENUM (enumState, enumText, GameOn);
    MATCHENUM (enumState, enumText, GameWon);
    MATCHENUM (enumState, enumText, GameLost);
    Assert (enumState >= 0, "Attempt to find unknown game state");
    goal = newCheckGameStateGoal (enumState);

  } else if (ATTRMATCHES (goalTypeAttr, CHARGE_GOAL)) {
    tool = (Tool*) StringMapFind (game->toolByName, (const char*) CHILDSTRING (goalNode, TOOLNAME_GPARAM))->value;
    goal = newChargeToolPseudoGoal (tool,
				    OPTCHILDFLOAT(goalNode,RESERVE_GPARAM,tool->maxReserve));

  } else if (ATTRMATCHES (goalTypeAttr, SETEXIT_GOAL)) {
    enumText = (const char*) CHILDSTRING (goalNode, STATE_GPARAM);
    enumState = -1;
    MATCHENUM (enumState, enumText, PortalCounting);
    MATCHENUM (enumState, enumText, PortalUnlocked);
    MATCHENUM (enumState, enumText, PortalDestroyed);
    Assert (enumState >= 0, "Attempt to set unknown portal state");
    goal = newSetPortalStatePseudoGoal ((void*) &game->theExit,
					enumState);

  } else if (ATTRMATCHES (goalTypeAttr, SETGAME_GOAL)) {
    enumText = (const char*) CHILDSTRING (goalNode, STATE_GPARAM);
    enumState = -1;
    MATCHENUM (enumState, enumText, GameWon);
    MATCHENUM (enumState, enumText, GameLost);
    Assert (enumState >= 0, "Attempt to set unknown game state");
    goal = newSetGameStatePseudoGoal (enumState);

  } else if (ATTRMATCHES (goalTypeAttr, USETOOL_GOAL)) {
    tool = (Tool*) StringMapFind (game->toolByName, (const char*) CHILDSTRING (goalNode, TOOLNAME_GPARAM))->value;
    goal = newUseToolPseudoGoal (tool, OPTCHILDFLOAT (goalNode, DURATION_GPARAM, game->updatesPerSecond / game->goalTestsPerSecond));

  } else if (ATTRMATCHES (goalTypeAttr, PRINT_GOAL)) {
    goal = newPrintMessagePseudoGoal ((const char*) CHILDSTRING (goalNode, TEXT_GPARAM));

  } else if (ATTRMATCHES (goalTypeAttr, TRUE_GOAL)) {
    goal = newTrueGoal();

  } else if (ATTRMATCHES (goalTypeAttr, FALSE_GOAL)) {
    goal = newFalseGoal();

  } else {
    Abort ("Unknown goal type");
  }

  return goal;
}
