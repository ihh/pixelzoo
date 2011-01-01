#include <string.h>
#include "xmlgoal.h"
#include "xmlutil.h"

Goal* newGoalFromXmlNode (xmlNode *goalNode) {
  Goal *goal;
  const char *goalTypeNode;
  Assert (goalNode != NULL, "newGoalFromXmlNode: null goal node");
  goal = NULL;
  goalTypeNode = ATTR(goalNode,GOALTYPE);
  if (ATTRMATCHES (goalTypeNode, AREA_GOAL)) {
    /* TODO: write me! */
  } else if (ATTRMATCHES (goalTypeNode, CAGE_GOAL)) {
    /* TODO: write me! */
  } else if (ATTRMATCHES (goalTypeNode, COUNT_GOAL)) {
    /* TODO: write me! */
  } else if (ATTRMATCHES (goalTypeNode, ONCE_GOAL)) {
    /* TODO: write me! */
  } else if (ATTRMATCHES (goalTypeNode, AND_GOAL)) {
    /* TODO: write me! */
  } else if (ATTRMATCHES (goalTypeNode, OR_GOAL)) {
    /* TODO: write me! */
  } else if (ATTRMATCHES (goalTypeNode, NOT_GOAL)) {
    /* TODO: write me! */
  } else if (ATTRMATCHES (goalTypeNode, REPEAT_GOAL)) {
    goal = newRepeatGoal (newGoalFromXmlNode(CHILD(goalNode,GOAL_GPARAM)),
			  CHILDINT(goalNode,REPS_GPARAM));
  } else if (ATTRMATCHES (goalTypeNode, TRUE_GOAL)) {
    goal = newTrueGoal();
  } else if (ATTRMATCHES (goalTypeNode, FALSE_GOAL)) {
    goal = newFalseGoal();
  }
  return goal;
}
