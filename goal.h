#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "board.h"
#include "xymap.h"
#include "statemap.h"
#include "list.h"

/* GoalType enumeration */
/* Where parent is undefined, "parent area" is entire board */
enum GoalType { Area,        /* subgoal (l) is met for given constant area */
		Enclosures,  /* define enclosures by masking every state in parent area with intData[0], treating states in ((StateSet*)tree) as walls, and allowing diagonal connections if intData[1] is true.
				Subgoal (l) is met in at least intData[2] enclosures satisfying (intData[3] <= enclosureArea <= intData[4]) */
		Once,        /* subgoal (l) has been met at least once within parent area (caches result) */
		And,         /* both subgoals (l & r) are met simultaneously within parent area */
		Or,          /* at least one of the two subgoals (l & r) is met within parent area */
		Not,         /* subgoal (l) is not met within parent area */
		Entropy,     /* mask every state in parent area with intData[0], and keep states in ((StateSet*)tree). Goal is met if remaining states satisfy (intData[0] <= population <= intData[1]) and (dblData[0] <= entropy <= dblData[1]) */
		Repeat,      /* subgoal (l) is currently met & has been met consecutively at least intData[0] times within parent area */
		True,        /* always met */
		False        /* never met */
};

/* Goal */
typedef struct Goal {
  enum GoalType goalType;  /* type */
  struct Goal *l, *r, *parent;  /* parent & subgoals */
  RBTree *tree;  /* red-black tree goal params */
  double *dblData;  /* floating-point goal params */
  unsigned long *intData;  /* integer goal params */
} Goal;

/* accessors */
int testGoalMet (Goal* goal, Board* board);
XYSet* getGoalArea (Goal* goal);  /* returns parent area; NULL means the whole board. If non-NULL, caller must call deleteXYSet() to dealloc */

/* Constructors */
/* All parameters become the responsibility of ("owned" by) the Goal & will be deleted by Goal's destructor */
Goal* newTrueGoal();
Goal* newAreaGoal (XYSet* area);
Goal* newEnclosuresGoal (State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections, Goal* subGoal);
Goal* newOnceGoal (Goal* l);
Goal* newAndGoal (Goal* l, Goal* r);
Goal* newOrGoal (Goal* l, Goal* r);
Goal* newNotGoal (Goal* g);
Goal* newEntropyGoal (State typeMask, StateSet* typeSet, unsigned int minCount, unsigned int maxCount, double minEntropy, double maxEntropy);
Goal* newRepeatGoal (Goal* subGoal, unsigned int minReps);

/* destructor */
void deleteGoal (Goal* goal);

/* helpers */
List* getEnclosures (Board* board, XYSet* area, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections);  /* returns a List of XYList's; caller must call deleteList to dealloc */

#endif /* GOAL_INCLUDED */