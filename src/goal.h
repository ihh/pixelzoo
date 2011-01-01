#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "board.h"
#include "xymap.h"
#include "statemap.h"
#include "stringmap.h"
#include "list.h"

/* GoalType enumeration */
/* When a goal has no parent, "parent area" is entire board */
enum GoalType { Area,        /* subgoal (l) is met for given constant area
			      */

		Enclosures,  /* define enclosures by masking every state in parent area with intData[0], treating masked-states in ((StateSet*)tree) as walls,
				and allowing diagonal connections if intData[1] is true.
				Goal is met if subgoal (l) is met by N enclosures satisfying (intData[2] <= enclosureArea <= intData[3]),
				where (intData[4] <= N <= intData[5]) */

		Entropy,     /* mask every state in parent area with intData[0], and keep the subset of masked-states that are in ((StateSet*)tree).
				Goal is met if remaining states satisfy (intData[1] <= population <= intData[2]) and (dblData[0] <= entropy in bits <= dblData[1]) */

		Once,        /* subgoal (l) has been met at least once within parent area (has the effect of caching evaluation of l) */
		And,         /* both subgoals (l & r) are met simultaneously within parent area */
		Or,          /* at least one of the two subgoals (l & r) is met within parent area */
		Not,         /* subgoal (l) is not met within parent area */

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
  StringVector *stringData;  /* string params */
} Goal;

/* accessors */
int testGoalMet (Goal* goal, Board *board);
XYSet* getGoalArea (Goal* goal);  /* returns parent area; NULL means the whole board. If non-NULL, caller must call deleteXYSet() to dealloc */

/* Constructors */
/* All parameters become the responsibility of ("owned" by) the Goal & will be deleted by Goal's destructor */
Goal* newTrueGoal();
Goal* newAreaGoal (XYSet* area);
Goal* newEnclosuresGoal (State wallMask,
			 StateSet* wallSet,
			 unsigned long minNumEnclosures,
			 unsigned long maxNumEnclosures,
			 unsigned long minEnclosureArea,
			 unsigned long maxEnclosureArea,
			 unsigned char allowDiagonalConnections,
			 Goal* subGoal);
Goal* newOnceGoal (Goal* l);
Goal* newAndGoal (Goal* l, Goal* r);
Goal* newOrGoal (Goal* l, Goal* r);
Goal* newNotGoal (Goal* g);
Goal* newEntropyGoal (State typeMask, StateSet* typeSet, unsigned long minCount, unsigned long maxCount, double minEntropy, double maxEntropy);
Goal* newRepeatGoal (Goal* subGoal, unsigned long minReps);

/* destructor */
void deleteGoal (Goal* goal);

/* helpers */
List* getEnclosures (Board* board, XYSet* area, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections);  /* returns a List of XYList's; caller must call deleteList to dealloc */

#endif /* GOAL_INCLUDED */
