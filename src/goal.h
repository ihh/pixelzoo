#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "board.h"
#include "xymap.h"
#include "statemap.h"
#include "stringmap.h"
#include "list.h"

/* GoalType enumeration */
/* When a goal has no parent, "parent area" is entire board */
enum GoalType { AreaGoal,        /* subgoal (l) is met for given constant area
				  */

		EnclosuresGoal,  /* define enclosures by masking every state in parent area with intData[0], treating masked-states in ((StateSet*)tree) as walls,
				    and allowing diagonal connections if intData[1] is true.
				    Goal is met if subgoal (l) is met by N enclosures satisfying (intData[2] <= enclosureArea <= intData[3]),
				    where (intData[4] <= N <= intData[5]) */

		EntropyGoal,     /* for every state S in parent area, if (S & TypeMask) is in ((StateSet*)tree), then add (S & intData[0]) to set.
				    Goal is met if states in set satisfy (intData[1] <= population <= intData[2]) and (dblData[0] <= entropy in bits <= dblData[1]) */

		OnceGoal,        /* subgoal (l) has been met at least once within parent area (has the effect of caching evaluation of l) */
		AndGoal,         /* both subgoals (l & r) are met simultaneously within parent area. Both l & r are guaranteed to be evaluated */
		OrGoal,          /* at least one of the two subgoals (l & r) is met within parent area. Both l & r are guaranteed to be evaluated */
		LazyAndGoal,     /* both subgoals (l & r) are met simultaneously within parent area. r is not evaluated unless l evaluates true */
		LazyOrGoal,      /* at least one of the two subgoals (l & r) is met within parent area. r is not evaluated unless l evaluates false */
		NotGoal,         /* subgoal (l) is not met within parent area */

		RepeatGoal,      /* subgoal (l) is currently met & has been met consecutively at least intData[0] times within parent area */

		TrueGoal,        /* always met */
		FalseGoal        /* never met */
};

/* Goal */
typedef struct Goal {
  enum GoalType goalType;  /* type */
  struct Goal *l, *r, *parent;  /* subgoals & parent */
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
Goal* newFalseGoal();
Goal* newAreaGoal (XYSet* area, Goal *subGoal);
Goal* newEnclosuresGoal (State wallMask,
			 StateSet* wallSet,
			 unsigned long minNumEnclosures,
			 unsigned long maxNumEnclosures,
			 unsigned long minEnclosureArea,
			 unsigned long maxEnclosureArea,
			 unsigned char allowDiagonalConnections,
			 Goal* subGoal);
Goal* newOnceGoal (Goal* l);
Goal* newAndGoal (Goal* l, Goal* r, int lazy);
Goal* newOrGoal (Goal* l, Goal* r, int lazy);
Goal* newNotGoal (Goal* g);
Goal* newEntropyGoal (StateSet* typeSet, State stateMask, unsigned long minCount, unsigned long maxCount, double minEntropy, double maxEntropy);
Goal* newRepeatGoal (Goal* subGoal, unsigned long minReps);

/* destructor */
void deleteGoal (Goal* goal);

/* helpers */
List* getEnclosures (Board* board, XYSet* area, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections);  /* returns a List of XYList's; caller must call deleteList to dealloc */

#endif /* GOAL_INCLUDED */
