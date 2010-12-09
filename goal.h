#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "board.h"
#include "xymap.h"
#include "statemap.h"
#include "list.h"

/* Goal */
typedef struct Goal {
  enum GoalType { Area,        /* subgoal (l) is met for given constant area */
		  Enclosures,  /* subgoal (l) is met in at least intData[0] enclosures within parent area satisfying (intData[1] <= enclosureArea <= intData[2]) */
		  Once,        /* subgoal (l) has been met at least once within parent area (caches result) */
		  And,         /* both subgoals (l & r) are met simultaneously within parent area */
		  Or,          /* at least one of the two subgoals (l & r) is met within parent area */
		  Not,         /* subgoal (l) is not met within parent area */
		  Entropy,     /* mask every state in parent area with intData[0], and keep states in ((StateSet*)tree). Goal is met if remaining states satisfy (intData[0] <= population <= intData[1]) and (dblData[0] <= entropy <= dblData[1]) */
		  Repeat,      /* subgoal (l) is currently met & has been met consecutively at least intData[0] times within parent area */
		  True,        /* always met */
		  False        /* never met */
  } goalType;
  struct Goal *l, *r, *parent;  /* parent & subgoals */
  RBTree *tree;  /* red-black tree goal params */
  double *dblData;  /* floating-point goal params */
  unsigned long *intData;  /* integer goal params */
  int ownsParent;  /* true if parent should be deleted by goal's destructor */
} Goal;

/* accessors */
int testGoalMet (Goal* goal, Board* board);
XYSet* getGoalArea (Goal* goal);  /* caller must call deleteXYSet() to dealloc */

/* constructors */
Goal* newTrueGoal();  /* also serves as a base constructor for all other Goal types */
Goal* newAreaGoal (XYSet* area, Goal* subGoal);
Goal* newEnclosuresGoal (Goal* parent, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections, Goal* subGoal);
Goal* newOnceGoal (Goal* parent, Goal* l);
Goal* newAndGoal (Goal* parent, Goal* l, Goal* r);
Goal* newOrGoal (Goal* parent, Goal* l, Goal* r);
Goal* newNotGoal (Goal* parent, Goal* g);
Goal* newEntropyGoal (Goal* parent, State typeMask, StateSet* typeSet, unsigned int minCount, unsigned int maxCount, double minEntropy, double maxEntropy);
Goal* newRepeatGoal (Goal* parent, Goal* subGoal, unsigned int minReps);

/* destructor */
void deleteGoal (Goal* goal);

/* helpers */
List* getEnclosures (Board* board, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections);  /* returns a List of XYList's; caller must call deleteList to dealloc */

#endif /* GOAL_INCLUDED */
