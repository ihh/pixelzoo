#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "board.h"
#include "xymap.h"
#include "statemap.h"
#include "vector.h"

/* Goal */
typedef struct Goal {
  enum GoalType { Area, Enclosures, Then, And, Or, True, Not, Entropy, Repeat } goalType;
  struct Goal *l, *r, *parent;  /* parent & subgoals */
  RBTree *tree;  /* red-black tree goal params */
  double *dbl;  /* floating-point goal params */
  State *state;  /* integer goal params */
} Goal;

/* accessors */
int testGoalAchieved (Goal* goal, Board* board);
XYSet* getGoalArea (Goal* goal, Board* board);  /* caller must call deleteXYSet() to dealloc */

/* constructors */
Goal* newTrueGoal();  /* also serves as a base constructor for all other Goal types */
Goal* newAreaGoal (XYSet* area, Goal* subGoal);
Goal* newEnclosuresGoal (Goal* parent, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections, Goal* subGoal);
Goal* newAndGoal (Goal* parent, Goal* l, Goal* r);
Goal* newOrGoal (Goal* parent, Goal* l, Goal* r);
Goal* newThenGoal (Goal* parent, Goal* l, Goal* r);
Goal* newNotGoal (Goal* parent, Goal* g);
Goal* newEntropyGoal (Goal* parent, State typeMask, StateSet* typeSet, unsigned int minCount, unsigned int maxCount, double minEntropy);
Goal* newRepeatGoal (Goal* parent, Goal* subGoal, unsigned int minReps);

/* destructor */
void deleteGoal (Goal* goal);

/* helpers */
Vector* getEnclosures (Board* board, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections);  /* returns a List of XYList's; caller must call deleteVector to dealloc */

#endif /* GOAL_INCLUDED */
