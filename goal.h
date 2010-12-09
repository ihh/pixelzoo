#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "board.h"
#include "xymap.h"
#include "statemap.h"

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
XYSet* getGoalArea (Goal* goal, Board* board);  /* caller must call xySetDestroy() to dealloc */

/* constructors */
Goal* newTrueGoal();  /* also serves as a base constructor for all other Goal types */
Goal* newAreaGoal (XYSet* area, Goal* subGoal);
Goal* newEnclosuresGoal (Goal* parent, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections, Goal* subGoal);
Goal* newAndGoal (Goal* parent, Goal* l, Goal* r);
Goal* newOrGoal (Goal* parent, Goal* l, Goal* r);
Goal* newThenGoal (Goal* parent, Goal* l, Goal* r);
Goal* newNotGoal (Goal* parent, Goal* g);
Goal* newEntropyGoal (Goal* parent, StateSet* particleTypeSet, unsigned int minCount, unsigned int maxCount, double minEntropy);
Goal* newRepeatGoal (Goal* parent, Goal* subGoal, unsigned int minReps);

/* destructor */
void deleteGoal (Goal* goal);

/* helpers */


#endif /* GOAL_INCLUDED */
