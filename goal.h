#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "board.h"
#include "xymap.h"
#include "statemap.h"

/* Goal */
typedef struct Goal {
  enum GoalType { Area, Enclosures, Then, And, Or, True, Not, Particles, Entropy, Repeat } goalType;
  struct Goal *l, *r, *parent;
  RBTree *data;
  double d1, d2;
  int i1, i2;
} Goal;

/* accessors */
int testGoalAchieved (Goal* goal, Board* board);
XYSet* getGoalArea (Goal* goal, Board* board);  /* caller must call xySetDestroy() to dealloc */

/* constructors */
Goal* newAreaGoal (XYSet* area, Goal* subGoal);
Goal* newEnclosuresGoal (Goal* parent, int minEnclosureArea, int maxEnclosureArea, int allowDiagonalConnections, Goal* subGoal);
Goal* newAndGoal (Goal* parent, Goal* l, Goal* r);
Goal* newOrGoal (Goal* parent, Goal* l, Goal* r);
Goal* newThenGoal (Goal* parent, Goal* l, Goal* r);
Goal* newNotGoal (Goal* parent, Goal* g);
Goal* newParticlesGoal (Goal* parent, StateSet* particleTypeSet, int minCount);
Goal* newEntropyGoal (Goal* parent, StateSet* particleTypeSet, int minCount, double minEntropy);
Goal* newRepeatGoal (Goal* parent, Goal* subGoal, int minReps);

/* destructor */
void deleteGoal (Goal* goal);

#endif /* GOAL_INCLUDED */
