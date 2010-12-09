#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "board.h"
#include "xymap.h"

/* Goal */
typedef struct Goal {
  enum GoalType { Area, Enclosures, Then, And, Or, True, Not, Particles, Entropy, Repeat } goalType;
  struct Goal *l, *r, *parent;
  rb_tree *data;
  double min, max;
} Goal;

/* accessors */
int testGoalAchieved (Goal* goal, Board* board);
XYSet* getGoalArea (Goal* goal, Board* board);  /* caller must call xySetDestroy() to dealloc */

/* constructors */
Goal* newAreaGoal (Goal* subgoal, Goal* parent, rb_tree* area);
Goal* newEnclosuresGoal (Goal* subgoal, Goal* parent, rb_tree* area);
/* more to go here */

/* destructor */
void deleteGoal (Goal* goal);

#endif /* GOAL_INCLUDED */
