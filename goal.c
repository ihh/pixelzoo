#include <stdio.h>
#include "goal.h"

Goal* newTrueGoal() {
  Goal* g;
  g = SafeMalloc (sizeof (Goal));
  g->goalType = True;
  g->l = g->r = g->parent = NULL;
  g->tree = NULL;
  g->dbl = NULL;
  g->state = NULL;
  return g;
}

