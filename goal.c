#include <stdio.h>
#include "goal.h"

int testEntropyGoal (Goal* goal, Board* board);
int testEnclosuresGoal (Goal* goal, Board* board);
Goal* newGoal (enum GoalType type, int dblDataSize, int intDataSize);

Goal* newGoal (enum GoalType type, int dblDataSize, int intDataSize) {
  Goal* g;
  g = SafeMalloc (sizeof (Goal));
  g->goalType = type;
  g->l = g->r = g->parent = NULL;
  g->tree = NULL;
  g->dblData = dblDataSize ? SafeMalloc(dblDataSize*sizeof(double)) : NULL;
  g->intData = intDataSize ? SafeMalloc(intDataSize*sizeof(unsigned long)) : NULL;
  return g;
}

Goal* newTrueGoal() {
  return newGoal (True, 0, 0);
}

Goal* newAreaGoal (XYSet* area) {
  Goal* g;
  g = newGoal (Area, 0, 0);
  g->tree = (RBTree*) area;
  return g;
}

void deleteGoal (Goal* goal) {
  SafeFreeOrNull (goal->intData);
  SafeFreeOrNull (goal->dblData);
  if (goal->tree) deleteRBTree (goal->tree);
  if (goal->l) deleteGoal (goal->l);
  if (goal->r) deleteGoal (goal->r);
  SafeFree (goal);
}

List* getEnclosures (Board* board, XYSet* area, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections) {
  List *enclosureList;
  XYList *enclosure, *pointsToVisit;
  int **mark, xLoop, yLoop, x, y, dx, dy, currentMark, enclosureDone, enclosureArea;
  XYCoord tempXYCoord;
  State state;

  /* enclosureList is a List of List's (or more specifically, a list of XYList's),
     so the copy, delete & print functions are the List versions of those functions
  */
  enclosureList = newList (ListDeepCopyVoid, ListDeleteVoid, ListPrintVoid);

  /* create an array of enclosure indices */
  mark = SafeMalloc (board->size * sizeof(int*));
  for (x = 0; x < board->size; ++x)
    mark[x] = SafeCalloc (board->size, sizeof(int));

  /* mark the walls as -1 */
  for (x = 0; x < board->size; ++x)
    for (y = 0; y < board->size; ++y) {
      state = readBoardState(board,x,y) & wallMask;
      if (StateSetFind(wallSet,state))
	mark[x][y] = -1;
    }

  /* if we've been given a sub-area of the board to look at, mark everything outside that area as -1 */
  if (area)
    for (x = 0; x < board->size; ++x)
      for (y = 0; y < board->size; ++y)
	if (XYSetFind(area,x,y,tempXYCoord) == NULL)
	  mark[x][y] = -1;

  /* loop over the board, starting a breadth-first search from every unvisited cell */
  pointsToVisit = newXYList();
  currentMark = 0;
  for (xLoop = 0; xLoop < board->size; ++xLoop)
    for (yLoop = 0; yLoop < board->size; ++yLoop)
      if (mark[xLoop][yLoop] == 0) {
	++currentMark;
	enclosure = newXYList();

	x = xLoop;
	y = yLoop;
	enclosureDone = 0;
	while (!enclosureDone) {
	  mark[x][y] = currentMark;
	  (void) XYListAppend (enclosure, x, y);

	  /* loop over the neighborhood */
	  for (dx = -1; dx <= +1; ++dx)
	    for (dy = -1; dy <= +1; ++dy)
	      if ((dx || dy) && (allowDiagonalConnections || (dx == 0 || dy == 0)))
		if (onBoard(board,x+dx,y+dy) && mark[x+dx][y+dy] == 0)
		  (void) XYListAppend (pointsToVisit, x+dx, y+dy);

	  while (mark[x][y] != 0) {
	    if (XYListEmpty (pointsToVisit)) {
	      enclosureDone = 1;
	      break;
	    }
	    XYListPop (pointsToVisit, x, y);
	  }
	}

	enclosureArea = XYListSize (enclosure);
	if (enclosureArea >= minEnclosureArea && enclosureArea <= maxEnclosureArea)
	  ListAppend (enclosureList, (void*) enclosure);
	else
	  deleteXYList (enclosure);
      }

  /* cleanup & return */
  deleteXYList (pointsToVisit);
  for (x = 0; x < board->size; ++x)
    SafeFree (mark[x]);
  SafeFree (mark);

  return enclosureList;
}

XYSet* getGoalArea (Goal* goal) {
  return
    goal->goalType == Area
    ? (XYSet*) RBTreeShallowCopy (goal->tree)
    : (goal->parent
       ? getGoalArea (goal->parent)
       : NULL);
}

int testGoalMet (Goal* goal, Board* board) {
  Assert (goal != NULL, "testGoalMet: null goal");
  switch (goal->goalType) {
  case Area:
    return goal->l ? testGoalMet(goal->l,board) : 1;
  case Enclosures:
    return testEnclosuresGoal (goal, board);
    return 1;
  case Once:
    if (!*goal->intData)
      if (testGoalMet(goal->l,board))
	*goal->intData = 1;
    return *goal->intData;
  case And:
    return testGoalMet(goal->l,board) && testGoalMet(goal->r,board);
  case Or:
    return testGoalMet(goal->l,board) || testGoalMet(goal->r,board);
  case Not:
    return !testGoalMet(goal->l,board);
  case Entropy:
    return testEntropyGoal (goal, board);
  case Repeat:
    if (testGoalMet(goal->l,board))
      ++goal->intData[1];
    else
      goal->intData[1] = 0;
    return goal->intData[1] >= goal->intData[0];
  case True:
    return 1;
  case False:
  default:
    break;
  }
  return 0;
}

int testEnclosuresGoal (Goal* goal, Board* board) {
  List *enclosureList;
  ListNode *enclosureListNode, *enclosureNode;
  XYList *enclosure;
  XYSet *pointSet, *parentArea;
  Goal *tempAreaGoal;
  unsigned long wallMask, allowDiagonals, minEncSize, maxEncSize, minCount, count;
  wallMask = goal->intData[0];
  allowDiagonals = goal->intData[1];
  minCount = goal->intData[2];
  minEncSize = goal->intData[3];
  maxEncSize = goal->intData[4];
  parentArea = getGoalArea (goal);
  enclosureList = getEnclosures (board, parentArea, wallMask, (StateSet*) goal->tree, minEncSize, maxEncSize, allowDiagonals);
  count = 0;
  for (enclosureListNode = enclosureList->head; enclosureListNode && count < minCount; enclosureListNode = enclosureListNode->next) {
    enclosure = (XYList*) enclosureListNode->value;
    pointSet = newXYSet();
    for (enclosureNode = enclosure->head; enclosureNode; enclosureNode = enclosureNode->next)
      (void) XYSetInsert (pointSet, ((XYCoord*)enclosureNode->value)->x, ((XYCoord*)enclosureNode->value)->y);
    if (goal->l == NULL)
      ++count;
    else {
      tempAreaGoal = newAreaGoal (pointSet);
      goal->l->parent = tempAreaGoal;
      if (testGoalMet (goal->l, board))
	++count;
      goal->l->parent = goal;  /* this is not really necessary, but what the heck */
      deleteGoal (tempAreaGoal);  /* this also deletes pointSet */
    }
  }
  deleteList (enclosureList);
  if (parentArea)
    deleteXYSet (parentArea);
  return count >= minCount;
}

int testEntropyGoal (Goal* goal, Board* board) {
  /* more to go here */
  return 0;
}
