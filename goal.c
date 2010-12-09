#include <stdio.h>
#include "goal.h"
#include "stack.h"

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

List* getEnclosures (Board* board, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections) {
  List *enclosureList;
  XYList *enclosure, *pointsToVisit;
  int **mark, xLoop, yLoop, x, y, dx, dy, currentMark, stackEmpty;
  State state;

  enclosureList = newList (ListDeleteVoid, ListPrintVoid);

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

	do {
	  mark[x][y] = currentMark;
	  XYListAppend (enclosure, x, y);

	  for (dx = -1; dx <= +1; ++dx)
	    for (dy = -1; dy <= +1; ++dy)
	      if ((dx || dy) && (allowDiagonalConnections || (dx == 0 || dy == 0)))
		if (onBoard(board,x+dx,y+dy) && mark[x+dx][y+dy] == 0)
		  XYListAppend (pointsToVisit, x+dx, y+dy);

	  stackEmpty = 0;
	  while (mark[x][y] != 0) {
	    if (XYListEmpty (pointsToVisit)) {
	      stackEmpty = 1;
	      break;
	    }
	    x = ((XYCoord*) pointsToVisit->tail->value)->x;
	    y = ((XYCoord*) pointsToVisit->tail->value)->y;
	    XYListErase (pointsToVisit, pointsToVisit->tail);
	  }
	} while (!stackEmpty);

	ListAppend (enclosureList, enclosure);
      }

  /* cleanup & return */
  deleteXYList (pointsToVisit);

  return enclosureList;
}

