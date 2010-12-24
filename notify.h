#ifndef NOTIFY_INCLUDED
#define NOTIFY_INCLUDED

#include "rule.h"
#include "particle.h"
#include "board.h"
#include "xymap.h"

/* WriteInterceptFunction is called when the rule has just been triggered, but before the new state is written.
   The return value of WriteInterceptFunction is the State that will actually be written.
 */
typedef State (*WriteInterceptFunction) (CellWatcher *watcher,
					 Board *board,
					 int x,
					 int y,
					 State state);

/* a CellWatcher is a WriteInterceptFunction and some context */
struct CellWatcher {
  WriteInterceptFunction intercept;  /* function that maps intercepted writes to actual writes */
  void *context;   /* pointer to miscellaneous extra context */
  DestroyFunction contextDestroy;  /* function to free memory associated with context */
};

/* methods */
CellWatcher* newCellWatcher (WriteInterceptFunction intercept, void *context, DestroyFunction contextDestroy);
void deleteCellWatcher (CellWatcher *watcher);
int registerCellWatcher (Board *board, int x, int y, CellWatcher *watcher);  /* returns 1 if successful, 0 if cell already being watched */
void unregisterCellWatcher (Board *board, CellWatcher *watcher);  /* unregisters all instances of this watcher on the board */

#endif /* NOTIFY_INCLUDED */
