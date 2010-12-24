#ifndef NOTIFY_INCLUDED
#define NOTIFY_INCLUDED

#include "rule.h"
#include "particle.h"
#include "board.h"
#include "xymap.h"

/* RuleNotifyFunction is called when the rule has just been triggered, but before the new state is written.
   It is possible for the RuleNotifyFunction to modify newDests to change the impact of the rule.
 */
typedef State (*CellNotifyFunction) (BoardWatcher *watcher,
				     Board *board,
				     int x,
				     int y,
				     State state);

/* a BoardWatcher is a CellNotifyFunction and some context */
struct BoardWatcher {
  char *name;
  CellNotifyFunction notify;
  void *context;   /* pointer to miscellaneous extra context */
};

/* methods */
BoardWatcher* newBoardWatcher (char *name);  /* copies 'name' */
void deleteBoardWatcher (BoardWatcher* watcher);

#endif /* NOTIFY_INCLUDED */
