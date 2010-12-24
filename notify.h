#ifndef NOTIFY_INCLUDED
#define NOTIFY_INCLUDED

#include "rule.h"
#include "particle.h"
#include "board.h"
#include "xymap.h"

/* WriteInterceptFunction is called when the rule has just been triggered, but before the new state is written.
   It is possible for the RuleNotifyFunction to modify newDests to change the impact of the rule.
 */
typedef State (*WriteInterceptFunction) (BoardWatcher *watcher,
					 Board *board,
					 int x,
					 int y,
					 State state);

/* a BoardWatcher is a WriteInterceptFunction and some context */
struct BoardWatcher {
  char *name;
  WriteInterceptFunction intercept;
  void *context;   /* pointer to miscellaneous extra context */
};

/* methods */
BoardWatcher* newBoardWatcher (char *name, WriteInterceptFunction intercept, void *context);  /* copies 'name' */
void deleteBoardWatcher (BoardWatcher* watcher);

#endif /* NOTIFY_INCLUDED */
