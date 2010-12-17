#ifndef PLAY_STATE_INCLUDED
#define PLAY_STATE_INCLUDED

#include "board.h"
#include "stringmap.h"
#include "xymap.h"

/* state of play */
typedef struct PlayState {
  Board *board;

  StringMap *allTools;     /* map from String's to Tool's */
  StringSet *ownedTools;
  char *selectedTool;
  XYCoord toolPos;

  StringSet *achievments;  /* achievments unlocked */
  StringIntMap *scores;    /* coins, xp, alignment, etc */
  StringMap *npc;          /* map from String's (npc names) to NPC's */

} PlayState;

/*
  Game threads (all on timers):
  Evolve thread: if not paused, evolve board, recalculate overload, sleep
  Redraw thread: redraw board, sleep
  NPC thread: use tools, test goals -> award rewards, sleep
  Tool thread: use current selected tool (if active), recharge inactive tools, sleep

  UI events:
  Key press (or toolbar touch): select current tool
  Mouse down (or board touch): set current tool active flag
  Mouse move (or board touch): set current tool x, y
  Mouse up (or board touch release): clear tool active flag
 */


#endif /* PLAY_STATE_INCLUDED */
