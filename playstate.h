#ifndef PLAY_STATE_INCLUDED
#define PLAY_STATE_INCLUDED

#include "board.h"
#include "tool.h"
#include "challenge.h"

typedef struct PlayState {
  Board *board;
  StringMap *stageChallenges, *toolBox;
  char *currentStage, *selectedTool;
} PlayState;


#endif /* PLAY_STATE_INCLUDED */
