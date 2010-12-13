#ifndef PLAY_STATE_INCLUDED
#define PLAY_STATE_INCLUDED

#include "board.h"
#include "tool.h"
#include "challenge.h"

typedef struct PlayState {
  Board *board;
  StringMap *stageChallenges;
  char *currentStage;
  Vector *toolVector;
} PlayState;


#endif /* PLAY_STATE_INCLUDED */
