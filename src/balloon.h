#ifndef BALLOON_INCLUDED
#define BALLOON_INCLUDED

#include "color.h"

/* non-obvious default values */
#define DefaultBalloonTTL   1.   /* timeToLive */
#define DefaultBalloonRise  1.1  /* zInc */
#define DefaultBalloonZoom  1.1  /* sizeMul */
#define DefaultBalloonFade  .9   /* opacityMul */

/* text balloons */
typedef struct Balloon {
  char *text;
  int x, y;
  PaletteIndex color;
  double timeToLive;   /* measured in board time, i.e. updates per second */
  double z, size, opacity, zInc, sizeMul, opacityMul;
  double prob;
} Balloon;

/* methods */
Balloon *newProtoBalloon (char *text, int xOffset, int yOffset, PaletteIndex color, double size, double timeToLive, double zInc, double sizeMul, double opacityMul, double prob);  /* copies text */
Balloon *newPlacedBalloon (Balloon *proto, int x, int y);
void deleteBalloon (void *balloon);
int BalloonCompare (const void *b1, const void *b2);

#endif /* BALLOON_INCLUDED */
