#include "balloon.h"
#include "util.h"
#include "stringmap.h"

Balloon *newProtoBalloon (char *text, int x, int y, PaletteIndex color, double size, double timeToLive, double zInc, double sizeMul, double opacityMul, double prob) {
  Balloon *b;
  b = SafeMalloc (sizeof (Balloon));
  b->text = StringCopy (text);
  b->x = x;
  b->y = y;
  b->color = color;
  b->timeToLive = timeToLive;
  b->zInc = zInc;
  b->size = size;
  b->sizeMul = sizeMul;
  b->opacity = 1;
  b->opacityMul = opacityMul;
  b->prob = prob;
  b->reset = NULL;
  return b;
}

Balloon *newPlacedBalloon (Balloon *proto, int x, int y, double z) {
  Balloon *b;
  b = newProtoBalloon (proto->text, proto->x + x, proto->y + y, proto->color, proto->size, proto->timeToLive, proto->zInc, proto->sizeMul, proto->opacityMul, proto->prob);
  b->z = z;
  b->opacity = proto->opacity;
  b->reset = proto->reset;
  return b;
}

int resetBalloon (Balloon *b) {
  if (b->reset) {
    b->timeToLive = b->reset->timeToLive;
    b->size = b->reset->size;
    b->z = b->reset->z;
    b->opacity = b->reset->opacity;
    return 1;
  }
  return 0;
}

void deleteBalloon (void *vb) {
  Balloon *b;
  b = (Balloon*) vb;
  StringDelete (b->text);
  SafeFree (b);
}

int BalloonCompare (const void *vb1, const void *vb2) {
  Balloon *b1, *b2;
  b1 = *(Balloon**) vb1;
  b2 = *(Balloon**) vb2;
  if (b1->z > b2->z) return +1;
  if (b1->z < b2->z) return -1;
  return 0;
}
