#include "move.h"

Move* newMove (int64_Microticks t, int x, int y, State state) {
  Move *m;
  m = SafeMalloc (sizeof (Move));
  m->t = t;
  m->x = x;
  m->y = y;
  m->state = state;
  return m;
}

void deleteMove (void* move) {
  SafeFree (move);
}

void* copyMove (void* voidMove) {
  Move *move;
  move = (Move*) voidMove;
  return newMove (move->t, move->x, move->y, move->state);
}
