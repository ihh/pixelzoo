#include "move.h"

Move* newMove (int64_Microticks t, signed long long int u, int x, int y, State state) {
  Move *m;
  m = SafeMalloc (sizeof (Move));
  m->t = t;
  m->u = u;
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
  return newMove (move->t, move->u, move->x, move->y, move->state);
}
