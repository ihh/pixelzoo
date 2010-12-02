#include <stdlib.h>
#include <stdio.h>
#include "board.h"

Board* newBoard (int size) {
  Board* board;
  int x;
  board = malloc (sizeof (Board));
  board->by_type = calloc (NumTypes, sizeof(Particle*));
  board->size = size;
  board->cell = malloc (size * sizeof(State*));
  for (x = 0; x < size; ++x)
    board->cell[x] = calloc (size, sizeof(State));
  board->quad = newQuadTree (size);
  return board;
}

void deleteBoard (Board* board) {
  unsigned long t;
  int x;
  deleteQuadTree (board->quad);
  for (x = 0; x < board->size; ++x)
    free (board->cell[x]);
  free (board->cell);
  for (t = 0; t < NumTypes; ++t)
    if (board->by_type[(Type) t])
      deleteParticle (board->by_type[(Type) t]);
  free (board->by_type);
  free (board);
}

Particle* newBoardParticle (Board* board, char* name, Type type, int nRules) {
  Particle* p;
  p = newParticle (name, nRules);
  p->type = type;
  board->by_type[type] = p;
  return p;
}

void writeBoard (Board* board, int x, int y, State state) {
  Type t;
  Particle* p;
  if (x >= 0 && x < board->size && y >= 0 && y < board->size) {
    t = state & TypeMask;
    p = board->by_type[t];
    if (p) {
      board->cell[x][y] = state;
      updateQuadTree (board->quad, x, y, p->totalRate);
    } else {
      board->cell[x][y] = 0;
      updateQuadTree (board->quad, x, y, 0.);
    }
  }
}

Particle* readBoard (Board* board, int x, int y) {
  return board->by_type[board->cell[x][y] & TypeMask];
}
