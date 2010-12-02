#ifndef BOARD_INCLUDED
#define BOARD_INCLUDED

#include "rule.h"
#include "particle.h"
#include "quadtree.h"

typedef struct Board {
  Particle** by_type;  /* Type t; by_type[t] */
  int size;
  State** cell;   /* int x, y; cell[x][y] */
  QuadTree* quad;  /* private */
} Board;

Board* newBoard (int size);
void deleteBoard (Board* board);
Particle* newBoardParticle (Board* board, char* name, Type type, int nRules);
void writeBoard (Board* board, int x, int y, State value);
Particle* readBoard (Board* board, int x, int y);
void evolveBoard (Board* board, double targetUpdatesPerCell, double maxTimeInSeconds);

#endif /* BOARD_INCLUDED */
