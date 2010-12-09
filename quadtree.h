#ifndef QUADTREE_INCLUDED
#define QUADTREE_INCLUDED

#include "rule.h"

/* quad tree */
typedef struct QuadTree {
  double* quadRate;
  unsigned char K;  /* K = log_2(size) */
} QuadTree;

QuadTree* newQuadTree (int size);
void deleteQuadTree (QuadTree* quad);
double readQuadTree (QuadTree* quad, int x, int y);
void updateQuadTree (QuadTree* quad, int x, int y, double val);
void sampleQuadLeaf (QuadTree* quad, int* x_ret, int* y_ret);
double topQuadRate (QuadTree* quad);
double getQuadRate (QuadTree* quad, int x, int y, int level);

/* macro to get the number of cells at a particular quad level */
#define quadCells(QUAD,LEVEL) (1 << (((QUAD)->K - LEVEL) << 1))

#endif /* QUADTREE_INCLUDED */
