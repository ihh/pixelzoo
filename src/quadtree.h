#ifndef QUADTREE_INCLUDED
#define QUADTREE_INCLUDED

#include "rule.h"

/* a fully-populated quad tree of double's with fast lookup & sampling */
typedef struct QuadTree {
  double* quadRate;
  unsigned char K;  /* K = log_2(size) */
} QuadTree;

QuadTree* newQuadTree (int size);
void deleteQuadTree (QuadTree* quad);
void copyQuadTree (QuadTree* src, QuadTree* dest);
double readQuadTree (QuadTree* quad, int x, int y);
void updateQuadTree (QuadTree* quad, int x, int y, double val);
void sampleQuadLeaf (QuadTree* quad, int* x_ret, int* y_ret);
double topQuadRate (QuadTree* quad);
double getQuadRate (QuadTree* quad, int x, int y, int level);

/* macro for size */
#define quadTreeSize(QUAD_PTR) (1 << (QUAD_PTR)->K)

/* macro to get the number of cells per node at a particular quad level */
#define cellsPerQuadNode(QUAD_PTR,LEVEL) (1 << (((QUAD_PTR)->K - (LEVEL)) << 1))

#endif /* QUADTREE_INCLUDED */
