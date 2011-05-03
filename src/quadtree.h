#ifndef QUADTREE_INCLUDED
#define QUADTREE_INCLUDED

#include "rule.h"

/* a fully-populated quad tree of int64_Millionths's with fast lookup & sampling */
typedef struct QuadTree {
  int64_Millionths* quadRate;
  unsigned char K;  /* K = log_2(size) */
} QuadTree;

QuadTree* newQuadTree (int size);
void deleteQuadTree (QuadTree* quad);
void copyQuadTree (QuadTree* src, QuadTree* dest);
int64_Millionths readQuadTree (QuadTree* quad, int x, int y);
void updateQuadTree (QuadTree* quad, int x, int y, int64_Millionths val);
void sampleQuadLeaf (QuadTree* quad, RandomNumberGenerator rng, int* x_ret, int* y_ret);
int64_Millionths topQuadRate (QuadTree* quad);
int64_Millionths getQuadRate (QuadTree* quad, int x, int y, int level);

/* macro for size */
#define quadTreeSize(QUAD_PTR) (1 << (QUAD_PTR)->K)

/* macro to get the number of cells per node at a particular quad level */
#define cellsPerQuadNode(QUAD_PTR,LEVEL) (1 << (((QUAD_PTR)->K - (LEVEL)) << 1))

#endif /* QUADTREE_INCLUDED */
