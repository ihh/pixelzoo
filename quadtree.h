#ifndef QUADTREE_INCLUDED
#define QUADTREE_INCLUDED

#include "rule.h"

typedef struct QuadTree {
    double* quadRate;
    int K; // K = log_2(size)
} QuadTree;

QuadTree* newQuadTree (int size);
void updateQuadTree(QuadTree* quad, GlobalCoord* p, double val);
void sampleQuadLeaf(QuadTree* quad, GlobalCoord* p_ret);
double topQuadRate(QuadTree* quad);

#endif /* QUADTREE_INCLUDED */
