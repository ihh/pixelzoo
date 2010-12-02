#ifndef QUADTREE_INCLUDED
#define QUADTREE_INCLUDED

#include "rule.h"

/* quad tree */
typedef struct QuadTree {
    double* quadRate;
    unsigned char K; // K = log_2(size)
} QuadTree;

QuadTree* newQuadTree (int size);
void deleteQuadTree (QuadTree* quad);
void updateQuadTree(QuadTree* quad, int x, int y, double val);
void sampleQuadLeaf(QuadTree* quad, int* x_ret, int* y_ret);
double topQuadRate(QuadTree* quad);

#endif /* QUADTREE_INCLUDED */
