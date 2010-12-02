#ifndef QUADTREE_INCLUDED
#define QUADTREE_INCLUDED

typedef struct QuadTree {
    double* quadRate;
    int K; // K = log_2(size)
} QuadTree;

QuadTree* newQuadTree (int size);
void updateQuadTree(QuadTree* quad, GlobalCoord p, double val);
void sampleQuadLeaf(QuadTree* quad, Point* p);
double topQuadRate(QuadTree* quad);

#endif /* QUADTREE_INCLUDED */
