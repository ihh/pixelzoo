#include <stdlib.h>
#include <stdio.h>
#include "quadtree.h"

int quadNodeIndex (QuadTree* quad, GlobalCoord* p, int level);
int quadChildIndex (QuadTree* quad, int parentIndex, int parentLevel, int whichChild);

QuadTree* newQuadTree (int size) {
  QuadTree* quad;
  int tmp, totalNodes;

  quad = malloc (sizeof(QuadTree));

  tmp = size;
  for (quad->K = 0; tmp > 1; ) {
    if ((tmp & 1) != 0) {
      fprintf (stderr, "While building quad tree: board size is not a power of 2");
      exit(1);
    }
    tmp = tmp >> 1;
    ++quad->K;
  }
  totalNodes = (4 * size * size - 1) / 3;
  quadRate = calloc (sizeof(double) * totalNodes); // initialized to zero
}

void updateQuadTree(QuadTree* quad, GlobalCoord* p, double val) {
  double oldVal, diff;
  int lev, n;
  oldVal = quadRate[quadNodeIndex(quad, p, K)];
  diff = val - oldVal;
  for (lev = 0; lev <= quad->K; ++lev) {
    n = quadNodeIndex(quad, p, lev);
    quadRate[n] = max(quad->quadRate[n] + diff, 0);
  }
}

void sampleQuadLeaf(QuadTree* quad, GlobalCoord* p_ret) {
  int node, lev, whichChild, childNode;
  double prob;

 node = 0;
 p_ret->x = p_ret->y = 0;
 for (lev = 0; lev < K; ++lev) {
   prob = random() * quadRate[node];
            int whichChild = 0, childNode = -1;
            while (true) {
                childNode = quadChildIndex(node, lev, whichChild);
                prob -= quadRate[childNode];
                if (prob < 0 || whichChild == 3)
                    break;
                ++whichChild;
            }
            node = childNode;
            p.y = (p.y << 1) | (whichChild >> 1);
            p.x = (p.x << 1) | (whichChild & 1);
        }
}

double topQuadRate(QuadTree* quad);

    /**
     *Updates the quad tree with the transform rate of the particle in this point
     * Note: p is edited in this call!
     * @param p
     * @param val
     */
    public void updateQuadTree(Point p, double val) {
        double oldVal = quadRate[quadNodeIndex(p, K)];
        double diff = val - oldVal;
        for (int lev = 0; lev <= K; ++lev) {
            int n = quadNodeIndex(p, lev);
            quadRate[n] = Math.max(quadRate[n] + diff, 0);
        }
    }

    public void sampleQuadLeaf(Point p) {
        int node = 0;
        p.x = p.y = 0;
        for (int lev = 0; lev < K; ++lev) {
            double prob = Math.random() * quadRate[node];
            int whichChild = 0, childNode = -1;
            while (true) {
                childNode = quadChildIndex(node, lev, whichChild);
                prob -= quadRate[childNode];
                if (prob < 0 || whichChild == 3)
                    break;
                ++whichChild;
            }
            node = childNode;
            p.y = (p.y << 1) | (whichChild >> 1);
            p.x = (p.x << 1) | (whichChild & 1);
        }
    }

    public double topQuadRate() {
        return quadRate[0];
    }

    // private methods
    // quad-tree indexing
    private int quadNodeIndex(Point p, int level) {
        int nodesBeforeLevel = ((1 << (level << 1)) - 1) / 3;
        int msbY = p.y >> (K - level);
        int msbX = p.x >> (K - level);
        return msbX + (msbY << level) + nodesBeforeLevel;
    }

    private int quadChildIndex(int parentIndex, int parentLevel, int whichChild) {
        int childLevel = parentLevel + 1;
        int nodesBeforeParent = ((1 << (parentLevel << 1)) - 1) / 3;
        int nodesBeforeChild = ((1 << (childLevel << 1)) - 1) / 3;
        int parentOffset = parentIndex - nodesBeforeParent;
        int msbParentY = parentOffset >> parentLevel;
        int msbParentX = parentOffset - (msbParentY << parentLevel);
        int msbChildY = (msbParentY << 1) | (whichChild >> 1);
        int msbChildX = (msbParentX << 1) | (whichChild & 1);
        return msbChildX + (msbChildY << childLevel) + nodesBeforeChild;
    }
}
