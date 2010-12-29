#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "quadtree.h"
#include "util.h"

#define totalQuadTreeNodes(SIZE) ((4 * (SIZE) * (SIZE) - 1) / 3)

int quadNodeIndex (QuadTree* quad, int x, int y, int level);
int quadChildIndex (int parentIndex, int parentLevel, int whichChild);

QuadTree* newQuadTree (int size) {
  QuadTree* quad;
  int tmp, totalNodes;

  quad = SafeMalloc (sizeof(QuadTree));

  tmp = size;
  for (quad->K = 0; tmp > 1; ) {
    if ((tmp & 1) != 0) {
      fprintf (stderr, "While building quad tree: board size is not a power of 2");
      exit(1);
    }
    tmp = tmp >> 1;
    ++quad->K;
  }
  totalNodes = totalQuadTreeNodes(size);
  quad->quadRate = SafeCalloc (totalNodes, sizeof(double));  /* initialized to zero */
  return quad;
}

void copyQuadTree (QuadTree* src, QuadTree* dest) {
  int size;
  Assert (src->K == dest->K, "QuadTree sizes don't match");
  size = quadTreeSize (src);
  memcpy (dest->quadRate, src->quadRate, totalQuadTreeNodes(size) * sizeof(double));
}

void deleteQuadTree (QuadTree* quad) {
  SafeFree(quad->quadRate);
  SafeFree(quad);
}

double readQuadTree (QuadTree* quad, int x, int y) {
  return quad->quadRate[quadNodeIndex(quad, x, y, quad->K)];
}

void updateQuadTree(QuadTree* quad, int x, int y, double val) {
  double oldVal, diff;
  int lev, n;
  oldVal = quad->quadRate[quadNodeIndex(quad, x, y, quad->K)];
  diff = val - oldVal;
  for (lev = 0; lev <= quad->K; ++lev) {
    n = quadNodeIndex(quad, x, y, lev);
    quad->quadRate[n] = MAX (quad->quadRate[n] + diff, 0);
  }
}

void sampleQuadLeaf(QuadTree* quad, int* x_ret, int* y_ret) {
  int node, lev, whichChild, childNode;
  double prob;

  node = 0;
  *x_ret = *y_ret = 0;
  for (lev = 0; lev < quad->K; ++lev) {
    prob = randomDouble() * quad->quadRate[node];
    whichChild = 0;
    childNode = -1;
    while (1) {
      childNode = quadChildIndex(node, lev, whichChild);
      prob -= quad->quadRate[childNode];
      if (prob < 0 || whichChild == 3)
	break;
      ++whichChild;
    }
    node = childNode;
    *y_ret = (*y_ret << 1) | (whichChild >> 1);
    *x_ret = (*x_ret << 1) | (whichChild & 1);
  }
}

double topQuadRate(QuadTree* quad) {
  return quad->quadRate[0];
}

double getQuadRate (QuadTree* quad, int x, int y, int level) {
  return quad->quadRate[quadNodeIndex(quad,x,y,level)];
}

int quadNodeIndex(QuadTree* quad, int x, int y, int level) {
  int nodesBeforeLevel, msbY, msbX;
  nodesBeforeLevel = ((1 << (level << 1)) - 1) / 3;
  msbY = y >> (quad->K - level);
  msbX = x >> (quad->K - level);
  return msbX + (msbY << level) + nodesBeforeLevel;
}

int quadChildIndex(int parentIndex, int parentLevel, int whichChild) {
  int childLevel, nodesBeforeParent, nodesBeforeChild, parentOffset;
  int msbParentY, msbParentX, msbChildY, msbChildX;
  childLevel = parentLevel + 1;
  nodesBeforeParent = ((1 << (parentLevel << 1)) - 1) / 3;
  nodesBeforeChild = ((1 << (childLevel << 1)) - 1) / 3;
  parentOffset = parentIndex - nodesBeforeParent;
  msbParentY = parentOffset >> parentLevel;
  msbParentX = parentOffset - (msbParentY << parentLevel);
  msbChildY = (msbParentY << 1) | (whichChild >> 1);
  msbChildX = (msbParentX << 1) | (whichChild & 1);
  return msbChildX + (msbChildY << childLevel) + nodesBeforeChild;
}
