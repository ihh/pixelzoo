#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "bintree.h"
#include "util.h"

#define totalBinTreeNodes(K) ((1 << (K + 1)) - 1)

#define binNodeIndex(BIN_PTR,X,LEVEL) ((1 << (LEVEL)) - 1 + ((X) >> ((BIN_PTR)->K - (LEVEL))))
#define binLeftChildIndex(PARENT_INDEX) (((PARENT_INDEX) << 1) + 1)
#define binRightChildIndex(PARENT_INDEX) (((PARENT_INDEX) << 1) + 2)

BinTree* newBinTree (int size) {
  BinTree* bin;
  int tmp, totalNodes;

  bin = SafeMalloc (sizeof(BinTree));

  tmp = size;
  for (bin->K = 0; tmp > 1; ) {
    if ((tmp & 1) != 0) {
      fprintf (stderr, "While building bin tree: board size is not a power of 2");
      exit(1);
    }
    tmp = tmp >> 1;
    ++bin->K;
  }
  totalNodes = totalBinTreeNodes (bin->K);
  bin->binRate = SafeCalloc (totalNodes, sizeof(double));  /* initialized to zero */
  return bin;
}

void copyBinTree (BinTree* src, BinTree* dest) {
  int size;
  Assert (src->K == dest->K, "BinTree sizes don't match");
  size = binTreeSize (src);
  memcpy (dest->binRate, src->binRate, totalBinTreeNodes(src->K) * sizeof(double));
}

void deleteBinTree (BinTree* bin) {
  SafeFree(bin->binRate);
  SafeFree(bin);
}

double readBinTree (BinTree* bin, int x) {
  return bin->binRate[binNodeIndex(bin, x, bin->K)];
}

void updateBinTree(BinTree* bin, int x, double val) {
  double oldVal, diff;
  int lev, n;
  oldVal = bin->binRate[binNodeIndex(bin, x, bin->K)];
  diff = val - oldVal;
  for (lev = 0; lev <= bin->K; ++lev) {
    n = binNodeIndex(bin, x, lev);
    bin->binRate[n] = MAX (bin->binRate[n] + diff, 0);
  }
}

void sampleBinLeaf(BinTree* bin, int* x_ret) {
  int node, lev, whichChild, leftChild;
  double prob;

  node = 0;
  *x_ret = 0;
  for (lev = 0; lev < bin->K; ++lev) {
    prob = randomDouble() * bin->binRate[node];
    leftChild = binLeftChildIndex(node);
    whichChild = (prob < bin->binRate[leftChild]) ? 0 : 1;
    node = leftChild + whichChild;
    *x_ret = (*x_ret << 1) | whichChild;
  }
}

double topBinRate(BinTree* bin) {
  return bin->binRate[0];
}

double getBinRate (BinTree* bin, int x, int level) {
  return bin->binRate[binNodeIndex(bin,x,level)];
}
