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
      Abort ("While building bin tree: dataset is not a power of 2");
    }
    tmp = tmp >> 1;
    ++bin->K;
  }
  totalNodes = totalBinTreeNodes (bin->K);
  bin->binRate = SafeCalloc (totalNodes, sizeof(int64_Millionths));  /* initialized to zero */
  return bin;
}

void copyBinTree (BinTree* src, BinTree* dest) {
  int size;
  Assert (src->K == dest->K, "BinTree sizes don't match");
  size = binTreeSize (src);
  memcpy (dest->binRate, src->binRate, totalBinTreeNodes(src->K) * sizeof(int64_Millionths));
}

void deleteBinTree (BinTree* bin) {
  SafeFree(bin->binRate);
  SafeFree(bin);
}

int64_Millionths readBinTree (BinTree* bin, int x) {
  return bin->binRate[binNodeIndex(bin, x, bin->K)];
}

void updateBinTree(BinTree* bin, int x, int64_Millionths val) {
  int64_Millionths oldVal, diff;
  int lev, n;
  oldVal = bin->binRate[binNodeIndex(bin, x, bin->K)];
  diff = val - oldVal;
  for (lev = 0; lev <= bin->K; ++lev) {
    n = binNodeIndex(bin, x, lev);
    bin->binRate[n] = MAX (bin->binRate[n] + diff, 0);
  }
}

void sampleBinLeaf(BinTree* bin, RandomNumberGenerator* rng, int* x_ret) {
  int node, lev, whichChild, leftChild;
  int64_Millionths prob;

  node = 0;
  *x_ret = 0;
  for (lev = 0; lev < bin->K; ++lev) {
    prob = rngRandomProb(rng) * bin->binRate[node] / PowerOfTwoClosestToOneMillion;
    leftChild = binLeftChildIndex(node);
    whichChild = (prob < bin->binRate[leftChild]) ? 0 : 1;
    node = leftChild + whichChild;
    *x_ret = (*x_ret << 1) | whichChild;
  }
}

int64_Millionths topBinRate(BinTree* bin) {
  return bin->binRate[0];
}

int64_Millionths getBinRate (BinTree* bin, int x, int level) {
  return bin->binRate[binNodeIndex(bin,x,level)];
}
