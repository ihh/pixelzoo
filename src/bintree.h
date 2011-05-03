#ifndef BINTREE_INCLUDED
#define BINTREE_INCLUDED

#include "rule.h"
#include "util.h"

/* a fully-populated binary tree of int64_Millionth's with fast lookup & sampling */
typedef struct BinTree {
  int64_Millionths* binRate;
  unsigned char K;  /* K = log_2(size) */
} BinTree;

BinTree* newBinTree (int size);
void deleteBinTree (BinTree* bin);
void copyBinTree (BinTree* src, BinTree* dest);
int64_Millionths readBinTree (BinTree* bin, int x);
void updateBinTree (BinTree* bin, int x, int64_Millionths val);
void sampleBinLeaf (BinTree* bin, RandomNumberGenerator rng, int* x_ret);
int64_Millionths topBinRate (BinTree* bin);
int64_Millionths getBinRate (BinTree* bin, int x, int level);

/* macro for size */
#define binTreeSize(BIN_PTR) (1 << (BIN_PTR)->K)

/* macro to get the number of cells per node at a particular bin level */
#define cellsPerBinNode(BIN_PTR,LEVEL) (1 << ((BIN_PTR)->K - (LEVEL)))

#endif /* BINTREE_INCLUDED */
