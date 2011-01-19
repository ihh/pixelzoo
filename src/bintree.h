#ifndef BINTREE_INCLUDED
#define BINTREE_INCLUDED

#include "rule.h"

/* a fully-populated binary tree of double's with fast lookup & sampling */
typedef struct BinTree {
  double* binRate;
  unsigned char K;  /* K = log_2(size) */
} BinTree;

BinTree* newBinTree (int size);
void deleteBinTree (BinTree* bin);
void copyBinTree (BinTree* src, BinTree* dest);
double readBinTree (BinTree* bin, int x);
void updateBinTree (BinTree* bin, int x, double val);
void sampleBinLeaf (BinTree* bin, int* x_ret);
double topBinRate (BinTree* bin);
double getBinRate (BinTree* bin, int x, int level);

/* macro for size */
#define binTreeSize(BIN_PTR) (1 << (BIN_PTR)->K)

/* macro to get the number of cells per node at a particular bin level */
#define cellsPerBinNode(BIN_PTR,LEVEL) (1 << ((BIN_PTR)->K - (LEVEL)))

#endif /* BINTREE_INCLUDED */
