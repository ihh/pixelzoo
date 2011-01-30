#ifndef RED_BLACK_TREE_INCLUDED
#define RED_BLACK_TREE_INCLUDED

#include "util.h"
#include "list.h"

/* comment out the line below to remove all the debugging assertion */
/* checks from the compiled code.  */
#define DEBUG_ASSERT 1

typedef struct RBNode {
  void* key;
  void* value;
  int red; /* if red=0 then the node is black */
  struct RBNode* left;
  struct RBNode* right;
  struct RBNode* parent;
} RBNode;

/* Compare(a,b) should return 1 if *a > *b, -1 if *a < *b, and 0 otherwise */
/* Destroy(a) takes a pointer to whatever 'a' might be and frees it accordingly */
typedef struct RBTree {
  CompareFunction Compare;
  CopyFunction CopyKey, CopyValue;
  DestroyFunction DestroyKey, DestroyValue;
  PrintFunction PrintKey, PrintValue;
  /*  A sentinel is used for root and for nilt.  These sentinels are */
  /*  created when newRBTree is caled.  root->left should always */
  /*  point to the node which is the root of the tree.  nilt points to a */
  /*  node which should always be black but has aribtrary children and */
  /*  parent and no key or value.  The point of using these sentinels is so */
  /*  that the root and nilt nodes do not require special cases in the code */
  /* ("nil" renamed to "nilt" to avoid objective-C clash - IH 12/29/2010) */
  RBNode* root;
  RBNode* nilt;
} RBTree;

RBTree* newRBTree(CompareFunction KeyCompareFunc,
		  CopyFunction KeyCopyFunc,
		  CopyFunction ValueCopyFunc,
		  DestroyFunction KeyDestroyFunc,
		  DestroyFunction ValueDestroyFunc,
		  PrintFunction KeyPrintFunc,
		  PrintFunction ValuePrintFunc);
void deleteRBTree(RBTree*);
size_t RBTreeSize(RBTree* tree);
RBTree* RBTreeDeepCopy(RBTree* tree);  /* uses CopyFunction's to copy keys & values */
RBTree* RBTreeShallowCopy(RBTree* tree);  /* does not use CopyFunction's. Do not delete original tree before shallow-copying! */
RBNode* RBTreeInsert(RBTree*, void* key, void* value);
void RBTreeEraseUnguarded(RBTree* , RBNode* );
void RBTreeErase(RBTree* , void* key);
RBNode* RBTreeFind(RBTree*, void* key);
Stack* RBTreeEnumerate(RBTree* tree,void* low, void* high);  /* set low and/or high to NULL for unbounded/semibounded enumeration */
RBNode* RBTreePredecessor(RBTree*,RBNode*);
RBNode* RBTreeSuccessor(RBTree*,RBNode*);
RBNode* RBTreeFirst(RBTree*);
RBNode* RBTreeLast(RBTree*);
void RBTreePrint(RBTree*);  /* debug */

void RBTreeRetain(RBTree*, RBTree*);  /* retains all keys of first RBTree that are also in second RBTree */
void RBTreeRemove(RBTree*, RBTree*);  /* removes all keys of first RBTree that are also in second RBTree */

/* void versions of copy, print & delete */
void* RBTreeDeepCopyVoid(void*);
void RBTreePrintVoid(void*);
void RBTreeDeleteVoid(void*);

#endif /* RED_BLACK_TREE_INCLUDED */
