#ifndef RED_BLACK_TREE_INCLUDED
#define RED_BLACK_TREE_INCLUDED

#include"util.h"
#include"stack.h"

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
  DestroyFunction DestroyKey, DestroyValue;
  PrintFunction PrintKey, PrintValue;
  /*  A sentinel is used for root and for nil.  These sentinels are */
  /*  created when newRBTree is caled.  root->left should always */
  /*  point to the node which is the root of the tree.  nil points to a */
  /*  node which should always be black but has aribtrary children and */
  /*  parent and no key or value.  The point of using these sentinels is so */
  /*  that the root and nil nodes do not require special cases in the code */
  RBNode* root;             
  RBNode* nil;              
} RBTree;

RBTree* newRBTree(CompareFunction KeyCompareFunc,
		  DestroyFunction KeyDestroyFunc,
		  DestroyFunction ValueDestroyFunc,
		  PrintFunction KeyPrintFunc,
		  PrintFunction ValuePrintFunc);
void deleteRBTree(RBTree*);
RBNode* RBTreeInsert(RBTree*, void* key, void* value);
void RBTreeEraseUnguarded(RBTree* , RBNode* );
void RBTreeErase(RBTree* , void* key);
RBNode* RBTreeFind(RBTree*, void* key);
stk_stack* RBTreeEnumerate(RBTree* tree,void* low, void* high);
RBNode* RBTreePredecessor(RBTree*,RBNode*);
RBNode* RBTreeSuccessor(RBTree*,RBNode*);
void RBTreePrint(RBTree*);  /* debug */

/* void versions of print & delete */
void RBTreePrintVoid(const void*);
void RBTreeDeleteVoid(void*);

#endif /* RED_BLACK_TREE_INCLUDED */
