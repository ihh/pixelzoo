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
  int (*Compare)(const void* a, const void* b); 
  void (*DestroyKey)(void* a);
  void (*DestroyValue)(void* a);
  void (*PrintKey)(const void* a);
  void (*PrintValue)(void* a);
  /*  A sentinel is used for root and for nil.  These sentinels are */
  /*  created when newRBTree is caled.  root->left should always */
  /*  point to the node which is the root of the tree.  nil points to a */
  /*  node which should always be black but has aribtrary children and */
  /*  parent and no key or value.  The point of using these sentinels is so */
  /*  that the root and nil nodes do not require special cases in the code */
  RBNode* root;             
  RBNode* nil;              
} RBTree;

RBTree* newRBTree(int  (*KeyCompareFunc)(const void*, const void*),
		  void (*KeyDestroyFunc)(void*), 
		  void (*ValueDestroyFunc)(void*), 
		  void (*KeyPrintFunc)(const void*),
		  void (*ValuePrintFunc)(void*));
void deleteRBTree(RBTree*);
RBNode* RBTreeInsert(RBTree*, void* key, void* value);
void RBTreeEraseUnguarded(RBTree* , RBNode* );
void RBTreeErase(RBTree* , void* key);
RBNode* RBTreeFind(RBTree*, void* key);
stk_stack* RBTreeEnumerate(RBTree* tree,void* low, void* high);
RBNode* RBTreePredecessor(RBTree*,RBNode*);
RBNode* RBTreeSuccessor(RBTree*,RBNode*);
void RBTreePrint(RBTree*);  /* debug */

void NullFunction(void*);

#endif /* RED_BLACK_TREE_INCLUDED */
