#ifndef RED_BLACK_TREE_INCLUDED
#define RED_BLACK_TREE_INCLUDED

#include"util.h"
#include"stack.h"

/*  CONVENTIONS:  All data structures for red-black trees have the prefix */
/*                "rb_" to prevent name conflicts. */
/*                                                                      */
/*                Function names: Each word in a function name begins with */
/*                a capital letter.  An example funcntion name is  */
/*                CreateRedTree(a,b,c). Furthermore, each function name */
/*                should begin with a capital letter to easily distinguish */
/*                them from variables. */
/*                                                                     */
/*                Variable names: Each word in a variable name begins with */
/*                a capital letter EXCEPT the first letter of the variable */
/*                name.  For example, int newLongInt.  Global variables have */
/*                names beginning with "g".  An example of a global */
/*                variable name is gNewtonsConstant. */

/* comment out the line below to remove all the debugging assertion */
/* checks from the compiled code.  */
#define DEBUG_ASSERT 1

typedef struct rb_node {
  void* key;
  void* value;
  int red; /* if red=0 then the node is black */
  struct rb_node* left;
  struct rb_node* right;
  struct rb_node* parent;
} rb_node;


/* Compare(a,b) should return 1 if *a > *b, -1 if *a < *b, and 0 otherwise */
/* Destroy(a) takes a pointer to whatever key might be and frees it accordingly */
typedef struct rb_tree {
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
  rb_node* root;             
  rb_node* nil;              
} rb_tree;

rb_tree* newRBTree(int  (*KeyCompareFunc)(const void*, const void*),
		   void (*KeyDestroyFunc)(void*), 
		   void (*ValueDestroyFunc)(void*), 
		   void (*KeyPrintFunc)(const void*),
		   void (*ValuePrintFunc)(void*));
void deleteRBTree(rb_tree*);
rb_node* RBTreeInsert(rb_tree*, void* key, void* value);
void RBTreeEraseUnguarded(rb_tree* , rb_node* );
void RBTreeErase(rb_tree* , void* key);
rb_node* RBTreeFind(rb_tree*, void* key);
stk_stack* RBTreeEnumerate(rb_tree* tree,void* low, void* high);
rb_node* RBTreePredecessor(rb_tree*,rb_node*);
rb_node* RBTreeSuccessor(rb_tree*,rb_node*);
void RBTreePrint(rb_tree*);

void NullFunction(void*);

#endif /* RED_BLACK_TREE_INCLUDED */
