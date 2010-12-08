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
  void* info;
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
  void (*DestroyInfo)(void* a);
  void (*PrintKey)(const void* a);
  void (*PrintInfo)(void* a);
  /*  A sentinel is used for root and for nil.  These sentinels are */
  /*  created when RBTreeCreate is caled.  root->left should always */
  /*  point to the node which is the root of the tree.  nil points to a */
  /*  node which should always be black but has aribtrary children and */
  /*  parent and no key or info.  The point of using these sentinels is so */
  /*  that the root and nil nodes do not require special cases in the code */
  rb_node* root;             
  rb_node* nil;              
} rb_tree;

rb_tree* RBTreeCreate(int  (*CompFunc)(const void*, const void*),
			     void (*DestFunc)(void*), 
			     void (*InfoDestFunc)(void*), 
			     void (*PrintFunc)(const void*),
			     void (*PrintInfo)(void*));
rb_node * RBTreeInsert(rb_tree*, void* key, void* info);
void RBTreePrint(rb_tree*);
void RBDelete(rb_tree* , rb_node* );
void RBTreeDestroy(rb_tree*);
rb_node* TreePredecessor(rb_tree*,rb_node*);
rb_node* TreeSuccessor(rb_tree*,rb_node*);
rb_node* RBExactQuery(rb_tree*, void*);
stk_stack * RBEnumerate(rb_tree* tree,void* low, void* high);
void NullFunction(void*);

#endif /* RED_BLACK_TREE_INCLUDED */
