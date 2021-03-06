#include <stdio.h>
#include <stdlib.h>
#include "rbtree.h"

/***********************************************************************/
/*  FUNCTION:  newRBTree */
/**/
/*  INPUTS:  All the inputs are names of functions.  CompFunc takes to */
/*  void pointers to keys and returns 1 if the first arguement is */
/*  "greater than" the second.   DestFunc takes a pointer to a key and */
/*  destroys it in the appropriate manner when the node containing that */
/*  key is deleted.  ValueDestFunc is similiar to DestFunc except it */
/*  recieves a pointer to the value of a node and destroys it. */
/*  PrintFunc recieves a pointer to the key of a node and prints it. */
/*  PrintValue recieves a pointer to the value of a node and prints it. */
/*  If RBTreePrint is never called the print functions don't have to be */
/*  defined and NullPrintFunction can be used.  */
/**/
/*  OUTPUT:  This function returns a pointer to the newly created */
/*  red-black tree. */
/**/
/*  Modifies Input: none */
/***********************************************************************/

RBTree* newRBTree( int (*CompFunc) (void*,void*),
		   void* (*CopyFunc) (void*),
		   void* (*ValueCopyFunc) (void*),
		   void (*DestFunc) (void*),
		   void (*ValueDestFunc) (void*),
		   void (*PrintFunc) (void*),
		   void (*PrintValue)(void*)) {
  RBTree* newTree;
  RBNode* temp;

  newTree=(RBTree*) SafeMalloc(sizeof(RBTree));
  newTree->Compare=  CompFunc;
  newTree->CopyKey= CopyFunc;
  newTree->CopyValue= ValueCopyFunc;
  newTree->DestroyKey= DestFunc;
  newTree->PrintKey= PrintFunc;
  newTree->PrintValue= PrintValue;
  newTree->DestroyValue= ValueDestFunc;

  /*  see the comment in the RBTree structure in red_black_tree.h */
  /*  for valuermation on nilt and root */
  temp=newTree->nilt= (RBNode*) SafeMalloc(sizeof(RBNode));
  temp->parent=temp->left=temp->right=temp;
  temp->red=0;
  temp->key=0;
  temp=newTree->root= (RBNode*) SafeMalloc(sizeof(RBNode));
  temp->parent=temp->left=temp->right=newTree->nilt;
  temp->key=0;
  temp->red=0;
  return(newTree);
}

RBTree* RBTreeDeepCopy(RBTree* tree) {
  Stack *stack;
  RBNode *treeNode;
  RBTree *newTree;
  Assert (tree->CopyKey != NULL && tree->CopyValue != NULL, "RBTreeDeepCopy: no copy constructors provided");
  newTree = newRBTree (tree->Compare, tree->CopyKey, tree->CopyValue, tree->DestroyKey, tree->DestroyValue, tree->PrintKey, tree->PrintValue);
  stack = RBTreeEnumerate (tree, NULL, NULL);
  while (!StackEmpty (stack)) {
    treeNode = (RBNode*) StackPop (stack);
    RBTreeInsert (newTree, (*tree->CopyKey) (treeNode->key), (*tree->CopyValue) (treeNode->value));
  }
  deleteStack (stack);
  return newTree;
}

RBTree* RBTreeShallowCopy(RBTree* tree) {
  RBTree* newTree;
  CopyFunction copyKey, copyValue;
  copyKey = tree->CopyKey;
  copyValue = tree->CopyValue;
  tree->CopyKey = tree->CopyValue = NullCopyFunction;
  newTree = RBTreeDeepCopy (tree);
  newTree->DestroyKey = newTree->DestroyValue = NullDestroyFunction;
  tree->CopyKey = copyKey;
  tree->CopyValue = copyValue;
  return newTree;
}

/***********************************************************************/
/*  FUNCTION:  LeftRotate */
/**/
/*  INPUTS:  This takes a tree so that it can access the appropriate */
/*           root and nilt pointers, and the node to rotate on. */
/**/
/*  OUTPUT:  None */
/**/
/*  Modifies Input: tree, x */
/**/
/*  EFFECTS:  Rotates as described in _Introduction_To_Algorithms by */
/*            Cormen, Leiserson, Rivest (Chapter 14).  Basically this */
/*            makes the parent of x be to the left of x, x the parent of */
/*            its parent before the rotation and fixes other pointers */
/*            accordingly. */
/***********************************************************************/

void LeftRotate(RBTree* tree, RBNode* x) {
  RBNode* y;
  RBNode* nilt=tree->nilt;

  /*  I originally wrote this function to use the sentinel for */
  /*  nilt to avoid checking for nilt.  However this introduces a */
  /*  very subtle bug because sometimes this function modifies */
  /*  the parent pointer of nilt.  This can be a problem if a */
  /*  function which calls LeftRotate also uses the nilt sentinel */
  /*  and expects the nilt sentinel's parent pointer to be unchanged */
  /*  after calling this function.  For example, when RBTreeEraseFixUP */
  /*  calls LeftRotate it expects the parent pointer of nilt to be */
  /*  unchanged. */

  y=x->right;
  x->right=y->left;

  if (y->left != nilt) y->left->parent=x; /* used to use sentinel here */
  /* and do an unconditional assignment instead of testing for nilt */
  
  y->parent=x->parent;   

  /* instead of checking if x->parent is the root as in the book, we */
  /* count on the root sentinel to implicitly take care of this case */
  if( x == x->parent->left) {
    x->parent->left=y;
  } else {
    x->parent->right=y;
  }
  y->left=x;
  x->parent=y;

#ifdef DEBUG_ASSERT
  Assert(!tree->nilt->red,"nilt not red in LeftRotate");
#endif
}


/***********************************************************************/
/*  FUNCTION:  RighttRotate */
/**/
/*  INPUTS:  This takes a tree so that it can access the appropriate */
/*           root and nilt pointers, and the node to rotate on. */
/**/
/*  OUTPUT:  None */
/**/
/*  Modifies Input?: tree, y */
/**/
/*  EFFECTS:  Rotates as described in _Introduction_To_Algorithms by */
/*            Cormen, Leiserson, Rivest (Chapter 14).  Basically this */
/*            makes the parent of x be to the left of x, x the parent of */
/*            its parent before the rotation and fixes other pointers */
/*            accordingly. */
/***********************************************************************/

void RightRotate(RBTree* tree, RBNode* y) {
  RBNode* x;
  RBNode* nilt=tree->nilt;

  /*  I originally wrote this function to use the sentinel for */
  /*  nilt to avoid checking for nilt.  However this introduces a */
  /*  very subtle bug because sometimes this function modifies */
  /*  the parent pointer of nilt.  This can be a problem if a */
  /*  function which calls LeftRotate also uses the nilt sentinel */
  /*  and expects the nilt sentinel's parent pointer to be unchanged */
  /*  after calling this function.  For example, when RBTreeEraseFixUP */
  /*  calls LeftRotate it expects the parent pointer of nilt to be */
  /*  unchanged. */

  x=y->left;
  y->left=x->right;

  if (nilt != x->right)  x->right->parent=y; /*used to use sentinel here */
  /* and do an unconditional assignment instead of testing for nilt */

  /* instead of checking if x->parent is the root as in the book, we */
  /* count on the root sentinel to implicitly take care of this case */
  x->parent=y->parent;
  if( y == y->parent->left) {
    y->parent->left=x;
  } else {
    y->parent->right=x;
  }
  x->right=y;
  y->parent=x;

#ifdef DEBUG_ASSERT
  Assert(!tree->nilt->red,"nilt not red in RightRotate");
#endif
}

/***********************************************************************/
/*  FUNCTION:  TreeInsertHelp  */
/**/
/*  INPUTS:  tree is the tree to insert into and z is the node to insert */
/**/
/*  OUTPUT:  none */
/**/
/*  Modifies Input:  tree, z */
/**/
/*  EFFECTS:  Inserts z into the tree as if it were a regular binary tree */
/*            using the algorithm described in _Introduction_To_Algorithms_ */
/*            by Cormen et al.  This funciton is only intended to be called */
/*            by the RBTreeInsert function and not by the user */
/***********************************************************************/

void TreeInsertHelp(RBTree* tree, RBNode* z) {
  /*  This function should only be called by InsertRBTree (see above) */
  RBNode* x;
  RBNode* y;
  RBNode* nilt=tree->nilt;
  
  z->left=z->right=nilt;
  y=tree->root;
  x=tree->root->left;
  while( x != nilt) {
    y=x;
    if (0 < (*tree->Compare)(x->key,z->key)) { /* x.key > z.key */
      x=x->left;
    } else { /* x,key <= z.key */
      x=x->right;
    }
  }
  z->parent=y;
  if ( (y == tree->root) ||
       (0 < (*tree->Compare)(y->key,z->key))) { /* y.key > z.key */
    y->left=z;
  } else {
    y->right=z;
  }

#ifdef DEBUG_ASSERT
  Assert(!tree->nilt->red,"nilt not red in TreeInsertHelp");
#endif
}

/*  Before calling Insert RBTree the node x should have its key set */

/***********************************************************************/
/*  FUNCTION:  RBTreeInsert */
/**/
/*  INPUTS:  tree is the red-black tree to insert a node which has a key */
/*           pointed to by key and value pointed to by value.  */
/**/
/*  OUTPUT:  This function returns a pointer to the newly inserted node */
/*           which is guarunteed to be valid until this node is deleted. */
/*           What this means is if another data structure stores this */
/*           pointer then the tree does not need to be searched when this */
/*           is to be deleted. */
/**/
/*  Modifies Input: tree */
/**/
/*  EFFECTS:  Creates a node node which contains the appropriate key and */
/*            value pointers and inserts it into the tree. */
/***********************************************************************/

RBNode * RBTreeInsert(RBTree* tree, void* key, void* value) {
  RBNode * y;
  RBNode * x;
  RBNode * newNode;

  x=(RBNode*) SafeMalloc(sizeof(RBNode));
  x->key=(*tree->CopyKey) (key);
  x->value=value;

  TreeInsertHelp(tree,x);
  newNode=x;
  x->red=1;
  while(x->parent->red) { /* use sentinel instead of checking for root */
    if (x->parent == x->parent->parent->left) {
      y=x->parent->parent->right;
      if (y->red) {
	x->parent->red=0;
	y->red=0;
	x->parent->parent->red=1;
	x=x->parent->parent;
      } else {
	if (x == x->parent->right) {
	  x=x->parent;
	  LeftRotate(tree,x);
	}
	x->parent->red=0;
	x->parent->parent->red=1;
	RightRotate(tree,x->parent->parent);
      } 
    } else { /* case for x->parent == x->parent->parent->right */
      y=x->parent->parent->left;
      if (y->red) {
	x->parent->red=0;
	y->red=0;
	x->parent->parent->red=1;
	x=x->parent->parent;
      } else {
	if (x == x->parent->left) {
	  x=x->parent;
	  RightRotate(tree,x);
	}
	x->parent->red=0;
	x->parent->parent->red=1;
	LeftRotate(tree,x->parent->parent);
      } 
    }
  }
  tree->root->left->red=0;
  return(newNode);

#ifdef DEBUG_ASSERT
  Assert(!tree->nilt->red,"nilt not red in RBTreeInsert");
  Assert(!tree->root->red,"root not red in RBTreeInsert");
#endif
}

/***********************************************************************/
/*  FUNCTION:  RBTreeSuccessor  */
/**/
/*    INPUTS:  tree is the tree in question, and x is the node we want the */
/*             the successor of. */
/**/
/*    OUTPUT:  This function returns the successor of x or NULL if no */
/*             successor exists. */
/**/
/*    Modifies Input: none */
/**/
/*    Note:  uses the algorithm in _Introduction_To_Algorithms_ */
/***********************************************************************/
  
RBNode* RBTreeSuccessor(RBTree* tree,RBNode* x) { 
  RBNode* y;
  RBNode* nilt=tree->nilt;
  RBNode* root=tree->root;

  if (nilt != (y = x->right)) { /* assignment to y is intentional */
    while(y->left != nilt) { /* returns the minium of the right subtree of x */
      y=y->left;
    }
    return(y);
  } else {
    y=x->parent;
    while(x == y->right) { /* sentinel used instead of checking for nilt */
      x=y;
      y=y->parent;
    }
    if (y == root) return(nilt);
    return(y);
  }
}

/***********************************************************************/
/*  FUNCTION:  Treepredecessor  */
/**/
/*    INPUTS:  tree is the tree in question, and x is the node we want the */
/*             the predecessor of. */
/**/
/*    OUTPUT:  This function returns the predecessor of x or NULL if no */
/*             predecessor exists. */
/**/
/*    Modifies Input: none */
/**/
/*    Note:  uses the algorithm in _Introduction_To_Algorithms_ */
/***********************************************************************/

RBNode* RBTreePredecessor(RBTree* tree, RBNode* x) {
  RBNode* y;
  RBNode* nilt=tree->nilt;
  RBNode* root=tree->root;

  if (nilt != (y = x->left)) { /* assignment to y is intentional */
    while(y->right != nilt) { /* returns the maximum of the left subtree of x */
      y=y->right;
    }
    return(y);
  } else {
    y=x->parent;
    while(x == y->left) { 
      if (y == root) return(nilt); 
      x=y;
      y=y->parent;
    }
    return(y);
  }
}

/***********************************************************************/
/*  FUNCTION:  InorderTreePrint */
/**/
/*    INPUTS:  tree is the tree to print and x is the current inorder node */
/**/
/*    OUTPUT:  none  */
/**/
/*    EFFECTS:  This function recursively prints the nodes of the tree */
/*              inorder using the PrintKey and PrintValue functions. */
/**/
/*    Modifies Input: none */
/**/
/*    Note:    This function should only be called from RBTreePrint */
/***********************************************************************/

void InorderTreePrint(RBTree* tree, RBNode* x) {
  RBNode* nilt=tree->nilt;
  RBNode* root=tree->root;
  if (x != tree->nilt) {
    InorderTreePrint(tree,x->left);
    printf("value=");
    tree->PrintValue(x->value);
    printf("  key="); 
    tree->PrintKey(x->key);
    printf("  l->key=");
    if( x->left == nilt) printf("NULL"); else tree->PrintKey(x->left->key);
    printf("  r->key=");
    if( x->right == nilt) printf("NULL"); else tree->PrintKey(x->right->key);
    printf("  p->key=");
    if( x->parent == root) printf("NULL"); else tree->PrintKey(x->parent->key);
    printf("  red=%i\n",x->red);
    InorderTreePrint(tree,x->right);
  }
}


/***********************************************************************/
/*  FUNCTION:  TreeDestHelper */
/**/
/*    INPUTS:  tree is the tree to destroy and x is the current node */
/**/
/*    OUTPUT:  none  */
/**/
/*    EFFECTS:  This function recursively destroys the nodes of the tree */
/*              postorder using the DestroyKey and DestroyValue functions. */
/**/
/*    Modifies Input: tree, x */
/**/
/*    Note:    This function should only be called by deleteRBTree */
/***********************************************************************/

void TreeDestHelper(RBTree* tree, RBNode* x) {
  RBNode* nilt=tree->nilt;
  if (x != nilt) {
    TreeDestHelper(tree,x->left);
    TreeDestHelper(tree,x->right);
    tree->DestroyKey(x->key);
    tree->DestroyValue(x->value);
    SafeFree(x);
  }
}


/***********************************************************************/
/*  FUNCTION:  deleteRBTree */
/**/
/*    INPUTS:  tree is the tree to destroy */
/**/
/*    OUTPUT:  none */
/**/
/*    EFFECT:  Destroys the key and frees memory */
/**/
/*    Modifies Input: tree */
/**/
/***********************************************************************/

void deleteRBTree(RBTree* tree) {
  TreeDestHelper(tree,tree->root->left);
  SafeFree(tree->root);
  SafeFree(tree->nilt);
  SafeFree(tree);
}


/***********************************************************************/
/*  FUNCTION:  RBTreePrint */
/**/
/*    INPUTS:  tree is the tree to print */
/**/
/*    OUTPUT:  none */
/**/
/*    EFFECT:  This function recursively prints the nodes of the tree */
/*             inorder using the PrintKey and PrintValue functions. */
/**/
/*    Modifies Input: none */
/**/
/***********************************************************************/

void RBTreePrint(RBTree* tree) {
  InorderTreePrint(tree,tree->root->left);
}


/***********************************************************************/
/*  FUNCTION:  RBTreeFind */
/**/
/*    INPUTS:  tree is the tree to print and q is a pointer to the key */
/*             we are searching for */
/**/
/*    OUTPUT:  returns the a node with key equal to q.  If there are */
/*             multiple nodes with key equal to q this function returns */
/*             the one highest in the tree */
/**/
/*    Modifies Input: none */
/**/
/***********************************************************************/
  
RBNode* RBTreeFind(RBTree* tree, void* q) {
  RBNode* x=tree->root->left;
  RBNode* nilt=tree->nilt;
  int compVal;
  if (x == nilt) return(0);
  compVal=(*tree->Compare)(x->key,(int*) q);
  while(0 != compVal) {/*assignemnt*/
    if (0 < compVal) { /* x->key > q */
      x=x->left;
    } else {
      x=x->right;
    }
    if ( x == nilt) return(0);
    compVal=(*tree->Compare)(x->key,(int*) q);
  }
  return(x);
}


/***********************************************************************/
/*  FUNCTION:  RBTreeEraseFixUp */
/**/
/*    INPUTS:  tree is the tree to fix and x is the child of the spliced */
/*             out node in RBTreeEraseUnguarded. */
/**/
/*    OUTPUT:  none */
/**/
/*    EFFECT:  Performs rotations and changes colors to restore red-black */
/*             properties after a node is deleted */
/**/
/*    Modifies Input: tree, x */
/**/
/*    The algorithm from this function is from _Introduction_To_Algorithms_ */
/***********************************************************************/

void RBTreeEraseFixUp(RBTree* tree, RBNode* x) {
  RBNode* root=tree->root->left;
  RBNode* w;

  while( (!x->red) && (root != x)) {
    if (x == x->parent->left) {
      w=x->parent->right;
      if (w->red) {
	w->red=0;
	x->parent->red=1;
	LeftRotate(tree,x->parent);
	w=x->parent->right;
      }
      if ( (!w->right->red) && (!w->left->red) ) { 
	w->red=1;
	x=x->parent;
      } else {
	if (!w->right->red) {
	  w->left->red=0;
	  w->red=1;
	  RightRotate(tree,w);
	  w=x->parent->right;
	}
	w->red=x->parent->red;
	x->parent->red=0;
	w->right->red=0;
	LeftRotate(tree,x->parent);
	x=root; /* this is to exit while loop */
      }
    } else { /* the code below is has left and right switched from above */
      w=x->parent->left;
      if (w->red) {
	w->red=0;
	x->parent->red=1;
	RightRotate(tree,x->parent);
	w=x->parent->left;
      }
      if ( (!w->right->red) && (!w->left->red) ) { 
	w->red=1;
	x=x->parent;
      } else {
	if (!w->left->red) {
	  w->right->red=0;
	  w->red=1;
	  LeftRotate(tree,w);
	  w=x->parent->left;
	}
	w->red=x->parent->red;
	x->parent->red=0;
	w->left->red=0;
	RightRotate(tree,x->parent);
	x=root; /* this is to exit while loop */
      }
    }
  }
  x->red=0;

#ifdef DEBUG_ASSERT
  Assert(!tree->nilt->red,"nilt not black in RBTreeEraseFixUp");
#endif
}


/***********************************************************************/
/*  FUNCTION:  RBTreeEraseUnguarded */
/**/
/*    INPUTS:  tree is the tree to delete node z from */
/**/
/*    OUTPUT:  none */
/**/
/*    EFFECT:  Erases z from tree and frees the key and value of z */
/*             using DestroyKey and DestroyValue.  Then calls */
/*             RBTreeEraseFixUp to restore red-black properties */
/**/
/*    Modifies Input: tree, z */
/**/
/*    The algorithm from this function is from _Introduction_To_Algorithms_ */
/***********************************************************************/

void RBTreeEraseUnguarded(RBTree* tree, RBNode* z){
  RBNode* y;
  RBNode* x;
  RBNode* nilt=tree->nilt;
  RBNode* root=tree->root;

  y= ((z->left == nilt) || (z->right == nilt)) ? z : RBTreeSuccessor(tree,z);
  x= (y->left == nilt) ? y->right : y->left;
  if (root == (x->parent = y->parent)) { /* assignment of y->p to x->p is intentional */
    root->left=x;
  } else {
    if (y == y->parent->left) {
      y->parent->left=x;
    } else {
      y->parent->right=x;
    }
  }
  if (y != z) { /* y should not be nilt in this case */

#ifdef DEBUG_ASSERT
    Assert( (y!=tree->nilt),"y is nilt in RBTreeEraseUnguarded\n");
#endif
    /* y is the node to splice out and x is its child */

    if (!(y->red)) RBTreeEraseFixUp(tree,x);
  
    tree->DestroyKey(z->key);
    tree->DestroyValue(z->value);
    y->left=z->left;
    y->right=z->right;
    y->parent=z->parent;
    y->red=z->red;
    z->left->parent=z->right->parent=y;
    if (z == z->parent->left) {
      z->parent->left=y; 
    } else {
      z->parent->right=y;
    }
    SafeFree(z); 
  } else {
    tree->DestroyKey(y->key);
    tree->DestroyValue(y->value);
    if (!(y->red)) RBTreeEraseFixUp(tree,x);
    SafeFree(y);
  }
  
}

void RBTreeErase(RBTree* tree, void* key) {
  RBNode* node;
  if ( ( node = RBTreeFind (tree, key) ) ) /*assignment*/
    RBTreeEraseUnguarded(tree,node);
}


/***********************************************************************/
/*  FUNCTION:  RBTreeEnumerate */
/**/
/*    INPUTS:  tree is the tree to look for keys >= low */
/*             and <= high with respect to the Compare function */
/**/
/*    OUTPUT:  stack containing pointers to the nodes between [low,high] */
/**/
/*    Modifies Input: none */
/***********************************************************************/

Stack* RBTreeEnumerate(RBTree* tree, void* low, void* high) {
  Stack* enumResultStack;
  RBNode* nilt=tree->nilt;
  RBNode* x=tree->root->left;
  RBNode* lastBest=nilt;

  enumResultStack=newStack();
  while(nilt != x) {
    if ( high != NULL && 0 < (*tree->Compare)(x->key,high)) { /* x->key > high */
      x=x->left;
    } else {
      lastBest=x;
      x=x->right;
    }
  }
  while ( (lastBest != nilt) && (low == NULL || 0 >= (*tree->Compare)(low,lastBest->key)) ) {  /* !(low > lastBest->key) == (low <= lastBest->key) */
    StackPush(enumResultStack,lastBest);
    lastBest=RBTreePredecessor(tree,lastBest);
  }
  return(enumResultStack);
}

size_t RBTreeSize(RBTree* tree) {
  size_t size;
  RBNode* nilt=tree->nilt;
  RBNode* y;

  size = 0;
  y = RBTreeLast (tree);
  while ( y != nilt ) {
    ++size;
    y = RBTreePredecessor(tree,y);
  }

  return size;
}

RBNode* RBTreeFirst(RBTree* tree) {
  RBNode* nilt=tree->nilt;
  RBNode* x=tree->root->left;
  RBNode* y=nilt;

  while(nilt != x) {
    y = x;
    x = x->left;
  }

  return y;
}

RBNode* RBTreeLast(RBTree* tree) {
  RBNode* nilt=tree->nilt;
  RBNode* x=tree->root->left;
  RBNode* y=nilt;

  while(nilt != x) {
    y = x;
    x = x->right;
  }

  return y;
}

int RBTreeIsBeforeFirst (RBTree* tree, void* key) {
  return RBTreeSize(tree) && (*tree->Compare) (key, RBTreeFirst(tree)->key) < 0;
}

int RBTreeIsAfterLast (RBTree* tree, void* key) {
  return RBTreeSize(tree) && (*tree->Compare) (key, RBTreeLast(tree)->key) > 0;
}

void* RBTreeDeepCopyVoid(void* rbTree) { return (void*) RBTreeDeepCopy ((RBTree*) rbTree); }
void RBTreePrintVoid(void* rbTree) { RBTreePrint ((RBTree*) rbTree); }
void RBTreeDeleteVoid(void* rbTree) { deleteRBTree ((RBTree*) rbTree); }

void RBTreeRetain(RBTree* t, RBTree* u) {
  Stack* stack;
  void* key;
  stack = RBTreeEnumerate (t, NULL, NULL);
  while (!StackEmpty (stack)) {
    key = ((RBNode*) StackPop (stack))->key;
    if (RBTreeFind ((RBTree*) u, key) == NULL)
      RBTreeErase (t, key);
  }
  deleteStack (stack);
}

void RBTreeRemove(RBTree* t, RBTree* u) {
  Stack* stack;
  void* key;
  stack = RBTreeEnumerate (t, NULL, NULL);
  while (!StackEmpty (stack)) {
    key = ((RBNode*) StackPop (stack))->key;
    if (RBTreeFind ((RBTree*) u, key) != NULL)
      RBTreeErase (t, key);
  }
  deleteStack (stack);
}

int RBTreeIteratorFinished(RBTree* t,RBNode* n) {
  return n == t->nilt;
}
