#include"rbtree.h"
#include<stdio.h>
#include<ctype.h>


/*  this file has functions to test a red-black tree of integers */

void InfoPrint(void* a) {
  ;
}

void InfoDestroy(void *a){
  ;
}

void* InfoCopy(void *a){
  return a;
}

int main() {
  Stack* enumResult;
  int option=0;
  int newKey,newKey2;
  int *newInt;
  RBNode *newNode;
  RBTree *tree, *treeCopy;

  tree=newRBTree(IntCompare,IntCopy,InfoCopy,IntDestroy,InfoDestroy,IntPrint,InfoPrint);
  while(option!=9) {
    printf("choose one of the following:\n");
    printf("(0) copy tree\n(1) add to tree\n(2) delete from tree\n(3) query\n");
    printf("(4) find predecessor\n(5) find sucessor\n(6) enumerate\n");
    printf("(7) print tree\n(8) tree size\n(9) quit\n");
    do option=fgetc(stdin); while(-1 != option && isspace(option));
    option-='0';
    switch(option)
      {
      case 0:
	treeCopy = RBTreeDeepCopy (tree);
	deleteRBTree (tree);
	tree = treeCopy;
	break;
      case 1:
	{
	  printf("type key for new node\n");
	  scanf("%i",&newKey);
	  newInt=(int*) SafeMalloc(sizeof(int));
	  *newInt=newKey;
	  RBTreeInsert(tree,newInt,0);
	}
	break;
	
      case 2:
	{
	  printf("type key of node to remove\n");
	  scanf("%i",&newKey);
	  if ( ( newNode=RBTreeFind(tree,&newKey ) ) ) RBTreeEraseUnguarded(tree,newNode);/*assignment*/
	  else printf("key not found in tree, no action taken\n");
	}
	break;

      case 3:
	{
	  printf("type key of node to query for\n");
	  scanf("%i",&newKey);
	  if ( ( newNode = RBTreeFind(tree,&newKey) ) ) {/*assignment*/
	    printf("data found in tree at location %lx\n",(unsigned long)newNode);
	  } else {
	    printf("data not in tree\n");
	  }
	}
	break;
      case 4:
	{
	  printf("type key of node to find predecessor of\n");
	  scanf("%i",&newKey);
	  if ( ( newNode = RBTreeFind(tree,&newKey) ) ) {/*assignment*/
	    newNode=RBTreePredecessor(tree,newNode);
	    if(tree->nil == newNode) {
	      printf("there is no predecessor for that node (it is a minimum)\n");
	    } else {
	      printf("predecessor has key %i\n",*(int*)newNode->key);
	    }
	  } else {
	    printf("data not in tree\n");
	  }
	}
	break;
      case 5:
	{
	  printf("type key of node to find successor of\n");
	  scanf("%i",&newKey);
	  if ( (newNode = RBTreeFind(tree,&newKey) ) ) {
	    newNode=RBTreeSuccessor(tree,newNode);
	    if(tree->nil == newNode) {
	      printf("there is no successor for that node (it is a maximum)\n");
	    } else {
	      printf("successor has key %i\n",*(int*)newNode->key);
	    }
	  } else {
	    printf("data not in tree\n");
	  }
	}
	break;
      case 6:
	{
	  printf("type low and high keys to see all keys between them\n");
	  scanf("%i %i",&newKey,&newKey2);
	  enumResult=RBTreeEnumerate(tree,&newKey,&newKey2);	  
	  while ( (newNode = StackPop(enumResult)) ) {
	    tree->PrintKey(newNode->key);
	    printf("\n");
	  }
	  deleteStack(enumResult);
	}
	break;
      case 7:
	{
	  RBTreePrint(tree);
	}
	break;
      case 8:
	{
	  printf("Size of tree: %d\n", (int) RBTreeSize(tree));
	}
	break;
      case 9:
	{
	  deleteRBTree(tree);
	  return 0;
	}
	break;
      default:
	printf("Invalid input; Please try again.\n");
      }
  }
  return 0;
}
