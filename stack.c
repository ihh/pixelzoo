#include "stack.h"

int StackNotEmpty(stk_stack * theStack) {
  return( theStack ? (theStack->top != 0) : 0);
}

stk_stack * StackJoin(stk_stack * stack1, stk_stack * stack2) {
  if (!stack1->tail) {
    free(stack1);
    return(stack2);
  } else {
    stack1->tail->next=stack2->top;
    stack1->tail=stack2->tail;
    free(stack2);
    return(stack1);
  }
}

stk_stack * newStack() {
  stk_stack * theStack;
  
  theStack=(stk_stack *) SafeMalloc(sizeof(stk_stack));
  theStack->top=theStack->tail=NULL;
  return(theStack);
}


void StackPush(stk_stack * theStack, void* newInfoPointer) {
  stk_stack_node * newNode;

  if(!theStack->top) {
    newNode=(stk_stack_node *) SafeMalloc(sizeof(stk_stack_node));
    newNode->info=newInfoPointer;
    newNode->next=theStack->top;
    theStack->top=newNode;
    theStack->tail=newNode;
  } else {
    newNode=(stk_stack_node *) SafeMalloc(sizeof(stk_stack_node));
    newNode->info=newInfoPointer;
    newNode->next=theStack->top;
    theStack->top=newNode;
  }
  
}

void* StackPop(stk_stack * theStack) {
  void* popInfo;
  stk_stack_node * oldNode;

  if(theStack->top) {
    popInfo=theStack->top->info;
    oldNode=theStack->top;
    theStack->top=theStack->top->next;
    free(oldNode);
    if (!theStack->top) theStack->tail=NULL;
  } else {
    popInfo=NULL;
  }
  return(popInfo);
}
