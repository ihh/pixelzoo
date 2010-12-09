#ifndef STACK_INCLUDED
#define STACK_INCLUDED

#include "util.h"

/*  In contrast to a List, a Stack does not free its info pointers on destruction.
    It is kept around for legacy reasons (RBTree uses it to enumerate the results of a range query on the tree).
    For many stack-like purposes (where referenced data is copied & must be freed on destruction), a List may be a better choice.
    Note that a Stack is more-or-less equivalent to a List with a dummy destroy function, anyway.
 */

typedef struct stk_stack_node {
  void* info;
  struct stk_stack_node * next;
} stk_stack_node;

typedef struct stk_stack { 
  stk_stack_node * top;
  stk_stack_node * tail;
} stk_stack ;

stk_stack* newStack();
#define deleteStack(STACK) free((void*)STACK)

stk_stack* StackJoin(stk_stack* stack1, stk_stack* stack2);
void StackPush(stk_stack* theStack, void* newInfoPointer);
void* StackPop(stk_stack* theStack);
int StackNotEmpty(stk_stack*);

#endif /* STACK_INCLUDED */

