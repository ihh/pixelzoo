#ifndef DOUBLY_LINKED_LIST_INCLUDED
#define DOUBLY_LINKED_LIST_INCLUDED

#include <stdlib.h>
#include "util.h"

typedef struct ListNode {
  void* value;
  struct ListNode *next, *prev;
} ListNode;

typedef struct List {
  DestroyFunction Destroy;
  PrintFunction Print;
  ListNode *head, *tail;
} List;

List* newList(DestroyFunction DestroyFunc,
	      PrintFunction PrintFunc);
void deleteList(List* list);
size_t ListSize (List* list);
void ListInsertBefore(List* list, ListNode* node, void* value);  /* call with node==NULL to insert at end of list */
List* ListSpliceBefore(List* list, ListNode* node, List* subList);  /* frees list & subList, returns new list. Call with node==NULL to append subList at end of list */
void ListErase(List* list, ListNode* node);
void* ListPop (List* list);  /* removes last node in list, returns value; caller must dealloc by calling (*list->Destroy)(poppedItem) */
void* ListShift (List* list);  /* removes first node in list, returns value; caller must dealloc by calling (*list->Destroy)(poppedItem) */
void ListPrint (List* list);  /* debug */

#define ListAppend(LIST,VALUE) ListInsertBefore(LIST,NULL,VALUE)
#define ListPrepend(LIST,VALUE) ListInsertBefore(LIST,(LIST)->head,VALUE)
#define ListJoin(LIST1,LIST2) ListSpliceBefore(LIST1,NULL,LIST2)

#define ListEmpty(LIST) ((LIST)->head == NULL)

/* void versions of print & delete */
void ListPrintVoid(const void*);
void ListDeleteVoid(void*);

/* A Stack is like a List, but never deletes its values & only returns them via StackPop */
typedef List Stack;
typedef ListNode StackNode;
#define newStack() ((Stack*) newList(NullDestroyFunction,NullPrintFunction))
#define deleteStack(STACK) deleteList((List*) STACK)
#define StackJoin(STACK1,STACK2) ((Stack*) ListJoin((List*)STACK1,(List*)STACK2))
#define StackPush(STACK,DATA) ListAppend((List*)STACK,DATA)
#define StackPop(STACK) ListPop((List*)STACK)
#define StackEmpty(STACK) ListEmpty((List*)STACK)

#endif /* DOUBLY_LINKED_LIST_INCLUDED */
