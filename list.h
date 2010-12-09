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
void ListErase(List* list, ListNode* node);
void ListPrint (List* list);  /* debug */

#define ListAppend(LIST,VALUE) ListInsertBefore(LIST,NULL,VALUE)
#define ListPrepend(LIST,VALUE) ListInsertBefore(LIST,(LIST)->head,VALUE)

#define ListEmpty(LIST) ((LIST)->head == NULL)

/* void versions of print & delete */
void ListPrintVoid(const void*);
void ListDeleteVoid(void*);

#endif /* DOUBLY_LINKED_LIST_INCLUDED */
