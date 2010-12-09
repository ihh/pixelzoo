#ifndef DOUBLY_LINKED_LIST_INCLUDED
#define DOUBLY_LINKED_LIST_INCLUDED

#include <stdlib.h>

typedef struct ListNode {
  void* value;
  struct ListNode *next, *prev;
} ListNode;

typedef struct List {
  void (*Destroy)(void* a);
  void (*Print)(const void* a);
  ListNode *head, *tail;
} List;

List* newList(void (*DestroyFunc)(void*),
	      void (*PrintFunc)(const void*));
void deleteList(List* list);
void ListInsertBefore(List* list, ListNode* node, void* value);  /* call with node==NULL to insert at end of list */
void ListErase(List* list, ListNode* node);
void ListPrint (List* list);  /* debug */

#define ListAppend(LIST,VALUE) ListInsertBefore(LIST,NULL,VALUE)
#define ListPrepend(LIST,VALUE) ListInsertBefore(LIST,(LIST)->head,VALUE)

#endif /* DOUBLY_LINKED_LIST_INCLUDED */
