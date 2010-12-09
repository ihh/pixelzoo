#include "list.h"
#include "util.h"

ListNode* newListNode (void* value);

List* newList(void (*DestroyFunc)(void*),
	      void (*PrintFunc)(const void*)) {
  List* list;
  list = SafeMalloc (sizeof(List));
  list->Destroy = DestroyFunc;
  list->Print = PrintFunc;
  list->head = list->tail = NULL;
  return list;
}

void deleteList(List* list) {
  ListNode* node;
  for (node = list->head; node != NULL; node = node->next)
    (*list->Destroy) (node->value);
  for (node = list->head; node != NULL; node = node->next)
    free (node);
  free (list);
}

void ListInsertBefore(List* list, ListNode* node, void* value) {
  ListNode* newNode;
  newNode = newListNode(value);
  if (node == NULL) {
    /* insert at end of list */
    if (list->tail) {
      list->tail->next = newNode;
      newNode->prev = list->tail;
    }
    if (list->head == NULL)
      list->head = newNode;
    list->tail = newNode;
  } else {
    /* insert before node */
    if (node->prev) {
      node->prev->next = newNode;
      newNode->prev = node->prev;
    }
    node->prev = newNode;
    newNode->next = node;
    if (list->head == node)
      list->head = newNode;
    if (list->tail == NULL)
      list->tail = newNode;
  }
}

void ListErase(List* list, ListNode* node) {
  Assert (node != NULL, "ListErase: null node pointer");
  if (node->prev)
    node->prev->next = node->next;
  if (node->next)
    node->next->prev = node->prev;
  if (list->head == node)
    list->head = node->next;
  if (list->tail == node)
    list->tail = node->prev;
  (*list->Destroy) (node->value);
  free (node);
}

void ListPrint (List* list) {
  ListNode* node;
  for (node = list->head; node != NULL; node = node->next)
    (*list->Print) (node->value);
}

ListNode* newListNode (void* value) {
  ListNode* newNode;
  newNode = SafeMalloc (sizeof (ListNode));
  newNode->value = value;
  newNode->prev = newNode->next = NULL;
  return newNode;
}
