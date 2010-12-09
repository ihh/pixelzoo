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
    SafeFree(node);
  SafeFree(list);
}

size_t ListSize (List* list) {
  size_t n;
  ListNode *node;
  n = 0;
  for (node = list->head; node; node = node->next)
    ++n;
  return n;
}

void ListInsertBefore(List* list, ListNode* node, void* value) {
  ListNode *newNode, *prevNode;
  newNode = newListNode(value);
  prevNode = node ? node->prev : list->tail;
  if (prevNode) {
    prevNode->next = newNode;
    newNode->prev = prevNode;
  }
  if (node) {
    node->prev = newNode;
    newNode->next = node;
  }
  if (list->head == node)
    list->head = newNode;
  if (list->tail == prevNode)
    list->tail = newNode;
}

List* ListSpliceBefore(List* list, ListNode* node, List* subList) {
  ListNode *prevNode;
  Assert (list->Destroy == subList->Destroy, "ListSpliceBefore: list element destructors don't match");
  if (!ListEmpty (subList)) {
    prevNode = node ? node->prev : list->tail;
    *(prevNode ? &prevNode->next : &list->head) = subList->head;
    *(node ? &node->prev : &list->tail) = subList->tail;
    subList->head->prev = prevNode;
    subList->tail->next = node;
  }
  SafeFree(subList);
  return list;
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
  SafeFree(node);
}

void* ListPop (List* list) {
  void* val;
  if (ListEmpty(list))
    val = NULL;
  else {
    val = list->tail->value;
    if (list->tail->prev) {
      list->tail->prev->next = NULL;
      list->tail = list->tail->prev;
    } else
      list->head = list->tail = NULL;
    SafeFree(list->tail);
  }
  return val;
}

void* ListShift (List* list) {
  void* val;
  if (ListEmpty(list))
    val = NULL;
  else {
    val = list->head->value;
    if (list->head->next) {
      list->head->next = NULL;
      list->head = list->head->next;
    } else
      list->head = list->tail = NULL;
    SafeFree(list->head);
  }
  return val;
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

void ListPrintVoid(const void* list) { ListPrint ((List*) list); }
void ListDeleteVoid(void* list) { deleteList ((List*) list); }
