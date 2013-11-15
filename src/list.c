#include "list.h"
#include "util.h"

ListNode* newListNode (void* value);

List* newList(void* (*CopyFunc)(void*),
	      void (*DestroyFunc)(void*),
	      void (*PrintFunc)(void*)) {
  List* list;
  list = SafeMalloc (sizeof(List));
  list->Copy = CopyFunc;
  list->Destroy = DestroyFunc;
  list->Print = PrintFunc;
  list->head = list->tail = NULL;
  return list;
}

void deleteList(List* list) {
  ListNode *node, *next;
  for (node = list->head; node != NULL; node = node->next)
    (*list->Destroy) (node->value);
    for (node = list->head; node != NULL; node = next) {
        next = node->next;
        SafeFree(node);
    }
  SafeFree(list);
}

List* ListDeepCopy (List* list) {
  List* copyList;
  ListNode* node;
  copyList = newList (list->Copy, list->Destroy, list->Print);
  for (node = list->head; node; node = node->next)
    ListAppend (copyList, (*list->Copy) (node->value));
  return copyList;
}

size_t ListSize (List* list) {
  size_t n;
  ListNode *node;
  n = 0;
  for (node = list->head; node; node = node->next)
    ++n;
  return n;
}

ListNode* ListInsertBefore(List* list, ListNode* node, void* value) {
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
  return newNode;
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
  ListNode *oldTail;
  void* val;
  if (ListEmpty(list))
    val = NULL;
  else {
    oldTail = list->tail;
    val = oldTail->value;
    if (oldTail->prev) {
      oldTail->prev->next = NULL;
      list->tail = oldTail->prev;
    } else
      list->head = list->tail = NULL;
    SafeFree(oldTail);
  }
  return val;
}

void* ListShift (List* list) {
  ListNode *oldHead;
  void* val;
  if (ListEmpty(list))
    val = NULL;
  else {
    oldHead = list->head;
    val = oldHead->value;
    if (oldHead->next) {
      oldHead->next->prev = NULL;
      list->head = oldHead->next;
    } else
      list->head = list->tail = NULL;
    SafeFree(oldHead);
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

void* ListDeepCopyVoid(void* list) { return (void*) ListDeepCopy ((List*) list); }
void ListPrintVoid(void* list) { ListPrint ((List*) list); }
void ListDeleteVoid(void* list) { deleteList ((List*) list); }
