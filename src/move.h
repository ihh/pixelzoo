#ifndef MOVE_INCLUDED
#define MOVE_INCLUDED

#include "util.h"
#include "rule.h"

typedef struct Move {
  int64_Microticks t;
  int x, y;
  State state;
} Move;

Move* newMove (int64_Microticks t, int x, int y, State state);
void deleteMove (void* move);
void* copyMove (void* move);

typedef List MoveList;
typedef ListNode MoveListNode;

#define newMoveList() ((MoveList*) newList (copyMove, deleteMove, NullPrintFunction))
#define deleteMoveList(MLPTR) deleteList ((List*) MLPTR)
#define MoveListEmpty(MLPTR) ListEmpty ((List*) MLPTR)
#define MoveListSize(MLPTR) ListSize ((List*) MLPTR)
#define MoveListInsertBefore(MLPTR,MLNODEPTR,T,X,Y,S) ((MoveListNode*) ListInsertBefore ((List*) MLPTR, (ListNode*) MLNODEPTR, (void*) newMove(T,X,Y,S)))
#define MoveListAppend(MLPTR,T,X,Y,S) MoveListInsertBefore (MLPTR, NULL, T, X, Y, S)
#define MoveListShift(MLPTR) ListShift ((List*) MLPTR)
#define MoveListFront(MLPTR) ((Move*) ((List*) MLPTR)->head->value)

#endif /* MOVE_INCLUDED */
