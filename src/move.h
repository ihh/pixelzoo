#ifndef MOVE_INCLUDED
#define MOVE_INCLUDED

#include "util.h"
#include "rule.h"

typedef struct Move {
  int64_Microticks t;  /* corresponds to Board::microticks */
  signed long long int u;  /* corresponds to Board::updateCount; -1 signifies that update# is unknown or irrelevant, and Move should be scheduled using t */
  int x, y, z;
  State state;
} Move;

Move* newMove (int64_Microticks t, signed long long int u, int x, int y, int z, State state);
void deleteMove (void* move);
void* copyMove (void* move);

typedef List MoveList;
typedef ListNode MoveListNode;

#define newMoveList() ((MoveList*) newList (copyMove, deleteMove, NullPrintFunction))
#define deleteMoveList(MLPTR) deleteList ((List*) MLPTR)
#define MoveListEmpty(MLPTR) ListEmpty ((List*) MLPTR)
#define MoveListSize(MLPTR) ListSize ((List*) MLPTR)
#define MoveListInsertBefore(MLPTR,MLNODEPTR,T,U,X,Y,Z,S) ((MoveListNode*) ListInsertBefore ((List*) MLPTR, (ListNode*) MLNODEPTR, (void*) newMove(T,U,X,Y,Z,S)))
#define MoveListAppend(MLPTR,T,U,X,Y,Z,S) MoveListInsertBefore (MLPTR, NULL, T, U, X, Y, Z, S)
#define MoveListShift(MLPTR) ((Move*) ListShift ((List*) MLPTR))
#define MoveListFront(MLPTR) ((Move*) ((List*) MLPTR)->head->value)

#endif /* MOVE_INCLUDED */
