#include "notify.h"

CellWatcher* newCellWatcher (WriteInterceptFunction intercept, void *context, DestroyFunction contextDestroy) {
  CellWatcher* watcher;
  watcher = SafeMalloc (sizeof (CellWatcher));
  watcher->intercept = intercept;
  watcher->context = context;
  watcher->contextDestroy = contextDestroy;
  return watcher;
}

void deleteCellWatcher (CellWatcher* watcher) {
  (*watcher->contextDestroy) (watcher->context);
  SafeFree (watcher);
}

int registerCellWatcher (Board *board, int x, int y, CellWatcher *watcher) {
  int i;
  i = boardIndex(board->size,x,y);
  if (board->watcher[i])
    return 0;
  board->watcher[i] = watcher;
  return 1;
}
