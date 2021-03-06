#include "notify.h"

CellWatcher* newCellWatcher (WriteInterceptFunction intercept, void *game, void *context, DestroyFunction contextDestroy) {
  CellWatcher* watcher;
  watcher = SafeMalloc (sizeof (CellWatcher));
  watcher->intercept = intercept;
  watcher->game = game;
  watcher->context = context;
  watcher->contextDestroy = contextDestroy;
  return watcher;
}

void deleteCellWatcher (CellWatcher* watcher) {
  (*watcher->contextDestroy) (watcher->context);
  SafeFree (watcher);
}

int registerCellWatcher (Board *board, int x, int y, int z, CellWatcher *watcher) {
  int i;
  i = boardIndex(board->size,x,y,z);
  if (board->watcher[i])
    return 0;
  board->watcher[i] = watcher;
  return 1;
}

void unregisterCellWatcher (Board *board, CellWatcher *watcher) {
  int x, y, z, i, size;
  size = board->size;
  for (x = 0; x < size; ++x)
    for (y = 0; y < size; ++y)
      for (z = 0; z < size; ++z) {
	i = boardIndex(board->size,x,y,z);
	if (board->watcher[i] == watcher)
	  board->watcher[i] = NULL;
      }
}

