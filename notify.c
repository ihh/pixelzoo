#include "notify.h"

BoardWatcher* newBoardWatcher (char *name, WriteInterceptFunction intercept, void *context) {
  BoardWatcher* watcher;
  watcher = SafeMalloc (sizeof (BoardWatcher));
  watcher->name = (char*) StringCopy ((void*) name);
  watcher->intercept = intercept;
  watcher->context = context;
  return watcher;
}

void deleteBoardWatcher (BoardWatcher* watcher) {
  StringDelete (watcher->name);
  SafeFree (watcher);
}
