#ifndef STRINGMAP_INCLUDED
#define STRINGMAP_INCLUDED

#include "rbtree.h"
#include "list.h"
#include "vector.h"

/* container functions for strings */
void* StringNew(char *a);
void* StringCopy(void* a);
void StringDestroy(void* a);
int StringCompare(void* a, void* b);
void StringPrint(void* a);

/* mappings from strings to values - basically wrappers for RBTree functions */
typedef RBTree StringMap;
typedef RBNode StringMapNode;
#define newStringMap(ValueCopyFunc,ValueDestroyFunc,ValuePrintFunc) ((StringMap*) newRBTree (StringCompare, StringCopy, ValueCopyFunc, StringDestroy, ValueDestroyFunc, StringPrint, ValuePrintFunc))
#define deleteStringMap(STRINGMAPPTR) deleteRBTree ((RBTree*) STRINGMAPPTR)
#define StringMapInsert(STRINGMAPPTR,STRING,VALUE) ((StringMapNode*) RBTreeInsert ((RBTree*) STRINGMAPPTR, (void*) StringNew(STRING), VALUE))
#define StringMapErase(STRINGMAPPTR,STRING) RBTreeErase ((RBTree*) STRINGMAPPTR, (void*) STRING)
#define StringMapFind(STRINGMAPPTR,STRING) ((StringMapNode*) RBTreeFind ((RBTree*) STRINGMAPPTR, (void*) STRING))

/* typedefs & macros for StringSet, a value-less StringMap */
typedef StringMap StringSet;
typedef StringMapNode StringSetNode;
#define newStringSet() ((StringSet*) newStringMap (NullCopyFunction, NullDestroyFunction, NullPrintFunction))
#define deleteStringSet(STRINGSETPTR) deleteStringMap((StringMap*)STRINGSETPTR)
#define StringSetInsert(STRINGSETPTR,STRING) ((StringSetNode*) StringMapInsert((StringMap*)STRINGSETPTR,STRING,NULL))
#define StringSetErase(STRINGSETPTR,STRING) StringMapErase((StringMap*)STRINGSETPTR,STRING)
#define StringSetFind(STRINGSETPTR,STRING) ((StringSetNode*) StringMapFind((StringMap*)STRINGSETPTR,STRING))


#endif /* STRINGMAP_INCLUDED */
