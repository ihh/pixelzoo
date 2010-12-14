#ifndef STRINGMAP_INCLUDED
#define STRINGMAP_INCLUDED

#include "rbtree.h"
#include "list.h"
#include "vector.h"

/* container functions for strings */
void* StringNew(char *a);
void* StringCopy(void* a);
void StringDelete(void* a);
int StringCompare(void* a, void* b);
void StringPrint(void* a);

/* mappings from strings to values - basically wrappers for RBTree functions */
typedef RBTree StringMap;
typedef RBNode StringMapNode;
#define newStringMap(ValueCopyFunc,ValueDestroyFunc,ValuePrintFunc) ((StringMap*) newRBTree (StringCompare, StringCopy, ValueCopyFunc, StringDelete, ValueDestroyFunc, StringPrint, ValuePrintFunc))
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

/* typedefs & macros for Dictionary, a map from Strings to Strings */
typedef StringMap Dictionary;
typedef StringMapNode DictionaryNode;
#define newDictionary() ((Dictionary*) newStringMap (StringCopy, StringDelete, StringPrint))
#define deleteDictionary(DICTPTR) deleteStringMap((StringMap*)DICTPTR)
#define DictionaryInsert(DICTPTR,STRING1,STRING2) ((DictionaryNode*) StringMapInsert((StringMap*)DICTPTR,STRING1,STRING2))
#define DictionaryErase(DICTPTR,STRING) StringMapErase((StringMap*)DICTPTR,STRING)
#define DictionaryFind(DICTPTR,STRING) ((DictionaryNode*) StringMapFind((StringMap*)DICTPTR,STRING))

/* StringVector */
typedef Vector StringVector;
#define newStringVector() ((StringVector*) newVector (StringCopy, StringDelete, StringPrint))
#define deleteStringVector(STRINGVECPTR) deleteVector ((Vector*) STRINGVECPTR)
#define StringVectorGet(STRINGVECPTR,N) VectorGet ((Vector*) STRINGVECPTR, N)
#define StringVectorSet(STRINGVECPTR,N,STRING) VectorSet ((Vector*) STRINGVECPTR, N, (void*) StringNew(STRING))
#define StringVectorReserve(STRINGVECPTR,N) VectorReserve ((Vector*) STRINGVECPTR, N)
#define StringVectorPushBack(STRINGVECPTR,STRING) VectorPushBack ((Vector*) STRINGVECPTR, (void*) StringNew(STRING))
#define StringVectorSize(STRINGVECPTR) VectorSize ((Vector*) STRINGVECPTR)


#endif /* STRINGMAP_INCLUDED */
