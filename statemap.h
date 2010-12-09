#ifndef STATE_MAP_INCLUDED
#define STATE_MAP_INCLUDED

#include "rbtree.h"
#include "rule.h"

/* StateMap, a map from State's (particle states) to arbitrary values */
State* newState(State s);
void deleteState(void* a);
int compareState(const void* a,const void* b);
void printState(const void* a);
typedef RBTree StateMap;
typedef RBNode StateMapNode;
#define newStateMap(DELETEVAL,PRINTVAL) ((StateMap*) newRBTree(compareState,deleteState,DELETEVAL,printState,PRINTVAL))
#define deleteStateMap(STATEMAP) deleteRBTree((RBTree*)STATEMAP)
#define StateMapInsert(STATEMAP,STATE,VALUE) ((StateMapNode*) RBTreeInsert((RBTree*)STATEMAP,newState(STATE),(void*)VALUE)
#define StateMapErase(STATEMAP,STATE) RBTreeErase((RBTree*)STATEMAP,newState(STATE))
#define StateMapFind(STATEMAP,STATE) ((StateMapNode*) RBTreeFind((RBTree*)STATEMAP,newState(STATE)))

/* StateSet, a value-less StateMap */
typedef StateMap StateSet;
typedef StateMapNode StateSetNode;
StateSet* newStateSet();
#define deleteStateSet(STATESET) deleteStateMap((StateMap*)STATESET)
#define StateSetInsert(STATESET,STATE) ((StateSetNode*) StateMapInsert((StateMap*)STATESET,STATE,NULL)
#define StateSetErase(STATESET,STATE) StateMapErase((StateMap*)STATESET,STATE)
#define StateSetFind(STATESET,STATE) ((StateSetNode*) StateMapFind((StateMap*)STATESET,STATE))

#endif /* STATE_MAP_INCLUDED */
