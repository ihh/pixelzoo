#ifndef STATE_MAP_INCLUDED
#define STATE_MAP_INCLUDED

#include "rbtree.h"
#include "rule.h"

/* State new/delete/compare/print functions */
State* newState(State s);
void deleteState(void* a);
int compareState(void* a,void* b);
void printState(void* a);

/* StateMap, a map from State's (particle states) to arbitrary values */
typedef RBTree StateMap;
typedef RBNode StateMapNode;
#define newStateMap(ValueDestroyFunc,ValuePrintFunc) ((StateMap*) newRBTree(compareState,deleteState,ValueDestroyFunc,printState,ValuePrintFunc))
#define deleteStateMap(STATEMAPPTR) deleteRBTree((RBTree*)STATEMAPPTR)
#define StateMapInsert(STATEMAPPTR,STATE,VALUE) ((StateMapNode*) RBTreeInsert((RBTree*)STATEMAPPTR,(void*)newState(STATE),(void*)VALUE)
#define StateMapErase(STATEMAPPTR,STATE) RBTreeErase((RBTree*)STATEMAPPTR,(void*)&STATE)
#define StateMapFind(STATEMAPPTR,STATE) ((StateMapNode*) RBTreeFind((RBTree*)STATEMAPPTR,(void*)&STATE))

/* StateSet, a value-less StateMap */
typedef StateMap StateSet;
typedef StateMapNode StateSetNode;
#define newStateSet() ((StateSet*) newStateMap (NullDestroyFunction, NullPrintFunction))
#define deleteStateSet(STATESETPTR) deleteStateMap((StateMap*)STATESETPTR)
#define StateSetInsert(STATESETPTR,STATE) ((StateSetNode*) StateMapInsert((StateMap*)STATESETPTR,STATE,NULL)
#define StateSetErase(STATESETPTR,STATE) StateMapErase((StateMap*)STATESETPTR,STATE)
#define StateSetFind(STATESETPTR,STATE) ((StateSetNode*) StateMapFind((StateMap*)STATESETPTR,STATE))

#endif /* STATE_MAP_INCLUDED */