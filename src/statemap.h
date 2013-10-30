#ifndef STATE_MAP_INCLUDED
#define STATE_MAP_INCLUDED

#include "rbtree.h"
#include "state.h"

/* State new/delete/compare/print functions */
State* newState(State s);
void* copyState(void* sPtr);
void deleteState(void* sPtr);
int compareState(void* aPtr,void* bPtr);
void printState(void* sPtr);

/* StateMap, a map from State's (particle states) to arbitrary values */
typedef RBTree StateMap;
typedef RBNode StateMapNode;
#define newStateMap(ValueCopyFunc,ValueDestroyFunc,ValuePrintFunc) ((StateMap*) newRBTree(compareState,copyState,ValueCopyFunc,deleteState,ValueDestroyFunc,printState,ValuePrintFunc))
#define deleteStateMap(STATEMAPPTR) deleteRBTree((RBTree*)STATEMAPPTR)
#define StateMapInsert(STATEMAPPTR,STATE,VALUE) ((StateMapNode*) RBTreeInsert((RBTree*)STATEMAPPTR,(void*)newState(STATE),(void*)VALUE))
#define StateMapErase(STATEMAPPTR,STATE) RBTreeErase((RBTree*)STATEMAPPTR,(void*)&STATE)
#define StateMapFind(STATEMAPPTR,STATE) ((StateMapNode*) RBTreeFind((RBTree*)STATEMAPPTR,(void*)&STATE))
#define StateMapIsBeforeFirst(STATEMAPPTR,STATE) RBTreeIsBeforeFirst((RBTree*)STATEMAPPTR,(void*)&STATE)
#define StateMapIsAfterLast(STATEMAPPTR,STATE) RBTreeIsAfterLast((RBTree*)STATEMAPPTR,(void*)&STATE)

/* StateSet, a value-less StateMap */
typedef StateMap StateSet;
typedef StateMapNode StateSetNode;
#define newStateSet() ((StateSet*) newStateMap (NullCopyFunction, NullDestroyFunction, NullPrintFunction))
#define deleteStateSet(STATESETPTR) deleteStateMap((StateMap*)STATESETPTR)
#define StateSetInsert(STATESETPTR,STATE) ((StateSetNode*) StateMapInsert((StateMap*)STATESETPTR,STATE,NULL))
#define StateSetErase(STATESETPTR,STATE) StateMapErase((StateMap*)STATESETPTR,STATE)
#define StateSetFind(STATESETPTR,STATE) ((StateSetNode*) StateMapFind((StateMap*)STATESETPTR,STATE))

/* Message new/delete/compare/print functions */
Message* newMessage(Message m);
void* copyMessage(void* mPtr);
void deleteMessage(void* mPtr);
int compareMessage(void* aPtr,void* bPtr);
void printMessage(void* mPtr);

/* MessageRuleMap, a map from Message's to ParticleRule*'s */
typedef RBTree MessageRuleMap;
typedef RBNode MessageRuleMapNode;
#define newMessageRuleMap() ((MessageRuleMap*) newRBTree(compareMessage, copyMessage, AbortCopyFunction, deleteMessage, deleteParticleRule, printMessage, NullPrintFunction))
#define deleteMessageRuleMap(MRMAPPTR) deleteRBTree((RBTree*)MRMAPPTR)
#define MessageRuleMapInsert(MRMAPPTR,MSG,RULEPTR) ((MessageRuleMapNode*) RBTreeInsert((RBTree*)MRMAPPTR,(void*)newMessage(MSG),(void*)RULEPTR))
#define MessageRuleMapFind(MRMAPPTR,MSG) ((MessageRuleMapNode*) RBTreeFind((RBTree*)MRMAPPTR,(void*)&MSG))


#endif /* STATE_MAP_INCLUDED */
