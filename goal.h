#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "playstate.h"
#include "board.h"
#include "xymap.h"
#include "statemap.h"
#include "stringmap.h"
#include "list.h"

/* GoalType enumeration */
/* When a goal has no parent, "parent area" is entire board */
enum GoalType { Area,        /* subgoal (l) is met for given constant area
				TODO: if stringData[0] is defined, interpret it as an NPC name, and use focus location of that NPC as an XY offset
			      */
		Enclosures,  /* define enclosures by masking every state in parent area with intData[0], treating masked-states in ((StateSet*)tree) as walls,
				and allowing diagonal connections if intData[1] is true.
				Goal is met if subgoal (l) is met by N enclosures satisfying (intData[2] <= enclosureArea <= intData[3]),
				where (intData[4] <= N <= intData[5]) */
		Once,        /* subgoal (l) has been met at least once within parent area (has the effect of caching evaluation of l) */
		And,         /* both subgoals (l & r) are met simultaneously within parent area */
		Or,          /* at least one of the two subgoals (l & r) is met within parent area */
		Not,         /* subgoal (l) is not met within parent area */
		Entropy,     /* mask every state in parent area with intData[0], and keep the subset of masked-states that are in ((StateSet*)tree).
				Goal is met if remaining states satisfy (intData[1] <= population <= intData[2]) and (dblData[0] <= entropy <= dblData[1]) */
		Repeat,      /* subgoal (l) is currently met & has been met consecutively at least intData[0] times within parent area */

		BoardState,  /* TODO: tree is an XY->State map; at least intData[2] of the specified board positions must be in the specified states
				TODO: if stringData[0] is defined, interpret it as an NPC name, and use focus location of that NPC as an XY offset
			     */
		NPCState,    /* TODO: NPC with name stringData[0] is asleep/awake (intData[0]) and/or in a given set of states (tree = StringSet) */
		Score,       /* TODO: intData[0] <= score[stringData[0]] <= intData[1] */
		Rule,        /* TODO: rule named stringData[0] has been triggered N times, where intData[0] <= N <= intData[1] */
		Achievement, /* TODO: achievment named stringData[0] is still locked (intData[0]=0) or has been unlocked (intData[0]=1) */

		PlayerYesNo, /* TODO: player, prompted with stringData[0], chooses "yes" (alternatives: "stop bugging me", "ask again later") */
		PlayerPlace, /* TODO: player must choose a location for NPC with name stringData[0] */

		True,        /* always met */
		False        /* never met */
};

/* Goal */
typedef struct Goal {
  enum GoalType goalType;  /* type */
  struct Goal *l, *r, *parent;  /* parent & subgoals */
  RBTree *tree;  /* red-black tree goal params */
  double *dblData;  /* floating-point goal params */
  unsigned long *intData;  /* integer goal params */
  StringVector *stringData;  /* string params */
} Goal;

/* accessors */
int testGoalMet (Goal* goal, PlayState* play);
XYSet* getGoalArea (Goal* goal);  /* returns parent area; NULL means the whole board. If non-NULL, caller must call deleteXYSet() to dealloc */

/* Constructors */
/* All parameters become the responsibility of ("owned" by) the Goal & will be deleted by Goal's destructor */
Goal* newTrueGoal();
Goal* newAreaGoal (XYSet* area);
Goal* newEnclosuresGoal (State wallMask,
			 StateSet* wallSet,
			 unsigned long minNumEnclosures,
			 unsigned long maxNumEnclosures,
			 unsigned long minEnclosureArea,
			 unsigned long maxEnclosureArea,
			 unsigned char allowDiagonalConnections,
			 Goal* subGoal);
Goal* newOnceGoal (Goal* l);
Goal* newAndGoal (Goal* l, Goal* r);
Goal* newOrGoal (Goal* l, Goal* r);
Goal* newNotGoal (Goal* g);
Goal* newEntropyGoal (State typeMask, StateSet* typeSet, unsigned long minCount, unsigned long maxCount, double minEntropy, double maxEntropy);
Goal* newRepeatGoal (Goal* subGoal, unsigned long minReps);

/* destructor */
void deleteGoal (Goal* goal);

/* helpers */
List* getEnclosures (Board* board, XYSet* area, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections);  /* returns a List of XYList's; caller must call deleteList to dealloc */

#endif /* GOAL_INCLUDED */
