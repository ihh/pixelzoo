#ifndef GOAL_INCLUDED
#define GOAL_INCLUDED

#include "board.h"
#include "tool.h"
#include "xymap.h"
#include "statemap.h"
#include "stringmap.h"
#include "list.h"

/* GoalType enumeration */
/* When a goal has no parent, "parent area" is entire board */
enum GoalType { AreaGoal,        /* subgoal (l) is met for given constant area
				  */

		EnclosuresGoal,  /* define enclosures by masking every state in parent area with intData[0], treating masked-states in ((StateSet*)tree) as walls,
				    and allowing diagonal connections if intData[1] is true.
				    Goal is met if subgoal (l) is met by N enclosures satisfying (intData[2] <= enclosureArea <= intData[3]),
				    where (intData[4] <= N <= intData[5]) */

		EntropyGoal,     /* for every state S in parent area, if (S & TypeMask) is in ((StateSet*)tree), then add (S & intData[0]) to set.
				    Goal is met if states in set satisfy (intData[1] <= population <= intData[2]) and (dblData[0] <= entropy in bits <= dblData[1]) */

		OnceGoal,        /* subgoal (l) has been met at least once within parent area (has the effect of caching evaluation of l, so after it's true for the first time, it's never tested again) */
		AndGoal,         /* both subgoals (l & r) are met simultaneously within parent area. Both l & r are guaranteed to be evaluated */
		OrGoal,          /* at least one of the two subgoals (l & r) is met within parent area. Both l & r are guaranteed to be evaluated */
		LazyAndGoal,     /* both subgoals (l & r) are met simultaneously within parent area. r is not evaluated unless l evaluates true */
		LazyOrGoal,      /* at least one of the two subgoals (l & r) is met within parent area. r is not evaluated unless l evaluates false */
		NotGoal,         /* subgoal (l) is not met within parent area */

		RepeatGoal,      /* subgoal (l) is currently met & has been met consecutively at least intData[0] times within parent area */
		BoardTimeGoal,   /* dblData[0] <= board->updatesPerCell <= dblData[1] */

		TrueGoal,        /* always met */
		FalseGoal,       /* never met */

		CheckToolGoal,               /* dblData[0] <= ((Tool*)context)->reserve <= dblData[1] */
		CheckPortalGoal,             /* ((ExitPortal*)context)->portalState == intData[0] && intData[1] <= ((ExitPortal*)context)->soFar <= intData[2] */
		CheckGameStateGoal,          /* ((Game*)context)->gameState == intData[0] */

/* "pseudo-goals" are dummy goals that always evaluate true, with side effects */
		ChargeToolPseudoGoal,        /* sets ((Tool*)context)->reserve += dblData[0], returns true */
		SetPortalStatePseudoGoal,    /* sets ((ExitPortal*)context)->portalState = intData[0], returns true */
		SetGameStatePseudoGoal,      /* sets ((Game*)context)->gameState = intData[0], returns true */
		UseToolPseudoGoal,           /* calls useTool((Tool*)context,board,x,y,dblData[0]), where (x,y) is randomly sampled from parent area; returns true */
		PrintMessagePseudoGoal       /* prints (char*) context, returns true */
		};

/* Goal */
typedef struct Goal {
  enum GoalType goalType;  /* type */
  struct Goal *l, *r, *parent;  /* subgoals & parent */
  RBTree *tree;  /* red-black tree goal params */
  double *dblData;  /* floating-point goal params */
  unsigned long *intData;  /* integer goal params */
  void *context;  /* misc extra context */
} Goal;

/* accessors */
int testGoalMet (Goal* goal, Board *board);
XYSet* getGoalArea (Goal* goal);  /* returns parent area; NULL means the whole board. If non-NULL, caller must call deleteXYSet() to dealloc */

/* Constructors */
/* All parameters become the responsibility of ("owned" by) the Goal & will be deleted by Goal's destructor */
Goal* newTrueGoal();
Goal* newFalseGoal();
Goal* newAreaGoal (XYSet* area, Goal *subGoal);
Goal* newEnclosuresGoal (State wallMask,
			 StateSet* wallSet,
			 unsigned long minNumEnclosures,
			 unsigned long maxNumEnclosures,
			 unsigned long minEnclosureArea,
			 unsigned long maxEnclosureArea,
			 unsigned char allowDiagonalConnections,
			 Goal* subGoal);
Goal* newOnceGoal (Goal* l);
Goal* newAndGoal (Goal* l, Goal* r, int lazy);
Goal* newOrGoal (Goal* l, Goal* r, int lazy);
Goal* newNotGoal (Goal* g);
Goal* newEntropyGoal (StateSet* typeSet, State stateMask, unsigned long minCount, unsigned long maxCount, double minEntropy, double maxEntropy);
Goal* newRepeatGoal (Goal* subGoal, unsigned long minReps);
Goal *newBoardTimeGoal (double minUpdatesPerCell, double maxUpdatesPerCell);

Goal *newCheckToolGoal (void *tool, double minReserve, double maxReserve);
Goal *newCheckPortalGoal (void *portal, int portalState, int minCount, int maxCount);
Goal *newCheckGameStateGoal (void *game, int gameState);

Goal *newChargeToolPseudoGoal (void *tool, double reserveDelta);
Goal *newSetPortalStatePseudoGoal (void *portal, int portalState);
Goal *newSetGameStatePseudoGoal (void *game, int gameState);
Goal *newUseToolPseudoGoal (void *tool, double duration);
Goal *newPrintMessagePseudoGoal (const char* message);

/* destructor */
void deleteGoal (Goal* goal);

/* helpers */
List* getEnclosures (Board* board, XYSet* area, State wallMask, StateSet* wallSet, unsigned int minEnclosureArea, unsigned int maxEnclosureArea, unsigned char allowDiagonalConnections);  /* returns a List of XYList's; caller must call deleteList to dealloc */

#endif /* GOAL_INCLUDED */
