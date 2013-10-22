#ifndef XMLGOAL_INCLUDED
#define XMLGOAL_INCLUDED

#include "goal.h"
#include "game.h"

/* XML node names */
#define XMLZOO_AREA_GOAL       "area"
#define XMLZOO_CAGE_GOAL       "cage"   /* EnclosuresGoal */
#define XMLZOO_POPULATION_GOAL "population"  /* EntropyGoal */
#define XMLZOO_REPEAT_GOAL     "repeat"
#define XMLZOO_CACHED_GOAL     "cached"
#define XMLZOO_AND_GOAL        "and"
#define XMLZOO_OR_GOAL         "or"
#define XMLZOO_NOT_GOAL        "not"
#define XMLZOO_TRUE_GOAL       "true"
#define XMLZOO_FALSE_GOAL      "false"
#define XMLZOO_MAYBE_GOAL      "maybe"
#define XMLZOO_TIME_GOAL       "time"
#define XMLZOO_TESTTOOL_GOAL   "testtool"
#define XMLZOO_TESTEXIT_GOAL   "testexit"
#define XMLZOO_TESTGAME_GOAL   "testgame"
#define XMLZOO_CHARGE_GOAL     "charge"
#define XMLZOO_SETEXIT_GOAL    "setexit"
#define XMLZOO_SETGAME_GOAL    "setgame"
#define XMLZOO_USETOOL_GOAL    "usetool"
#define XMLZOO_PRINT_GOAL      "print"
#define XMLZOO_BALLOON_GOAL    "place"

#define XMLZOO_REPS_GPARAM     "reps"
#define XMLZOO_PROB_GPARAM     XMLZOO_PROB
#define XMLZOO_POS_GPARAM      XMLZOO_POS
#define XMLZOO_WALL_GPARAM     "wall"
#define XMLZOO_COUNT_GPARAM    "count"
#define XMLZOO_POINTS_GPARAM   "points"
#define XMLZOO_MIN_GPARAM      "min"
#define XMLZOO_MAX_GPARAM      "max"
#define XMLZOO_MOORE_GPARAM    "moore"
#define XMLZOO_ENTROPY_GPARAM  "entropy"
#define XMLZOO_ALLOWMASK_GPARAM  "allowmask"
#define XMLZOO_VALLOWMASK_GPARAM "vallowmask"
#define XMLZOO_TALLOWMASK_GPARAM "tallowmask"
#define XMLZOO_LAZY_GPARAM     "lazy"
#define XMLZOO_CACHE_GPARAM    "cache"
#define XMLZOO_TOOL_GPARAM     XMLZOO_TOOL
#define XMLZOO_TOOLNAME_GPARAM XMLZOO_NAME
#define XMLZOO_RESERVE_GPARAM  "toolres"
#define XMLZOO_EXSTATE_GPARAM  "exitstate"
#define XMLZOO_GMSTATE_GPARAM  "gamestate"
#define XMLZOO_DURATION_GPARAM "duration"
#define XMLZOO_BALLOON_GPARAM  "balloon"
#define XMLZOO_MESSAGE_GPARAM  "message"
#define XMLZOO_GOAL_GPARAM     XMLZOO_GOAL

#define XMLZOO_TEXT        "text"
#define XMLZOO_COLOR       "color"
#define XMLZOO_HEXCOLOR    "hexcolor"
#define XMLZOO_TTL         "ttl"
#define XMLZOO_RISE        "rise"
#define XMLZOO_ZOOM        "zoom"
#define XMLZOO_FADE        "fade"
#define XMLZOO_PERSIST     "persist"

/* methods */
Goal* newGoalFromXmlParentNode (xmlNode *node, Game *game);

State getAllowMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask);
State getCountMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask);

#endif /* XMLGOAL_INCLUDED */
