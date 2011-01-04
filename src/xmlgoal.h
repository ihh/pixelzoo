#ifndef XMLGOAL_INCLUDED
#define XMLGOAL_INCLUDED

#include <libxml/tree.h>
#include "goal.h"
#include "game.h"

/* XML node names */
#define XMLZOO_GOAL            "goal"
#define XMLZOO_GOALTYPE        "type"
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
#define XMLZOO_TIME_GOAL       "time"
#define XMLZOO_TESTTOOL_GOAL   "tool"
#define XMLZOO_TESTEXIT_GOAL   "exit"
#define XMLZOO_TESTGAME_GOAL   "game"
#define XMLZOO_CHARGE_GOAL     "charge"
#define XMLZOO_SETEXIT_GOAL    "setexit"
#define XMLZOO_SETGAME_GOAL    "setgame"
#define XMLZOO_USETOOL_GOAL    "use"
#define XMLZOO_PRINT_GOAL      "print"

#define XMLZOO_REPS_GPARAM     "reps"
#define XMLZOO_GOAL_GPARAM     "goal"
#define XMLZOO_POS_GPARAM      "pos"
#define XMLZOO_WALL_GPARAM     "wall"
#define XMLZOO_COUNT_GPARAM    "count"
#define XMLZOO_AREA_GPARAM     "area"
#define XMLZOO_MIN_GPARAM      "min"
#define XMLZOO_MAX_GPARAM      "max"
#define XMLZOO_MOORE_GPARAM    "moore"
#define XMLZOO_ENTROPY_GPARAM  "entropy"
#define XMLZOO_DECTYPE_GPARAM  "type"
#define XMLZOO_HEXTYPE_GPARAM  "hextype"
#define XMLZOO_MASK_GPARAM     "mask"
#define XMLZOO_LAZY_GPARAM     "lazy"
#define XMLZOO_CACHED_GPARAM   "cached"
#define XMLZOO_TOOLNAME_GPARAM "name"
#define XMLZOO_RESERVE_GPARAM  "reserve"
#define XMLZOO_STATE_GPARAM    "state"
#define XMLZOO_DURATION_GPARAM "duration"
#define XMLZOO_TEXT_GPARAM     "text"

/* methods */
Goal* newGoalFromXmlNode (xmlNode *node, Game *game);

#endif /* XMLGOAL_INCLUDED */
