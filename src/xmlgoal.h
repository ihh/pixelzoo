#ifndef XMLGOAL_INCLUDED
#define XMLGOAL_INCLUDED

#include <libxml/tree.h>
#include "goal.h"

/* XML node names */
#define XMLZOO_GOAL            "goal"
#define XMLZOO_GOALTYPE        "type"
#define XMLZOO_AREA_GOAL       "area"
#define XMLZOO_CAGE_GOAL       "cage"   /* EnclosuresGoal */
#define XMLZOO_COUNT_GOAL      "count"  /* EntropyGoal */
#define XMLZOO_ONCE_GOAL       "once"
#define XMLZOO_AND_GOAL        "and"
#define XMLZOO_OR_GOAL         "or"
#define XMLZOO_NOT_GOAL        "not"
#define XMLZOO_REPEAT_GOAL     "repeat"
#define XMLZOO_TRUE_GOAL       "true"
#define XMLZOO_FALSE_GOAL      "false"

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

/* methods */
Goal* newGoalFromXmlNode (xmlNode *node);

#endif /* XMLGOAL_INCLUDED */
