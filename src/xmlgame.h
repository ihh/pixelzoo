#ifndef XMLGAME_INCLUDED
#define XMLGAME_INCLUDED

#include <libxml/tree.h>
#include "game.h"

/* XML node names */
#define XMLZOO_GAME       "game"
#define XMLZOO_PROTECT    "protect"
#define XMLZOO_ENTRANCE   "entrance"
#define XMLZOO_COUNT      "count"
#define XMLZOO_EXIT       "exit"
#define XMLZOO_TOOL       "tool"
#define XMLZOO_HIDE       "hide"
#define XMLZOO_DECSTATE   "state"
#define XMLZOO_HEXSTATE   "hexstate"
#define XMLZOO_BRUSH      "brush"
#define XMLZOO_CENTER     "center"
#define XMLZOO_INTENSITY  "intensity"
#define XMLZOO_PATTERN    "pattern"
#define XMLZOO_OVERWRITE  "overwrite"
#define XMLZOO_DISALLOW   "disallow"
#define XMLZOO_SPRAY      "spray"
#define XMLZOO_RESERVE    "reserve"
#define XMLZOO_RECHARGE   "recharge"
#define XMLZOO_MAXRESERVE "maxreserve"
#define XMLZOO_TRIGGER    "trigger"

/* methods */
Game* newGameFromXmlDocument (xmlDoc *doc);
Game* newGameFromXmlRoot (xmlNode *root);
Game* newGameFromXmlFile (const char* filename);
Game* newGameFromXmlString (const char* string);

#endif /* XMLGAME_INCLUDED */
