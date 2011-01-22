#ifndef XMLBOARD_INCLUDED
#define XMLBOARD_INCLUDED

#include <libxml/tree.h>
#include "board.h"

/* XML node names */
#define XMLZOO_BOARD       "board"
#define XMLZOO_SIZE        "size"
#define XMLZOO_GRAMMAR     "grammar"
#define XMLZOO_PARTICLE    "particle"
#define XMLZOO_SYNC        "sync"
#define XMLZOO_PERIOD      "period"
#define XMLZOO_PHASE       "phase"
#define XMLZOO_DECTYPE     "type"
#define XMLZOO_HEXTYPE     "hextype"
#define XMLZOO_NAME        "name"
#define XMLZOO_RULE        "rule"
#define XMLZOO_RATE        "rate"
#define XMLZOO_LOOKUP      "lookup"
#define XMLZOO_MODIFY      "modify"
#define XMLZOO_RANDOM      "random"
#define XMLZOO_OVERLOAD    "overload"
#define XMLZOO_GOAL        "goal"
#define XMLZOO_POS         "pos"
#define XMLZOO_X           "x"
#define XMLZOO_Y           "y"
#define XMLZOO_MASK        "mask"
#define XMLZOO_DECVAL      "val"
#define XMLZOO_HEXVAL      "hexval"
#define XMLZOO_SRC         "src"
#define XMLZOO_DEST        "dest"
#define XMLZOO_DECINC      "inc"
#define XMLZOO_HEXINC      "hexinc"
#define XMLZOO_SRCMASK     "srcmask"
#define XMLZOO_DESTMASK    "destmask"
#define XMLZOO_LSHIFT      "lshift"
#define XMLZOO_RSHIFT      "rshift"
#define XMLZOO_COLOR       "color"
#define XMLZOO_HEXCOLOR    "hexcolor"
#define XMLZOO_DECMUL      "mul"
#define XMLZOO_HEXMUL      "hexmul"
#define XMLZOO_INIT        "init"

/* methods */
Board* newBoardFromXmlDocument (void *game, xmlDoc *doc);
Board* newBoardFromXmlRoot (void *game, xmlNode *root);
Board* newBoardFromXmlFile (void *game, const char* filename);
Board* newBoardFromXmlString (void *game, const char* string);

#endif /* XMLBOARD_INCLUDED */
