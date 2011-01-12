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
#define XMLZOO_SHUFFLE     "shuffle"
#define XMLZOO_FAILS       "failures"
#define XMLZOO_SUCCEEDS    "successes"
#define XMLZOO_PERIOD      "period"
#define XMLZOO_PHASE       "phase"
#define XMLZOO_DECTYPE     "type"
#define XMLZOO_HEXTYPE     "hextype"
#define XMLZOO_NAME        "name"
#define XMLZOO_RULE        "rule"
#define XMLZOO_RATE        "rate"
#define XMLZOO_OVERLOAD    "overload"
#define XMLZOO_TEST        "test"
#define XMLZOO_LOC         "loc"
#define XMLZOO_X           "x"
#define XMLZOO_Y           "y"
#define XMLZOO_MASK        "mask"
#define XMLZOO_DECVAL      "val"
#define XMLZOO_HEXVAL      "hexval"
#define XMLZOO_OP          "op"
#define XMLZOO_IGNORE      "ignore"
#define XMLZOO_EXEC        "exec"
#define XMLZOO_SRC         "src"
#define XMLZOO_DEST        "dest"
#define XMLZOO_DECINC      "inc"
#define XMLZOO_HEXINC      "hexinc"
#define XMLZOO_SRCMASK     "srcmask"
#define XMLZOO_DESTMASK    "destmask"
#define XMLZOO_LSHIFT      "lshift"
#define XMLZOO_RSHIFT      "rshift"
#define XMLZOO_FAIL        "fail"
#define XMLZOO_COLOR       "color"
#define XMLZOO_HEXCOLOR    "hexcolor"
#define XMLZOO_DECMUL      "mul"
#define XMLZOO_HEXMUL      "hexmul"
#define XMLZOO_INIT        "init"
#define XMLZOO_BALLOON     "balloon"
#define XMLZOO_TEXT        "text"
#define XMLZOO_TTL         "ttl"
#define XMLZOO_RISE        "rise"
#define XMLZOO_ZOOM        "zoom"
#define XMLZOO_FADE        "fade"
#define XMLZOO_PERSIST     "persist"

/* methods */
Board* newBoardFromXmlDocument (xmlDoc *doc);
Board* newBoardFromXmlRoot (xmlNode *root);
Board* newBoardFromXmlFile (const char* filename);
Board* newBoardFromXmlString (const char* string);

/* helpers */
Balloon* newBalloonFromXmlNode (xmlNode* node);

#endif /* XMLBOARD_INCLUDED */
