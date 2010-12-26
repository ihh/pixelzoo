#ifndef XMLBOARD_INCLUDED
#define XMLBOARD_INCLUDED

#include <libxml/tree.h>
#include "board.h"

/* XML node names */
#define XMLBOARD_BOARD    "board"
#define XMLBOARD_SIZE     "size"
#define XMLBOARD_GRAMMAR  "grammar"
#define XMLBOARD_PARTICLE "particle"
#define XMLBOARD_SYNC     "sync"
#define XMLBOARD_SHUFFLE  "shuffle"
#define XMLBOARD_FAILS    "failures"
#define XMLBOARD_SUCCEEDS "successes"
#define XMLBOARD_PERIOD   "period"
#define XMLBOARD_PHASE    "phase"
#define XMLBOARD_DECTYPE  "type"
#define XMLBOARD_HEXTYPE  "hextype"
#define XMLBOARD_NAME     "name"
#define XMLBOARD_RULE     "rule"
#define XMLBOARD_RATE     "rate"
#define XMLBOARD_OVERLOAD "overload"
#define XMLBOARD_TEST     "test"
#define XMLBOARD_LOC      "loc"
#define XMLBOARD_X        "x"
#define XMLBOARD_Y        "y"
#define XMLBOARD_DECMASK  "mask"
#define XMLBOARD_HEXMASK  "hexmask"
#define XMLBOARD_DECVAL   "val"
#define XMLBOARD_HEXVAL   "hexval"
#define XMLBOARD_OP       "op"
#define XMLBOARD_IGNORE   "ignore"
#define XMLBOARD_EXEC     "exec"
#define XMLBOARD_SRC      "src"
#define XMLBOARD_DEST     "dest"
#define XMLBOARD_DECINC   "inc"
#define XMLBOARD_HEXINC   "hexinc"
#define XMLBOARD_LSHIFT   "lshift"
#define XMLBOARD_RSHIFT   "rshift"
#define XMLBOARD_FAIL     "fail"
#define XMLBOARD_COLOR    "color"
#define XMLBOARD_DECMUL   "mul"
#define XMLBOARD_HEXMUL   "hexmul"
#define XMLBOARD_INIT     "init"

/* methods */
Board* newBoardFromXmlDocument (xmlDoc *doc);
Board* newBoardFromXmlFile (const char* filename);
Board* newBoardFromXmlString (const char* string);

#endif /* XMLBOARD_INCLUDED */
