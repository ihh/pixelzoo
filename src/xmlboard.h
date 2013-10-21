#ifndef XMLBOARD_INCLUDED
#define XMLBOARD_INCLUDED

#include "board.h"
#include "proto.h"
#include "xmlparser.h"

/* XML node names */
#define XMLZOO_BOARD       "board"
#define XMLZOO_SIZE        "size"
#define XMLZOO_SEED        "seed"
#define XMLZOO_GRAMMAR     "grammar"
#define XMLZOO_SCHEME      "scheme"
#define XMLZOO_VALUE       "value"
#define XMLZOO_PARTICLE    "particle"
#define XMLZOO_SYNC        "sync"
#define XMLZOO_PERIOD      "period"
#define XMLZOO_PHASE       "phase"
#define XMLZOO_DECTYPE     "type"
#define XMLZOO_HEXTYPE     "hextype"
#define XMLZOO_GTYPE       "gtype"
#define XMLZOO_GSTATE      "gstate"
#define XMLZOO_GVARS       "gvars"
#define XMLZOO_VARS        "vars"
#define XMLZOO_VARSIZE     "varsize"
#define XMLZOO_READONLY    "readonly"
#define XMLZOO_INDEX       "index"
#define XMLZOO_SUBRULE     "subrule"
#define XMLZOO_NAME        "name"
#define XMLZOO_RULE        "rule"
#define XMLZOO_RATE        "rate"
#define XMLZOO_SWITCH      "switch"
#define XMLZOO_MODIFY      "modify"
#define XMLZOO_RANDOM      "random"
#define XMLZOO_GOAL        "goal"
#define XMLZOO_CASE        "case"
#define XMLZOO_DEFAULT     "default"
#define XMLZOO_NEXT        "next"
#define XMLZOO_PROB        "prob"
#define XMLZOO_PASS        "pass"
#define XMLZOO_FAIL        "fail"
#define XMLZOO_POS         "pos"
#define XMLZOO_X           "x"
#define XMLZOO_Y           "y"
#define XMLZOO_MASK        "mask"
#define XMLZOO_TMASK       "tmask"
#define XMLZOO_VMASK       "vmask"
#define XMLZOO_SRC         "src"
#define XMLZOO_DEST        "dest"
#define XMLZOO_DECINC      "inc"
#define XMLZOO_HEXINC      "hexinc"
#define XMLZOO_SRCMASK     "srcmask"
#define XMLZOO_VSRCMASK    "vsrcmask"
#define XMLZOO_TSRCMASK    "tsrcmask"
#define XMLZOO_DESTMASK    "destmask"
#define XMLZOO_VDESTMASK   "vdestmask"
#define XMLZOO_TDESTMASK   "tdestmask"
#define XMLZOO_LSHIFT      "lshift"
#define XMLZOO_RSHIFT      "rshift"
#define XMLZOO_VLSHIFT     "vlshift"
#define XMLZOO_VRSHIFT     "vrshift"
#define XMLZOO_COLRULE     "colrule"
#define XMLZOO_DECMUL      "mul"
#define XMLZOO_HEXMUL      "hexmul"
#define XMLZOO_INIT        "init"
#define XMLZOO_QUEUE       "queue"
#define XMLZOO_DECSTATE    "state"
#define XMLZOO_HEXSTATE    "hexstate"
#define XMLZOO_TYPE        "type"
#define XMLZOO_VAL         "val"
#define XMLZOO_VAR         "var"
#define XMLZOO_DELIVER     "deliver"
#define XMLZOO_DECMESSAGE  "msg"
#define XMLZOO_HEXMESSAGE  "hexmsg"
#define XMLZOO_GOTO        "goto"
#define XMLZOO_DISPATCH    "dispatch"

/* methods */
Board* newBoardFromXmlDocument (void *game, xmlDoc *doc);
Board* newBoardFromXmlRoot (void *game, xmlNode *root);
Board* newBoardFromXmlString (void *game, const char* string);

void writeBoardXml (Board* board, xmlTextWriterPtr writer, int reverseCompile);  /* if reverseCompile=1, output uses type-vars notation; otherwise, hexadecimal */
void writeCellXml (Board* board, xmlTextWriterPtr writer, int x, int y, int reverseCompile);  /* if reverseCompile=1, output uses type-vars notation; otherwise, hexadecimal */
void writeTypesXml (Board* board, xmlTextWriterPtr writer);
void writeGVarsXml (Board* board, State s, xmlTextWriterPtr writer);

/* private builder methods */

int testNodeHasType (xmlNode* node);
int testNodeHasState (xmlNode* node);
xmlNode* nextNodeWithState (xmlNode* node);

Type getTypeFromNode (xmlNode* node, ProtoTable* protoTable, Type defaultType);
State getStateFromNode (xmlNode* node, ProtoTable* protoTable, State defaultState);
State getStateFromChild (xmlNode* child, ProtoTable* protoTable, State defaultState);

State getMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask);
State getSrcMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask);
State getDestMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask);
State getTaggedMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask, const char* maskTag, const char* vmaskTag, const char* tmaskTag);

unsigned char getLShiftFromNode (xmlNode* node, ProtoTable* protoTable);
unsigned char getRShiftFromNode (xmlNode* node, ProtoTable* protoTable);
unsigned char getTaggedShiftFromNode (xmlNode* node, ProtoTable* protoTable, const char* shiftTag, const char* vShiftTag);

#endif /* XMLBOARD_INCLUDED */
