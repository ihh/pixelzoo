#ifndef XMLBOARD_INCLUDED
#define XMLBOARD_INCLUDED

#include "board.h"
#include "proto.h"
#include "xmlparser.h"

/* XML node names */
#define XMLZOO_BOARD       "board"
#define XMLZOO_SIZE        "size"
#define XMLZOO_DEPTH       "depth"
#define XMLZOO_SEED        "seed"
#define XMLZOO_GRAMMAR     "grammar"
#define XMLZOO_SCHEME      "scheme"
#define XMLZOO_SCHEMEDEF   "schemedef"
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
#define XMLZOO_COMPARE     "compare"
#define XMLZOO_MODIFY      "modify"
#define XMLZOO_RANDOM      "random"
#define XMLZOO_CASE        "case"
#define XMLZOO_DEFAULT     "default"
#define XMLZOO_REGINDEX    "regindex"
#define XMLZOO_LOW         "low"
#define XMLZOO_HIGH        "high"
#define XMLZOO_LT          "lt"
#define XMLZOO_LEQ         "leq"
#define XMLZOO_GT          "gt"
#define XMLZOO_GEQ         "geq"
#define XMLZOO_EQ          "eq"
#define XMLZOO_NEQ         "neq"
#define XMLZOO_NEXT        "next"
#define XMLZOO_PROB        "prob"
#define XMLZOO_PASS        "pass"
#define XMLZOO_FAIL        "fail"
#define XMLZOO_POS         "pos"
#define XMLZOO_X           "x"
#define XMLZOO_Y           "y"
#define XMLZOO_Z           "z"
#define XMLZOO_MODE        "mode"
#define XMLZOO_DIRECT      "direct"
#define XMLZOO_INDIRECT    "indirect"
#define XMLZOO_MASK        "mask"
#define XMLZOO_TMASK       "tmask"
#define XMLZOO_VMASK       "vmask"
#define XMLZOO_SRC         "src"
#define XMLZOO_DEST        "dest"
#define XMLZOO_DECINC      "inc"
#define XMLZOO_HEXINC      "hexinc"
#define XMLZOO_REGINC      "reginc"
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
#define XMLZOO_MESSAGE     "message"
#define XMLZOO_GOTO        "goto"
#define XMLZOO_DISPATCH    "dispatch"
#define XMLZOO_LOAD        "load"
#define XMLZOO_REGISTER    "register"
#define XMLZOO_GOAL        "goal"

/* methods */
Board* newBoardFromXmlDocument (xmlDoc *doc);
Board* newBoardFromXmlRoot (xmlNode *root);
Board* newBoardFromXmlString (const char* string);

void writeBoardXml (Board* board, xmlTextWriterPtr writer, int reverseCompile);  /* if reverseCompile=1, output uses type-vars notation; otherwise, hexadecimal */
void writeCellXml (Board* board, xmlTextWriterPtr writer, int x, int y, int z, int reverseCompile);  /* if reverseCompile=1, output uses type-vars notation; otherwise, hexadecimal */
void writeTypesXml (Board* board, xmlTextWriterPtr writer);
void writeGVarsXml (Board* board, State s, xmlTextWriterPtr writer);

/* private builder methods */

int testNodeHasState (xmlNode* node);
xmlNode* nextNodeWithState (xmlNode* node);

State getGTypeOrStateFromNode (xmlNode* node, ProtoTable* protoTable);

State getStateFromNode (xmlNode* node, ProtoTable* protoTable, State defaultState);
State getStateFromChild (xmlNode* child, ProtoTable* protoTable, State defaultState);

State getGStateOrGVarsFromChild (xmlNode* child, ProtoTable* protoTable, State defaultState);

State getIncOrStateFromNode (xmlNode* node, ProtoTable* protoTable, unsigned char* registerFlag);

State getMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask);
State getSrcMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask);
State getDestMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask);
State getTaggedMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask, const char* maskTag, const char* vmaskTag, const char* tmaskTag);

unsigned char getLShiftFromNode (xmlNode* node, ProtoTable* protoTable);
unsigned char getRShiftFromNode (xmlNode* node, ProtoTable* protoTable);
unsigned char getTaggedShiftFromNode (xmlNode* node, ProtoTable* protoTable, const char* shiftTag, const char* vShiftTag);

Message getMessageFromNode (xmlNode* node, ProtoTable* protoTable);

#endif /* XMLBOARD_INCLUDED */
