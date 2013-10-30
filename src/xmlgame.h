#ifndef XMLGAME_INCLUDED
#define XMLGAME_INCLUDED

#include "game.h"
#include "xmlparser.h"

/* XML node names */
#define XMLZOO_GAME       "game"
#define XMLZOO_PROTECT    "protect"
#define XMLZOO_TOOL       "tool"
#define XMLZOO_BRUSH      "brush"
#define XMLZOO_CENTER     "center"
#define XMLZOO_INTENSITY  "intensity"
#define XMLZOO_SPOT       "spot"
#define XMLZOO_PATTERN    "pattern"
#define XMLZOO_PIXEL      "pixel"
#define XMLZOO_OVERWRITE  "overwrite"
#define XMLZOO_DISALLOW   "disallow"
#define XMLZOO_SPRAY      "spray"
#define XMLZOO_RESERVE    "reserve"
#define XMLZOO_RECHARGE   "recharge"
#define XMLZOO_MAXRESERVE "maxreserve"

/* methods */
Game* newGameFromXmlDocument (xmlDoc *doc);
Game* newGameFromXmlRoot (xmlNode *root);
Game* newGameFromXmlString (const char* string);

Game* newGameFromXmlDocumentWithSeparateBoard (xmlDoc *gameDoc, xmlDoc *separateBoardDoc);
Game* newGameFromXmlRootWithSeparateBoard (xmlNode *gameRoot, xmlNode *separateBoardRoot);
Game* newGameFromXmlStringWithSeparateBoard (const char* gameString, const char* separateBoardString);

Tool* newToolFromXmlNode (xmlNode *node, ProtoTable *protoTable);

#endif /* XMLGAME_INCLUDED */
