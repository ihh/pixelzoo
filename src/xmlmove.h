#ifndef XMLMOVE_INCLUDED
#define XMLMOVE_INCLUDED

#include <stdio.h>

#include "move.h"
#include "xmlboard.h"

/* XML node names */
#define XMLZOO_MOVE    "move"
#define XMLZOO_TIME    "t"
#define XMLZOO_UPDATE  "u"
#define XMLZOO_LOG     "log"

/* methods */
Move* newMoveFromXmlNode (xmlNode *node, ProtoTable *protoTable);
MoveList* newMoveListFromXmlNode (xmlNode *node, ProtoTable *protoTable);

void writeMove (Move* move, xmlTextWriterPtr writer);
void writeMoveList (MoveList* moveList, xmlTextWriterPtr writer, const xmlChar* name);

#endif /* XMLMOVE_INCLUDED */
