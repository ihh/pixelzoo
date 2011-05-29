#ifndef XMLMOVE_INCLUDED
#define XMLMOVE_INCLUDED

#include <stdio.h>
#include <libxml/xmlwriter.h>

#include "move.h"
#include "xmlboard.h"

/* XML node names */
#define XMLZOO_MOVE    "move"
#define XMLZOO_TIME    "t"
#define XMLZOO_UPDATE  "u"
#define XMLZOO_LOG     "log"

/* methods */
Move* newMoveFromXmlNode (xmlNode *node);
MoveList* newMoveListFromXmlNode (xmlNode *node);

void writeMove (Move* move, xmlTextWriterPtr writer);
void writeMoveList (MoveList* moveList, xmlTextWriterPtr writer, const xmlChar* name);

#endif /* XMLMOVE_INCLUDED */
