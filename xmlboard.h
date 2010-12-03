#ifndef XMLBOARD_INCLUDED
#define XMLBOARD_INCLUDED

#include <libxml/parser.h>
#include <libxml/tree.h>

#include "board.h"

Board* newBoardFromXmlDocument (xmlDoc *doc);
Board* newBoardFromXmlFile (const char* filename);

#endif /* XMLBOARD_INCLUDED */
