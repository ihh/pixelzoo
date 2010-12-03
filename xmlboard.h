#ifndef XMLBOARD_INCLUDED
#define XMLBOARD_INCLUDED

#include <libxml/tree.h>
#include "board.h"

Board* newBoardFromXmlDocument (xmlDoc *doc);
Board* newBoardFromXmlFile (const char* filename);
Board* newBoardFromXmlString (const char* string);

#endif /* XMLBOARD_INCLUDED */
