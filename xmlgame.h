#ifndef XMLGAME_INCLUDED
#define XMLGAME_INCLUDED

#include <libxml/tree.h>
#include "game.h"

Game* newGameFromXmlDocument (xmlDoc *doc);
Game* newGameFromXmlFile (const char* filename);
Game* newGameFromXmlString (const char* string);

#endif /* XMLGAME_INCLUDED */
