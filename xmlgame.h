#ifndef XMLGAME_INCLUDED
#define XMLGAME_INCLUDED

#include <libxml/tree.h>
#include "game.h"

/* XML node names */
#define XMLZOO_GAME    "game"

/* methods */
Game* newGameFromXmlDocument (xmlDoc *doc);
Game* newGameFromXmlNode (xmlNode *node);
Game* newGameFromXmlFile (const char* filename);
Game* newGameFromXmlString (const char* string);

#endif /* XMLGAME_INCLUDED */
