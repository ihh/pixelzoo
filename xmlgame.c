#include <string.h>
#include "xmlgame.h"
#include "xmlboard.h"
#include "xmlutil.h"

Game* newGameFromXmlFile (const char* filename) {
  xmlDoc* doc;
  Game* game = NULL;
  doc = xmlReadFile (filename, NULL, 0);
  if (doc)
    game = newGameFromXmlDocument (doc);
  return game;
}

Game* newGameFromXmlString (const char* string) {
  xmlDoc* doc;
  Game* game = NULL;
  doc = xmlReadMemory (string, strlen(string), "noname.xml", NULL, 0);
  if (doc)
    game = newGameFromXmlDocument (doc);
  return game;
}

Game* newGameFromXmlDocument (xmlDoc *doc) {
  return newGameFromXmlNode (xmlDocGetRootElement (doc));
}

Game* newGameFromXmlNode (xmlNode *root) {
  Game *game;
  xmlNode *gameNode;

  gameNode = CHILD(root,GAME);
 
  game = newGame();
  game->board = newBoardFromXmlNode (gameNode);

  /* more to go here */

  return game;
}

