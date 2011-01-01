#include <string.h>
#include "notify.h"
#include "xmlgame.h"
#include "xmlboard.h"
#include "xmlgoal.h"
#include "xmlutil.h"

/* prototypes for private builder methods */
Tool* newToolFromXmlNode (xmlNode* node);
ToolCharger* newToolChargerFromXmlNode (Game *game, xmlNode* node);

/* method defs */

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
  return newGameFromXmlRoot (xmlDocGetRootElement (doc));
}

Game* newGameFromXmlRoot (xmlNode *root) {
  Game *game;
  xmlNode *gameNode, *entranceNode, *exitNode, *node;

  gameNode = CHILD(root,GAME);
 
  game = newGame();
  game->board = newBoardFromXmlRoot (gameNode);

  game->updatesPerSecond = OPTCHILDFLOAT (gameNode, RATE, DefaultUpdatesPerSecond);

  for (node = gameNode->children; node; node = node->next)
    if (MATCHES(node,TOOL))
      ListInsertBefore (game->allTools, NULL, newToolFromXmlNode (node));

  for (node = gameNode->children; node; node = node->next)
    if (MATCHES(node,CHARGER))
      ListInsertBefore (game->charger, NULL, newToolChargerFromXmlNode (game, node));

  Assert (game->allTools->head != NULL, "You need some tools!");
  game->selectedTool = (Tool*) game->allTools->head->value;

  for (node = gameNode->children; node; node = node->next)
    if (MATCHES(node,PROTECT))
      registerCellWatcher (game->board, CHILDINT(node,X), CHILDINT(node,Y), game->writeProtectWatcher);

  entranceNode = CHILD(gameNode,ENTRANCE);
  game->theEntrance.pos.x = CHILDINT(entranceNode,X);
  game->theEntrance.pos.y = CHILDINT(entranceNode,Y);
  game->theEntrance.state = OPTCHILDINT(entranceNode,DECSTATE,CHILDHEX(entranceNode,HEXSTATE));
  game->theEntrance.total = CHILDINT(entranceNode,COUNT);
  game->theEntrance.rate = OPTCHILDFLOAT(entranceNode,RATE,1.);

  exitNode = CHILD(gameNode,EXIT);
  for (node = exitNode->children; node; node = node->next)
    if (MATCHES(node,LOC))
      registerCellWatcher (game->board, CHILDINT(node,X), CHILDINT(node,Y), game->theExit.watcher);
  game->theExit.toWin = CHILDINT(exitNode,COUNT);
  game->theExit.type = OPTCHILDINT(exitNode,DECTYPE,CHILDHEX(entranceNode,HEXTYPE));

  game->timeLimit = OPTCHILDFLOAT(gameNode,TIME,-1.);

  return game;
}

Tool* newToolFromXmlNode (xmlNode* toolNode) {
  Tool *tool;
  xmlNode *brushNode, *intensityNode, *patternNode, *overwriteNode, *node;
  int x, y, size;
  size = CHILDINT(toolNode,SIZE);
  tool = newTool ((char*) CHILDSTRING(toolNode,NAME), size);
  if ((brushNode = CHILD(toolNode,BRUSH))) {
    if ((node = CHILD(brushNode,CENTER))) {
      tool->brushCenter.x = CHILDINT(node,X);
      tool->brushCenter.y = CHILDINT(node,Y);
    }
    if ((intensityNode = CHILD(brushNode,INTENSITY))) {
      for (x = 0; x < size; ++x)
	for (y = 0; y < size; ++y)
	  updateQuadTree (tool->brushIntensity, x, y, 0.);
      for (node = intensityNode->children; node; node = node->next)
	if (MATCHES(node,LOC))
	  updateQuadTree (tool->brushIntensity, CHILDINT(node,X), CHILDINT(node,Y), OPTCHILDINT(node,RATE,1.));
    }
    if ((patternNode = CHILD(brushNode,PATTERN))) {
      tool->brushState = newXYMap (copyState, deleteState, printState);
      for (node = patternNode->children; node; node = node->next)
	if (MATCHES(node,LOC))
	  (void) XYMapInsert (tool->brushState, CHILDINT(node,X), CHILDINT(node,Y), newState(OPTCHILDINT(node,DECSTATE,CHILDHEX(node,HEXSTATE))));
    }
  }
  if (tool->brushState == NULL)
    tool->defaultBrushState = OPTCHILDINT(toolNode,DECSTATE,CHILDHEX(toolNode,HEXSTATE));
  if ((overwriteNode = CHILD(toolNode,OVERWRITE))) {
    if (CHILD(overwriteNode,DISALLOW)) {
      tool->overwriteDisallowLoc = newXYSet();
      for (node = overwriteNode->children; node; node = node->next)
	if (MATCHES(node,DISALLOW))
	  (void) XYSetInsert (tool->overwriteDisallowLoc, CHILDINT(node,X), CHILDINT(node,Y));
    }
    if (CHILD(overwriteNode,DECSTATE) || CHILD(overwriteNode,HEXSTATE)) {
      tool->overwriteStates = newStateSet();
      for (node = overwriteNode->children; node; node = node->next)
	if (MATCHES(node,DECSTATE))
	  (void) StateSetInsert (tool->overwriteStates, NODEINTVAL(node));
	else if (MATCHES(node,HEXSTATE))
	  (void) StateSetInsert (tool->overwriteStates, NODEHEXVAL(node));
      tool->overwriteMask = OPTCHILDINT(overwriteNode,DECMASK,OPTCHILDHEX(node,HEXMASK,TypeMask));
    }
  }
  tool->sprayRate = OPTCHILDFLOAT(toolNode,SPRAY,1.);
  tool->reserve = OPTCHILDFLOAT(toolNode,RESERVE,1.);
  tool->rechargeRate = OPTCHILDFLOAT(toolNode,RECHARGE,0.);
  tool->maxReserve = OPTCHILDFLOAT(toolNode,MAXRESERVE,tool->reserve);
  return tool;
}

ToolCharger* newToolChargerFromXmlNode (Game *game, xmlNode* chargerNode) {
  ListNode *listNode;
  Tool *tool;
  ToolCharger *charger;
  char *toolName;
  charger = newToolCharger();
  charger->overwriteType = OPTCHILDINT(chargerNode,DECTYPE,CHILDHEX(chargerNode,HEXTYPE));
  toolName = (char*) CHILDSTRING(chargerNode,NAME);
  charger->tool = NULL;
  for (listNode = game->allTools->head; listNode; listNode = listNode->next) {
    tool = (Tool*) listNode->value;
    if (strcmp (tool->name, toolName) == 0) {
      charger->tool = tool;
      break;
    }
  }
  Assert (charger->tool != NULL, "Couldn't find tool");
  return charger;
}

