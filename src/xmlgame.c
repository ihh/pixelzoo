#include <string.h>
#include "notify.h"
#include "xmlgame.h"
#include "xmlboard.h"
#include "xmlgoal.h"
#include "xmlutil.h"

/* prototypes for private builder methods */
GoalTrigger* newGoalTriggerFromXmlNode (Game *game, xmlNode *node);

/* method defs */
Game* newGameFromXmlFile (const char* filename) {
  xmlDoc* doc;
  Game* game = NULL;
  doc = xmlReadFile (filename, NULL, 0);
  Assert (doc != NULL, "XML file not found");
  if (doc)
    game = newGameFromXmlDocument (doc);
  return game;
}

Game* newGameFromXmlString (const char* string) {
  xmlDoc* doc;
  Game* game = NULL;
  doc = xmlReadMemory (string, strlen(string), "noname.xml", NULL, 0);
  Assert (doc != NULL, "XML string not parsed");
  game = newGameFromXmlDocument (doc);
  return game;
}

Game* newGameFromXmlDocument (xmlDoc *doc) {
  return newGameFromXmlRoot (xmlDocGetRootElement (doc));
}

Game* newGameFromXmlRoot (xmlNode *root) {
  Game *game;
  xmlNode *gameNode, *exitNode, *goalNode, *node;
  Tool *tool, *selectedTool;

  gameNode = CHILD(root,GAME);
 
  game = newGame();
  game->board = newBoardFromXmlRoot ((void*)game, gameNode);

  game->updatesPerSecond = OPTCHILDFLOAT (gameNode, RATE, DefaultUpdatesPerSecond);

  selectedTool = NULL;
  for (node = gameNode->children; node; node = node->next)
  if (MATCHES(node,TOOL)) {
    selectedTool = tool = newToolFromXmlNode (node);
    addToolToGame (game, tool);
  }

  for (node = gameNode->children; node; node = node->next)
    if (MATCHES(node,TRIGGER))
      (void) ListInsertBefore (game->trigger, NULL, newGoalTriggerFromXmlNode (game, node));

  Assert (RBTreeSize(game->toolByName) > 0 && selectedTool != NULL, "You need some tools!");

  for (node = gameNode->children; node; node = node->next)
    if (MATCHES(node,PROTECT))
      registerCellWatcher (game->board, CHILDINT(node,X), CHILDINT(node,Y), game->writeProtectWatcher);

  exitNode = CHILD(gameNode,EXIT);
  for (node = exitNode->children; node; node = node->next)
    if (MATCHES(node,POS))
      registerCellWatcher (game->board, CHILDINT(node,X), CHILDINT(node,Y), game->theExit.watcher);
  game->theExit.type = OPTCHILDINT(exitNode,DECTYPE,CHILDHEX(exitNode,HEXTYPE));

  goalNode = CHILD (gameNode, GOAL);
  if (goalNode)
    game->goal = newGoalFromXmlNode (goalNode, game);

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
	if (MATCHES(node,POS))
	  updateQuadTree (tool->brushIntensity, CHILDINT(node,X), CHILDINT(node,Y), OPTCHILDINT(node,RATE,1.));
    }
    if ((patternNode = CHILD(brushNode,PATTERN))) {
      tool->brushState = newXYMap (copyState, deleteState, printState);
      for (node = patternNode->children; node; node = node->next)
	if (MATCHES(node,POS))
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
      tool->overwriteMask = OPTCHILDHEX(node,MASK,TypeMask);
    }
  }
  tool->sprayRate = OPTCHILDFLOAT(toolNode,SPRAY,1.);
  tool->reserve = OPTCHILDFLOAT(toolNode,RESERVE,1.);
  tool->rechargeRate = OPTCHILDFLOAT(toolNode,RECHARGE,0.);
  tool->maxReserve = OPTCHILDFLOAT(toolNode,MAXRESERVE,tool->reserve);
  tool->hidden = CHILD(toolNode,HIDE) != NULL;
  return tool;
}

GoalTrigger* newGoalTriggerFromXmlNode (Game *game, xmlNode *triggerNode) {
  GoalTrigger *trigger;
  xmlNode *node;
  trigger = newGoalTrigger (game, newGoalFromXmlNode (CHILD(triggerNode,GOAL_GPARAM), game));
  trigger->overwriteType = OPTCHILDINT(triggerNode,DECTYPE,CHILDHEX(triggerNode,HEXTYPE));
  for (node = triggerNode->children; node; node = node->next)
    if (MATCHES(node,POS))
      registerCellWatcher (game->board, CHILDINT(node,X), CHILDINT(node,Y), trigger->watcher);
  return trigger;
}
