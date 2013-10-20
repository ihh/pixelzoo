#include <string.h>
#include "notify.h"
#include "xmlgame.h"
#include "xmlboard.h"
#include "xmlgoal.h"
#include "xmlutil.h"

/* prototypes for private builder methods */
GoalTrigger* newGoalTriggerFromXmlNode (Game *game, xmlNode *node);

/* method defs */
Game* newGameFromXmlString (const char* string) {
  xmlDoc* doc;
  Game* game = NULL;
  doc = xmlTreeFromString (string);
  Assert (doc != NULL, "XML string not parsed");
  game = newGameFromXmlDocument (doc);
  return game;
}

Game* newGameFromXmlDocument (xmlDoc *doc) {
  return newGameFromXmlRoot (xmlDocGetRootElement (doc));
}

Game* newGameFromXmlRoot (xmlNode *gameNode) {
  return newGameFromXmlRootWithSeparateBoard (gameNode, gameNode);
}

Game* newGameFromXmlDocumentWithSeparateBoard (xmlDoc *gameDoc, xmlDoc *separateBoardDoc) {
  return newGameFromXmlRootWithSeparateBoard (xmlDocGetRootElement (gameDoc),
					      xmlDocGetRootElement (separateBoardDoc));
}

Game* newGameFromXmlStringWithSeparateBoard (const char* gameString, const char* boardString) {
  xmlDoc *doc, *boardDoc;
  Game* game = NULL;
  doc = xmlTreeFromString (gameString);
  Assert (doc != NULL, "Game XML string not parsed");
  boardDoc = boardString==gameString ? doc : xmlTreeFromString (boardString);
  Assert (boardDoc != NULL, "Board XML string not parsed");
  game = newGameFromXmlDocumentWithSeparateBoard (doc, boardDoc);
  deleteXmlTree (doc);
  if (boardDoc != doc)
    deleteXmlTree (boardDoc);
  return game;
}

Game* newGameFromXmlRootWithSeparateBoard (xmlNode *gameNode, xmlNode *separateBoardRoot) {
  Game *game;
  xmlNode *exitNode, *goalNode, *node;
  Tool *tool, *selectedTool;
  xmlNode *endGoalParentNode, *endGoalNode;

  game = newGame();
  game->board = newBoardFromXmlRoot ((void*)game, separateBoardRoot);

  game->ticksPerSecond = OPTCHILDFLOAT (gameNode, RATE, DefaultTicksPerSecond);

  selectedTool = NULL;
  for (node = gameNode->children; node; node = node->next)
  if (MATCHES(node,TOOL)) {
    selectedTool = tool = newToolFromXmlNode (node, game->board->protoTable);
    addToolToGame (game, tool);
  }

  for (node = gameNode->children; node; node = node->next)
    if (MATCHES(node,TRIGGER))
      (void) ListInsertBefore (game->trigger, NULL, newGoalTriggerFromXmlNode (game, node));

  for (node = gameNode->children; node; node = node->next)
    if (MATCHES(node,PROTECT))
      registerCellWatcher (game->board, CHILDINT(node,X), CHILDINT(node,Y), game->writeProtectWatcher);

  exitNode = CHILD(gameNode,EXIT);
  if (exitNode) {
    for (node = exitNode->children; node; node = node->next)
      if (MATCHES(node,POS))
	registerCellWatcher (game->board, CHILDINT(node,X), CHILDINT(node,Y), game->theExit.watcher);
    game->theExit.type = OPTCHILDINT(exitNode,DECTYPE,CHILDHEX(exitNode,HEXTYPE));
  } else
    game->theExit.type = PortalDestroyed;

  goalNode = CHILD (gameNode, GOAL);
  if (goalNode)
    game->goal = newGoalFromXmlNode (goalNode, game);

  endGoalParentNode = CHILD (gameNode, ENDTURN);
  if (endGoalParentNode && (endGoalNode = CHILD (endGoalParentNode, GOAL)))
    game->endGoal = newGoalFromXmlNode (endGoalNode, game);

  return game;
}

Tool* newToolFromXmlNode (xmlNode* toolNode, ProtoTable *protoTable) {
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
	if (MATCHES(node,SPOT))
	  updateQuadTree (tool->brushIntensity, CHILDINT(node,X), CHILDINT(node,Y), OPTCHILDINT(node,RATE,1.));
    }
    if ((patternNode = CHILD(brushNode,PATTERN))) {
      tool->brushState = newXYMap (copyState, deleteState, printState);
      for (node = patternNode->children; node; node = node->next)
	if (MATCHES(node,PIXEL))
	  (void) XYMapInsert (tool->brushState, CHILDINT(node,X), CHILDINT(node,Y), newState(getStateFromNode(node,protoTable,0)));
    }
  }
  if (tool->brushState == NULL)
    tool->defaultBrushState = getStateFromNode(toolNode,protoTable,0);
  if ((overwriteNode = CHILD(toolNode,OVERWRITE))) {
    if (CHILD(overwriteNode,DISALLOW)) {
      tool->overwriteDisallowLoc = newXYSet();
      for (node = overwriteNode->children; node; node = node->next)
	if (MATCHES(node,DISALLOW))
	  (void) XYSetInsert (tool->overwriteDisallowLoc, CHILDINT(node,X), CHILDINT(node,Y));
    }
    if (testNodeHasState(overwriteNode)) {
      tool->overwriteStates = newStateSet();
      for (node = nextNodeWithState(overwriteNode->children); node; node = nextNodeWithState(node->next))
	(void) StateSetInsert (tool->overwriteStates, getStateFromChild (node, protoTable, 0));
      tool->overwriteMask = getMaskFromNode(overwriteNode,protoTable,TypeMask);
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

void writeBoardAndEndGoalStatusXml (Game* game, xmlTextWriterPtr writer, int reverseCompile) {
  xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_GAME);  /* begin game element */
  writeBoardXml (game->board, writer, reverseCompile);  /* board element */
  xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_ENDTURN);  /* begin endturn element */
  xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_GOAL);  /* begin goal element */
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) (testGoalMet(game->endGoal,game) ? XMLZOO_TRUE_GOAL : XMLZOO_FALSE_GOAL), "");  /* empty true or false element */
  xmlTextWriterFullEndElement (writer);  /* end of goal element */
  xmlTextWriterFullEndElement (writer);  /* end of endgoal element */
  xmlTextWriterFullEndElement (writer);  /* end of game element */
}
