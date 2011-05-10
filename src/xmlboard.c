#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xmlboard.h"
#include "xmlutil.h"
#include "xmlgoal.h"
#include "xmlmove.h"
#include "game.h"
#include "vars.h"

/* prototypes for private builder methods */
Particle* newParticleFromXmlNode (void *game, xmlNode* node);
void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node);
ParticleRule* newRuleFromXmlNode (void *game, xmlNode* node);
ParticleRule* newRuleFromXmlParentNode (void *game, xmlNode* parent);

void initLookupRuleFromXmlNode (LookupRuleParams* lookup, xmlNode* node, void *game);
void initModifyRuleFromXmlNode (ModifyRuleParams* op, xmlNode* node, void *game);
void initDeliverRuleFromXmlNode (DeliverRuleParams* deliver, xmlNode* node, void *game);
void initGotoRuleFromXmlNode (ParticleRule** gotoLabelRef, xmlNode* node, void *game);

/* method defs */
Board* newBoardFromXmlDocument (void *game, xmlDoc *doc) {
  return newBoardFromXmlRoot (game, xmlDocGetRootElement (doc));
}

Board* newBoardFromXmlRoot (void *game, xmlNode *root) {
  Board *board;
  xmlNode *boardNode, *ruleNode, *queueNode, *grammarNode, *seedNode, *node;
  int x, y;
  State state;
  const char* subRuleName;
  ParticleRule *rule;

  boardNode = CHILD(root,BOARD);
  Assert (boardNode != NULL, "XML board tag not found");

  board = newBoard (CHILDINT(boardNode,SIZE));
  board->game = game;

  seedNode = CHILD(boardNode,SEED);
  if (seedNode)
    rngSetStateString (board->rng, (char*) getNodeContent (seedNode));

  board->microticks = OPTCHILDINT(boardNode,TIME,0);

  grammarNode = CHILD(boardNode,GRAMMAR);
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,SUBRULE)) {
      subRuleName = (const char*) CHILDSTRING(node,NAME);
      ruleNode = CHILD(node,RULE);
      rule = newRuleFromXmlNode (game, ruleNode);
      Assert (StringMapFind (board->subRule, subRuleName) == 0, "Duplicate subrule name");
      (void) StringMapInsert (board->subRule, (char*) subRuleName, rule);
    }
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      addParticleToBoard (newParticleFromXmlNode(game,node), board);

  for (node = boardNode->children; node; node = node->next)
    if (MATCHES(node,INIT)) {
      x = CHILDINT(node,X);
      y = CHILDINT(node,Y);
      state = OPTCHILDINT(node,DECSTATE,OPTCHILDHEX(node,HEXSTATE,OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE)) << TypeShift));
      writeBoardState (board, x, y, state);
    }

  queueNode = CHILD (boardNode, QUEUE);
  if (queueNode)
    board->moveQueue = newMoveListFromXmlNode (queueNode);

  return board;
}

Board* newBoardFromXmlFile (void *game, const char* filename) {
  xmlDoc* doc;
  Board* board = NULL;
  doc = xmlReadFile (filename, NULL, 0);
  if (doc)
    board = newBoardFromXmlDocument (game, doc);
  return board;
}

Board* newBoardFromXmlString (void *game, const char* string) {
  xmlDoc* doc;
  Board* board = NULL;
  doc = xmlReadMemory (string, strlen(string), "noname.xml", NULL, 0);
  if (doc)
    board = newBoardFromXmlDocument (game, doc);
  return board;
}


Particle* newParticleFromXmlNode (void *game, xmlNode* node) {
  Particle* p;
  Message message;
  VarsDescriptor* vd;
  int nColorRules, readOnlyIndex, varOffset, varWidth;
  xmlNode *curNode, *syncNode, *varsNode;
  p = newParticle ((const char*) CHILDSTRING(node,NAME));

  if ((syncNode = CHILD(node,SYNC))) {
    p->synchronous = 1;
    p->syncPeriod = OPTCHILDINT(syncNode,PERIOD,0);  /* leaving this as zero means that it will be auto-set at 1/(total rate) by addParticleToBoard */
    p->syncPhase = OPTCHILDINT(syncNode,PHASE,0);
  }

  p->rule = newRuleFromXmlNode (game, CHILD(node,RULE));

  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,DISPATCH)) {
      message = OPTCHILDINT(curNode,DECMESSAGE,CHILDHEX(curNode,HEXMESSAGE));
      addParticleMessageHandler (p, message, newRuleFromXmlParentNode (game, curNode));
    }

  readOnlyIndex = 0;
  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,READONLY)) {
      readOnlyIndex = OPTCHILDINT (curNode, INDEX, readOnlyIndex + 1);
      Assert (readOnlyIndex >= 0 && readOnlyIndex < ReadOnlyStates, "newParticleFromXmlNode: read-only state index out of range");
      p->readOnly[readOnlyIndex] = OPTCHILDINT(curNode,DECSTATE,CHILDHEX(curNode,HEXSTATE));
    }

  p->rate = FloatToIntMillionths (OPTCHILDFLOAT(node,RATE,1.));
  p->type = OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE));

  nColorRules = 0;
  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,COLRULE)) {
      Assert (nColorRules < NumColorRules, "newParticleFromXmlNode: too many color rules");
      initColorRuleFromXmlNode (&p->colorRule[nColorRules++], curNode);
    }

  varsNode = CHILD (node, VARS);
  if (varsNode) {
    varOffset = 0;
    for (curNode = varsNode->children; curNode; curNode = curNode->next)
      if (MATCHES(curNode,VARSIZE)) {
	varWidth = CHILDINT (curNode, SIZE);
	vd = newVarsDescriptor ((const char*) CHILDSTRING (curNode, NAME), varOffset, varWidth);
	Assert (xmlValidateNameValue ((xmlChar*) vd->name), "Variable names must also be valid XML tags");
	Assert (strcmp (vd->name, XMLZOO_TYPE) != 0, "'type' is a reserved word that cannot be used as a variable name");
	varOffset += varWidth;
	Assert (varOffset <= 48 && varWidth > 0, "newParticleFromXmlNode: bad vars descriptor");
	ListAppend (p->vars, vd);
      }
  }

  return p;
}

void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node) {
  colorRule->rightShift = OPTCHILDINT(node,RSHIFT,0);
  colorRule->mask = OPTCHILDHEX(node,MASK,VarsMask);
  colorRule->multiplier = OPTCHILDINT(node,DECMUL,OPTCHILDHEX(node,HEXMUL,1));
  colorRule->offset = OPTCHILDINT(node,DECINC,OPTCHILDHEX(node,HEXINC,0));
}

ParticleRule* newRuleFromXmlParentNode (void *game, xmlNode* parent) {
  ParticleRule* rule;
  rule = NULL;
  if (parent)
    rule = newRuleFromXmlNode (game, CHILD (parent, RULE));
  return rule;
}

ParticleRule* newRuleFromXmlNode (void *game, xmlNode *ruleParentNode) {
  xmlNode *ruleNode;
  ParticleRule *rule, **gotoLabelRef;
  LookupRuleParams *lookup;
  ModifyRuleParams *modify;
  DeliverRuleParams *deliver;
  RandomRuleParams *random;

  rule = NULL;

  if (ruleParentNode != NULL) {
    ruleNode = ruleParentNode->children;
    while (ruleNode != NULL && ruleNode->type != XML_ELEMENT_NODE)
      ruleNode = ruleNode->next;
    Assert (ruleNode != NULL && ruleNode->type == XML_ELEMENT_NODE, "Missing rule node");

    if (MATCHES (ruleNode, SWITCH)) {
      rule = newLookupRule();
      lookup = &rule->param.lookup;
      initLookupRuleFromXmlNode (lookup, ruleNode, game);

    } else if (MATCHES (ruleNode, MODIFY)) {
      rule = newModifyRule();
      modify = &rule->param.modify;
      initModifyRuleFromXmlNode (modify, ruleNode, game);

    } else if (MATCHES (ruleNode, DELIVER)) {
      rule = newDeliverRule();
      deliver = &rule->param.deliver;
      initDeliverRuleFromXmlNode (deliver, ruleNode, game);

    } else if (MATCHES (ruleNode, GOTO)) {
      rule = newGotoRule();
      gotoLabelRef = &rule->param.gotoLabel;
      initGotoRuleFromXmlNode (gotoLabelRef, ruleNode, game);

    } else if (MATCHES (ruleNode, RANDOM)) {
      rule = newRandomRule();
      random = &rule->param.random;
      random->prob = FloatToIntMillionths (OPTCHILDFLOAT (ruleNode, PROB, 0.5));
      random->passRule = newRuleFromXmlParentNode (game, CHILD (ruleNode, PASS));
      random->failRule = newRuleFromXmlParentNode (game, CHILD (ruleNode, FAIL));

    } else if (MATCHES (ruleNode, GOAL)) {
      rule = newGoalRule();
      rule->param.goal = newGoalFromXmlNode (ruleNode, game);

    } else {
      Abort ("Unknown rule type");
    }
  }

  return rule;
}

void initLookupRuleFromXmlNode (LookupRuleParams* lookup, xmlNode* node, void *game) {
  xmlNode *loc, *curNode;
  State state;
  ParticleRule *rule;

  loc = CHILD(node,POS);
  if (loc) {
    lookup->loc.x = OPTCHILDINT(loc,X,0);
    lookup->loc.y = OPTCHILDINT(loc,Y,0);
  } else
    lookup->loc.x = lookup->loc.y = 0;

  lookup->mask = OPTCHILDHEX(node,MASK,StateMask);
  lookup->shift = OPTCHILDINT(node,RSHIFT,0);

  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,CASE)) {
      state = OPTCHILDINT(curNode,DECSTATE,OPTCHILDHEX(curNode,HEXSTATE,0));
      rule = newRuleFromXmlParentNode (game, curNode);
      Assert (StateMapFind (lookup->matchRule, state) == NULL, "Duplicate key in lookup, or more than one key unassigned/zero");
      (void) StateMapInsert (lookup->matchRule, state, rule);
    }

  lookup->defaultRule = newRuleFromXmlParentNode (game, CHILD (node, DEFAULT));
}

void initModifyRuleFromXmlNode (ModifyRuleParams* op, xmlNode* node, void *game) {
  xmlNode *src, *dest;

  op->rightShift = OPTCHILDINT(node,RSHIFT,0);
  op->offset = OPTCHILDINT(node,DECINC,OPTCHILDHEX(node,HEXINC,0));
  op->leftShift = OPTCHILDINT(node,LSHIFT,0);

  op->srcMask = OPTCHILDHEX(node,SRCMASK,StateMask);
  op->destMask = OPTCHILDHEX(node,DESTMASK,StateMask);

  src = CHILD(node,SRC);
  dest = CHILD(node,DEST);
  if (src) {
    op->src.x = OPTCHILDINT(src,X,0);
    op->src.y = OPTCHILDINT(src,Y,0);
  } else
    op->src.x = op->src.y = 0;
  if (dest) {
    op->dest.x = OPTCHILDINT(dest,X,0);
    op->dest.y = OPTCHILDINT(dest,Y,0);
  } else
    op->dest = op->src;

  op->nextRule = newRuleFromXmlParentNode (game, CHILD (node, NEXT));
}

void initDeliverRuleFromXmlNode (DeliverRuleParams* deliver, xmlNode* node, void *game) {
  xmlNode *recip;
  recip = CHILD(node,POS);
  deliver->recipient.x = CHILDINT(recip,X);
  deliver->recipient.y = CHILDINT(recip,Y);
  deliver->message = OPTCHILDINT(node,DECMESSAGE,CHILDHEX(node,HEXMESSAGE));
}

void initGotoRuleFromXmlNode (ParticleRule** gotoLabelRef, xmlNode* node, void *game) {
  StringMapNode *subRuleNode;
  const char* label;
  label = (const char*) getNodeContentOrComplain (node, NULL);
  subRuleNode = StringMapFind (((Game*)game)->board->subRule, label);
  *gotoLabelRef = subRuleNode ? ((ParticleRule*) (subRuleNode->value)) : ((ParticleRule*) NULL);
  if (*gotoLabelRef == NULL) {
    fprintf (stderr, "Unresolved goto label: %s\n", label);
    Abort ("Couldn't find goto label");
  }
}

void writeBoard (Board* board, xmlTextWriterPtr writer, int reverseCompile) {
  int x, y, i;
  State s;
  char *rngState;

  Assert (board->rngReleased, "writeBoard: random number generator not released. You need to call boardReleaseRandomNumbers");

  xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_BOARD);  /* begin board */
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_SIZE, "%d", board->size);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_TIME, "%lld", board->microticks);

  rngState = getRngStateString (board->rng);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_SEED, "%s", rngState);
  SafeFree(rngState);

  if (!reverseCompile)
    writeTypes (board, writer);

  for (x = 0; x < board->size; ++x)
    for (y = 0; y < board->size; ++y) {
      i = boardIndex (board->size, x, y);
      s = board->cell[i];
      if (s) {
	xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_INIT);  /* begin init */
	xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_X, "%d", x);
	xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_Y, "%d", y);
	if (reverseCompile)
	  writeGVars (board, s, writer);
	else
	  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_HEXSTATE, "%llx", s);
	xmlTextWriterFullEndElement (writer);  /* end init */
      }
    }

  xmlTextWriterFullEndElement (writer);  /* end board */
}

void writeTypes (Board* board, xmlTextWriterPtr writer) {
  State t;
  ListNode *node;
  VarsDescriptor *vd;
  xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_GRAMMAR);   /* begin grammar */
  for (t = 0; t <= MaxType; ++t)
    if (board->byType[t]) {
      xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_PARTICLE);   /* begin particle */
      xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_NAME, "%s", board->byType[t]->name);
      xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_HEXTYPE, "%hx", board->byType[t]->type);
      if (!ListEmpty (board->byType[t]->vars)) {
	xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_VARS);   /* begin vars */
	for (node = board->byType[t]->vars->head; node; node = node->next) {
	  vd = (VarsDescriptor*) node->value;
	  xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_VARSIZE);   /* begin varsize */
	  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_NAME, "%s", vd->name);
	  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_SIZE, "%d", vd->width);
	  xmlTextWriterFullEndElement (writer);  /* end varsize */
	}
	xmlTextWriterFullEndElement (writer);  /* end vars */
      }
      xmlTextWriterFullEndElement (writer);  /* end particle */
    }
  xmlTextWriterFullEndElement (writer);  /* end grammar */
}

void writeGVars (Board* board, State s, xmlTextWriterPtr writer) {
  State t, varVal;
  ListNode *node;
  VarsDescriptor *vd;
  t = StateType(s);
  if (board->byType[t] && board->byType[t]->name) {
    xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_GVARS);  /* begin gvars */
    xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_TYPE, "%s", board->byType[t]->name);
    for (node = board->byType[t]->vars->head; node; node = node->next) {
      vd = (VarsDescriptor*) node->value;
      varVal = (s >> vd->offset) & ((1 << vd->width) - 1);
      if (varVal != 0)
	xmlTextWriterWriteFormatElement (writer, (xmlChar*) vd->name, "%lld", varVal);
    }
    xmlTextWriterFullEndElement (writer);  /* end gvars */
  }
}

