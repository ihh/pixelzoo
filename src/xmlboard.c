#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xmlparser.h"
#include "xmlboard.h"
#include "xmlutil.h"
#include "xmlgoal.h"
#include "xmlmove.h"
#include "game.h"
#include "vars.h"

/* prototypes for private builder methods */
Particle* newParticleFromXmlNode (void *game, xmlNode* node, ProtoTable *protoTable);
void parseVarsDescriptors (Proto *proto, xmlNode* particleNode);
void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node, ProtoTable *protoTable);
ParticleRule* newRuleFromXmlNode (void *game, xmlNode* node, ProtoTable *protoTable, StringMap **localSubRule);
ParticleRule* newRuleFromXmlParentNode (void *game, xmlNode* parent, ProtoTable *protoTable, StringMap **localSubRule);
ParticleRule* newRuleFromXmlGrandparentNode (void *game, xmlNode* grandparent, ProtoTable *protoTable, StringMap **localSubRule);

void initLookupRuleFromXmlNode (LookupRuleParams* lookup, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule);
void initModifyRuleFromXmlNode (ModifyRuleParams* op, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule);
void initDeliverRuleFromXmlNode (DeliverRuleParams* deliver, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule);
void initGotoRuleFromXmlNode (ParticleRule** gotoLabelRef, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule);
void initLoadRuleFromXmlNode (LoadRuleParams* load, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule);

/* method defs */
Board* newBoardFromXmlDocument (void *game, xmlDoc *doc) {
  return newBoardFromXmlRoot (game, xmlDocGetRootElement (doc));
}

Board* newBoardFromXmlRoot (void *game, xmlNode *root) {
  Board *board;
  xmlNode *boardNode, *queueNode, *grammarNode, *seedNode, *node;
  int x, y;
  State state;
  const char* subRuleName;
  ParticleRule *rule;
  ProtoTable *protoTable;
  Proto *proto;

  boardNode = CHILD(root,BOARD);
  Assert (boardNode != NULL, "XML board tag not found");

  board = newBoard (CHILDINT(boardNode,SIZE));
  board->game = game;

  seedNode = CHILD(boardNode,SEED);
  if (seedNode)
    rngSetStateString (board->rng, (char*) getNodeContent (seedNode));

  board->microticks = OPTCHILDINT(boardNode,TIME,0);
  board->updateCount = OPTCHILDINT(boardNode,UPDATE,0);

  /* parse grammar */
  grammarNode = CHILD(boardNode,GRAMMAR);

  /* create ProtoTable in two passes (first Particles that have Types, then those that don't) */
  protoTable = newProtoTable();
  board->protoTable = protoTable;
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      if (CHILD(node,DECTYPE) || CHILD(node,HEXTYPE)) {
	proto = protoTableAddTypedParticle (protoTable, (char*) CHILDSTRING(node,NAME), OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE)));
	parseVarsDescriptors (proto, node);
      }

  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      if (CHILD(node,DECTYPE) || CHILD(node,HEXTYPE)) {
	proto = protoTableAddParticle (protoTable, (char*) CHILDSTRING(node,NAME));
	parseVarsDescriptors (proto, node);
      }

  /* evaluate top-level Scheme expressions */
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,SCHEME))
      (void) protoTableEval (protoTable, (const char*) getNodeContent (node));

  /* create subrules */
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,SUBRULE)) {
      subRuleName = (const char*) ATTR(node,NAME);
      Assert (StringMapFind (board->subRule, subRuleName) == 0, "Duplicate global subrule name %s", subRuleName);
      rule = newRuleFromXmlParentNode (game, node, protoTable, &board->subRule);
      defineSubRule (&board->subRule, subRuleName, rule, NULL);
    }

  /* create particles */
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      addParticleToBoard (newParticleFromXmlNode(game,node,protoTable), board);

  /* initialize board */
  for (node = boardNode->children; node; node = node->next)
    if (MATCHES(node,INIT)) {
      x = CHILDINT(node,X);
      y = CHILDINT(node,Y);
      state = getStateFromNode (node, protoTable, 0);
      writeBoardState (board, x, y, state);
    }

  queueNode = CHILD (boardNode, QUEUE);
  if (queueNode)
    board->moveQueue = newMoveListFromXmlNode (queueNode, protoTable);

  return board;
}

Board* newBoardFromXmlString (void *game, const char* string) {
  xmlDoc* doc;
  Board* board = NULL;
  doc = xmlTreeFromString (string);
  if (doc)
    board = newBoardFromXmlDocument (game, doc);
  return board;
}


Particle* newParticleFromXmlNode (void *game, xmlNode* node, ProtoTable *protoTable) {
  Particle* p;
  Message message;
  int nColorRules, readOnlyIndex;
  xmlNode *curNode, *syncNode, *schemeNode, *childNode;
  ParticleRule *subRule;
  Proto *proto;
  const char *subRuleName, *evalResult;
  xmlNode *evalNode;

  if ( (schemeNode = CHILD (node, SCHEME)) ) {  /* assignment intentional */
    evalResult = protoTableEvalSxml (protoTable, (const char*) getNodeContent(schemeNode));
    evalNode = xmlTreeFromString (evalResult);

    p = newParticleFromXmlNode (game, evalNode, protoTable);

    deleteXmlTree (evalNode);
    StringDelete ((void*) evalResult);

  } else {
    p = newParticle ((const char*) CHILDSTRING(node,NAME));

    if ((syncNode = CHILD(node,SYNC))) {
      p->synchronous = 1;
      p->syncPeriod = OPTCHILDINT(syncNode,PERIOD,0);  /* leaving this as zero means that it will be auto-set at 1/(total rate) by addParticleToBoard */
      p->syncPhase = OPTCHILDINT(syncNode,PHASE,0);
    }

    protoTableSetSelfType (protoTable, p->name);

    /* create subrules local to this Particle */
    for (childNode = node->children; childNode; childNode = childNode->next)
      if (MATCHES(childNode,SUBRULE)) {
	subRuleName = (const char*) ATTR(childNode,NAME);
	subRule = newRuleFromXmlParentNode (game, childNode, protoTable, &p->subRule);
	addParticleSubRule (p, subRuleName, subRule, game);
      }

    /* create main update rule */
    p->rule = newRuleFromXmlParentNode (game, CHILD(node,RULE), protoTable, &p->subRule);

    /* message dispatch table */
    for (curNode = node->children; curNode; curNode = curNode->next)
      if (MATCHES(curNode,DISPATCH)) {
	message = OPTCHILDINT(curNode,DECMESSAGE,CHILDHEX(curNode,HEXMESSAGE));
	addParticleMessageHandler (p, message, newRuleFromXmlGrandparentNode (game, curNode, protoTable, &p->subRule));
      }

    readOnlyIndex = 0;
    for (curNode = node->children; curNode; curNode = curNode->next)
      if (MATCHES(curNode,READONLY)) {
	readOnlyIndex = OPTCHILDINT (curNode, INDEX, readOnlyIndex + 1);
	Assert (readOnlyIndex >= 0 && readOnlyIndex < ReadOnlyStates, "newParticleFromXmlNode: read-only state index out of range");
	p->readOnly[readOnlyIndex] = getStateFromNode (curNode, protoTable, 0);
      }

    p->rate = FloatToIntMillionths (OPTCHILDFLOAT(node,RATE,1.));

    proto = protoTableGetProto (protoTable, p->name);
    p->type = proto->type;
    protoCopyVarsDescriptorsToList (proto, p->vars);

    nColorRules = 0;
    for (curNode = node->children; curNode; curNode = curNode->next)
      if (MATCHES(curNode,COLRULE)) {
	Assert (nColorRules < NumColorRules, "newParticleFromXmlNode: too many color rules");
	initColorRuleFromXmlNode (&p->colorRule[nColorRules++], curNode, protoTable);
      }
  }

  return p;
}

void parseVarsDescriptors (Proto *proto, xmlNode* node) {
  xmlNode *curNode, *varsNode;
  int varWidth;
  const char *varName;

  varsNode = CHILD (node, VARS);
  if (varsNode) {
    for (curNode = varsNode->children; curNode; curNode = curNode->next)
      if (MATCHES(curNode,VARSIZE)) {
	varName = (const char*) CHILDSTRING (curNode, NAME);
	varWidth = CHILDINT (curNode, SIZE);
	Assert (xmlValidateNameValue ((xmlChar*) varName), "Variable names must also be valid XML tags");
	Assert (strcmp (varName, XMLZOO_TYPE) != 0, "'type' is a reserved word that cannot be used as a variable name");
	(void) protoAddVar (proto, varName, varWidth);
      }
  }
}


void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node, ProtoTable *protoTable) {
  colorRule->rightShift = getRShiftFromNode (node, protoTable);
  colorRule->mask = getMaskFromNode (node, protoTable, VarsMask);
  colorRule->multiplier = OPTCHILDINT(node,DECMUL,OPTCHILDHEX(node,HEXMUL,1));
  colorRule->offset = OPTCHILDINT(node,DECINC,OPTCHILDHEX(node,HEXINC,0));
}

ParticleRule* newRuleFromXmlGrandparentNode (void *game, xmlNode* parent, ProtoTable *protoTable, StringMap **localSubRule) {
  ParticleRule* rule;
  rule = NULL;
  if (parent)
    rule = newRuleFromXmlParentNode (game, CHILD (parent, RULE), protoTable, localSubRule);
  return rule;
}

ParticleRule* newRuleFromXmlParentNode (void *game, xmlNode *ruleParentNode, ProtoTable *protoTable, StringMap **localSubRule) {
  xmlNode *childNode, *ruleNode;
  ParticleRule *rule, *subRule;
  const char* subRuleName;

  rule = NULL;

  if (ruleParentNode != NULL) {

    ruleNode = NULL;
    for (childNode = ruleParentNode->children; childNode; childNode = childNode->next)
      if (MATCHES (childNode, SUBRULE)) {
	subRuleName = (const char*) ATTR(childNode,NAME);
	subRule = newRuleFromXmlParentNode (game, childNode, protoTable, localSubRule);
	defineSubRule (localSubRule, subRuleName, subRule, ((Game*) game)->board->subRule);

      } else if (MATCHES(childNode,SWITCH) || MATCHES(childNode,MODIFY) || MATCHES(childNode,GOTO) || MATCHES(childNode,DELIVER) || MATCHES(childNode,RANDOM) || MATCHES(childNode,GOAL) || MATCHES(childNode,LOAD) || MATCHES(childNode,SCHEME)) {
	if (ruleNode)
	  Warn ("Ignoring <%s> child of <rule> node, in favor of older sibling <%s>", (const char*) childNode->name, (const char*) ruleNode->name);
	else
	  ruleNode = childNode;
      }

    Assert (ruleNode != NULL, "Missing rule node");
    rule = newRuleFromXmlNode (game, ruleNode, protoTable, localSubRule);

    subRuleName = ATTR(ruleParentNode,NAME);
    if (subRuleName) {
      subRule = rule;
      defineSubRule (localSubRule, subRuleName, subRule, ((Game*) game)->board->subRule);
      rule = newGotoRule();
      rule->param.gotoLabel = subRule;
    }
  }

  return rule;
}

ParticleRule* newRuleFromXmlNode (void *game, xmlNode *ruleNode, ProtoTable *protoTable, StringMap **localSubRule) {
  ParticleRule *rule, **gotoLabelRef;
  LookupRuleParams *lookup;
  ModifyRuleParams *modify;
  DeliverRuleParams *deliver;
  RandomRuleParams *random;
  LoadRuleParams *load;
  const char *evalResult;
  xmlNode *evalNode;

  rule = NULL;

  if (MATCHES (ruleNode, SWITCH)) {
    rule = newLookupRule();
    lookup = &rule->param.lookup;
    initLookupRuleFromXmlNode (lookup, ruleNode, game, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, MODIFY)) {
    rule = newModifyRule();
    modify = &rule->param.modify;
    initModifyRuleFromXmlNode (modify, ruleNode, game, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, GOTO)) {
    rule = newGotoRule();
    gotoLabelRef = &rule->param.gotoLabel;
    initGotoRuleFromXmlNode (gotoLabelRef, ruleNode, game, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, DELIVER)) {
    rule = newDeliverRule();
    deliver = &rule->param.deliver;
    initDeliverRuleFromXmlNode (deliver, ruleNode, game, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, RANDOM)) {
    rule = newRandomRule();
    random = &rule->param.random;
    random->prob = FloatToIntMillionths (OPTCHILDFLOAT (ruleNode, PROB, 0.5));
    random->passRule = newRuleFromXmlGrandparentNode (game, CHILD (ruleNode, PASS), protoTable, localSubRule);
    random->failRule = newRuleFromXmlGrandparentNode (game, CHILD (ruleNode, FAIL), protoTable, localSubRule);

  } else if (MATCHES (ruleNode, GOAL)) {
    rule = newGoalRule();
    rule->param.goal = newGoalFromXmlParentNode (ruleNode, game);

  } else if (MATCHES (ruleNode, LOAD)) {
    rule = newLoadRule();
    load = &rule->param.load;
    initLoadRuleFromXmlNode (load, ruleNode, game, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, SCHEME)) {
    evalResult = protoTableEvalSxml (protoTable, (const char*) getNodeContent(ruleNode));
    evalNode = xmlTreeFromString (evalResult);

    rule = newRuleFromXmlNode (game, evalNode, protoTable, localSubRule);

    deleteXmlTree (evalNode);
    StringDelete ((void*) evalResult);

  } else {
    Abort ("Unknown rule type");
  }

  return rule;
}

void initLookupRuleFromXmlNode (LookupRuleParams* lookup, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule) {
  xmlNode *loc, *curNode;
  const char* mode;
  State state;
  ParticleRule *rule;

  loc = CHILD(node,POS);
  if (loc) {
    lookup->loc.x = OPTCHILDINT(loc,X,0);
    lookup->loc.y = OPTCHILDINT(loc,Y,0);
    lookup->loc.xyAreRegisters = (mode = ATTR(loc,MODE)) && ATTRMATCHES(mode,INDIRECT);
  } else {
    lookup->loc.x = lookup->loc.y = 0;
    lookup->loc.xyAreRegisters = 0;
  }

  lookup->mask = getMaskFromNode (node, protoTable, StateMask);
  lookup->shift = getRShiftFromNode (node, protoTable);

  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,CASE)) {
       state = getGTypeOrStateFromNode (curNode, protoTable);
       rule = newRuleFromXmlGrandparentNode (game, curNode, protoTable, localSubRule);
       Assert (StateMapFind (lookup->matchRule, state) == NULL, "Duplicate key in lookup, or more than one key unassigned/zero");
       (void) StateMapInsert (lookup->matchRule, state, rule);
    }

  lookup->lowRule = newRuleFromXmlGrandparentNode (game, CHILD (node, LOW), protoTable, localSubRule);
  lookup->highRule = newRuleFromXmlGrandparentNode (game, CHILD (node, HIGH), protoTable, localSubRule);
  lookup->defaultRule = newRuleFromXmlGrandparentNode (game, CHILD (node, DEFAULT), protoTable, localSubRule);
}

void initModifyRuleFromXmlNode (ModifyRuleParams* op, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule) {
  xmlNode *src, *dest;
  const char *mode;

  op->rightShift = getRShiftFromNode (node, protoTable);
  op->offset = getIncOrStateFromNode (node, protoTable, &op->offsetIsRegister);
  op->leftShift = getLShiftFromNode (node, protoTable);

  op->srcMask = getSrcMaskFromNode (node, protoTable, StateMask);
  op->destMask = getDestMaskFromNode (node, protoTable, StateMask);

  src = CHILD(node,SRC);
  dest = CHILD(node,DEST);
  if (src) {
    op->src.x = OPTCHILDINT(src,X,0);
    op->src.y = OPTCHILDINT(src,Y,0);
    op->src.xyAreRegisters = (mode = ATTR(src,MODE)) && ATTRMATCHES(mode,INDIRECT);
  } else {
    op->src.x = op->src.y = 0;
    op->src.xyAreRegisters = 0;
  }
  if (dest) {
    op->dest.x = OPTCHILDINT(dest,X,0);
    op->dest.y = OPTCHILDINT(dest,Y,0);
    op->dest.xyAreRegisters = (mode = ATTR(dest,MODE)) && ATTRMATCHES(mode,INDIRECT);
  } else
    op->dest = op->src;

  op->nextRule = newRuleFromXmlGrandparentNode (game, CHILD (node, NEXT), protoTable, localSubRule);
}

void initDeliverRuleFromXmlNode (DeliverRuleParams* deliver, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule) {
  xmlNode *recip;
  recip = CHILD(node,POS);
  deliver->recipient.x = CHILDINT(recip,X);
  deliver->recipient.y = CHILDINT(recip,Y);
  deliver->message = OPTCHILDINT(node,DECMESSAGE,CHILDHEX(node,HEXMESSAGE));
}

void initGotoRuleFromXmlNode (ParticleRule** gotoLabelRef, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule) {
  StringMapNode *subRuleNode;
  const char* label;
  label = (const char*) getNodeContentOrComplain (node, NULL);
  subRuleNode = *localSubRule ? StringMapFind (*localSubRule, label) : (StringMapNode*) NULL;
  if (!subRuleNode)
    subRuleNode = StringMapFind (((Game*)game)->board->subRule, label);
  *gotoLabelRef = subRuleNode ? ((ParticleRule*) (subRuleNode->value)) : ((ParticleRule*) NULL);
  if (*gotoLabelRef == NULL) {
    fprintf (stderr, "Unresolved goto label: %s\n", label);
    Abort ("Couldn't find goto label");
  }
}

void initLoadRuleFromXmlNode (LoadRuleParams* load, xmlNode* node, void *game, ProtoTable *protoTable, StringMap **localSubRule) {
  int n;
  xmlNode *child;
  for (n = 0, child = node->children; child; child = child->next)
    if (MATCHES(child,REGISTER))
      ++n;
  load->n = n;
  load->reg = SafeCalloc (n, sizeof(unsigned char));
  load->state = SafeCalloc (n, sizeof(State));
  for (n = 0, child = node->children; child; child = child->next)
    if (MATCHES(child,REGISTER)) {
      load->reg[n] = (unsigned char) CHILDINT(child,INDEX);
      load->state[n] = getStateFromNode (child, protoTable, 0);
      Assert (load->reg[n] < NumberOfRegisters, "Register index %d is out of range", load->reg[n]);
      ++n;
    }

  load->nextRule = newRuleFromXmlGrandparentNode (game, CHILD (node, NEXT), protoTable, localSubRule);
}

void writeBoardXml (Board* board, xmlTextWriterPtr writer, int reverseCompile) {
  int x, y;
  char *rngState;

  Assert (board->rngReleased, "writeBoard: random number generator not released. You need to call boardReleaseRandomNumbers");

  xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_BOARD);  /* begin board */
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_SIZE, "%d", board->size);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_TIME, "%lld", board->microticks);

  rngState = getRngStateString (board->rng);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_SEED, "%s", rngState);
  SafeFree(rngState);

  if (!reverseCompile)
    writeTypesXml (board, writer);

  for (x = 0; x < board->size; ++x)
    for (y = 0; y < board->size; ++y)
      writeCellXml (board, writer, x, y, reverseCompile);

  xmlTextWriterFullEndElement (writer);  /* end board */
}

void writeCellXml (Board* board, xmlTextWriterPtr writer, int x, int y, int reverseCompile) {
  int i;
  State s;

  i = boardIndex (board->size, x, y);
  s = board->cell[i];
  if (s) {
    xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_INIT);  /* begin init */
    xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_X, "%d", x);
    xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_Y, "%d", y);
    if (reverseCompile)
      writeGVarsXml (board, s, writer);
    else
      xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_HEXSTATE, "%llx", s);
    xmlTextWriterFullEndElement (writer);  /* end init */
  }
}

void writeTypesXml (Board* board, xmlTextWriterPtr writer) {
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

static const char* varAttrName = XMLZOO_VAR;
void writeGVarsXml (Board* board, State s, xmlTextWriterPtr writer) {
  State t, varVal;
  ListNode *node;
  VarsDescriptor *vd;
  xmlNode *attr;
  t = StateType(s);
  if (s != 0 && board->byType[t] && board->byType[t]->name) {
    xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_GVARS);  /* begin gvars */
    xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_TYPE, "%s", board->byType[t]->name);
    for (node = board->byType[t]->vars->head; node; node = node->next) {
      vd = (VarsDescriptor*) node->value;
      varVal = (s >> vd->offset) & ((1 << vd->width) - 1);
      if (varVal != 0) {
	attr = newXmlNode (XML_ATTRIBUTE_NODE, varAttrName, varAttrName + strlen(varAttrName), vd->name, vd->name + strlen(vd->name));
	xmlTextWriterStartElementWithAttrs (writer, (xmlChar*) XMLZOO_VAL, attr); /* begin val */
	xmlTextWriterWriteFormatCDATA (writer, "%lld", varVal);
	xmlTextWriterEndElement (writer);  /* end val */
	deleteXmlTree (attr);
      }
    }
    xmlTextWriterFullEndElement (writer);  /* end gvars */
  }
}


int testNodeHasState (xmlNode* node) {
  return nextNodeWithState(node->children) ? 1 : 0;
}

xmlNode* nextNodeWithState (xmlNode* node) {
  for (; node; node = node->next)
    if (MATCHES(node,DECSTATE) || MATCHES(node,HEXSTATE) || MATCHES(node,DECTYPE) || MATCHES(node,HEXTYPE) || MATCHES(node,GSTATE) || MATCHES(node,GVARS))
      break;
  return node;
}

State getGTypeOrStateFromNode (xmlNode* node, ProtoTable* protoTable) {
  xmlNode *child;
  Proto *proto;
  State t;
  t = 0;
  if ( (child = CHILD(node,DECSTATE)) )  /* assignment intentional */
    t = NODEINTVAL (child);
  else if ( (child = CHILD(node,HEXSTATE)) )  /* assignment intentional */
    t = NODEHEXVAL (child);
  else if ( (child = CHILD(node,GTYPE)) ) {  /* assignment intentional */
    proto = protoTableGetProto (protoTable, (const char*) getNodeContent (child));
    if (proto)
      t = proto->type;
    else
      Warn ("Couldn't find Particle %s", (const char*) getNodeContent(child));
  } else
    Warn ("Couldn't find <%s>, <%s> or <%s> in <%s>", XMLPREFIX(DECSTATE), XMLPREFIX(HEXSTATE), XMLPREFIX(GTYPE), (const char*) node->name);
  return t;
}

State getStateFromNode (xmlNode* node, ProtoTable* protoTable, State defaultState) {
  return getStateFromChild (nextNodeWithState (node->children), protoTable, defaultState);
}

State getStateFromChild (xmlNode* child, ProtoTable* protoTable, State defaultState) {
  State s;
  s = defaultState;
  if ( MATCHES(child,DECSTATE) )
    s = NODEINTVAL (child);
  else if ( MATCHES(child,HEXSTATE) )
    s = NODEHEXVAL (child);
  else if ( MATCHES(child,DECTYPE) )
    s = ((State) NODEINTVAL (child)) << TypeShift;
  else if ( MATCHES(child,HEXTYPE) )
    s = ((State) NODEHEXVAL (child)) << TypeShift;
  else if ( MATCHES(child,GSTATE) || MATCHES(child,GVARS) )
    s = getGStateOrGVarsFromChild (child, protoTable, defaultState);
  else
    Warn ("Found element <%s> where I was expecting <%s>, <%s>, <%s>, <%s>, <%s> or <%s>", child->name, XMLPREFIX(DECSTATE), XMLPREFIX(HEXSTATE), XMLPREFIX(DECTYPE), XMLPREFIX(HEXTYPE), XMLPREFIX(GSTATE), XMLPREFIX(GVARS));
  return s;
}

State getIncOrStateFromNode (xmlNode* node, ProtoTable* protoTable, unsigned char* registerFlag) {
  xmlNode *child;
  State s;
  s = 0;
  *registerFlag = 0;
  if ( (child = CHILD(node,DECINC)) )  /* assignment intentional */
    s = NODEINTVAL (child);
  else if ( (child = CHILD(node,HEXINC)) )  /* assignment intentional */
    s = NODEHEXVAL (child);
  else if ( (child = CHILD(node,GSTATE)) || (child = CHILD(node,GVARS)) )  /* assignments intentional */
    s = getGStateOrGVarsFromChild (child, protoTable, 0);
  else if ( (child = CHILD(node,REGINC)) ) {  /* assignment intentional */
    s = NODEINTVAL (child);
    *registerFlag = 1;
  }
  return s;
}

State getGStateOrGVarsFromChild (xmlNode* child, ProtoTable* protoTable, State defaultState) {
  xmlNode *field;
  const char *type, *varName;
  Proto *proto;
  VarsDescriptor *vd;
  State s;
  s = defaultState;
  if ( MATCHES(child,GSTATE) ) {
    type = (const char*) getNodeContent (child);
    proto = protoTableGetProto (protoTable, type);
    if (proto)
      s = ((State) proto->type) << TypeShift;
    else
      Warn ("<%s> refers to type %s, but no such type was found", XMLPREFIX(GSTATE), type);
  } else if ( MATCHES(child,GVARS) ) {
    proto = NULL;
    for (field = child->children; field != NULL; field = field->next)
      if (MATCHES(field,TYPE)) {
	if (proto)
	  Warn ("Multiple type fields in <%s>", XMLPREFIX(GVARS));
	type = (const char*) getNodeContent (field);
	proto = protoTableGetProto (protoTable, type);
	if (proto == NULL)
	  Warn ("<%s> element refers to type %s, but no such type was found", XMLPREFIX(GVARS), type);
      }
    if (proto) {
      s = ((State) proto->type) << TypeShift;
      for (field = child->children; field != NULL; field = field->next)
	if (MATCHES(field,VAL)) {
	  varName = ATTR (field, VAR);
	  if (varName) {
	    vd = protoGetVarsDescriptor (proto, varName);
	    if (vd)
	      s = s | ((NODEINTVAL(field) & ((1 << vd->width) - 1)) << vd->offset);
	    else
	      Warn ("In <%s> for type %s: encountered <%s %s=\"%s\">, but no such var could be found", XMLPREFIX(GVARS), proto->name, XMLPREFIX(VAL), XMLPREFIX(VAR), varName);
	  } else
	    Warn ("<%s> element without %s attribute in <%s> for type %s", XMLPREFIX(VAL), XMLPREFIX(VAR), XMLPREFIX(GVARS), type);
	}
    } else
      Warn ("Couldn't find <%s> in <%s>", XMLPREFIX(TYPE), XMLPREFIX(GVARS));
  } else
    Warn ("Found element <%s> where I was expecting <%s> or <%s>", child->name, XMLPREFIX(GTYPE), XMLPREFIX(GSTATE));
  return s;
}

State getMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask) {
  return getTaggedMaskFromNode (node, protoTable, defaultMask, XMLPREFIX(MASK), XMLPREFIX(VMASK), XMLPREFIX(TMASK));
}

State getSrcMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask) {
  return getTaggedMaskFromNode (node, protoTable, defaultMask, XMLPREFIX(SRCMASK), XMLPREFIX(VSRCMASK), XMLPREFIX(TSRCMASK));
}

State getDestMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask) {
  return getTaggedMaskFromNode (node, protoTable, defaultMask, XMLPREFIX(DESTMASK), XMLPREFIX(VDESTMASK), XMLPREFIX(TDESTMASK));
}

State getTaggedMaskFromNode (xmlNode* node, ProtoTable* protoTable, State defaultMask, const char* maskTag, const char* vmaskTag, const char* tmaskTag) {
  xmlNode *child;
  const char *typeName, *varName;
  Proto *proto;
  VarsDescriptor *vd;
  State m;
  m = defaultMask;
  if ( (child = getNodeByName(node->children,maskTag)) )  /* assignment intentional */
    m = NODEHEXVAL (child);
  else if ( getNodeByName(node->children,tmaskTag) )
    m = TypeMask;
  else if ( (child = getNodeByName(node->children,vmaskTag)) ) {  /* assignment intentional */
    typeName = (const char*) CHILDSTRING(child,TYPE);
    if (typeName) {
      proto = protoTableGetProto (protoTable, typeName);
      if (proto) {
	varName = (const char*) CHILDSTRING(child,VAR);
	vd = protoGetVarsDescriptor (proto, varName);
	if (vd)
	  m = ((1 << vd->width) - 1) << vd->offset;
	else
	  Warn ("In <%s> for type %s: encountered <%s %s=\"%s\">, but no such var could be found", vmaskTag, proto->name, XMLPREFIX(VAL), XMLPREFIX(VAR), varName);
      }
    } else
      m = VarsMask;
  }
  return m;
}

unsigned char getLShiftFromNode (xmlNode* node, ProtoTable* protoTable) {
  return getTaggedShiftFromNode (node, protoTable, XMLPREFIX(LSHIFT), XMLPREFIX(VLSHIFT));
}

unsigned char getRShiftFromNode (xmlNode* node, ProtoTable* protoTable) {
  return getTaggedShiftFromNode (node, protoTable, XMLPREFIX(RSHIFT), XMLPREFIX(VRSHIFT));
}

unsigned char getTaggedShiftFromNode (xmlNode* node, ProtoTable* protoTable, const char* shiftTag, const char* vShiftTag) {
  xmlNode *child;
  const char *varName;
  Proto *proto;
  VarsDescriptor *vd;
  unsigned char s;
  s = 0;
  if ( (child = getNodeByName(node->children,shiftTag)) )  /* assignment intentional */
    s = NODEINTVAL (child);
  else if ( (child = getNodeByName(node->children,vShiftTag)) ) {  /* assignment intentional */
    proto = protoTableGetProto (protoTable, (const char*) CHILDSTRING(child,TYPE));
    if (proto) {
      varName = (const char*) CHILDSTRING(child,VAR);
      vd = protoGetVarsDescriptor (proto, varName);
      if (vd)
	s = vd->offset;
      else
	Warn ("In <%s> for type %s: encountered <%s %s=\"%s\">, but no such var could be found", vShiftTag, proto->name, XMLPREFIX(VAL), XMLPREFIX(VAR), varName);
    } else
      Warn ("<%s> lacks type", vShiftTag);
  }
  return s;
}
