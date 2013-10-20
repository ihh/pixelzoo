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

  /* create subrules */
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,SUBRULE)) {
      subRuleName = (const char*) CHILDSTRING(node,NAME);
      Assert (StringMapFind (board->subRule, subRuleName) == 0, "Duplicate subrule name");
      ruleNode = CHILD(node,RULE);
      rule = newRuleFromXmlNode (game, ruleNode);
      (void) StringMapInsert (board->subRule, (char*) subRuleName, (void*) rule);
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
      state = OPTCHILDINT(node,DECSTATE,OPTCHILDHEX(node,HEXSTATE,OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE)) << TypeShift));
      writeBoardState (board, x, y, state);
    }

  queueNode = CHILD (boardNode, QUEUE);
  if (queueNode)
    board->moveQueue = newMoveListFromXmlNode (queueNode);

  /* free ProtoTable and return */
  deleteProtoTable (protoTable);

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
  xmlNode *curNode, *syncNode;
  Proto *proto;

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

  proto = protoTableGetProto (protoTable, p->name);
  p->type = proto->type;
  protoCopyVarsDescriptorsToList (proto, p->vars);

  nColorRules = 0;
  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,COLRULE)) {
      Assert (nColorRules < NumColorRules, "newParticleFromXmlNode: too many color rules");
      initColorRuleFromXmlNode (&p->colorRule[nColorRules++], curNode);
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


int testNodeHasType (xmlNode* node) {
  return CHILD(node,DECTYPE) || CHILD(node,HEXTYPE) || CHILD(node,GTYPE);
}

int testNodeHasState (xmlNode* node) {
  return CHILD(node,DECSTATE) || CHILD(node,HEXSTATE) || CHILD(node,GSTATE) || CHILD(node,GVARS);
}

int testNodeHasStateOrType (xmlNode* node) {
  return CHILD(node,DECSTATE) || CHILD(node,HEXSTATE) || CHILD(node,GSTATE) || CHILD(node,GVARS) || CHILD(node,DECTYPE) || CHILD(node,HEXTYPE) || CHILD(node,GTYPE);
}

Type getTypeFromNode (xmlNode* node, ProtoTable* protoTable, Type defaultType) {
  xmlNode *child;
  Proto *proto;
  Type t;
  t = defaultType;
  if ( (child = CHILD(node,DECTYPE)) )  /* assignment intentional */
    t = decToSignedLongLong ((const char*) getNodeContent (child));
  else if ( (child = CHILD(node,HEXTYPE)) )  /* assignment intentional */
    t = hexToUnsignedLongLong ((const char*) getNodeContent (child));
  else if ( (child = CHILD(node,GTYPE)) ) {  /* assignment intentional */
    proto = protoTableGetProto (protoTable, (const char*) getNodeContent (child));
    if (proto)
      t = proto->type;
    else
      Warn ("Couldn't find Particle %s", (const char*) getNodeContent(child));
  } else
    Warn ("Couldn't find Type specifier");
  return t;
}

State getStateFromNode (xmlNode* node, ProtoTable* protoTable, State defaultState) {
  xmlNode *child, *field;
  Proto *proto;
  VarsDescriptor *vd;
  State s;
  s = defaultState;
  if ( (child = CHILD(node,DECSTATE)) )  /* assignment intentional */
    s = decToSignedLongLong ((const char*) getNodeContent (child));
  else if ( (child = CHILD(node,HEXSTATE)) )  /* assignment intentional */
    s = hexToUnsignedLongLong ((const char*) getNodeContent (child));
  else if ( (child = CHILD(node,GSTATE)) ) {  /* assignment intentional */
    proto = protoTableGetProto (protoTable, (const char*) getNodeContent (child));
    if (proto)
      s = ((State) proto->type) << TypeShift;
    else
      Warn ("Couldn't find Particle %s", (const char*) getNodeContent(child));
  } else if ( (child = CHILD(node,GVARS)) ) {  /* assignment intentional */
    for (field = child->children; field != NULL; field = field->next)
      if (MATCHES(field,TYPE))
	proto = protoTableGetProto (protoTable, (const char*) getNodeContent (child));
    if (proto) {
      s = ((State) proto->type) << TypeShift;
      for (field = child->children; field != NULL; field = field->next)
	if (MATCHES(field,VAL)) {
	  vd = protoGetVarsDescriptor (proto, ATTR (field, VAR));
	  if (vd)
	    s = s | ((decToSignedLongLong ((const char*) getNodeContent (field)) & ((1 << vd->width) - 1)) << vd->offset);
	  else
	    Warn ("Couldn't find Var %s in Particle %s", ATTR(field,VAR), (const char*) getNodeContent(child));
	}
    } else
      Warn ("Couldn't find Particle %s", (const char*) getNodeContent(child));
  } else
    Warn ("Couldn't find State specifier");
  return s;
}

State getVarMaskFromNode (xmlNode* node, ProtoTable* protoTable) {
  /* WRITE ME */
  return 0;
}

unsigned char getLShiftFromNode (xmlNode* node, ProtoTable* protoTable) {
  /* WRITE ME */
  return 0;
}

unsigned char getRShiftFromNode (xmlNode* node, ProtoTable* protoTable) {
  /* WRITE ME */
  return 0;
}
