#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xmlparser.h"
#include "xmlboard.h"
#include "xmlutil.h"
#include "xmlmove.h"
#include "vars.h"

/* prototypes for private builder methods */
Particle* newParticleFromXmlNode (Board *board, xmlNode* node, ProtoTable *protoTable);
void parseVarsDescriptors (Proto *proto, xmlNode* particleNode);
void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node, Proto *proto);
ParticleRule* newRuleFromXmlNode (Board *board, xmlNode* node, ProtoTable *protoTable, StringMap **localSubRule);
ParticleRule* newRuleFromXmlParentNode (Board *board, xmlNode* parent, ProtoTable *protoTable, StringMap **localSubRule);
ParticleRule* newRuleFromXmlGrandparentNode (Board *board, xmlNode* grandparent, ProtoTable *protoTable, StringMap **localSubRule);

void initLookupRuleFromXmlNode (LookupRuleParams* lookup, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule);
void initCompareRuleFromXmlNode (CompareRuleParams* lookup, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule);
void initModifyRuleFromXmlNode (ModifyRuleParams* op, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule);
void initDeliverRuleFromXmlNode (DeliverRuleParams* deliver, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule);
void initGotoRuleFromXmlNode (ParticleRule** gotoLabelRef, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule);
void initLoadRuleFromXmlNode (LoadRuleParams* load, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule);
void initFunctionRuleFromXmlNode (FunctionRuleParams* function, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule);

void initLocalOffsetFromXmlNode (LocalOffset* loc, int xDefault, int yDefault, int zDefault, xmlNode* node);

/* method defs */
Board* newBoardFromXmlDocument (xmlDoc *doc) {
  return newBoardFromXmlRoot (xmlDocGetRootElement (doc));
}

Board* newBoardFromXmlRoot (xmlNode *root) {
  Board *board;
  xmlNode *boardNode, *queueNode, *grammarNode, *schemeNode, *seedNode, *contestNode, *node;
  int x, y, z;
  State state;
  ProtoTable *protoTable;
  Proto *proto;
  VarsDescriptor *vd;
  const char *subRuleName;

  boardNode = CHILD(root,BOARD);
  Assert (boardNode != NULL, "XML board tag not found");

  board = newBoard (CHILDINT(boardNode,SIZE), OPTCHILDINT(boardNode,DEPTH,1));

  seedNode = CHILD(boardNode,SEED);
  if (seedNode)
    rngSetStateString (board->rng, (char*) getNodeContent (seedNode));

  board->microticks = OPTCHILDINT(boardNode,TIME,0);
  board->updateCount = OPTCHILDINT(boardNode,UPDATE,0);

  /* parse grammar */
  grammarNode = CHILD(boardNode,GRAMMAR);

  /* create ProtoTable */
  protoTable = newProtoTable();
  board->protoTable = protoTable;

  /* evaluate top-level Scheme expressions */
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,SCHEMEDEF))
      (void) protoTableEval (protoTable, (const char*) getNodeContent (node));

  /* expand any Particle-level Scheme blocks */
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      if ( (schemeNode = CHILD (node, SCHEME)) )  /* assignment intentional */
	node = protoTableExpandSchemeNode (protoTable, schemeNode, node, grammarNode);

  /* populate ProtoTable in two passes (first Particles that have Types, then those that don't) */
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      if (CHILD(node,DECTYPE) || CHILD(node,HEXTYPE)) {
	proto = protoTableAddTypedParticle (protoTable, (char*) CHILDSTRING(node,NAME), OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE)));
	parseVarsDescriptors (proto, node);
      }

  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      if (!CHILD(node,DECTYPE) && !CHILD(node,HEXTYPE)) {
	proto = protoTableAddParticle (protoTable, (char*) CHILDSTRING(node,NAME));
	parseVarsDescriptors (proto, node);
      }

  /* get contest info */
  if ( (contestNode = CHILD(boardNode,CONTEST)) ) {  /* assignment intentional */
    proto = protoTableGetProto (protoTable, (const char*) CHILDSTRING(contestNode,TYPE));
    vd = protoGetVarsDescriptor (proto, (const char*) CHILDSTRING(contestNode,VAR));
    board->winType = proto->type;
    board->winVarOffset = vd->offset;
    board->winVarWidth = vd->width;
    board->incumbentWinVar = OPTCHILDINT(contestNode,INCUMBENT,-1);
    board->challengerWinVar = OPTCHILDINT(contestNode,CHALLENGER,board->incumbentWinVar);
    protoTableSetContestInfo (protoTable, proto->name, vd->name, board->incumbentWinVar, board->challengerWinVar);
  }

  /* create subrules */
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,SUBRULE)) {
      subRuleName = (const char*) ATTR(node,NAME);
      Assert (StringMapFind (board->subRule, subRuleName) == 0, "Duplicate global subrule name %s", subRuleName);
      (void) newRuleFromXmlParentNode (board, node, protoTable, &board->subRule);
    }

  /* create particles */
  for (node = grammarNode->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      addParticleToBoard (newParticleFromXmlNode(board,node,protoTable), board);

  /* initialize board */
  for (node = boardNode->children; node; node = node->next)
    if (MATCHES(node,INIT)) {
      x = CHILDINT(node,X);
      y = CHILDINT(node,Y);
      z = OPTCHILDINT(node,Z,0);
      state = getStateFromNode (node, protoTable, 0);
      writeBoardState (board, x, y, z, state);
      if (CHILD(node,META))
	board->meta[boardIndex(board->size,x,y,z)] = (char*) StringNew((const char*)CHILDSTRING(node,META));
    }

  queueNode = CHILD (boardNode, QUEUE);
  if (queueNode)
    board->moveQueue = newMoveListFromXmlNode (queueNode, protoTable);

  return board;
}

Board* newBoardFromXmlString (const char* string) {
  xmlDoc* doc;
  Board* board = NULL;
  doc = xmlTreeFromString (string);
  if (doc)
    board = newBoardFromXmlDocument (doc);
  return board;
}


Particle* newParticleFromXmlNode (Board *board, xmlNode* node, ProtoTable *protoTable) {
  Particle* p;
  Message message;
  int nColorRules, readOnlyIndex;
  xmlNode *curNode, *syncNode, *childNode, *schemeNode;
  Proto *proto;

  p = newParticle ((const char*) CHILDSTRING(node,NAME));

  if ((syncNode = CHILD(node,SYNC))) {
    p->synchronous = 1;
    p->syncPeriod = OPTCHILDINT(syncNode,PERIOD,0);  /* leaving this as zero means that it will be auto-set at 1/(total rate) by addParticleToBoard */
    p->syncPhase = OPTCHILDINT(syncNode,PHASE,0);
  }

  protoTableSetSelfType (protoTable, p->name);

  /* create subrules local to this Particle */
  for (childNode = node->children; childNode; childNode = childNode->next)
    if (MATCHES(childNode,SUBRULE))
      (void) newRuleFromXmlParentNode (board, childNode, protoTable, &p->subRule);

  /* create main update rule */
  p->rule = newRuleFromXmlParentNode (board, CHILD(node,RULE), protoTable, &p->subRule);

  /* message dispatch table */
  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,DISPATCH)) {
      message = getMessageFromNode (curNode, protoTable);
      addParticleMessageHandler (p, message, newRuleFromXmlGrandparentNode (board, curNode, protoTable, &p->subRule));
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
      if ( (schemeNode = CHILD (curNode, SCHEME)) )  /* assignment intentional */
	curNode = protoTableExpandSchemeNode (protoTable, schemeNode, curNode, node);
      initColorRuleFromXmlNode (&p->colorRule[nColorRules++], curNode, proto);
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


void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node, Proto *proto) {
  colorRule->rightShift = getColShiftFromNode (node, proto);
  colorRule->mask = getColMaskFromNode (node, proto);
  colorRule->multiplier = OPTCHILDINT(node,DECMUL,OPTCHILDHEX(node,HEXMUL,1));
  colorRule->offset = OPTCHILDINT(node,DECINC,OPTCHILDHEX(node,HEXINC,0));
}

ParticleRule* newRuleFromXmlGrandparentNode (Board *board, xmlNode* parent, ProtoTable *protoTable, StringMap **localSubRule) {
  ParticleRule* rule;
  rule = NULL;
  if (parent)
    rule = newRuleFromXmlParentNode (board, CHILD (parent, RULE), protoTable, localSubRule);
  return rule;
}

ParticleRule* newRuleFromXmlParentNode (Board *board, xmlNode *ruleParentNode, ProtoTable *protoTable, StringMap **localSubRule) {
  xmlNode *childNode, *ruleNode;
  ParticleRule *rule, *subRule;
  const char *subRuleName, *dump;

  rule = NULL;
  if (ruleParentNode != NULL) {

    ruleNode = NULL;
    for (childNode = ruleParentNode->children; childNode; childNode = childNode->next)
      if (MATCHES (childNode, SUBRULE))
	(void) newRuleFromXmlParentNode (board, childNode, protoTable, localSubRule);

      else if (MATCHES(childNode,SWITCH) || MATCHES(childNode,COMPARE) || MATCHES(childNode,MODIFY) || MATCHES(childNode,GOTO) || MATCHES(childNode,DELIVER) || MATCHES(childNode,RANDOM) || MATCHES(childNode,LOAD) || MATCHES(childNode,DYNAMIC) || MATCHES(childNode,SCHEME) || MATCHES(childNode,NOP)) {
	if (ruleNode) {
	  dump = xmlTreeToString (ruleParentNode);
	  Warn ("Ignoring <%s> child of <%s> node, in favor of older sibling <%s>\nIn %s\n", (const char*) childNode->name, (const char*) ruleParentNode->name, (const char*) ruleNode->name, dump);
	  SafeFree ((void*) dump);
	} else
	  ruleNode = childNode;
      }

    Assert (ruleNode != NULL, "Missing rule node");

    rule = newRuleFromXmlNode (board, ruleNode, protoTable, localSubRule);

    subRuleName = ATTR(ruleParentNode,NAME);
    if (subRuleName) {
      subRule = rule;
      subRule->label = StringNew (subRuleName);
      defineSubRule (localSubRule, subRuleName, subRule, board->subRule);
      rule = newGotoRuleTo(subRule);
    }
  }

  return rule;
}

ParticleRule* newRuleFromXmlNode (Board *board, xmlNode *ruleNode, ProtoTable *protoTable, StringMap **localSubRule) {
  ParticleRule *rule, **gotoLabelRef;
  LookupRuleParams *lookup;
  CompareRuleParams *compare;
  ModifyRuleParams *modify;
  DeliverRuleParams *deliver;
  RandomRuleParams *random;
  LoadRuleParams *load;
  FunctionRuleParams *function;
  const char *evalResult;
  xmlNode *evalNode;

  rule = NULL;

  if (MATCHES (ruleNode, SWITCH)) {
    rule = newLookupRule();
    lookup = &rule->param.lookup;
    initLookupRuleFromXmlNode (lookup, ruleNode, board, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, COMPARE)) {
    rule = newCompareRule();
    compare = &rule->param.compare;
    initCompareRuleFromXmlNode (compare, ruleNode, board, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, MODIFY)) {
    rule = newModifyRule();
    modify = &rule->param.modify;
    initModifyRuleFromXmlNode (modify, ruleNode, board, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, GOTO)) {
    rule = newGotoRule();
    gotoLabelRef = &rule->param.gotoLabel;
    initGotoRuleFromXmlNode (gotoLabelRef, ruleNode, board, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, DELIVER)) {
    rule = newDeliverRule();
    deliver = &rule->param.deliver;
    initDeliverRuleFromXmlNode (deliver, ruleNode, board, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, RANDOM)) {
    rule = newRandomRule();
    random = &rule->param.random;
    random->prob = FloatToIntMillionths (OPTCHILDFLOAT (ruleNode, PROB, 0.5));
    random->passRule = newRuleFromXmlGrandparentNode (board, CHILD (ruleNode, PASS), protoTable, localSubRule);
    random->failRule = newRuleFromXmlGrandparentNode (board, CHILD (ruleNode, FAIL), protoTable, localSubRule);

  } else if (MATCHES (ruleNode, LOAD)) {
    rule = newLoadRule();
    load = &rule->param.load;
    initLoadRuleFromXmlNode (load, ruleNode, board, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, DYNAMIC)) {
    rule = newFunctionRule();
    function = &rule->param.function;
    initFunctionRuleFromXmlNode (function, ruleNode, board, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, SCHEME)) {
    evalResult = protoTableEvalSxml (protoTable, (const char*) getNodeContent(ruleNode));
    evalNode = xmlTreeFromString (evalResult);

    rule = evalNode ? newRuleFromXmlParentNode (board, evalNode, protoTable, localSubRule) : NULL;

    deleteXmlTree (evalNode);
    StringDelete ((void*) evalResult);

  } else if (MATCHES (ruleNode, RULE)) {
    rule = newRuleFromXmlParentNode (board, ruleNode, protoTable, localSubRule);

  } else if (MATCHES (ruleNode, NOP)) {
    /* nop rule is just NULL */

  } else {
    Abort ("Unknown rule type <%s>", ruleNode->name);
  }

  return rule;
}

void initLocalOffsetFromXmlNode (LocalOffset* loc, int xDefault, int yDefault, int zDefault, xmlNode* node) {
  const char *mode;
  if (node) {
    if ((mode = ATTR(node,MODE)) && ATTRMATCHES(mode,INDIRECT)) {
      loc->x = OPTCHILDINT(node,X,NumberOfRegisters);
      loc->y = OPTCHILDINT(node,Y,NumberOfRegisters);
      loc->z = OPTCHILDINT(node,Z,NumberOfRegisters);
      loc->xyzAreRegisters = 1;
      Assert (loc->x >= 0 && loc->x < NumberOfRegisters + 1, "In <%s %s=\"%s\">: x-register is %d, should range from 0 to %d", (const char*) node->name, XMLPREFIX(MODE), XMLPREFIX(INDIRECT), loc->x, NumberOfRegisters);
      Assert (loc->y >= 0 && loc->y < NumberOfRegisters + 1, "In <%s %s=\"%s\">: y-register is %d, should range from 0 to %d", (const char*) node->name, XMLPREFIX(MODE), XMLPREFIX(INDIRECT), loc->y, NumberOfRegisters);
      Assert (loc->z >= 0 && loc->z < NumberOfRegisters + 1, "In <%s %s=\"%s\">: z-register is %d, should range from 0 to %d", (const char*) node->name, XMLPREFIX(MODE), XMLPREFIX(INDIRECT), loc->z, NumberOfRegisters);
    } else {
      loc->x = OPTCHILDINT(node,X,xDefault);
      loc->y = OPTCHILDINT(node,Y,yDefault);
      loc->z = OPTCHILDINT(node,Z,zDefault);
      loc->xyzAreRegisters = 0;
    }
  } else {
    loc->x = xDefault;
    loc->y = yDefault;
    loc->z = zDefault;
    loc->xyzAreRegisters = 0;
  }
}

void initLookupRuleFromXmlNode (LookupRuleParams* lookup, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule) {
  xmlNode *curNode;
  State state;
  ParticleRule *rule;

  initLocalOffsetFromXmlNode (&lookup->loc, 0, 0, 0, CHILD(node,POS));

  lookup->mask = getMaskFromNode (node, protoTable, StateMask);
  lookup->shift = getRShiftFromNode (node, protoTable);

  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,CASE)) {
      state = getGTypeOrStateFromNode (curNode, protoTable);
      rule = newRuleFromXmlGrandparentNode (board, curNode, protoTable, localSubRule);
      Assert (StateMapFind (lookup->matchRule, state) == NULL, "Duplicate key in lookup, or more than one key unassigned/zero");
      (void) StateMapInsert (lookup->matchRule, state, rule);
    }

  lookup->lowRule = newRuleFromXmlGrandparentNode (board, CHILD (node, LOW), protoTable, localSubRule);
  lookup->highRule = newRuleFromXmlGrandparentNode (board, CHILD (node, HIGH), protoTable, localSubRule);
  lookup->defaultRule = newRuleFromXmlGrandparentNode (board, CHILD (node, DEFAULT), protoTable, localSubRule);
}

void initCompareRuleFromXmlNode (CompareRuleParams* compare, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule) {
  xmlNode *curNode;
  ParticleRule *rule;

  initLocalOffsetFromXmlNode (&compare->loc, 0, 0, 0, CHILD(node,POS));

  compare->mask = getMaskFromNode (node, protoTable, StateMask);
  compare->shift = getRShiftFromNode (node, protoTable);

  compare->registerIndex = CHILDINT(node,REGINDEX);

  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,LT) || MATCHES(curNode,LEQ) || MATCHES(curNode,EQ) || MATCHES(curNode,NEQ) || MATCHES(curNode,GT) || MATCHES(curNode,GEQ)) {
      rule = newRuleFromXmlGrandparentNode (board, curNode, protoTable, localSubRule);
      Assert (rule != NULL, "No rule found in <%s> clause", curNode->name);
      if (MATCHES(curNode,LT)) {
	Assert (compare->ltRule == NULL, "Redefinition of less-than rule");
	compare->ltRule = rule;
      } else if (MATCHES(curNode,EQ)) {
	Assert (compare->eqRule == NULL, "Redefinition of equal-to rule");
	compare->eqRule = rule;
      } else if (MATCHES(curNode,GT)) {
	Assert (compare->gtRule == NULL, "Redefinition of greater-than rule");
	compare->gtRule = rule;
      } else if (MATCHES(curNode,LEQ)) {
	Assert (compare->ltRule == NULL, "Redefinition of less-than rule");
	Assert (compare->eqRule == NULL, "Redefinition of equal-to rule");
	compare->ltRule = rule;
	compare->eqRule = newGotoRuleTo(rule);
      } else if (MATCHES(curNode,GEQ)) {
	Assert (compare->gtRule == NULL, "Redefinition of greater-than rule");
	Assert (compare->eqRule == NULL, "Redefinition of equal-to rule");
	compare->gtRule = rule;
	compare->eqRule = newGotoRuleTo(rule);
      } else if (MATCHES(curNode,NEQ)) {
	Assert (compare->gtRule == NULL, "Redefinition of greater-than rule");
	Assert (compare->ltRule == NULL, "Redefinition of less-than rule");
	compare->gtRule = rule;
	compare->ltRule = newGotoRuleTo(rule);
      }
    }
}

void initModifyRuleFromXmlNode (ModifyRuleParams* op, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule) {
  const char* modifyTypeAttr;
  op->rightShift = getRShiftFromNode (node, protoTable);
  op->offset = getIncOrStateFromNode (node, protoTable, &op->offsetIsRegister);
  op->leftShift = getLShiftFromNode (node, protoTable);

  op->srcMask = getSrcMaskFromNode (node, protoTable, StateMask);
  op->destMask = getDestMaskFromNode (node, protoTable, StateMask);

  initLocalOffsetFromXmlNode (&op->src, 0, 0, 0, CHILD(node,SRC));
  initLocalOffsetFromXmlNode (&op->dest, op->src.x, op->src.y, op->src.z, CHILD(node,DEST));

  modifyTypeAttr = (const char*) ATTR(node,ACTION);
  if (modifyTypeAttr) {
    if (strcmp (modifyTypeAttr, XMLZOO_ACTION_CONSERVE) == 0)
      op->modifyType = ConserveModify;
    else if (strcmp (modifyTypeAttr, XMLZOO_ACTION_KILL) == 0)
      op->modifyType = KillModify;
    else if (strcmp (modifyTypeAttr, XMLZOO_ACTION_EAT) == 0)
      op->modifyType = EatModify;
    else
      Abort ("Unknown action type: %s", modifyTypeAttr);
  } else
    op->modifyType = ((op->destMask & TypeMask) == TypeMask)
      ? (((op->srcMask & TypeMask) == TypeMask)
	 ? EatModify : KillModify) : ConserveModify;

  op->nextRule = newRuleFromXmlGrandparentNode (board, CHILD (node, NEXT), protoTable, localSubRule);
}

void initDeliverRuleFromXmlNode (DeliverRuleParams* deliver, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule) {
  initLocalOffsetFromXmlNode (&deliver->recipient, 0, 0, 0, CHILD(node,POS));
  deliver->message = getMessageFromNode (node, protoTable);
}

void initGotoRuleFromXmlNode (ParticleRule** gotoLabelRef, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule) {
  StringMapNode *subRuleNode;
  const char* label;
  label = (const char*) getNodeContentOrComplain (node, NULL);
  subRuleNode = *localSubRule ? StringMapFind (*localSubRule, label) : (StringMapNode*) NULL;
  if (!subRuleNode)
    subRuleNode = StringMapFind (board->subRule, label);
  *gotoLabelRef = subRuleNode ? ((ParticleRule*) (subRuleNode->value)) : ((ParticleRule*) NULL);
  if (*gotoLabelRef == NULL) {
    fprintf (stderr, "Unresolved goto label: %s\n", label);
    Abort ("Couldn't find goto label");
  }
}

void initLoadRuleFromXmlNode (LoadRuleParams* load, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule) {
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

  load->nextRule = newRuleFromXmlGrandparentNode (board, CHILD (node, NEXT), protoTable, localSubRule);
}

void initFunctionRuleFromXmlNode (FunctionRuleParams *function, xmlNode* node, Board *board, ProtoTable *protoTable, StringMap **localSubRule) {
  xmlNode *next, *pass, *fail;
  function->schemeExpr = StringNew ((const char*) CHILDSTRING(node,FUNCTION));
  if ((next = CHILD(node,NEXT))) {
    function->passRule = newRuleFromXmlGrandparentNode (board, next, protoTable, localSubRule);
    function->failRule = newGotoRuleTo(function->passRule);
  } else {
    if ((pass = CHILD(node,PASS)))
      function->passRule = newRuleFromXmlGrandparentNode (board, pass, protoTable, localSubRule);
    if ((fail = CHILD(node,FAIL)))
      function->failRule = newRuleFromXmlGrandparentNode (board, fail, protoTable, localSubRule);
  }
}

void writeBoardXml (Board* board, xmlTextWriterPtr writer, int reverseCompile) {
  int x, y, z, winner;
  char *rngState;

  Assert (board->rngReleased, "writeBoard: random number generator not released. You need to call boardReleaseRandomNumbers");

  xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_BOARD);  /* begin board */
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_SIZE, "%d", board->size);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_DEPTH, "%d", board->depth);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_TIME, "%lld", board->microticks);

  winner = boardWinner(board);
  if (winner >= 0)
    xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_WINNER, "%d", winner);

  rngState = getRngStateString (board->rng);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_SEED, "%s", rngState);
  SafeFree(rngState);

  if (!reverseCompile)
    writeTypesXml (board, writer);

  for (z = 0; z < board->depth; ++z)
    for (x = 0; x < board->size; ++x)
      for (y = 0; y < board->size; ++y)
	writeCellXml (board, writer, x, y, z, reverseCompile);

  xmlTextWriterFullEndElement (writer);  /* end board */
}

void writeCellXml (Board* board, xmlTextWriterPtr writer, int x, int y, int z, int reverseCompile) {
  int i;
  State s;

  i = boardIndex (board->size, x, y, z);
  s = board->cell[i];
  if (s) {
    xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_INIT);  /* begin init */
    xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_X, "%d", x);
    xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_Y, "%d", y);
    if (z)
      xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_Z, "%d", z);
    if (reverseCompile)
      writeGVarsXml (board, s, writer);
    else
      xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_HEXSTATE, "%llx", s);

    if (board->meta[i])
      xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_META, "%s", board->meta[i]);

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
      varVal = StateVar(s,vd->offset,vd->width);
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
	  Warn ("In <%s> for type %s: var %s was referenced, but no such var could be found", vmaskTag, proto->name, varName);
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
  const char *typeName, *varName;
  Proto *proto;
  VarsDescriptor *vd;
  unsigned char s;
  s = 0;
  if ( (child = getNodeByName(node->children,shiftTag)) )  /* assignment intentional */
    s = NODEINTVAL (child);
  else if ( (child = getNodeByName(node->children,vShiftTag)) ) {  /* assignment intentional */
    typeName = (const char*) CHILDSTRING(child,TYPE);
    if (typeName) {
      proto = protoTableGetProto (protoTable, typeName);
      if (proto) {
	varName = (const char*) CHILDSTRING(child,VAR);
	vd = protoGetVarsDescriptor (proto, varName);
	if (vd)
	  s = vd->offset;
	else
	  Warn ("In <%s> for type %s: var '%s' was referenced, but no such var could be found", vShiftTag, proto->name, varName);
      } else
	Warn ("In <%s>: type %s unknown", vShiftTag, typeName);
    } else
      Warn ("<%s> lacks type", vShiftTag);
  }
  return s;
}

unsigned char getColShiftFromNode (xmlNode* node, Proto* proto) {
  xmlNode *child;
  const char *varName;
  VarsDescriptor *vd;
  unsigned char s;
  s = 0;
  if ( (child = CHILD(node,VAR)) ) {  /* assignment intentional */
    varName = (const char*) getNodeContent(child);
    vd = protoGetVarsDescriptor (proto, varName);
    if (vd)
      s = vd->offset;
    else
      Warn ("In <%s> of <%s> for type %s: var '%s' was referenced, but no such var could be found", XMLPREFIX(VAR), XMLPREFIX(COLRULE), proto->name, varName);
  } else if ( (child = CHILD(node,RSHIFT)) )  /* assignment intentional */
    s = NODEINTVAL (child);
  return s;
}

State getColMaskFromNode (xmlNode* node, Proto* proto) {
  xmlNode *child;
  const char *varName;
  VarsDescriptor *vd;
  State m;
  m = VarsMask;
  if ( (child = CHILD(node,VAR)) ) {  /* assignment intentional */
    varName = (const char*) getNodeContent(child);
    vd = protoGetVarsDescriptor (proto, varName);
    if (vd)
      m = ((1 << vd->width) - 1) << vd->offset;
    else
      Warn ("In <%s> of <%s> for type %s: var %s was referenced, but no such var could be found", XMLPREFIX(VAR), XMLPREFIX(COLRULE), proto->name, varName);
  } else if ( (child = CHILD(node,MASK)) )  /* assignment intentional */
    m = NODEHEXVAL (child);
  return m;
}

Message getMessageFromNode (xmlNode* node, ProtoTable* protoTable) {
  return protoTableMessageLookup (protoTable, (const char*) CHILDSTRING (node, MESSAGE));
}
