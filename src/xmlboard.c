#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xmlboard.h"
#include "xmlutil.h"
#include "xmlgoal.h"
#include "game.h"

/* prototypes for private builder methods */
Particle* newParticleFromXmlNode (void *game, xmlNode* node);
void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node);
void initConditionFromXmlNode (RuleCondition* cond, xmlNode* node);
void initOperationFromXmlNode (RuleOperation* op, xmlNode* node);
StochasticRule* newRuleFromXmlNode (void *game, xmlNode* node);

/* method defs */
Board* newBoardFromXmlDocument (void *game, xmlDoc *doc) {
  return newBoardFromXmlRoot (game, xmlDocGetRootElement (doc));
}

Board* newBoardFromXmlRoot (void *game, xmlNode *root) {
  Board *board;
  xmlNode *boardNode, *grammar, *node;
  int x, y;
  State state;

  boardNode = CHILD(root,BOARD);
  Assert (boardNode != NULL, "XML board tag not found");

  board = newBoard (CHILDINT(boardNode,SIZE));
  board->game = game;

  grammar = CHILD(boardNode,GRAMMAR);
  for (node = grammar->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      addParticleToBoard (newParticleFromXmlNode(game,node), board);

  for (node = boardNode->children; node; node = node->next)
    if (MATCHES(node,INIT)) {
      x = CHILDINT(node,X);
      y = CHILDINT(node,Y);
      state = OPTCHILDINT(node,DECVAL,OPTCHILDHEX(node,HEXVAL,OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE)) << TypeShift));
      writeBoardState (board, x, y, state);
    }

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
  int nColorRules;
  xmlNode *curNode;
  nRules = 0;
  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,RULE))
      ++nRules;
  p = newParticle ((const char*) CHILDSTRING(node,NAME));
  if (CHILD(node,SYNC)) {
    p->synchronous = 1;
    p->syncPeriod = OPTCHILDINT(node,PERIOD,0);  /* leaving this as zero means that it will be auto-set at 1/(total rate) by addParticleToBoard */
    p->syncPhase = OPTCHILDINT(node,PHASE,0);
  }
  p->rule = newRuleFromXmlNode (game, CHILD(node,RULE));
  p->rate = OPTCHILDFLOAT(node,RATE,1.);
  p->type = OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE));
  nColorRules = 0;
  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,COLOR)) {
      Assert (nColorRules < NumColorRules, "newParticleFromXmlNode: too many color rules");
      initColorRuleFromXmlNode (&p->colorRule[nColorRules++], curNode);
    }
  return p;
}

void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node) {
  colorRule->rightShift = OPTCHILDINT(node,RSHIFT,0);
  colorRule->mask = OPTCHILDHEX(node,MASK,VarsMask);
  colorRule->multiplier = OPTCHILDINT(node,DECMUL,OPTCHILDHEX(node,HEXMUL,1));
  colorRule->offset = OPTCHILDINT(node,DECINC,OPTCHILDHEX(node,HEXINC,0));
}

StochasticRule* newRuleFromXmlNode (void *game, xmlNode* ruleNode) {
  StochasticRule* rule;
  const char *ruleTypeAttr;
  rule = NULL;
  Assert (ruleNode != NULL, "newRuleFromXmlNode: null rule node");
  ruleTypeAttr = ATTR(ruleNode,RULETYPE);
  if (ATTRMATCHES (ruleTypeAttr, LOOKUP)) {
    rule = newLookupRule();
    /* more to go here */

  } else if (ATTRMATCHES (ruleTypeAttr, MODIFY)) {
    rule = newModifyRule();
    /* more to go here */

  } else if (ATTRMATCHES (ruleTypeAttr, RANDOM)) {
    rule = newRandomRule();
    /* more to go here */

  } else if (ATTRMATCHES (ruleTypeAttr, OVERLOAD)) {
    rule = newOverloadRule();
    /* more to go here */

  } else if (ATTRMATCHES (ruleTypeAttr, GOAL)) {
    rule = newGoalRule();
    /* more to go here */
    /*    rule->trigger = newGoalFromXmlNode (goalNode, game);  */

  } else {
    Abort ("Unknown rule type");
  }

  return rule;
}

void initConditionFromXmlNode (RuleCondition* cond, xmlNode* node) {
  xmlNode* loc;
  const char* opcode;
  int rshift;

  loc = CHILD(node,POS);
  if (loc) {
    cond->loc.x = OPTCHILDINT(loc,X,0);
    cond->loc.y = OPTCHILDINT(loc,Y,0);
  } else
    cond->loc.x = cond->loc.y = 0;

  cond->mask = OPTCHILDHEX(node,MASK,StateMask);
  cond->rhs = OPTCHILDINT(node,DECVAL,OPTCHILDHEX(node,HEXVAL,0));
  cond->ignoreProb = OPTCHILDFLOAT(node,IGNORE,0.);
  cond->overloadIgnoreProb = OPTCHILDFLOAT(node,OVERLOAD,cond->ignoreProb);

  rshift = OPTCHILDINT(node,RSHIFT,0);
  if (rshift) {
    cond->mask = cond->mask << rshift;
    cond->rhs = cond->rhs << rshift;
  }

  cond->opcode = TestEQ;
  opcode = ATTR(node,OP);
  if (opcode) {
    if (strcmp(opcode,"=")==0 || strcmp(opcode,"==")==0) cond->opcode = TestEQ;
    else if (strcmp(opcode,"!=")==0) cond->opcode = TestNEQ;
    else if (strcmp(opcode,">")==0 || strcmp(opcode,"&gt;")==0) cond->opcode = TestGT;
    else if (strcmp(opcode,"<")==0 || strcmp(opcode,"&lt;")==0) cond->opcode = TestLT;
    else if (strcmp(opcode,">=")==0 || strcmp(opcode,"&gt;=")==0) cond->opcode = TestGEQ;
    else if (strcmp(opcode,"<=")==0 || strcmp(opcode,"&lt;=")==0) cond->opcode = TestLEQ;
    else if (strcmp(opcode,"1")==0) cond->opcode = TestTRUE;
    else if (strcmp(opcode,"0")==0) cond->opcode = TestFALSE;
    else Abort ("Unrecognized opcode");
  }
}

void initOperationFromXmlNode (RuleOperation* op, xmlNode* node) {
  xmlNode *src, *dest;
  State defaultSrcMask;

  op->rightShift = OPTCHILDINT(node,RSHIFT,0);
  op->offset = OPTCHILDINT(node,DECINC,OPTCHILDHEX(node,HEXINC,0));
  op->leftShift = OPTCHILDINT(node,LSHIFT,0);
  op->failProb = OPTCHILDFLOAT(node,FAIL,0.);
  op->overloadFailProb = OPTCHILDFLOAT(node,OVERLOAD,op->failProb);

  defaultSrcMask = op->rightShift >= BitsPerState ? 0 : StateMask;
  op->srcMask = OPTCHILDHEX(node,SRCMASK,defaultSrcMask);
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
}
