#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xmlboard.h"
#include "xmlutil.h"
#include "game.h"

/* prototypes for private builder methods */
Particle* newParticleFromXmlNode (xmlNode* node);
void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node);
void initRuleFromXmlNode (StochasticRule* rule, xmlNode* node);
void initConditionFromXmlNode (RuleCondition* cond, xmlNode* node);
void initOperationFromXmlNode (RuleOperation* op, xmlNode* node);

/* method defs */
Board* newBoardFromXmlDocument (xmlDoc *doc) {
  return newBoardFromXmlRoot (xmlDocGetRootElement (doc));
}

Board* newBoardFromXmlRoot (xmlNode *root) {
  Board *board;
  xmlNode *boardNode, *grammar, *node;
  int x, y;
  State state;

  boardNode = CHILD(root,BOARD);
  board = newBoard (CHILDINT(boardNode,SIZE));

  grammar = CHILD(boardNode,GRAMMAR);
  for (node = grammar->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      addParticleToBoard (newParticleFromXmlNode(node), board);

  for (node = boardNode->children; node; node = node->next)
    if (MATCHES(node,INIT)) {
      x = CHILDINT(node,X);
      y = CHILDINT(node,Y);
      state = OPTCHILDINT(node,DECVAL,OPTCHILDHEX(node,HEXVAL,OPTCHILDINT(node,DECTYPE,CHILDHEX(node,HEXTYPE)) << TypeShift));
      writeBoardState (board, x, y, state);
    }

  return board;
}

Board* newBoardFromXmlFile (const char* filename) {
  xmlDoc* doc;
  Board* board = NULL;
  doc = xmlReadFile (filename, NULL, 0);
  if (doc)
    board = newBoardFromXmlDocument (doc);
  return board;
}

Board* newBoardFromXmlString (const char* string) {
  xmlDoc* doc;
  Board* board = NULL;
  doc = xmlReadMemory (string, strlen(string), "noname.xml", NULL, 0);
  if (doc)
    board = newBoardFromXmlDocument (doc);
  return board;
}


Particle* newParticleFromXmlNode (xmlNode* node) {
  Particle* p;
  int nRules, nColorRules, n;
  xmlNode *curNode;
  nRules = 0;
  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,RULE))
      ++nRules;
  p = newParticle ((const char*) CHILDSTRING(node,NAME), nRules);
  if (CHILD(node,SYNC)) {
    p->synchronous = 1;
    if (CHILD(node,SHUFFLE))
      p->shuffle = 1;
    p->failures = OPTCHILDINT(node,FAILS,nRules);
    p->successes = OPTCHILDINT(node,SUCCEEDS,1);
    p->syncPeriod = OPTCHILDINT(node,PERIOD,0);  /* leaving this as zero means that it will be auto-set at 1/(total rate) by addParticleToBoard */
    p->syncPhase = OPTCHILDINT(node,PHASE,0);
  }
  n = 0;
  for (curNode = node->children; curNode; curNode = curNode->next)
    if (MATCHES(curNode,RULE))
      initRuleFromXmlNode (&p->rule[n++], curNode);
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

void initRuleFromXmlNode (StochasticRule* rule, xmlNode* ruleNode) {
  xmlNode *node, *balloonNode, *locNode;
  int nCond, nOp;
  HSB24 color;
  rule->rate = OPTCHILDFLOAT(ruleNode,RATE,1.);
  rule->overloadRate = OPTCHILDFLOAT(ruleNode,OVERLOAD,rule->rate);
  nCond = nOp = 0;
  for (node = ruleNode->children; node; node = node->next)
    if (MATCHES(node,TEST)) {
      Assert (nCond < NumRuleConditions, "initRuleFromXmlNode: too many rule conditions");
      initConditionFromXmlNode (&rule->cond[nCond++], node);
    } else if (MATCHES(node,EXEC)) {
      Assert (nOp < NumRuleOperations, "initRuleFromXmlNode: too many rule operations");
      initOperationFromXmlNode (&rule->op[nOp++], node);
    }
  balloonNode = CHILD (ruleNode, BALLOON);
  if (balloonNode) {
    locNode = CHILD (balloonNode, LOC);
    color = OPTCHILDINT (balloonNode, COLOR, OPTCHILDHEX (balloonNode, HEXCOLOR, HSB24White));
    rule->balloon = newProtoBalloon ((char*) CHILDSTRING (balloonNode, TEXT),
				     locNode ? OPTCHILDINT(locNode,X,0) : 0,
				     locNode ? OPTCHILDINT(locNode,Y,0) : 0,
				     ConvertHsb24ToPaletteIndex (color),
				     OPTCHILDFLOAT (balloonNode, SIZE, 1.),
				     OPTCHILDFLOAT (balloonNode, TTL, DefaultBalloonTTL),
				     OPTCHILDFLOAT (balloonNode, RISE, DefaultBalloonRise),
				     OPTCHILDFLOAT (balloonNode, ZOOM, DefaultBalloonZoom),
				     OPTCHILDFLOAT (balloonNode, FADE, DefaultBalloonFade),
				     OPTCHILDFLOAT (balloonNode, RATE, 1.));
  }
}

void initConditionFromXmlNode (RuleCondition* cond, xmlNode* node) {
  xmlNode* loc;
  const char* opcode;
  int rshift;

  loc = CHILD(node,LOC);
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
