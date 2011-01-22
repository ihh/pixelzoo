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
ParticleRule* newRuleFromXmlNode (void *game, xmlNode* node);
ParticleRule* newRuleFromXmlParentNode (void *game, xmlNode* parent);

void initLookupRuleFromXmlNode (LookupRuleParams* lookup, xmlNode* node, void *game);
void initModifyRuleFromXmlNode (ModifyRuleParams* op, xmlNode* node, void *game);

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

ParticleRule* newRuleFromXmlParentNode (void *game, xmlNode* parent) {
  ParticleRule* rule;
  rule = NULL;
  if (parent)
    rule = newRuleFromXmlNode (game, CHILD (parent, RULE));
  return rule;
}

ParticleRule* newRuleFromXmlNode (void *game, xmlNode* ruleNode) {
  ParticleRule* rule;
  const char *ruleTypeAttr;
  LookupRuleParams *lookup;
  ModifyRuleParams *modify;
  RandomRuleParams *random;
  OverloadRuleParams *overload;

  rule = NULL;

  if (ruleNode != NULL) {
    ruleTypeAttr = ATTR(ruleNode,RULETYPE);
    Assert (ruleTypeAttr != NULL, "Missing " XMLPREFIX(RULETYPE) " attribute in rule");
    if (ATTRMATCHES (ruleTypeAttr, SWITCH)) {
      rule = newLookupRule();
      lookup = &rule->param.lookup;
      initLookupRuleFromXmlNode (lookup, ruleNode, game);

    } else if (ATTRMATCHES (ruleTypeAttr, MODIFY)) {
      rule = newModifyRule();
      modify = &rule->param.modify;
      initModifyRuleFromXmlNode (modify, ruleNode, game);

    } else if (ATTRMATCHES (ruleTypeAttr, RANDOM)) {
      rule = newRandomRule();
      random = &rule->param.random;
      random->prob = OPTCHILDFLOAT (ruleNode, PROB, 0.5);
      random->passRule = newRuleFromXmlParentNode (game, CHILD (ruleNode, PASS));
      random->failRule = newRuleFromXmlParentNode (game, CHILD (ruleNode, FAIL));

    } else if (ATTRMATCHES (ruleTypeAttr, OVERLOAD)) {
      rule = newOverloadRule();
      overload = &rule->param.overload;
      overload->slowRule = newRuleFromXmlParentNode (game, CHILD (ruleNode, SLOW));
      overload->fastRule = newRuleFromXmlParentNode (game, CHILD (ruleNode, FAST));

    } else if (ATTRMATCHES (ruleTypeAttr, GOAL)) {
      rule = newGoalRule();
      rule->param.goal = newGoalFromXmlNode (CHILD (ruleNode, GOAL), game);

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
  State defaultSrcMask;

  op->rightShift = OPTCHILDINT(node,RSHIFT,0);
  op->offset = OPTCHILDINT(node,DECINC,OPTCHILDHEX(node,HEXINC,0));
  op->leftShift = OPTCHILDINT(node,LSHIFT,0);

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

  op->nextRule = newRuleFromXmlParentNode (game, CHILD (node, NEXT));
}
