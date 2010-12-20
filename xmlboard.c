#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xmlboard.h"

/* XML node names */
#define XMLBOARD_BOARD    "board"
#define XMLBOARD_SIZE     "size"
#define XMLBOARD_GRAMMAR  "grammar"
#define XMLBOARD_PARTICLE "particle"
#define XMLBOARD_SYNC     "sync"
#define XMLBOARD_SHUFFLE  "shuffle"
#define XMLBOARD_ATTEMPTS "attempts"
#define XMLBOARD_DECTYPE  "type"
#define XMLBOARD_HEXTYPE  "hextype"
#define XMLBOARD_NAME     "name"
#define XMLBOARD_RULE     "rule"
#define XMLBOARD_RATE     "rate"
#define XMLBOARD_OVERLOAD "overload"
#define XMLBOARD_TEST     "test"
#define XMLBOARD_LOC      "loc"
#define XMLBOARD_X        "x"
#define XMLBOARD_Y        "y"
#define XMLBOARD_DECMASK  "mask"
#define XMLBOARD_HEXMASK  "hexmask"
#define XMLBOARD_DECVAL   "val"
#define XMLBOARD_HEXVAL   "hexval"
#define XMLBOARD_OP       "op"
#define XMLBOARD_IGNORE   "ignore"
#define XMLBOARD_EXEC     "exec"
#define XMLBOARD_SRC      "src"
#define XMLBOARD_DEST     "dest"
#define XMLBOARD_DECINC   "inc"
#define XMLBOARD_HEXINC   "hexinc"
#define XMLBOARD_LSHIFT   "lshift"
#define XMLBOARD_RSHIFT   "rshift"
#define XMLBOARD_FAIL     "fail"
#define XMLBOARD_COLOR    "color"
#define XMLBOARD_DECMUL   "mul"
#define XMLBOARD_HEXMUL   "hexmul"
#define XMLBOARD_INIT     "init"

/* private macros for matching XML nodes */
#define MATCHES(NODE,KEYWORD) ((NODE)->type == XML_ELEMENT_NODE && strcmp ((const char*) (NODE)->name, XMLBOARD_ ## KEYWORD) == 0)
#define CHILD(NODE,KEYWORD) getNodeByName ((NODE)->children, XMLBOARD_ ## KEYWORD)
#define CHILDSTRING(NODE,KEYWORD) getNodeContentOrComplain (CHILD(NODE,KEYWORD), XMLBOARD_ ## KEYWORD)
#define CHILDINT(NODE,KEYWORD) atoi((const char*) CHILDSTRING(NODE,KEYWORD))
#define CHILDFLOAT(NODE,KEYWORD) atof((const char*) CHILDSTRING(NODE,KEYWORD))
#define CHILDHEX(NODE,KEYWORD) strtoul((const char*) CHILDSTRING(NODE,KEYWORD),0,16)
#define OPTCHILDINT(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDINT(NODE,KEYWORD) : (DEFAULT))
#define OPTCHILDFLOAT(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDFLOAT(NODE,KEYWORD) : (DEFAULT))
#define OPTCHILDHEX(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDHEX(NODE,KEYWORD) : (DEFAULT))
#define ATTR(NODE,KEYWORD) getAttrByName (NODE, XMLBOARD_ ## KEYWORD)

/* prototypes for private builder methods */
xmlNode* getNodeByName (xmlNode* node, char* name);  /* walks along the node->next list until it finds 'name' */
xmlChar* getNodeContentOrComplain (xmlNode* node, char* tag);
xmlChar* getAttrByName (xmlNode* node, char* name);
Particle* newParticleFromXmlNode (xmlNode* node);
void initColorRuleFromXmlNode (ColorRule *colorRule, xmlNode* node);
void initRuleFromXmlNode (StochasticRule* rule, xmlNode* node);
void initConditionFromXmlNode (RuleCondition* cond, xmlNode* node);
void initOperationFromXmlNode (RuleOperation* op, xmlNode* node);

/* method defs */
Board* newBoardFromXmlDocument (xmlDoc *doc) {
  Board *board;
  xmlNode *root, *boardNode, *grammar, *node;
  int x, y;
  State state;

  root = xmlDocGetRootElement (doc);

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

/* private builder methods */
xmlNode* getNodeByName (xmlNode* node, char* name) {
  for (; node; node = node->next)
    if (node->type == XML_ELEMENT_NODE && strcmp ((const char*) node->name, name) == 0)
      return node;
  return (xmlNode*) NULL;
}

xmlChar* getNodeContentOrComplain (xmlNode* node, char* tag) {
  if (!node)
    fprintf (stderr, "Missing tag: %s\n", tag);
  return node->children->content;
}

xmlChar* getAttrByName (xmlNode* node, char* name) {
  xmlAttr* attr;
  for (attr = node->properties; attr; attr = attr->next)
    if (strcmp ((const char*) attr->name, name) == 0)
      return attr->children->content;
  return (xmlChar*) NULL;
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
    if (CHILD(node,SHUFFLE)) {
      p->shuffle = 1;
      p->attempts = OPTCHILDINT(node,ATTEMPTS,nRules);
    }
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
  colorRule->mask = OPTCHILDINT(node,DECMASK,OPTCHILDHEX(node,HEXMASK,VarMask));
  colorRule->multiplier = OPTCHILDINT(node,DECMUL,OPTCHILDHEX(node,HEXMUL,1));
  colorRule->offset = OPTCHILDINT(node,DECINC,OPTCHILDHEX(node,HEXINC,0));
}

void initRuleFromXmlNode (StochasticRule* rule, xmlNode* node) {
  int nCond, nOp;
  rule->rate = OPTCHILDFLOAT(node,RATE,1.);
  rule->overloadRate = OPTCHILDFLOAT(node,OVERLOAD,rule->rate);
  nCond = nOp = 0;
  for (node = node->children; node; node = node->next)
    if (MATCHES(node,TEST)) {
      Assert (nCond < NumRuleConditions, "initRuleFromXmlNode: too many rule conditions");
      initConditionFromXmlNode (&rule->cond[nCond++], node);
    } else if (MATCHES(node,EXEC)) {
      Assert (nOp < NumRuleOperations, "initRuleFromXmlNode: too many rule operations");
      initOperationFromXmlNode (&rule->op[nOp++], node);
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

  cond->mask = OPTCHILDINT(node,DECMASK,OPTCHILDHEX(node,HEXMASK,StateMask));
  cond->rhs = OPTCHILDINT(node,DECVAL,OPTCHILDHEX(node,HEXVAL,0));
  cond->ignoreProb = OPTCHILDFLOAT(node,IGNORE,0.);
  cond->overloadIgnoreProb = OPTCHILDFLOAT(node,OVERLOAD,cond->ignoreProb);

  rshift = OPTCHILDINT(node,RSHIFT,TypeShift);
  if (rshift) {
    cond->mask = cond->mask << rshift;
    cond->rhs = cond->rhs << rshift;
  }

  cond->opcode = EQ;
  opcode = (const char*) ATTR(node,OP);
  if (opcode) {
    if (strcmp(opcode,"=")==0 || strcmp(opcode,"==")==0) cond->opcode = EQ;
    else if (strcmp(opcode,"!=")==0) cond->opcode = NEQ;
    else if (strcmp(opcode,">")==0) cond->opcode = GT;
    else if (strcmp(opcode,"<")==0) cond->opcode = LT;
    else if (strcmp(opcode,">=")==0) cond->opcode = GEQ;
    else if (strcmp(opcode,"<=")==0) cond->opcode = LEQ;
    else if (strcmp(opcode,"1")==0) cond->opcode = TRUE;
    else if (strcmp(opcode,"0")==0) cond->opcode = FALSE;
  }
}

void initOperationFromXmlNode (RuleOperation* op, xmlNode* node) {
  xmlNode *src, *dest;
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
  op->rightShift = OPTCHILDINT(node,RSHIFT,0);
  op->offset = OPTCHILDINT(node,DECINC,OPTCHILDHEX(node,HEXINC,0));
  op->mask = OPTCHILDINT(node,DECMASK,OPTCHILDHEX(node,HEXMASK,StateMask));
  op->leftShift = OPTCHILDINT(node,LSHIFT,0);
  op->failProb = OPTCHILDFLOAT(node,FAIL,0.);
  op->overloadFailProb = OPTCHILDFLOAT(node,OVERLOAD,op->failProb);
  op->preMask = op->rightShift >= 32 ? 0 : StateMask;
}
