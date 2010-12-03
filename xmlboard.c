#include <stdlib.h>
#include <string.h>
#include "xmlboard.h"

/* XML node names */
#define XMLBOARD_SIZE     "size"
#define XMLBOARD_PARTICLE "particle"
#define XMLBOARD_DECID    "decid"
#define XMLBOARD_HEXID    "hexid"
#define XMLBOARD_NAME     "name"
#define XMLBOARD_COLOR    "color"
#define XMLBOARD_R        "r"
#define XMLBOARD_G        "g"
#define XMLBOARD_B        "b"
#define XMLBOARD_RULE     "rule"
#define XMLBOARD_RATE     "rate"
#define XMLBOARD_OVERLOAD "overload"
#define XMLBOARD_TEST     "test"
#define XMLBOARD_LOC      "loc"
#define XMLBOARD_X        "x"
#define XMLBOARD_Y        "y"
#define XMLBOARD_MASK     "mask"
#define XMLBOARD_DECVAL   "decval"
#define XMLBOARD_HEXVAL   "hexval"
#define XMLBOARD_OP       "op"
#define XMLBOARD_IGNORE   "ignore"
#define XMLBOARD_EXEC     "exec"
#define XMLBOARD_SRC      "src"
#define XMLBOARD_DEST     "dest"
#define XMLBOARD_DECADD   "decadd"
#define XMLBOARD_HEXADD   "hexadd"
#define XMLBOARD_LSHIFT   "lshift"
#define XMLBOARD_RSHIFT   "rshift"
#define XMLBOARD_FAIL     "fail"
#define XMLBOARD_INIT     "init"
#define XMLBOARD_STATE    "state"

/* private macros for matching XML nodes */
#define MATCHES(NODE,KEYWORD) ((NODE)->type == XML_ELEMENT_NODE && strcmp ((NODE)->name, XMLBOARD_ ## KEYWORD) == 0)
#define CHILD(NODE,KEYWORD) getNodeByName ((NODE)->children, XMLBOARD_ ## KEYWORD)
#define CHILDSTRING(NODE,KEYWORD) CHILD(NODE,KEYWORD)->content
#define CHILDINT(NODE,KEYWORD) atoi(CHILDSTRING(NODE,KEYWORD))
#define CHILDFLOAT(NODE,KEYWORD) atof(CHILDSTRING(NODE,KEYWORD))
#define CHILDHEX(NODE,KEYWORD) strtoul(CHILDSTRING(NODE,KEYWORD),0,16)
#define OPTCHILDINT(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDINT(NODE,KEYWORD) : (DEFAULT))
#define OPTCHILDFLOAT(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDFLOAT(NODE,KEYWORD) : (DEFAULT))
#define OPTCHILDHEX(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDHEX(NODE,KEYWORD) : (DEFAULT))
#define ATTR(NODE,KEYWORD) getAttrByName (NODE, XMLBOARD_ ## KEYWORD)

/* prototypes for private builder methods */
xmlNode* getNodeByName (xmlNode* node, char* name);  /* walks along the node->next list until it finds 'name' */
xmlChar* getAttrByName (xmlNode* node, char* name);
Particle* newParticleFromXmlNode (xmlNode* node);
void initRuleFromXmlNode (StochasticRule* rule, xmlNode* node);
void initConditionFromXmlNode (RuleCondition* cond, xmlNode* node);
void initOperationFromXmlNode (RuleOperation* op, xmlNode* node);

/* method defs */
Board* newBoardFromXmlDocument (xmlDoc *doc) {
  Board *board;
  xmlNode *root, *node;
  int x, y;
  State state;

  root = xmlDocGetRootElement (doc);
  board = newBoard (CHILDINT(node,SIZE));

  for (node = root->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      addParticleToBoard (newParticleFromXmlNode(node), board);

  for (node = root->children; node; node = node->next)
    if (MATCHES(node,INIT)) {
      x = CHILDINT(node,X);
      y = CHILDINT(node,Y);
      state = CHILDHEX(node,STATE);
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
    if (node->type == XML_ELEMENT_NODE && strcmp (node->name, name) == 0)
      return node;
  return (xmlNode*) NULL;
}

xmlChar* getAttrByName (xmlNode* node, char* name) {
  xmlAttr* attr;
  for (attr = node->properties; attr; attr = attr->next)
    if (strcmp (attr->name, name) == 0)
      return attr->children->content;
  return (xmlChar*) NULL;
}

Particle* newParticleFromXmlNode (xmlNode* node) {
  Particle* p;
  int nRules, n;
  xmlNode *curNode, *color;
  xmlChar *content;
  nRules = 0;
  for (curNode = node; curNode; curNode = curNode->next)
    if (MATCHES(curNode,RULE))
      ++nRules;
  p = newParticle (CHILDSTRING(node,NAME), nRules);
  p->type = OPTCHILDINT(node,DECID,CHILDHEX(node,HEXID));
  if (color = CHILD(node,COLOR)) {
    p->color.r = CHILDINT(color,R);
    p->color.g = CHILDINT(color,G);
    p->color.b = CHILDINT(color,B);
  } else
    p->color.r = p->color.g = p->color.b = 255;
  n = 0;
  for (curNode = node; curNode; curNode = curNode->next)
    if (MATCHES(curNode,RULE))
      initRuleFromXmlNode (&p->rule[n++], curNode);
  return p;
}

void initRuleFromXmlNode (StochasticRule* rule, xmlNode* node) {
  int nCond, nOp;
  rule->rate = OPTCHILDFLOAT(node,RATE,1.);
  rule->overloadRate = OPTCHILDFLOAT(node,OVERLOAD,rule->rate);
  nCond = nOp = 0;
  for (; node; node = node->next)
    if (MATCHES(node,TEST))
      initConditionFromXmlNode (&rule->cond[nCond++], node);
    else if (MATCHES(node,EXEC))
      initOperationFromXmlNode (&rule->op[nOp++], node);
}

void initConditionFromXmlNode (RuleCondition* cond, xmlNode* node) {
  xmlNode* loc;
  xmlChar* opcode;
  int rshift;

  loc = CHILD(node,LOC);
  if (loc) {
    cond->loc.x = OPTCHILDINT(loc,X,0);
    cond->loc.y = OPTCHILDINT(loc,Y,0);
  } else
    cond->loc.x = cond->loc.y = 0;

  cond->mask = OPTCHILDHEX(node,MASK,StateMask);
  cond->rhs = OPTCHILDINT(node,DECVAL,OPTCHILDHEX(node,HEXVAL,0)) & cond->mask;
  cond->ignoreProb = OPTCHILDFLOAT(node,IGNORE,0.);

  rshift = OPTCHILDINT(node,RSHIFT,0);
  if (rshift) {
    cond->mask << rshift;
    cond->rhs << rshift;
  }

  cond->opcode = EQ;
  opcode = ATTR(node,OP);
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
  if (src && !dest) dest = src;
  else if (dest && !src) src = dest;
  if (src) {
    op->src.x = OPTCHILDINT(src,X,0);
    op->src.y = OPTCHILDINT(src,Y,0);
  } else
    op->src.x = op->src.y = 0;
  if (dest) {
    op->dest.x = OPTCHILDINT(dest,X,0);
    op->dest.y = OPTCHILDINT(dest,Y,0);
  } else
    op->dest.x = op->dest.y = 0;
  op->rightShift = OPTCHILDINT(node,RSHIFT,0);
  op->offset = OPTCHILDINT(node,DECADD,OPTCHILDHEX(node,HEXADD,0));
  op->mask = OPTCHILDHEX(node,MASK,StateMask);
  op->leftShift = OPTCHILDINT(node,LSHIFT,0);
  op->failProb = OPTCHILDFLOAT(node,FAIL,0.);
}
