#include <stdlib.h>
#include "xmlboard.h"

/* XML node names */
#define XMLBOARD_SIZE     "size"
#define XMLBOARD_PARTICLE "particle"
#define XMLBOARD_ID       "id"
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
#define XMLBOARD_VAL      "val"
#define XMLBOARD_OP       "op"
#define XMLBOARD_IGNORE   "ignore"
#define XMLBOARD_EXEC     "exec"
#define XMLBOARD_SRC      "src"
#define XMLBOARD_DEST     "dest"
#define XMLBOARD_FAIL     "fail"

/* private macros for matching XML nodes */
#define MATCHES(NODE,KEYWORD) ((NODE)->type == XML_ELEMENT_NODE && strcmp ((NODE)->name, XMLBOARD_ ## KEYWORD) == 0)
#define CHILD(NODE,KEYWORD) getChildByName (NODE, XMLBOARD_ ## KEYWORD)
#define CHILDSTRING(NODE,KEYWORD) CHILD(NODE,KEYWORD)->content
#define CHILDINT(NODE,KEYWORD) atoi(CHILDSTRING(NODE,KEYWORD))
#define CHILDFLOAT(NODE,KEYWORD) atof(CHILDSTRING(NODE,KEYWORD))
#define OPTCHILDINT(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDINT(NODE,KEYWORD) : (DEFAULT))
#define OPTCHILDFLOAT(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDFLOAT(NODE,KEYWORD) : (DEFAULT))

/* prototypes for private builder methods */
xmlNode* getChildByName (xmlNode* node, char* name);  /* walks along the node->next list until it finds 'name' */
Particle* newParticleFromXmlNode (xmlNode* node);
void initRuleFromXmlNode (StochasticRule* rule, xmlNode* node);
void initConditionFromXmlNode (RuleCondition* cond, xmlNode* node);
void initOperationFromXmlNode (RuleOperation* op, xmlNode* node);

/* method defs */
Board* newBoardFromXmlDocument (xmlDoc *doc) {
  Board *board;
  xmlNode *root, *node;

  root = xmlDocGetRootElement (doc);
  board = newBoard (CHILDINT(node,SIZE));

  for (node = root->children; node; node = node->next)
    if (MATCHES(node,PARTICLE))
      addParticleToBoard (newParticleFromXmlNode(node), board);

  return board;
}

Board* newBoardFromXmlFile (const char* filename) {
  xmlDoc* doc;
  Board* board = NULL;
  doc = xmlReadFile (filename, NULL, 0);
  if (doc != NULL)
    board = newBoardFromXmlDocument (doc);
  return board;
}

/* private builder methods */
xmlNode* getChildByName (xmlNode* parent, char* childName) {
  xmlNode* node;
  for (node = parent->children; node; node = node->next)
    if (node->type == XML_ELEMENT_NODE && strcmp (node->name, childName) == 0)
      return node;
  return (xmlNode*) NULL;
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
  p->type = CHILDINT(node,ID);
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
  loc = CHILD(node,LOC);
  cond->loc.x = CHILDINT(loc,X);
  cond->loc.y = CHILDINT(loc,Y);
  /* more to go here */
}

void initOperationFromXmlNode (RuleOperation* op, xmlNode* node) {
  /* more to go here */
}
