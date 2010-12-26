#ifndef XMLUTIL_INCLUDED
#define XMLUTIL_INCLUDED

#include <libxml/tree.h>

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

#endif /* XMLUTIL_INCLUDED */
