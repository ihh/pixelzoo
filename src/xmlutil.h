#ifndef XMLUTIL_INCLUDED
#define XMLUTIL_INCLUDED

#include <libxml/tree.h>

/* This header file is "private" to the PixelZoo XML adapter.
   It should not be #include'd from public .h files.
 */

/* all XML keyword #define's begin with this prefix */
#define XMLPREFIX(KEYWORD)  XMLZOO_ ## KEYWORD

/* functions to convert decimal/hexadecimal strings to 64-bit ints.
   basically the 64-bit versions of atoi() and strtoi()
 */
long long decToSignedLongLong (const char *);
unsigned long long hexToUnsignedLongLong (const char *);

/* private macros for matching XML nodes */
#define MATCHES(NODE,KEYWORD) ((NODE)->type == XML_ELEMENT_NODE && strcmp ((const char*) (NODE)->name, XMLPREFIX(KEYWORD)) == 0)
#define CHILD(NODE,KEYWORD) getNodeByName ((NODE)->children, XMLPREFIX(KEYWORD))
#define CHILDSTRING(NODE,KEYWORD) getNodeContentOrComplain (CHILD(NODE,KEYWORD), XMLPREFIX(KEYWORD))
#define CHILDINT(NODE,KEYWORD) decToSignedLongLong((const char*) CHILDSTRING(NODE,KEYWORD))
#define CHILDFLOAT(NODE,KEYWORD) atof((const char*) CHILDSTRING(NODE,KEYWORD))
#define CHILDHEX(NODE,KEYWORD) hexToUnsignedLongLong ((const char*) CHILDSTRING(NODE,KEYWORD))
#define OPTCHILDINT(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDINT(NODE,KEYWORD) : (DEFAULT))
#define OPTCHILDFLOAT(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDFLOAT(NODE,KEYWORD) : (DEFAULT))
#define OPTCHILDHEX(NODE,KEYWORD,DEFAULT) (CHILD(NODE,KEYWORD) ? CHILDHEX(NODE,KEYWORD) : (DEFAULT))
#define ATTR(NODE,KEYWORD) ((const char*) getAttrByName (NODE, XMLPREFIX(KEYWORD)))
#define ATTRMATCHES(ATTR_VAL,KEYWORD) (strcmp ((ATTR_VAL), XMLPREFIX(KEYWORD)) == 0)
#define NODESTRINGVAL(NODE) getNodeContent(NODE)
#define NODEINTVAL(NODE) decToSignedLongLong ((const char*) getNodeContent(NODE))
#define NODEFLOATVAL(NODE) atof ((const char*) getNodeContent(NODE))
#define NODEHEXVAL(NODE) hexToUnsignedLongLong ((const char*) getNodeContent(NODE))

/* prototypes for private builder methods */
xmlNode* getNodeByName (xmlNode* node, char* name);  /* walks along the node->next list until it finds 'name' */
xmlChar* getNodeContent (xmlNode* node);
xmlChar* getNodeContentOrComplain (xmlNode* node, char* tag);
xmlChar* getAttrByName (xmlNode* node, char* name);

#endif /* XMLUTIL_INCLUDED */
