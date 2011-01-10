#include <string.h>
#include <ctype.h>
#include "xmlutil.h"
#include "util.h"

/* char* to unsigned long long conversions */
long long decToSignedLongLong( const char *ca ) {
  long long ig;
  char c;
  int sign;
  ig = 0;
  /* test for prefixing white space */
  while (*ca == ' ' || *ca == '\t' ) 
    ca++;
  /* Check sign entered or no */
  sign = 1;
  if ( *ca == '-' )
    sign = -1;
  /* convert string to int */
  while ((c = tolower(*ca++)) != '\0')
    if (c >= '0' && c <= '9')
      ig = ig * 10LL + (long long) (c - '0');
  return ig * (long long) sign;
}

unsigned long long hexToUnsignedLongLong( const char *ca ) {
  unsigned long long ig;
  char c;
  ig = 0;
  /* test for prefixing white space */
  while (*ca == ' ' || *ca == '\t' ) 
    ca++;
  /* convert string to int */
  while ((c = tolower(*ca++)) != '\0')
    if (c >= '0' && c <= '9')
      ig = ig * 16LL + (unsigned long long) (c - '0');
    else if (c >= 'a' && c <= 'f')
      ig = ig * 16LL + 10LL + (unsigned long long) (c - 'a');
  return ig;
}

/* private builder methods */
xmlNode* getNodeByName (xmlNode* node, char* name) {
  for (; node; node = node->next)
    if (node->type == XML_ELEMENT_NODE && strcmp ((const char*) node->name, name) == 0)
      return node;
  return (xmlNode*) NULL;
}

xmlChar* getNodeContent (xmlNode* node) {
  return node->children->content;
}

xmlChar* getNodeContentOrComplain (xmlNode* node, char* tag) {
  if (node == NULL) {
    fprintf (stderr, "Missing tag: %s\n", tag);
    Abort("XML parse error");
  }
  if (node->children == NULL) {
    fprintf (stderr, "Missing children for tag: %s\n", tag);
    Abort("XML parse error");
  }
  return node->children->content;
}

xmlChar* getAttrByName (xmlNode* node, char* name) {
  xmlAttr* attr;
  for (attr = node->properties; attr; attr = attr->next)
    if (strcmp ((const char*) attr->name, name) == 0)
      return attr->children->content;
  return (xmlChar*) NULL;
}
