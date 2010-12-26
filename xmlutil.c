#include <string.h>
#include "xmlutil.h"

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
