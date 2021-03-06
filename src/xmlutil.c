#include <string.h>
#include "xmlutil.h"
#include "util.h"

/* private builder methods */
xmlNode* getNodeByName (xmlNode* node, const char* name) {
  for (; node; node = node->next)
    if (node->type == XML_ELEMENT_NODE && strcmp ((const char*) node->name, name) == 0)
      return node;
  return (xmlNode*) NULL;
}

xmlChar* getNodeContent (xmlNode* node) {
  return (xmlChar*) node->children->content;
}

xmlChar* getNodeContentOrComplain (xmlNode* node, const char* tag) {
  if (node == NULL) {
    if (tag)
      Abort ("Node is null, expected <%s>\n", tag);
    else
      Abort ("Node and tag are null\n");
  }
  if (node->children == NULL)
    Abort ("Missing children for tag: %s\n", tag);
  return (xmlChar*) node->children->content;
}

xmlChar* getAttrByName (xmlNode* node, const char* name) {
  xmlAttr* attr;
  for (attr = node->properties; attr; attr = attr->next)
    if (strcmp ((const char*) attr->name, name) == 0)
      return (xmlChar*) attr->content;
  return (xmlChar*) NULL;
}
