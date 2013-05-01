#ifndef XML_PARSER_INCLUDED
#define XML_PARSER_INCLUDED

#include <stdio.h>
#include "stringmap.h"

/* Lightweight recursive-descent XML parser that mimics the tiny subset of libxml2 that we use */

typedef unsigned char xmlChar;

typedef enum xmlElementType { XML_ELEMENT_NODE = 1, XML_ATTRIBUTE_NODE = 2, XML_TEXT_NODE = 3 } xmlElementType;
typedef struct xmlNode {
  xmlElementType type;
  const xmlChar *name;
  struct xmlNode *children;
  struct xmlNode *next;
  struct xmlNode *properties;
  const xmlChar *content;
} xmlNode;

typedef xmlNode xmlAttr;
typedef xmlNode xmlDoc;
#define xmlDocGetRootElement(doc) (doc)

typedef struct xmlTextWriter {
  xmlChar *start, *end, *cur;
  StringVector* tagStack;
} xmlTextWriter;
typedef xmlTextWriter* xmlTextWriterPtr;

xmlNode* newXmlNode (xmlElementType type, const char* nameStart, const char* nameEnd, const char* contentStart, const char* contentEnd);
void deleteXmlTree (xmlNode* root);

xmlNode* xmlTreeFromString (const char* str);
const char* xmlTreeToString (const xmlNode* node);

xmlTextWriterPtr newXmlTextWriter();
xmlChar* deleteXmlTextWriterLeavingText (xmlTextWriterPtr writer);

void xmlTextWriterStartElement (xmlTextWriterPtr writer, const xmlChar* tag);
void xmlTextWriterStartElementWithAttrs (xmlTextWriterPtr writer, const xmlChar* tag, const xmlAttr* properties);
void xmlTextWriterWriteFormatElement (xmlTextWriterPtr writer, const xmlChar* tag, const char* str, ...);
void xmlTextWriterEndElement (xmlTextWriterPtr writer);
void xmlTextWriterFullEndElement (xmlTextWriterPtr writer);

int xmlValidateNameValue (const xmlChar* tag);

#endif /* XML_PARSER_INCLUDED */
