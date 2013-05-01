#include <ctype.h>
#include <string.h>
#include <stdarg.h>

#include "xmlparser.h"
#include "xmlutil.h"
#include "util.h"

#define XML_WRITE_BUFFER_INITIAL_SIZE 1024

void writeXmlNode (xmlTextWriterPtr writer, const xmlNode* node);
xmlNode* newXmlNodeFromTagBlock (const char** s);
void xmlTextWriterVWriteFormat (xmlTextWriterPtr writer, const char* fmt, va_list argptr);
void xmlTextWriterWriteFormat (xmlTextWriterPtr writer, const char* fmt, ...);

xmlNode* newXmlNodeFromTagBlock (const char** s) {
  xmlNode *node, *child, *firstChild, *firstProp, *lastChild, *lastProp;
  xmlAttr *attr;
  const char *tagStart, *tagEnd, *contStart, *contEnd, *attrStart, *attrEnd, *valStart, *valEnd, *closeStart, *closeEnd;

  node = firstChild = firstProp = lastChild = lastProp = NULL;

  /* get opening tag */
  Assert (*(*s)++ == '<', "Not a tag block");
  tagStart = *s;
  while (isalpha(**s)) ++*s;
  tagEnd = *s;
  Assert (tagEnd > tagStart, "No alphabetic tag found");

  /* get attributes */
  while (**s != '/' && **s != '>' && **s != '\0') {
    if (isspace(**s))
      ++*s;
    else if (isalpha(**s)) {
      /* Parse 'attr', 'attr=val', or 'attr="val"' */
      attrStart = (*s)++;
      while (isalpha(**s)) ++*s;
      attrEnd = *s;
      while (isspace(**s)) ++*s;
      if (**s == '=') {
	++*s;
	while (isspace(**s)) ++*s;
	if (**s == '"') {
	  valStart = ++*s;
	  while (**s != '"' && **s != '\0') ++*s;
	  valEnd = (*s)++;
	} else {
	  valStart = *s;
	  while (!isspace(**s) && **s != '/' && **s != '>' && **s != '\0') ++*s;
	  valEnd = *s;
	}
	Assert (**s != '\0', "Premature termination");
      } else
	valStart = valEnd = NULL;
      attr = newXmlNode (XML_ATTRIBUTE_NODE, attrStart, attrEnd, valStart, valEnd);
      if (lastProp)
	lastProp->next = attr;
      else
	firstProp = attr;
      lastProp = attr;
    } else
      Abort ("Non-alphabetic attribute?");
  }

  Assert (*s != '\0', "Malformed tag");

  if (**s == '/') {
    ++*s;
    Assert (**s == '>', "Malformed tag");
    node = newXmlNode (XML_ELEMENT_NODE, tagStart, tagEnd, NULL, NULL);
    node->properties = firstProp;

  } else {

    contStart = ++*s;

    /* look for a child node or a closing tag */
    while (node == NULL) {
      switch (**s) {
      case '<':
	contEnd = *s;
	if (*(*s + 1) == '/') {
	  /* closing tag */
	  closeStart = (*s += 2);
	  while (**s != '>' && **s != '\0') ++*s;
	  closeEnd = *s++;
	  Assert (strncmp (tagStart, closeStart, tagEnd - tagStart) == 0, "Opening and closing tags don't match");
	  node = newXmlNode (XML_ELEMENT_NODE, tagStart, tagEnd, NULL, NULL);
	  node->children = firstChild ? firstChild : newXmlNode (XML_TEXT_NODE, NULL, NULL, contStart, contEnd);
	  node->properties = firstProp;
	  break;

	} else if (*(*s + 1) == '!') {
	  /* skip comment */
	  do
	    ++*s;
	  while (!(**s == '\0' || ((*s)[-2] == '-' && (*s)[-1] == '>')));
	} else {
	  /* child tag */
	  child = newXmlNodeFromTagBlock(s);
	  if (lastChild)
	    lastChild->next = child;
	  else
	    firstChild = child;
	  lastChild = child;
	}
	break;

      case '\0':
	Abort ("Premature termination");
	break;

      default:
	++*s;
	break;
      }
    }
  }

  return node;
}

xmlNode* newXmlNode (xmlElementType type, const char* nameStart, const char* nameEnd, const char* contentStart, const char* contentEnd) {
  xmlNode *node;

  node = SafeCalloc (1, sizeof(xmlNode));
  node->type = type;

  node->name = SafeMalloc (nameEnd + 1 - nameStart);
  strncpy ((char*) node->name, (char*) nameStart, nameEnd - nameStart);
  ((xmlChar*)node->name)[nameEnd - nameStart] = '\0';

  if (contentStart != NULL) {
    node->content = SafeMalloc (contentEnd + 1 - contentStart);
    strncpy ((char*) node->content, (char*) contentStart, contentEnd - contentStart);
    ((xmlChar*)node->content)[contentEnd - contentStart] = '\0';
  }

  return node;
}

xmlNode* xmlTreeFromString (const char* s) {
  while (1) {
    while (*s != '\0' && *s != '<')
      ++s;
    Assert (*s == '<', "No root tag found");
    if (s[1] == '!')
      while (!(*s == '\0' || (s[-2] == '-' && s[-1] == '>'))) ++s;
    else if (s[1] == '?')
      while (!(*s == '\0' || s[-1] == '>')) ++s;
    else
      break;
  }
  return newXmlNodeFromTagBlock (&s);
}

xmlNode* xmlTreeFromFile (const char* filename) {
  const char* s;
  xmlNode* node;
  s = readStringFromFile (filename);
  node = xmlTreeFromString (s);
  SafeFree ((char*) s);
  return node;
}

void deleteXmlTree (xmlNode* root) {
  xmlNode *child;
  for (child = root->children; child; child = child->next)
    deleteXmlTree (child);
  SafeFreeOrNull ((char*) root->content);
  SafeFreeOrNull ((char*) root->name);
  SafeFreeOrNull (root);
}

const char* xmlTreeToString (const xmlNode* node) {
  xmlTextWriterPtr writer;
  writer = newXmlTextWriter();
  writeXmlNode (writer, node);
  return (const char*) deleteXmlTextWriterLeavingText (writer);
}

xmlTextWriterPtr newXmlTextWriter() {
  xmlTextWriterPtr writer;
  writer = SafeCalloc (1, sizeof (xmlTextWriter));
  writer->tagStack = newStringVector();
  xmlTextWriterWriteFormat (writer, "<?xml version=\"1.0\"?>\n");  /* hack, trying to mimic libxml2 as closely as possible... */
  return writer;
}

xmlChar* deleteXmlTextWriterLeavingText (xmlTextWriterPtr writer) {
  xmlChar* text;
  xmlTextWriterWriteFormat (writer, "\n");  /* hack, trying to mimic libxml2 as closely as possible... */
  text = writer->start;
  deleteStringVector (writer->tagStack);
  SafeFreeOrNull (writer);
  return text;
}

void xmlTextWriterVWriteFormat (xmlTextWriterPtr writer, const char* fmt, va_list argptr) {
  va_list argCopy;
  size_t bufUsed, bufSize, length;
  xmlChar* newStart;

  va_copy (argCopy, argptr);
  length = vsnprintf (NULL, 0, fmt, argptr);
  bufUsed = writer->cur - writer->start;
  bufSize = writer->end - writer->start;

  if (length + bufUsed >= bufSize) {
    if (bufSize == 0) bufSize = XML_WRITE_BUFFER_INITIAL_SIZE;
    while (bufSize < bufUsed + length + 1)
      bufSize *= 2;
    newStart = SafeMalloc (bufSize);
    if (writer->start)
      strncpy ((char*) newStart, (char*) writer->start, writer->end - writer->start);
    writer->cur = writer->cur - writer->start + newStart;
    writer->end = bufSize + newStart;
    SafeFreeOrNull (writer->start);
    writer->start = newStart;
  }

  vsnprintf ((char*) writer->cur, bufSize - bufUsed, fmt, argCopy);
  writer->cur += length;
}

void xmlTextWriterWriteFormat (xmlTextWriterPtr writer, const char* fmt, ...) {
  va_list argptr;
  va_start (argptr, fmt);
  xmlTextWriterVWriteFormat (writer, fmt, argptr);
  va_end (argptr);
}

void xmlTextWriterStartElementWithAttrs (xmlTextWriterPtr writer, const xmlChar* tag, const xmlAttr* prop) {
  StringVectorPushBack (writer->tagStack, (char*) tag);
  xmlTextWriterWriteFormat (writer, "<%s", tag);
  for (; prop; prop = prop->next)
    xmlTextWriterWriteFormat (writer, " %s=\"%s\"", prop->name, prop->content);
  xmlTextWriterWriteFormat (writer, ">");
}

void xmlTextWriterStartElement (xmlTextWriterPtr writer, const xmlChar* tag) {
  xmlTextWriterStartElementWithAttrs (writer, tag, NULL);
}

void xmlTextWriterWriteFormatElement (xmlTextWriterPtr writer, const xmlChar* tag, const char* fmt, ...) {
  va_list argptr;
  char* elementFmt;
  elementFmt = SafeMalloc (2*strlen((char*) tag) + 6 + strlen(fmt));
  sprintf (elementFmt, "<%s>%s</%s>", tag, fmt, tag);
  va_start (argptr, fmt);
  xmlTextWriterVWriteFormat (writer, elementFmt, argptr);
  va_end (argptr);
}

void xmlTextWriterEndElement (xmlTextWriterPtr writer) {
  xmlTextWriterWriteFormat (writer,
			    "</%s>",
			    StringVectorGet (writer->tagStack, StringVectorSize (writer->tagStack) - 1));
  VectorPop (writer->tagStack);
}

void xmlTextWriterFullEndElement (xmlTextWriterPtr writer) {
  xmlTextWriterEndElement(writer);
}

void writeXmlNode (xmlTextWriterPtr writer, const xmlNode* node) {
  xmlNode *child;
  if (node->type == XML_TEXT_NODE)
    xmlTextWriterWriteFormat (writer, "%s", node->content);
  else if (node->type == XML_ELEMENT_NODE) {
    xmlTextWriterStartElementWithAttrs (writer, node->name, node->properties);
    if (node->children)
      for (child = node->children; child; child = child->next)
	writeXmlNode (writer, child);
    xmlTextWriterEndElement (writer);
  }
}

int xmlValidateNameValue (const xmlChar* tag) {
  while (*tag != '\0') {
    const char c = *tag++;
    if (!isalpha(c) && c != '-' && c != '_')
      return 0;
  }
  return 1;
}
