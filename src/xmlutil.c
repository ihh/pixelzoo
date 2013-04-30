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

/* file I/O */
const char* readStringFromFile (const char* filename) {
  FILE* fp;
  long size;
  char* buf;
  int success;
  buf = NULL;
  if (filename) {
    success = 0;
    fp = fopen(filename, "r");
    if (fp) {
      fseek(fp, 0, SEEK_END);
      size = ftell(fp);
      fseek(fp, 0, SEEK_SET);
      buf = SafeMalloc(size + 1);
      if (fread (buf, size, 1, fp)) {
	success = 1;
	buf[size] = '\0';
      }
      fclose(fp);
    }
    if (!success) {
      fprintf (stderr, "Couldn't read file '%s'\n", filename);
      exit(1);
    }
  }
  return buf;
}

void writeStringToFile (const char* filename, const char* contents) {
  FILE* fp;
  int success;
  if (filename) {
    fp = fopen(filename, "w");
    if (fp) {
      if (contents)
	if (fwrite (contents, strlen(contents), 1, fp))
	  success = 1;
      fclose(fp);
    }
    if (!success) {
      fprintf (stderr, "Couldn't write file '%s'\n", filename);
      exit(1);
    }
  }
}

void writeStringToFileAndDelete (const char* filename, const char* contents) {
  writeStringToFile (filename, contents);
  SafeFree ((void*) contents);
}

/* private builder methods */
xmlNode* getNodeByName (xmlNode* node, char* name) {
  for (; node; node = node->next)
    if (node->type == XML_ELEMENT_NODE && strcmp ((const char*) node->name, name) == 0)
      return node;
  return (xmlNode*) NULL;
}

xmlChar* getNodeContent (xmlNode* node) {
  return (xmlChar*) node->children->content;
}

xmlChar* getNodeContentOrComplain (xmlNode* node, char* tag) {
  if (node == NULL) {
    if (tag)
      fprintf (stderr, "Missing tag: %s\n", tag);
    else
      fprintf (stderr, "Missing node\n");
    Abort("XML parse error");
  }
  if (node->children == NULL) {
    fprintf (stderr, "Missing children for tag: %s\n", tag);
    Abort("XML parse error");
  }
  return (xmlChar*) node->children->content;
}

xmlChar* getAttrByName (xmlNode* node, char* name) {
  xmlAttr* attr;
  for (attr = node->properties; attr; attr = attr->next)
    if (strcmp ((const char*) attr->name, name) == 0)
      return (xmlChar*) attr->children->content;
  return (xmlChar*) NULL;
}
