#include <stdio.h>
#include <stdlib.h>

#include <time.h>

#include "xmlutil.h"
#include "fileio.h"
#include "proto.h"
#include "xmlboard.h"

//-----------------------------------------------------------------------------
// PROTOTYPES
//-----------------------------------------------------------------------------
int main(int argc, char *argv[]);
void expand (ProtoTable *protoTable, xmlNode *node, const char* xpath);

// Functions
int main( int argc, char *argv[] )
{
  char *gameFilename;

  /* get options */
  if (argc != 2)
    Abort ("Usage: %s gamefile.xml", argv[0]);

  gameFilename = argv[1];

  xmlDoc* doc;
  doc = xmlTreeFromFile (gameFilename);

  ProtoTable *protoTable = newProtoTable();
  expand (protoTable, doc, "/");

  const char* out = xmlTreeToString (doc);
  printf ("%s", out);

  return 0;
}

void expand (ProtoTable *protoTable, xmlNode *node, const char* xpath) {
  xmlNode *child, *scheme, *prev;
  char *nodeXpath;
  if (MATCHES(node,PARTICLE))
    protoTableSetSelfType (protoTable, (const char*) CHILDSTRING(node,NAME));
  if (node->children) {
    nodeXpath = SafeMalloc (strlen(xpath) + strlen((const char*) node->name) + 2);
    sprintf (nodeXpath, "%s/%s", xpath, node->name);
    Warn ("Scanning %s", nodeXpath);
    prev = node;
    child = node->children;
    while (child) {
      if ( (scheme = CHILD(child,SCHEME)) ) {  /* assignment intentional */
	Warn ("Expanding %s/%s/%s: %s", node->name, child->name, scheme->name, (const char*) getNodeContent(scheme));
	if (protoTableExpandSchemeNode (protoTable, scheme, child, node)) {
	  child = (prev == node ? node->children : prev);
	  Warn (" (backing up)");
	  continue;
	} else
	  Abort ("Expansion failed");
      }
      prev = child;
      child = child->next;
    }
    for (child = node->children; child; child = child->next)
      expand (protoTable, child, nodeXpath);
    SafeFree (nodeXpath);
  }
}
