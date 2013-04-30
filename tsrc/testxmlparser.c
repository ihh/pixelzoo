#include "util.h"
#include "xmlutil.h"
#include "xmlparser.h"

int main (int argc, char **argv) {
  const char *s, *t;
  xmlNode *node;

  Assert (argc == 2, "Usage: testxmlparser <file>");

  s = readStringFromFile (argv[1]);
  node = xmlTreeFromString (s);
  t = xmlTreeToString (node);

  printf ("%s", t);
  SafeFree ((char*) t);

  return 0;
}
