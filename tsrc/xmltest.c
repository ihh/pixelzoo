#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libxml/tree.h>

int main (int argc, char** argv) {
  xmlDoc* doc;
  if (argc != 2)
    fprintf (stderr, "Usage: %s <XML filename>\n", argv[0]);
  else {
    doc = xmlReadFile (argv[1], NULL, 0);
    printf ("OK\n");
  }
  return 0;
}
