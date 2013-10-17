#include <string.h>
#include "fileio.h"
#include "xmlboard.h"
#include "xmlgame.h"

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
  success = 0;
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

xmlNode* xmlTreeFromFile (const char* filename) {
  const char* s;
  xmlNode* node;
  s = readStringFromFile (filename);
  node = xmlTreeFromString (s);
  SafeFree ((char*) s);
  return node;
}

Board* newBoardFromXmlFile (void *game, const char* filename) {
  xmlDoc* doc;
  Board* board = NULL;
  doc = xmlTreeFromFile (filename);
  if (doc)
    board = newBoardFromXmlDocument (game, doc);
  return board;
}

Game* newGameFromXmlFile (const char* filename) {
  xmlDoc* doc;
  Game* game = NULL;
  doc = xmlTreeFromFile (filename);
  Assert (doc != NULL, "XML file not found");
  if (doc)
    game = newGameFromXmlDocument (doc);
  return game;
}

