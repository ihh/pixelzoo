#ifndef FILE_IO_INCLUDED
#define FILE_IO_INCLUDED

#include "xmlparser.h"
#include "board.h"
#include "game.h"

/* Functions to read/write files */
const char* readStringFromFile (const char* filename);  /* caller must free the string */
void writeStringToFile (const char* filename, const char* contents);
void writeStringToFileAndDelete (const char* filename, const char* contents);

/* File-accessing wrappers to various XML parser functions */
/* XML file -> XML tree */
xmlNode* xmlTreeFromFile (const char* filanem);

/* XML Board file -> Board object */
Board* newBoardFromXmlFile (void *game, const char* filename);

/* XML Game file -> Game object */
Game* newGameFromXmlFile (const char* filename);

#endif /* FILE_IO_INCLUDED */
