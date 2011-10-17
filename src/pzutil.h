#ifndef PIXELZOO_UTILS_INCLUDED
#define PIXELZOO_UTILS_INCLUDED

#include "pixelzoo.h"

/* Some helper functions that allow caller to read the whole board at one go */
int** pzNewCellRgbArray(pzGame);  /* Allocates boardSize*boardSize array of int's */
void pzDeleteCellRgbArray(pzGame);  /* Frees the array */

void pzWriteCellRgbArray(pzGame,int**cell);  /* cell[x][y] = pzGetCellRgb(pzGame,x,y) is 24-bit RGB */

/* Currently unimplemented: file-based versions of the XmlString functions */
pzGame pzNewGameFromXmlFile(const char*gameFilename,int moveLogFlag);
pzGame pzNewGameAndBoardFromXmlFiles(const char*gameFilename,const char*boardFilename,int moveLogFlag);
const char* pzSaveMoveAsXmlFile(pzGame,const char*filename);  /* use to store moves */
const char* pzSaveBoardAsXmlFile(pzGame,const char*filename);  /* use to store game state; restore with pzRestoreBoardFromXmlString */

#endif /* PIXELZOO_UTILS_INCLUDED */
