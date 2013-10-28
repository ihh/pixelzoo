#ifndef PIXELZOO_UTILS_INCLUDED
#define PIXELZOO_UTILS_INCLUDED

#include "pixelzoo.h"

/* Some helper functions that allow caller to read the whole board at one go */
int*** pzNewCellRgbArray(pzGame);  /* Allocates boardSize*boardSize array of int's */
void pzDeleteCellRgbArray(pzGame, int***cell);  /* Frees the array */

void pzReadCellRgbArray(pzGame,int***cell);  /* cell[x][y][z] <- pzGetCellRgb(pzGame,x,y,z)   (24-bit RGB) */

#endif /* PIXELZOO_UTILS_INCLUDED */
