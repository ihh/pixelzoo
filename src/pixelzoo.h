#ifndef PIXELZOO_INCLUDED
#define PIXELZOO_INCLUDED

/* Typedefs */

#undef USE_VOID_TYPEDEFS
/* Uncomment next line to use explicit typedefs */
/*
#define USE_VOID_TYPEDEFS
*/

#ifdef USE_VOID_TYPEDEFS
typedef void* pzGame;
typedef void* pzTool;
typedef void* pzBalloon;
#else
#include "xmlgame.h"
typedef Game* pzGame;
typedef Tool* pzTool;
typedef Balloon* pzBalloon;
#endif

/* Functions */

/* Functions to create & destroy a game.
   If moveLogFlag is true, moves will be saved,
   which is required if the move is to be uploaded to a store,
   but can be a memory leak if the game is being run indefinitely.
*/
pzGame pzNewGameFromXmlString(const char*gameString,int moveLogFlag);
pzGame pzNewGameAndBoardFromXmlStrings(const char*gameString,const char*boardString,int moveLogFlag);

void pzDeleteGame(pzGame);

/* The main game loop */
void pzStartGame(pzGame);
int pzGameRunning(pzGame);

void pzUpdateGame(pzGame,int callsPerSecond);  /* simplified wrapper for gameLoop */

/* Information required to render the board */
int pzGetBoardSize(pzGame);
int pzGetCellRgb(pzGame,int x,int y);  /* returns 24-bit RGB */

/* Information required to describe individual cells, for the "inspect" tool */
const char* pzGetCellName(pzGame,int x,int y);
int pzGetCellNameRgb(pzGame,int x,int y);  /* returns 24-bit RGB */

/* Using the current tool on the board */
void pzTouchCell(pzGame,int x,int y);
void pzUntouchCell(pzGame);

/* Information required to render tools */
int pzGetNumberOfTools(pzGame);
int getSelectedToolNumber(pzGame);  /* returns -1 if no Tool selected */
pzTool getToolByNumber(pzGame,int toolNum);

int pzGetToolRgb(pzGame,pzTool);  /* returns 24-bit RGB */

const char* pzGetToolName(pzTool);
double pzGetToolReserveLevel(pzTool);

/* Functions to select & deselect tools */
void pzSelectTool(pzGame,int toolNum);
void pzUnselectTool(pzGame);

/* Output text console */
int pzGetNumberOfConsoleLines(pzGame);
const char* pzGetConsoleText(pzGame,int lineNum);

/* Floating text balloons, over the board */
int pzGetNumberOfBalloons(pzGame);
pzBalloon pzGetBalloonByNumber(pzGame,int balloonNum);

int pzGetBalloonTextRgb(pzGame,pzBalloon);  /* returns 24-bit RGB */

const char* pzGetBalloonText(pzBalloon);
double pzGetBalloonXpos(pzBalloon);
double pzGetBalloonYpos(pzBalloon);
double pzGetBalloonCharSize(pzBalloon);
double pzGetBalloonCharSpacing(pzBalloon);
double pzGetBalloonOpacity(pzBalloon);

/* Functions to save the game state */
/* The following two functions return strings that must be free'd */
const char* pzSaveMoveAsXmlString(pzGame);  /* use to upload moves */
const char* pzSaveBoardAsXmlString(pzGame);  /* use to save game state; restore with pzRestoreBoardFromXmlString */

#endif /* PIXELZOO_INCLUDED */
