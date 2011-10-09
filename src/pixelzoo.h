#ifndef PIXELZOO_INCLUDED
#define PIXELZOO_INCLUDED

#include "xmlgame.h"

Game* pzNewGameFromXmlString(const char*gameString);
Game* pzRestoreBoardFromXmlString(const char*gameString,const char*boardString);

void pzDeleteGame(Game*);

void pzStartGame(Game*);
int pzGameRunning(Game*);

void pzUpdateGame(Game*,int callsPerSecond);  /* simplified wrapper for gameLoop */

int pzGetBoardSize(Game*);
int pzGetCellRgb(Game*,int x,int y);  /* returns 24-bit RGB */

const char* pzGetCellName(Game*,int x,int y);
int pzGetCellNameRgb(Game*,int x,int y);  /* returns 24-bit RGB */

int pzGetNumberOfTools(Game*);
int getSelectedToolNumber(Game*);  /* returns -1 if no Tool selected */
Tool* getToolByNumber(Game*,int toolNum);

int pzGetToolRgb(Game*,Tool*);  /* returns 24-bit RGB */

const char* pzGetToolName(Tool*);
double pzGetToolReserveLevel(Tool*);

void pzSelectTool(Game*,int toolNum);
void pzUnselectTool(Game*);

void pzTouchCell(Game*,int x,int y);
void pzUntouchCell(Game*);

int pzGetNumberOfConsoleLines(Game*);
const char* pzGetConsoleText(Game*,int lineNum);

int pzGetNumberOfBalloons(Game*);
Balloon* pzGetBalloonByNumber(Game*,int balloonNum);

int pzGetBalloonTextRgb(Game*,Balloon*);  /* returns 24-bit RGB */

const char* pzGetBalloonText(Balloon*);
double pzGetBalloonXpos(Balloon*);
double pzGetBalloonYpos(Balloon*);
double pzGetBalloonCharSize(Balloon*);
double pzGetBalloonCharSpacing(Balloon*);
double pzGetBalloonOpacity(Balloon*);

const char* pzGetMoveAsXmlString(Game*);  /* use to upload moves */
const char* pzSaveBoardAsXmlString(Game*);  /* use to save game state; restore with pzRestoreBoardFromXmlString */

#endif /* PIXELZOO_INCLUDED */
