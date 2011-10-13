#ifndef PIXELZOO_INCLUDED
#define PIXELZOO_INCLUDED

typedef void* pzGame;
typedef void* pzTool;
typedef void* pzBalloon;

pzGame pzNewGameFromXmlString(const char*gameString);
pzGame pzRestoreBoardFromXmlString(const char*gameString,const char*boardString);

void pzDeleteGame(pzGame);

void pzStartGame(pzGame);
int pzGameRunning(pzGame);

void pzUpdateGame(pzGame,int callsPerSecond);  /* simplified wrapper for gameLoop */

int pzGetBoardSize(pzGame);
int pzGetCellRgb(pzGame,int x,int y);  /* returns 24-bit RGB */

const char* pzGetCellName(pzGame,int x,int y);
int pzGetCellNameRgb(pzGame,int x,int y);  /* returns 24-bit RGB */

int pzGetNumberOfTools(pzGame);
int getSelectedToolNumber(pzGame);  /* returns -1 if no Tool selected */
pzTool getToolByNumber(pzGame,int toolNum);

int pzGetToolRgb(pzGame,pzTool);  /* returns 24-bit RGB */

const char* pzGetToolName(pzTool);
double pzGetToolReserveLevel(pzTool);

void pzSelectTool(pzGame,int toolNum);
void pzUnselectTool(pzGame);

void pzTouchCell(pzGame,int x,int y);
void pzUntouchCell(pzGame);

int pzGetNumberOfConsoleLines(pzGame);
const char* pzGetConsoleText(pzGame,int lineNum);

int pzGetNumberOfBalloons(pzGame);
pzBalloon pzGetBalloonByNumber(pzGame,int balloonNum);

int pzGetBalloonTextRgb(pzGame,pzBalloon);  /* returns 24-bit RGB */

const char* pzGetBalloonText(pzBalloon);
double pzGetBalloonXpos(pzBalloon);
double pzGetBalloonYpos(pzBalloon);
double pzGetBalloonCharSize(pzBalloon);
double pzGetBalloonCharSpacing(pzBalloon);
double pzGetBalloonOpacity(pzBalloon);

const char* pzGetMoveAsXmlString(pzGame);  /* use to upload moves */
const char* pzSaveBoardAsXmlString(pzGame);  /* use to save game state; restore with pzRestoreBoardFromXmlString */

#endif /* PIXELZOO_INCLUDED */
