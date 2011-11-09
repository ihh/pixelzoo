#ifndef PIXELZOO_INCLUDED
#define PIXELZOO_INCLUDED

/* Typedefs */

/**
If defined, pointers to game objects will be void*.
If undefined, pointers to (otherwise-opaque) internal C data structures will be used.
*/
#define USE_VOID_TYPEDEFS

#ifdef USE_VOID_TYPEDEFS

/**
Pointer to an instance of the PixelZoo game machine, including board, tools, goals, etc.
 */
typedef void* pzGame;

/**
Pointer to a data structure describing a tool that the player can use to paint on the board.
 */
typedef void* pzTool;

/**
Pointer to a data structure describing a text balloon. Client can display these for in-game feedback.
 */
typedef void* pzBalloon;
#else  /* !defined(USE_VOID_TYPEDEFS) */
#include "xmlgame.h"
typedef Game* pzGame;
typedef Tool* pzTool;
typedef Balloon* pzBalloon;
#endif  /* defined(USE_VOID_TYPEDEFS) */

/* Functions */

/**
 Creates a game from an XML string.

   @param gameString XML string using the "game" DTD.
   Specifies the game and board.

   @param moveLogFlag If true, moves will be saved.
   This is required if the move is to be uploaded to a store,
   but can be a memory leak if the game is being run indefinitely.
*/
pzGame pzNewGameFromXmlString(const char*gameString,int moveLogFlag);

/**
 Creates a game and initialize the board from XML strings.

   @param gameString XML string using the "game" DTD.
   Specifies the game, excluding the board.

   @param boardString XML string describing the "board" element of the "game" DTD.
   Separately specifies the board.

   @param moveLogFlag If true, moves will be saved.
   This is required if the move is to be uploaded to a store,
   but can be a memory leak if the game is being run indefinitely.
*/
pzGame pzNewGameAndBoardFromXmlStrings(const char*gameString,const char*boardString,int moveLogFlag);

/**
 Destroys a game.
*/
void pzDeleteGame(pzGame);

/* The main game loop */

/**
 Starts the game.
 */
void pzStartGame(pzGame);

/**
 Tests whether the board is still evolving.

 NB it is possible for the board to be evolving but the player unable to do anything except watch it
 (e.g. if the game is in the GameLost state).
 */
int pzGameRunning(pzGame);

/**
Forces the game to stop. Future calls to pzGameRunning() will return false.
 */
void pzQuitGame(pzGame);

/**
 The main game loop.
 @param boardClockTimeLimit Set to zero (or a negative number) for no board-imposed time limit.
 @param callsPerSecond Set to zero (or negative) for no system-imposed time limit.
 */
void pzUpdateGame(pzGame,int callsPerSecond,long long boardClockTimeLimit);

/**
Returns the board clock measured in "microticks" (2^20 microticks = 1 expected update per cell).
 */
unsigned long long pzBoardClock(pzGame);

/**
The conversion factor from board clock units ("microticks") to expected updates per cell; equal to 2^20 */
#define pzBoardClockMicroticksPerTick 1048576


/* Information required to render the board */

/**
 Length of one side of the square board. */
int pzGetBoardSize(pzGame);

/**
The color of a cell.

Equivalent to a call to pzGetCellPaletteIndex() followed by a call to pzGetPaletteRgb()

@return A 24-bit RGB value */
int pzGetCellRgb(pzGame,int x,int y);

/**
The color of a cell.
@return the board's internal palette index; see pzGetPaletteRgb() */
int pzGetCellPaletteIndex(pzGame,int x,int y);


/* Description of the board palette */

/**
 Number of colors in the board palette */
int pzGetPaletteSize(pzGame);

/**
 24-bit RGB value for a color in the board palette */
int pzGetPaletteRgb(pzGame,int paletteIndex);


/* Information required to describe individual cells, for the "inspect" tool */

/**
 Name of an individual cell, can be used by an "inspect" tool */
const char* pzGetCellName(pzGame,int x,int y);

/**
 Color of an individual cell's description text, can be used by an "inspect" tool */
int pzGetCellNameRgb(pzGame,int x,int y);  /* returns 24-bit RGB */


/* Using the current tool on the board */

/**
 Player wants to use the currently-selected tool */
void pzTouchCell(pzGame,int x,int y);

/**
 Player wants to stop using the currently-selected tool */
void pzUntouchCell(pzGame);


/* Information required to render tools */

/**
 Number of tools available to the player */
int pzGetNumberOfTools(pzGame);

/**
 Find out which tool is currently selected.

 Change this via pzSelectTool() and pzUnselectTool().

 @return Index of currently-selected tool, or -1 if none is selected.
 */
int pzGetSelectedToolNumber(pzGame);  /* returns -1 if no tool selected. Change via pzSelectTool() and pzUnselectTool() */

/**
 Get a descriptor for a tool available to the player.
 @param toolNum The index of the tool to get
 @return A tool descriptor
 */
pzTool pzGetToolByNumber(pzGame,int toolNum);  /* use the returned pzTool for subsequent access to the tool */

/**
 Color of a particular tool
 @return A 24-bit RGB color
 */
int pzGetToolRgb(pzGame,pzTool);  /* returns 24-bit RGB */

/**
 Name of a particular tool.
 */
const char* pzGetToolName(pzTool);

/**
 Reserve level of a particular tool (i.e. how much the player has in reserve).
 @return A value from 0 to 1 */
double pzGetToolReserveLevel(pzTool);  /* returns a value from 0 to 1 */


/* Functions to select & deselect tools */

/**
 Function to select a particular tool */
void pzSelectTool(pzGame,int toolNum);

/**
 Function to unselect any tool */
void pzUnselectTool(pzGame);


/* Output text console */

/**
 Get size of output text console */
int pzGetNumberOfConsoleLines(pzGame);

/**
 Get a line from the output text console */
const char* pzGetConsoleText(pzGame,int lineNum);


/* Floating text balloons, over the board */

/**
 Number of floating text balloons */
int pzGetNumberOfBalloons(pzGame);

/**
 Get data structure describing a floating text balloon */
pzBalloon pzGetBalloonByNumber(pzGame,int balloonNum);  /* use the returned pzBalloon for subsequent access to the text balloon */

/**
 Get 24-bit RGB color of a floating text balloon */
int pzGetBalloonTextRgb(pzGame,pzBalloon);  /* returns 24-bit RGB */

/**
 Get text in a floating text balloon */
const char* pzGetBalloonText(pzBalloon);

/**
 Get X co-ordinate of a floating text balloon */
double pzGetBalloonXpos(pzBalloon);

/**
 Get Y co-ordinate of a floating text balloon */
double pzGetBalloonYpos(pzBalloon);

/**
 Get font size of a floating text balloon */
double pzGetBalloonCharSize(pzBalloon);

/**
 Get font spacing of a floating text balloon */
double pzGetBalloonCharSpacing(pzBalloon);

/**
 Get opacity of text in a floating text balloon */
double pzGetBalloonOpacity(pzBalloon);


/* Functions to save the game state */
/* The following two functions return strings that must be free'd */

/**
 Convert the player's move to an XML string.

 @return A string that must be free'd */
const char* pzSaveMoveAsXmlString(pzGame);  /* use to upload moves */

/**
 Convert the current board state to an XML string.

 Board state can then be restored with a call to pzNewGameAndBoardFromXmlStrings().

 @return A string that must be free'd */
const char* pzSaveBoardAsXmlString(pzGame);  /* use to save game state; restore with pzNewGameAndBoardFromXmlStrings */


/* Wrappers to general utility functions */

/**
 General utility function that throws an error and aborts */
void pzAbort(char* error);

/**
 General utility function that tests an assertion and, if it fails, throws an error and aborts */
void pzAssert(int assertion, char* error);

#endif /* PIXELZOO_INCLUDED */
