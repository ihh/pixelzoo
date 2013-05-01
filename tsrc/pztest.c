#include <stdio.h>
#include <stdlib.h>

#include <time.h>

#include "pixelzoo.h" /* main PixelZoo API */
#include "xmlutil.h"  /* used only for string to 64-bit integer conversion */
#include "fileio.h"   /* file I/O helpers */
#include "optlist.h"  /* option parsing */

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

//-----------------------------------------------------------------------------
// SYMBOLIC CONSTANTS
//-----------------------------------------------------------------------------
const int GAME_LOOP_CALL_RATE = 0;

//-----------------------------------------------------------------------------
// PROTOTYPES
//-----------------------------------------------------------------------------
int main(int argc, char *argv[]);
pzGame newTestGame(char *filename,int logMoves);
void deleteTestGame(pzGame);

//-----------------------------------------------------------------------------
// Name: main()
// Desc: 
//-----------------------------------------------------------------------------
int main( int argc, char *argv[] )
{
  char *gameFilename, *moveLogFilename, *boardFilename;
  unsigned long long totalMicroticks;
  option_t *optList, *thisOpt;

  /* parse list of command line options and their arguments */
  optList = NULL;
  optList = GetOptList(argc, argv, "g:t:l:b:r:dh?");

  /* get options */
  gameFilename = NULL;
  moveLogFilename = NULL;
  boardFilename = NULL;
  totalMicroticks = 0;
  while (optList != NULL)
    {
      thisOpt = optList;
      optList = optList->next;

      if ('?' == thisOpt->option || 'h' == thisOpt->option) {
	printf("Usage: %s <options>\n\n", argv[0]);
	printf("options:\n");
	printf("     -g : specify input XML file describing game/board (mandatory).\n");
	printf("     -t : specify simulation time limit in microticks (mandatory).\n");
	printf("     -l : specify output XML file for move log (optional).\n");
	printf("     -b : specify output XML file for board (optional).\n");
	printf(" -h, -? : print out command line options.\n\n");

	FreeOptList(thisOpt); /* done with this list, free it */
	break;

      } else if ('g' == thisOpt->option) {
	gameFilename = thisOpt->argument;

      } else if ('l' == thisOpt->option) {
	moveLogFilename = thisOpt->argument;

      } else if ('b' == thisOpt->option) {
	boardFilename = thisOpt->argument;

      } else if ('t' == thisOpt->option) {
	totalMicroticks = decToSignedLongLong (thisOpt->argument);
      }
    }

  pzGame game = NULL;

  if (gameFilename == NULL) {
    pzAbort ("Game file not specified");
  }

  if (totalMicroticks == 0) {
    pzAbort ("Time limit not specified");
  }
  
  game = newTestGame (gameFilename, moveLogFilename != NULL);
  while( pzGameRunning(game) && (totalMicroticks == 0 || pzBoardClock(game) < totalMicroticks ) )
    pzUpdateGame (game, GAME_LOOP_CALL_RATE, totalMicroticks);

  if (moveLogFilename) {
    const char* moveStr = pzSaveMoveAsXmlString(game);
    writeStringToFile (moveLogFilename, moveStr);
    if (moveStr)
      free ((void*) moveStr);
  }

  if (boardFilename) {
    const char* boardStr = pzSaveBoardAsXmlString(game);
    writeStringToFile (boardFilename, boardStr);
    if (boardStr)
      free ((void*) boardStr);
  }

  deleteTestGame(game);

  return 0;
}

//-----------------------------------------------------------------------------
// Name: newTestGame()
// Desc: 
//-----------------------------------------------------------------------------
pzGame newTestGame( char *filename, int logMoves )
{
  pzGame game;

  const char* gameString = readStringFromFile(filename);
  game = pzNewGameFromXmlString(gameString,logMoves);
  free ((void*) gameString);

  if (pzGetNumberOfTools(game) > 0)
    pzSelectTool(game,0);

  /* return */
  return game;
}

//-----------------------------------------------------------------------------
// Name: deleteTestGame()
// Desc: 
//-----------------------------------------------------------------------------
void deleteTestGame( pzGame game )
{
  pzDeleteGame(game);
}
