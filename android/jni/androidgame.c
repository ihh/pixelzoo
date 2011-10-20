#include <androidgame.h>

//-----------------------------------------------------------------------------
// SYMBOLIC CONSTANTS
//-----------------------------------------------------------------------------
#define PIXELS_PER_CELL   4
const int COLOR_DEPTH     = 8;
const int RENDER_RATE = 50;

int launch( int argc, char *argv[], jobject thiz )
{
  char *gameFilename, *moveLogFilename, *boardFilename, *revcompiledBoardFilename;
  int userInputAllowed;
  option_t *optList, *thisOpt;
  Uint64 totalMicroticks;

  /* parse list of command line options and their arguments */
  optList = NULL;
  LOGV("Loading Options");
  optList = GetOptList(argc, argv, "g:t:l:b:r:dh?");

  /* get options */
  gameFilename = NULL;
  moveLogFilename = NULL;
  boardFilename = NULL;
  revcompiledBoardFilename = NULL;
  totalMicroticks = -1;
  userInputAllowed = 1;

  LOGV("Processing Options");
  while (optList != NULL)
  {
      thisOpt = optList;
      optList = optList->next;

      if ('?' == thisOpt->option || 'h' == thisOpt->option) {
          printf("Usage: %s <options>\n\n", argv[0]);
          printf("options:\n");
          printf("     -g : specify input XML file descr*ibing game/board (mandatory).\n");
          printf("     -l : specify output XML file for move log (optional).\n");
          printf("     -b : specify output XML file for board (optional).\n");
          printf("     -r : specify output XML file for reverse-compiled board (optional).\n");
          printf("     -t : specify simulation time limit in microticks (optional).\n");
          printf("     -d : disable user input (optional).\n");
          printf(" -h, -? : print out command line options.\n\n");

          FreeOptList(thisOpt); /* done with this list, free it */
          break;
      } else if ('g' == thisOpt->option) {
          gameFilename = thisOpt->argument;
      } else if ('l' == thisOpt->option) {
          moveLogFilename = thisOpt->argument;
      } else if ('b' == thisOpt->option) {
          boardFilename = thisOpt->argument;
      } else if ('r' == thisOpt->option) {
          revcompiledBoardFilename = thisOpt->argument;
      } else if ('t' == thisOpt->option) {
          totalMicroticks = decToSignedLongLong (thisOpt->argument);
      } else if ('d' == thisOpt->option) {
          userInputAllowed = 0;
      }
  }

  AndroidGame *androidGame = NULL;

  if (gameFilename == NULL) {
    LOGE ("Game file not specified");
  }

  androidGame = newAndroidGame(gameFilename, thiz, moveLogFilename != NULL);

  if (moveLogFilename != NULL) {
    // logBoardMoves (androidGame->game->board);
  }

  LOGV("Starting game loop");
  while( pzGameRunning(androidGame->game) && (totalMicroticks < 0 || pzBoardClock(androidGame->game) < totalMicroticks ) )
  {
      pzUpdateGame (androidGame->game, RENDER_RATE, totalMicroticks);
      LOGV("Rendered");
      renderAndDelay(androidGame);
      // */

      /*SDL_Event event;

      while( SDL_PollEvent( &event ) )
	if (userInputAllowed)
	  {
	    switch( event.type ) 
	      {
	      case SDL_QUIT:
		quitGame(AndroidGame->game);
		break;

	      case SDL_KEYDOWN:
		if( event.key.keysym.sym == SDLK_ESCAPE ) 
		  quitGame(AndroidGame->game);
		break;

	      case SDL_MOUSEMOTION:
		AndroidGame->game->toolPos.x = event.motion.x / PIXELS_PER_CELL;
		AndroidGame->game->toolPos.y = event.motion.y / PIXELS_PER_CELL;
		break;

	      case SDL_MOUSEBUTTONUP:
		if ( event.button.button == SDL_BUTTON_LEFT)
		  AndroidGame->game->toolActive = 0;
		break;

	      case SDL_MOUSEBUTTONDOWN:
		if ( event.button.button == SDL_BUTTON_LEFT) {
		  AndroidGame->game->toolActive = 1;
		  AndroidGame->game->lastToolPos = AndroidGame->game->toolPos;
		}
		break;

	      default:
		break;
	      }
	  }*/


      // test: do only one loop
      // quitGame(androidGame->game);
    }
  LOGV("Exiting");
  if (moveLogFilename) {
      const char* moveStr = pzSaveMoveAsXmlString(androidGame->game);
      writeStringToFile (moveLogFilename, moveStr);
      if (moveStr)
        free ((void*) moveStr);
  }
  LOGV("Finished writing moveLog");

  if (boardFilename) {
      const char* boardStr = pzSaveBoardAsXmlString(androidGame->game);
          writeStringToFile (boardFilename, boardStr);
          if (boardStr)
            free ((void*) boardStr);
  }
  LOGV("Finished writing board file");

  deleteAndroidGame(androidGame);
  LOGV("AndroidGame released");

  free(thisOpt); /* done with this item, free it */

  LOGV("Done cleanup");
  return 0;
}

//-----------------------------------------------------------------------------
// Name: newAndroidGame()
// Desc: 
//-----------------------------------------------------------------------------
AndroidGame* newAndroidGame(char *filename, jobject thiz, int logMoves)
{
  int pal;

  AndroidGame *androidGame = malloc(sizeof(AndroidGame));

  //
  // Initialize Game...
  //
  LOGV("Making new game");
  //androidGame->game = newGameFromXmlFile(filename);
  //androidGame->game->selectedTool = androidGame->game->toolByName->root->left->value;
  const char* gameString = readStringFromFile(filename);
  androidGame->game = pzNewGameFromXmlString(gameString,logMoves);
  free ((void*) gameString);
  LOGV("Made new game");

  /* init palette lookup */
  // LOGV("sizeof AndroidGame %i", sizeof(AndroidGame));
  androidGame->sdlColor = malloc (pzGetPaletteSize(androidGame->game) * sizeof(Uint32));
  for (pal = 0; pal <= pzGetPaletteSize(androidGame->game); ++pal) {
      int rgb = pzGetPaletteRgb (androidGame->game, pal);
	  androidGame->sdlColor[pal] = (Uint32)((0xFF000000) | rgb);
  }

  /* JNI bindings */
  androidGame->thiz = thiz;

  return androidGame;
}

//-----------------------------------------------------------------------------
// Name: deleteAndroidGame()
// Desc: 
//-----------------------------------------------------------------------------
void deleteAndroidGame( AndroidGame* androidGame )
{
  // SDL_FreeSurface( AndroidGame->g_screenSurface );
  pzDeleteGame(androidGame->game);
  free(androidGame->sdlColor);
  free(androidGame);
}

void renderAndDelay(AndroidGame* androidGame) {
  double renderPeriodInSeconds = 1. / RENDER_RATE, elapsedClockTime;
  clock_t start, now;
  start = clock();
  render(androidGame);
  now = clock();
  elapsedClockTime = ((double) now - start) / (double) CLOCKS_PER_SEC;
  SDL_Delay(1000 * MAX(0.,renderPeriodInSeconds - elapsedClockTime));
}

// Obsolete
void renderPixel(AndroidGame* androidGame, int x, int y, Uint32 color)
{
	drawParticle(androidGame, x, y, color);
}

void render(AndroidGame* androidGame) {
  // drawBoardTwoDTest(androidGame);
  drawBoard(androidGame);
}
