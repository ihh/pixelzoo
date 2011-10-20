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
  int64_Microticks totalMicroticks;

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

  androidGame = newAndroidGame(gameFilename, thiz);

  if (moveLogFilename != NULL) {
    // logBoardMoves (androidGame->game->board);
  }

  LOGV("Starting game loop");
  while( gameRunning(androidGame->game) && (totalMicroticks < 0 || androidGame->game->board->microticks < totalMicroticks ))
    {
      const double targetUpdatesPerCell = androidGame->game->ticksPerSecond / (double) RENDER_RATE;
      const double maxElapsedTimeInSeconds = 0.5 * targetUpdatesPerCell / androidGame->game->ticksPerSecond;
      int64_Microticks targetMicroticks = FloatToIntMillionths (targetUpdatesPerCell);
      if (totalMicroticks >= 0)
    	targetMicroticks = MIN (totalMicroticks - androidGame->game->board->microticks, targetMicroticks);

      double updatesPerCell, evolveTime;
      int64_Microticks microticks;
      int actualUpdates;

      innerGameLoop(androidGame->game, targetMicroticks, maxElapsedTimeInSeconds, &microticks, &updatesPerCell, &actualUpdates, &evolveTime);
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
    xmlTextWriterPtr writer = xmlNewTextWriterFilename (moveLogFilename, 0);
    writeMoveList (androidGame->game->board->moveLog, writer, (xmlChar*) XMLZOO_LOG);
    xmlFreeTextWriter (writer);
  }
  LOGV("Finished writing moveLog");

  if (boardFilename) {
    boardReleaseRandomNumbers (androidGame->game->board);

    xmlTextWriterPtr writer = xmlNewTextWriterFilename (boardFilename, 0);
    writeBoard (androidGame->game->board, writer, 0);
    xmlFreeTextWriter (writer);
  }
  LOGV("Finished writing board file");

  if (revcompiledBoardFilename) {
    boardReleaseRandomNumbers (androidGame->game->board);

    xmlTextWriterPtr writer = xmlNewTextWriterFilename (revcompiledBoardFilename, 0);
    writeBoard (androidGame->game->board, writer, 1);
    xmlFreeTextWriter (writer);
  }
  LOGV("Finished writing revCompiledBoard");

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
AndroidGame* newAndroidGame(char *filename, jobject thiz)
{
  PaletteIndex pal;

  AndroidGame *androidGame = SafeMalloc(sizeof(AndroidGame));

  //
  // Initialize Game...
  //
  LOGV("Making new game");
  androidGame->game = newGameFromXmlFile(filename);
  androidGame->game->selectedTool = androidGame->game->toolByName->root->left->value;
  LOGV("Made new game");

  /* init palette lookup */
  // LOGV("sizeof AndroidGame %i", sizeof(AndroidGame));
  for (pal = 0; pal <= PaletteMax; ++pal) {
	  // LOGV("%i %i %i %i", pal, androidGame->game->board->palette.rgb[pal].r, androidGame->game->board->palette.rgb[pal].g, androidGame->game->board->palette.rgb[pal].b);
	  androidGame->sdlColor[pal] = (Uint32)((0xFF000000) | (androidGame->game->board->palette.rgb[pal].r << 24) | (androidGame->game->board->palette.rgb[pal].g << 16) | (androidGame->game->board->palette.rgb[pal].b));
	  // androidGame->sdlColor[pal] = 0xFF00FF00;
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
  deleteGame(androidGame->game);
  SafeFree(androidGame);
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
