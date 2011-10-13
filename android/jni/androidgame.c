#include <androidgame.h>

//-----------------------------------------------------------------------------
// SYMBOLIC CONSTANTS
//-----------------------------------------------------------------------------
#define PIXELS_PER_CELL   4
const int COLOR_DEPTH     = 8;
const int RENDER_RATE = 50;

int launch( int argc, char *argv[], JNIEnv* env, jobject thiz )
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

  AndroidGame *AndroidGame = NULL;

  if (gameFilename == NULL) {
    LOGE ("Game file not specified");
  }

  AndroidGame = newAndroidGame(gameFilename, env, thiz);

  if (moveLogFilename != NULL) {
    logBoardMoves (AndroidGame->game->board);
  }

  LOGV("Starting game loop");
  while( gameRunning(AndroidGame->game) && (totalMicroticks < 0 || AndroidGame->game->board->microticks < totalMicroticks ))
    {
      /*const double targetUpdatesPerCell = AndroidGame->game->ticksPerSecond / (double) RENDER_RATE;
      const double maxElapsedTimeInSeconds = 0.5 * targetUpdatesPerCell / AndroidGame->game->ticksPerSecond;
      int64_Microticks targetMicroticks = FloatToIntMillionths (targetUpdatesPerCell);
      if (totalMicroticks >= 0)
    	targetMicroticks = MIN (totalMicroticks - AndroidGame->game->board->microticks, targetMicroticks);

      double updatesPerCell, evolveTime;
      int64_Microticks microticks;
      int actualUpdates;

      innerGameLoop(AndroidGame->game, targetMicroticks, maxElapsedTimeInSeconds, &microticks, &updatesPerCell, &actualUpdates, &evolveTime);
      // LOGV("About to render");
      // renderAndDelay(AndroidGame);
      // LOGV("Rendered"); // */

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
    }
  LOGV("Exiting");
  if (moveLogFilename) {
    xmlTextWriterPtr writer = xmlNewTextWriterFilename (moveLogFilename, 0);
    writeMoveList (AndroidGame->game->board->moveLog, writer, (xmlChar*) XMLZOO_LOG);
    xmlFreeTextWriter (writer);
  }
  LOGV("Finished writing moveLog");

  if (boardFilename) {
    boardReleaseRandomNumbers (AndroidGame->game->board);

    xmlTextWriterPtr writer = xmlNewTextWriterFilename (boardFilename, 0);
    writeBoard (AndroidGame->game->board, writer, 0);
    xmlFreeTextWriter (writer);
  }
  LOGV("Finished writing board file");

  if (revcompiledBoardFilename) {
    boardReleaseRandomNumbers (AndroidGame->game->board);

    xmlTextWriterPtr writer = xmlNewTextWriterFilename (revcompiledBoardFilename, 0);
    writeBoard (AndroidGame->game->board, writer, 1);
    xmlFreeTextWriter (writer);
  }
  LOGV("Finished writing revCompiledBoard");

  // deleteAndroidGame(AndroidGame);
  LOGV("AndroidGame released");

  // free(thisOpt); /* done with this item, free it */

  LOGV("Done cleanup");
  return 0;
}

//-----------------------------------------------------------------------------
// Name: newAndroidGame()
// Desc: 
//-----------------------------------------------------------------------------
AndroidGame* newAndroidGame( char *filename, JNIEnv* env, jobject thiz )
{
  PaletteIndex pal;

  AndroidGame *AndroidGame = SafeMalloc(sizeof(AndroidGame));

  //
  // Initialize Game...
  //
  LOGV("Making new game");
  AndroidGame->game = newGameFromXmlFile(filename);
  AndroidGame->game->selectedTool = AndroidGame->game->toolByName->root->left->value;
  LOGV("Made new game");

  /* init palette lookup */
  for (pal = 0; pal <= PaletteMax; ++pal) {
	  //LOGV("%i %i %i %i", pal, AndroidGame->game->board->palette.rgb[pal].r, AndroidGame->game->board->palette.rgb[pal].g, AndroidGame->game->board->palette.rgb[pal].b);
	  AndroidGame->sdlColor[pal] = (Uint32)((0xFF000000) | (AndroidGame->game->board->palette.rgb[pal].r << 24) | (AndroidGame->game->board->palette.rgb[pal].g << 16) | (AndroidGame->game->board->palette.rgb[pal].b));
  }

  /* JNI bindings */
  // AndroidGame->env = env;
  // AndroidGame->thiz = thiz;

  while(1) {

  }

  return AndroidGame;
}

//-----------------------------------------------------------------------------
// Name: deleteAndroidGame()
// Desc: 
//-----------------------------------------------------------------------------
void deleteAndroidGame( AndroidGame* AndroidGame )
{
  // SDL_FreeSurface( AndroidGame->g_screenSurface );
  deleteGame(AndroidGame->game);
  SafeFree(AndroidGame);
}

//-----------------------------------------------------------------------------
// Name: renderPixel()
// Desc: 
//-----------------------------------------------------------------------------
void renderPixel(AndroidGame* AndroidGame, int x, int y, Uint32 color)
{
	// do some magic here
	drawParticle(AndroidGame, x, y, color);
}


void renderAndDelay(AndroidGame* AndroidGame) {
  double renderPeriodInSeconds = 1. / RENDER_RATE, elapsedClockTime;
  clock_t start, now;
  start = clock();
  render(AndroidGame);
  now = clock();
  elapsedClockTime = ((double) now - start) / (double) CLOCKS_PER_SEC;
  LOGV("SDL delay?");
  SDL_Delay(1000 * MAX(0.,renderPeriodInSeconds - elapsedClockTime));
  LOGV("SDL delay works");
}

void render(AndroidGame* AndroidGame) {
  //
  // Plot each cell as a single pixel...
  //
  int x, y, i, j;
  int size = AndroidGame->game->board->size;
  for (x = 0; x < size; ++x)
    for (y = 0; y < size; ++y) {
      PaletteIndex pal = readBoardColor (AndroidGame->game->board, x, y);
      for (i = 0; i < PIXELS_PER_CELL; ++i)
	for (j = 0; j < PIXELS_PER_CELL; ++j)
	  renderPixel( AndroidGame, PIXELS_PER_CELL*x+i, PIXELS_PER_CELL*y+j, AndroidGame->sdlColor[pal] );
    }
}
