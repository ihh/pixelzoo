#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>

#include <time.h>

#include "pixelzoo.h"
#include "xmlutil.h"  /* used only for string to 64-bit integer conversion & file I/O helpers */
#include "optlist.h"

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

//-----------------------------------------------------------------------------
// SYMBOLIC CONSTANTS
//-----------------------------------------------------------------------------
#define PIXELS_PER_CELL   4
const int COLOR_DEPTH     = 8;
const int RENDER_RATE = 50;

//-----------------------------------------------------------------------------
// GLOBALS
//-----------------------------------------------------------------------------

typedef struct SDLGame {
  pzGame game;
  Uint32* sdlColor;
  SDL_Surface *g_screenSurface;
} SDLGame;

//-----------------------------------------------------------------------------
// PROTOTYPES
//-----------------------------------------------------------------------------
int main(int argc, char *argv[]);
SDLGame* newSDLGame(char *filename,int logMoves);
void deleteSDLGame(SDLGame*);
void render(SDLGame*);
void renderAndDelay(SDLGame*);
void renderPixel(SDL_Surface*, int x, int y, Uint32 color);

int evolveThreadFunc ( void *voidGame );
int renderThreadFunc( void *voidSdlGame );

//-----------------------------------------------------------------------------
// Name: main()
// Desc: 
//-----------------------------------------------------------------------------
int main( int argc, char *argv[] )
{
  char *gameFilename, *moveLogFilename, *boardFilename;
  Uint64 totalMicroticks;
  int userInputAllowed;
  option_t *optList, *thisOpt;

  /* parse list of command line options and their arguments */
  optList = NULL;
  optList = GetOptList(argc, argv, "g:t:l:b:r:dh?");

  /* get options */
  gameFilename = NULL;
  moveLogFilename = NULL;
  boardFilename = NULL;
  userInputAllowed = 1;
  totalMicroticks = 0;
  while (optList != NULL)
    {
      thisOpt = optList;
      optList = optList->next;

      if ('?' == thisOpt->option || 'h' == thisOpt->option) {
	printf("Usage: %s <options>\n\n", argv[0]);
	printf("options:\n");
	printf("     -g : specify input XML file describing game/board (mandatory).\n");
	printf("     -t : specify simulation time limit in microticks (optional).\n");
	printf("     -l : specify output XML file for move log (optional).\n");
	printf("     -b : specify output XML file for board (optional).\n");
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

      } else if ('t' == thisOpt->option) {
	totalMicroticks = decToSignedLongLong (thisOpt->argument);

      } else if ('d' == thisOpt->option) {
	userInputAllowed = 0;
      }
    }

  SDLGame *sdlGame = NULL;

  if (gameFilename == NULL) {
    pzAbort ("Game file not specified");
  }
  
  printf ("Loading XML game file...\n");

  sdlGame = newSDLGame (gameFilename, moveLogFilename != NULL);

  printf ("Game file successfully loaded.\n\nPress up- and down-arrow to cycle through tools, or 't' to list all available tools.\n(Any further text beyond this point comes from the game file.)\n======================================================================\n\n");

  while( pzGameRunning(sdlGame->game) && (totalMicroticks == 0 || pzBoardClock(sdlGame->game) < totalMicroticks ) )
    {
      pzUpdateGame (sdlGame->game, RENDER_RATE, totalMicroticks);
      renderAndDelay (sdlGame);

      SDL_Event event;
      int tools, t, toolNum;
      pzTool pzt;

      while( SDL_PollEvent( &event ) )
	if (userInputAllowed)
	  {
	    switch( event.type ) 
	      {
	      case SDL_QUIT:
		pzQuitGame(sdlGame->game);
		break;

	      case SDL_KEYDOWN:
		switch (event.key.keysym.sym) {

		case SDLK_ESCAPE:
		  pzQuitGame(sdlGame->game);
		  break;

		case SDLK_t:
		  tools = pzGetNumberOfTools(sdlGame->game);
		  toolNum = pzGetSelectedToolNumber(sdlGame->game);
		  printf("Available tools:\n");
		  for (t = 0; t < tools; ++t) {
		    pzt = pzGetToolByNumber(sdlGame->game,t);
		    printf ("Tool number: %d   Name: %s   Reserve: %g  %s\n", t+1, pzGetToolName(pzt), pzGetToolReserveLevel(pzt), t==toolNum ? "(currently selected)" : "");
		  }
		  printf("\n");
		  break;

		case SDLK_UP:
		  tools = pzGetNumberOfTools(sdlGame->game);
		  toolNum = (pzGetSelectedToolNumber(sdlGame->game) + 1) % tools;
		  pzSelectTool (sdlGame->game, toolNum);
		  pzt = pzGetToolByNumber(sdlGame->game,toolNum);
		  printf ("Selected tool (#%d): %s   Reserve: %g\n", toolNum + 1, pzGetToolName(pzt), pzGetToolReserveLevel(pzt));
		  break;

		case SDLK_DOWN:
		  tools = pzGetNumberOfTools(sdlGame->game);
		  toolNum = (pzGetSelectedToolNumber(sdlGame->game) + tools - 1) % tools;
		  pzSelectTool (sdlGame->game, toolNum);
		  pzt = pzGetToolByNumber(sdlGame->game,toolNum);
		  printf ("Selected tool (#%d): %s   Reserve: %g\n", toolNum + 1, pzGetToolName(pzt), pzGetToolReserveLevel(pzt));
		  break;

		default:
		  break;
		}
		break;

	      case SDL_MOUSEMOTION:
		if (event.motion.state & SDL_BUTTON(1))
		  pzTouchCell (sdlGame->game, event.motion.x / PIXELS_PER_CELL, event.motion.y / PIXELS_PER_CELL);
		break;

	      case SDL_MOUSEBUTTONUP:
		if ( event.button.button == SDL_BUTTON_LEFT)
		  pzUntouchCell (sdlGame->game);
		break;
		
	      case SDL_MOUSEBUTTONDOWN:
		if ( event.button.button == SDL_BUTTON_LEFT)
		  pzTouchCell (sdlGame->game, event.button.x / PIXELS_PER_CELL, event.button.y / PIXELS_PER_CELL);
		break;

	      default:
		break;
	      }
	  }
    }

  if (moveLogFilename) {
    const char* moveStr = pzSaveMoveAsXmlString(sdlGame->game);
    writeStringToFile (moveLogFilename, moveStr);
    if (moveStr)
      free ((void*) moveStr);
  }

  if (boardFilename) {
    const char* boardStr = pzSaveBoardAsXmlString(sdlGame->game);
    writeStringToFile (boardFilename, boardStr);
    if (boardStr)
      free ((void*) boardStr);
  }

  deleteSDLGame(sdlGame);

  return 0;
}

//-----------------------------------------------------------------------------
// Name: newSDLGame()
// Desc: 
//-----------------------------------------------------------------------------
SDLGame* newSDLGame( char *filename, int logMoves )
{
  int pal;

  SDLGame *sdlGame = malloc (sizeof (SDLGame));
  sdlGame->g_screenSurface = NULL;
  //
  // Initialize Game...
  //

  const char* gameString = readStringFromFile(filename);
  sdlGame->game = pzNewGameFromXmlString(gameString,logMoves);
  free ((void*) gameString);

  if (pzGetNumberOfTools(sdlGame->game) > 0)
    pzSelectTool(sdlGame->game,0);

  /* init SDL */
  if( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
      printf( "Unable to init SDL: %s\n", SDL_GetError() );
      exit(1);
    }

  atexit( SDL_Quit );

  int size = pzGetBoardSize(sdlGame->game);
  sdlGame->g_screenSurface = SDL_SetVideoMode( size * PIXELS_PER_CELL,
					       size * PIXELS_PER_CELL,
					       COLOR_DEPTH, 
					       SDL_HWSURFACE | SDL_DOUBLEBUF );

  if( sdlGame->g_screenSurface == NULL )
    {
      printf( "Unable to set video: %s\n", SDL_GetError() );
      exit(1);
    }

  /* init palette lookup */
  sdlGame->sdlColor = malloc (pzGetPaletteSize(sdlGame->game) * sizeof(Uint32));
  for (pal = 0; pal <= pzGetPaletteSize(sdlGame->game); ++pal) {
    int rgb = pzGetPaletteRgb (sdlGame->game, pal);
    sdlGame->sdlColor[pal] = SDL_MapRGB( sdlGame->g_screenSurface->format, (rgb >> 16) & 0xff, (rgb >> 8) & 0xff, rgb & 0xff );
  }

  /* return */
  return sdlGame;
}

//-----------------------------------------------------------------------------
// Name: deleteSDLGame()
// Desc: 
//-----------------------------------------------------------------------------
void deleteSDLGame( SDLGame* sdlGame )
{
  SDL_FreeSurface( sdlGame->g_screenSurface );
  pzDeleteGame(sdlGame->game);
  free (sdlGame->sdlColor);
  free (sdlGame);
}

//-----------------------------------------------------------------------------
// Name: renderPixel()
// Desc: 
//-----------------------------------------------------------------------------
void renderPixel( SDL_Surface *g_screenSurface, int x, int y, Uint32 color )
{
  switch( g_screenSurface->format->BytesPerPixel )
    {
    case 1: // Assuming 8-bpp
      {
	Uint8 *bufp;
	bufp = (Uint8 *)g_screenSurface->pixels + y*g_screenSurface->pitch + x;
	*bufp = color;
      }
      break;

    case 2: // Probably 15-bpp or 16-bpp
      {
	Uint16 *bufp;
	bufp = (Uint16 *)g_screenSurface->pixels + y*g_screenSurface->pitch/2 + x;
	*bufp = color;
      }
      break;

    case 3: // Slow 24-bpp mode, usually not used
      {
	Uint8 *bufp;
	bufp = (Uint8 *)g_screenSurface->pixels + y*g_screenSurface->pitch + x * 3;

	if( SDL_BYTEORDER == SDL_LIL_ENDIAN )
	  {
	    bufp[0] = color;
	    bufp[1] = color >> 8;
	    bufp[2] = color >> 16;
	  } 
	else 
	  {
	    bufp[2] = color;
	    bufp[1] = color >> 8;
	    bufp[0] = color >> 16;
	  }
      }
      break;

    case 4: // Probably 32-bpp
      {
	Uint32 *bufp;
	bufp = (Uint32 *)g_screenSurface->pixels + y*g_screenSurface->pitch/4 + x;
	*bufp = color;
      }
      break;
    }
}


void renderAndDelay(SDLGame* sdlGame) {
  double renderPeriodInSeconds = 1. / RENDER_RATE, elapsedClockTime;
  clock_t start, now;
  start = clock();
  render (sdlGame);
  now = clock();
  elapsedClockTime = ((double) now - start) / (double) CLOCKS_PER_SEC;
  SDL_Delay(1000 * MAX(0.,renderPeriodInSeconds - elapsedClockTime));
}

void render(SDLGame* sdlGame) {
  SDL_FillRect( sdlGame->g_screenSurface, NULL, SDL_MapRGB( sdlGame->g_screenSurface->format, 0, 0, 0));

  //
  // Lock the screen's surface...
  //

  if( SDL_MUSTLOCK( sdlGame->g_screenSurface ) )
    {
      if( SDL_LockSurface(sdlGame->g_screenSurface) < 0 )
	return;
    }

  //
  // Plot each cell as a single pixel...
  //

  int x, y, i, j;
  int size = pzGetBoardSize (sdlGame->game);
  for (x = 0; x < size; ++x)
    for (y = 0; y < size; ++y) {
      int pal = pzGetCellPaletteIndex (sdlGame->game, x, y);
      for (i = 0; i < PIXELS_PER_CELL; ++i)
	for (j = 0; j < PIXELS_PER_CELL; ++j)
	  renderPixel( sdlGame->g_screenSurface, PIXELS_PER_CELL*x+i, PIXELS_PER_CELL*y+j, sdlGame->sdlColor[pal] );
    }

  //
  // Unlock the screen's surface...
  //

  if( SDL_MUSTLOCK( sdlGame->g_screenSurface ) )
    SDL_UnlockSurface( sdlGame->g_screenSurface );

  SDL_Flip( sdlGame->g_screenSurface );
}
