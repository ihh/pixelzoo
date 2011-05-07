#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>

#include <time.h>

#include "xmlgame.h"
#include "xmlmove.h"

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
  Game *game;
  Uint32 sdlColor[PaletteSize];
  SDL_Surface *g_screenSurface;
} SDLGame;

//-----------------------------------------------------------------------------
// PROTOTYPES
//-----------------------------------------------------------------------------
int main(int argc, char *argv[]);
SDLGame* newSDLGame(char *filename);
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
  SDLGame *sdlGame = NULL;
  char *moveLogFilename = NULL;

  if (argc != 2 && argc != 3) {
    printf ("Usage: %s <XML game file> [<XML move log>]\n", argv[0]);
    Abort ("Missing game file");
  }
  
  sdlGame = newSDLGame (argv[1]);
  if (argc == 3) {
    logBoardMoves (sdlGame->game->board);
    moveLogFilename = argv[2];
  }

  while( gameRunning(sdlGame->game) )
    {
      const double targetUpdatesPerCell = sdlGame->game->ticksPerSecond / (double) RENDER_RATE;
      double updatesPerCell, evolveTime;
      int64_Microticks microticks;
      int actualUpdates;

      gameLoop (sdlGame->game, targetUpdatesPerCell, 0.5, &microticks, &updatesPerCell, &actualUpdates, &evolveTime);
      renderAndDelay (sdlGame);

      SDL_Event event;

      while( SDL_PollEvent( &event ) )
        {
	  switch( event.type ) 
            {
	    case SDL_QUIT:
	      quitGame(sdlGame->game);
	      break;

	    case SDL_KEYDOWN:
	      if( event.key.keysym.sym == SDLK_ESCAPE ) 
		quitGame(sdlGame->game);
	      break;

	    case SDL_MOUSEMOTION:
	      sdlGame->game->toolPos.x = event.motion.x / PIXELS_PER_CELL;
	      sdlGame->game->toolPos.y = event.motion.y / PIXELS_PER_CELL;
	      break;

	    case SDL_MOUSEBUTTONUP:
	      if ( event.button.button == SDL_BUTTON_LEFT)
		sdlGame->game->toolActive = 0;
	      break;

	    case SDL_MOUSEBUTTONDOWN:
	      if ( event.button.button == SDL_BUTTON_LEFT)
		sdlGame->game->toolActive = 1;
	      break;

	    default:
	      break;
	    }
        }
    }

  if (moveLogFilename) {
    xmlTextWriterPtr writer = xmlNewTextWriterFilename (moveLogFilename, 0);
    writeMoveList (sdlGame->game->board->moveLog, writer, (xmlChar*) XMLZOO_LOG);
    xmlFreeTextWriter (writer);
  }

  deleteSDLGame(sdlGame);

  return 0;
}

//-----------------------------------------------------------------------------
// Name: newSDLGame()
// Desc: 
//-----------------------------------------------------------------------------
SDLGame* newSDLGame( char *filename )
{
  PaletteIndex pal;

  SDLGame *sdlGame = SafeMalloc (sizeof (SDLGame));
  sdlGame->g_screenSurface = NULL;
  //
  // Initialize Game...
  //

  sdlGame->game = newGameFromXmlFile(filename);
  sdlGame->game->selectedTool = sdlGame->game->toolByName->root->left->value;

  /* init SDL */
  if( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
      printf( "Unable to init SDL: %s\n", SDL_GetError() );
      exit(1);
    }

  atexit( SDL_Quit );

  int size = sdlGame->game->board->size;
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
  for (pal = 0; pal <= PaletteMax; ++pal)
    sdlGame->sdlColor[pal] = SDL_MapRGB( sdlGame->g_screenSurface->format, sdlGame->game->board->palette.rgb[pal].r, sdlGame->game->board->palette.rgb[pal].g, sdlGame->game->board->palette.rgb[pal].b );


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
  deleteGame(sdlGame->game);
  SafeFree (sdlGame);
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
  int size = sdlGame->game->board->size;
  for (x = 0; x < size; ++x)
    for (y = 0; y < size; ++y) {
      PaletteIndex pal = readBoardColor (sdlGame->game->board, x, y);
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
