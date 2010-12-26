#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>

#include <time.h>

#include "xmlgame.h"

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
SDLGame* newSDLGame(void);
void deleteSDLGame(SDLGame*);
void render(SDLGame*);
void renderPixel(SDLGame*, int x, int y, PaletteIndex pal);

int evolveThreadFunc ( void *voidGame );
int renderThreadFunc( void *voidSdlGame );

//-----------------------------------------------------------------------------
// Name: main()
// Desc: 
//-----------------------------------------------------------------------------
int main( int argc, char *argv[] )
{
  SDLGame *sdlGame = NULL;
  SDL_Thread *evolveThread, *renderThread;
  
  sdlGame = newSDLGame();

  evolveThread = SDL_CreateThread(evolveThreadFunc, sdlGame->game);
  if ( evolveThread == NULL ) {
    fprintf(stderr, "Unable to create thread: %s\n", SDL_GetError());
    return 1;
  }

  renderThread = SDL_CreateThread(renderThreadFunc, sdlGame);
  if ( renderThread == NULL ) {
    fprintf(stderr, "Unable to create thread: %s\n", SDL_GetError());
    return 1;
  }

  while( sdlGame->game->gameState != GameQuit )
    {
      SDL_Event event;

      while( SDL_PollEvent( &event ) )
        {
	  if( event.type == SDL_QUIT )  
	    sdlGame->game->gameState = GameQuit;

	  if( event.type == SDL_KEYDOWN )
            {
	      if( event.key.keysym.sym == SDLK_ESCAPE ) 
		sdlGame->game->gameState = GameQuit;
            }
        }
    }

  SDL_WaitThread(renderThread, NULL);
  SDL_WaitThread(evolveThread, NULL);

  deleteSDLGame(sdlGame);

  return 0;
}

//-----------------------------------------------------------------------------
// Name: newSDLGame()
// Desc: 
//-----------------------------------------------------------------------------
SDLGame* newSDLGame( void )
{
  PaletteIndex pal;

  SDLGame *sdlGame = SafeMalloc (sizeof (SDLGame));
  sdlGame->g_screenSurface = NULL;
  //
  // Initialize Game...
  //

  sdlGame->game = newGameFromXmlString("<xml><board><size>128</size>"
				       "<grammar>"
				       "<particle>"
				       "<sync/><shuffle/>"
				       "<name>drifter</name><type>1</type><color><mask>3</mask><hexmul>80000</hexmul></color><color><mask>0</mask><hexinc>80ffff</hexinc></color>"
				       "<rule><rate>.0009</rate><test><loc><x>1</x></loc><val>0</val></test><exec><dest><x>1</x></dest><rshift>32</rshift><hexinc>20000</hexinc></exec></rule>"
				       "<rule><rate>.03</rate><test><loc><x>1</x></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><src><x>1</x></src><dest></dest><hexinc>49</hexinc><hexmask>ff9</hexmask></exec><exec><dest><x>1</x></dest><rshift>32</rshift><hexinc>10001</hexinc></exec></rule>"
				       "<rule><rate>.03</rate><test><loc><x>-1</x></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><src><x>-1</x></src><dest></dest><hexinc>109</hexinc><hexmask>ff9</hexmask></exec><exec><dest><x>-1</x></dest><rshift>32</rshift><hexinc>10002</hexinc></exec></rule>"
				       "<rule><rate>.03</rate><test><loc><y>1</y></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><src><y>1</y></src><dest></dest><hexinc>409</hexinc><hexmask>ff9</hexmask></exec><exec><dest><y>1</y></dest><rshift>32</rshift><hexinc>10003</hexinc></exec></rule>"
				       "<rule><rate>.03</rate><test><loc><y>-1</y></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><src><y>-1</y></src><dest></dest><hexinc>89</hexinc><hexmask>ff9</hexmask></exec><exec><dest><y>-1</y></dest><rshift>32</rshift><hexinc>10004</hexinc></exec></rule>"
				       "</particle>"
				
				       "<particle><name>stepper</name><type>2</type><color><mask>3</mask><hexmul>80000</hexmul></color><color><mask>0</mask><hexinc>ffff</hexinc></color>"
				       "<rule><rate>.0009</rate><test><loc><x>1</x></loc><val>0</val></test><exec><dest><x>1</x></dest><rshift>32</rshift><hexinc>10000</hexinc></exec></rule>"
				       "<rule><rate>.03</rate><test><loc><x>1</x></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><dest><x>1</x></dest><rshift>32</rshift><hexinc>20001</hexinc></exec></rule>"
				       "<rule><rate>.03</rate><test><loc><x>-1</x></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><dest><x>-1</x></dest><rshift>32</rshift><hexinc>20002</hexinc></exec></rule>"
				       "<rule><rate>.03</rate><test><loc><y>1</y></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><dest><y>1</y></dest><rshift>32</rshift><hexinc>20003</hexinc></exec></rule>"
				       "<rule><rate>.03</rate><test><loc><y>-1</y></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><dest><y>-1</y></dest><rshift>32</rshift><hexinc>20004</hexinc></exec></rule>"
				       "</particle>"
				       "</grammar><init><x>64</x><y>64</y><type>1</type></init></board></xml>");


  /* palette lookup */
  for (pal = 0; pal <= PaletteMax; ++pal)
    sdlGame->sdlColor[pal] = SDL_MapRGB( sdlGame->g_screenSurface->format, sdlGame->game->board->palette.rgb[pal].r, sdlGame->game->board->palette.rgb[pal].g, sdlGame->game->board->palette.rgb[pal].b );

  /* init SDL */
  if( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
      printf( "Unable to init SDL: %s\n", SDL_GetError() );
      exit(1);
    }

  atexit( SDL_Quit );

  sdlGame->g_screenSurface = SDL_SetVideoMode( sdlGame->game->board->size * PIXELS_PER_CELL,
					       sdlGame->game->board->size * PIXELS_PER_CELL,
					       COLOR_DEPTH, 
					       SDL_HWSURFACE | SDL_DOUBLEBUF );

  if( sdlGame->g_screenSurface == NULL )
    {
      printf( "Unable to set video: %s\n", SDL_GetError() );
      exit(1);
    }

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
void renderPixel( SDLGame *sdlGame, int x, int y, PaletteIndex pal )
{
  SDL_Surface *g_screenSurface = sdlGame->g_screenSurface;
  Uint32 color = sdlGame->sdlColor[pal];

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

int evolveThreadFunc(void *voidGame)
{
  Game *game = (Game*) voidGame;
  double targetUpdatesPerCell = 1., updatePeriodInSeconds = targetUpdatesPerCell / game->updatesPerSecond, updateRate, minUpdateRate, elapsedClockTime;

  while ( game->gameState != GameQuit ) {
    clock_t start, now;
    start = clock();
    evolveBoard (game->board, targetUpdatesPerCell, updatePeriodInSeconds, &updateRate, &minUpdateRate);
    updateGameState (game);
    now = clock();
    elapsedClockTime = ((double) now - start) / (double) CLOCKS_PER_SEC;
    SDL_Delay(1000 * MAX(0.,updatePeriodInSeconds - elapsedClockTime));
  }
  printf("Evolve thread quitting\n");
  return(0);
}

int renderThreadFunc( void *voidSdlGame )
{
  SDLGame* sdlGame = (SDLGame*) voidSdlGame;
  double renderPeriodInSeconds = 1. / RENDER_RATE, elapsedClockTime;

  while ( sdlGame->game->gameState != GameQuit ) {
    clock_t start, now;
    start = clock();
    render (sdlGame);
    now = clock();
    elapsedClockTime = ((double) now - start) / (double) CLOCKS_PER_SEC;
    SDL_Delay(1000 * MAX(0.,renderPeriodInSeconds - elapsedClockTime));
  }
  printf("Render thread quitting\n");
  return(0);
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
	  renderPixel( sdlGame, PIXELS_PER_CELL*x+i, PIXELS_PER_CELL*y+j, pal );
    }

  //
  // Unlock the screen's surface...
  //

  if( SDL_MUSTLOCK( sdlGame->g_screenSurface ) )
    SDL_UnlockSurface( sdlGame->g_screenSurface );

  SDL_Flip( sdlGame->g_screenSurface );
}
