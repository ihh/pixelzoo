#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>

#include <time.h>

#include "xmlboard.h"

//-----------------------------------------------------------------------------
// SYMBOLIC CONSTANTS
//-----------------------------------------------------------------------------
#define BOARD_SIZE 128
#define HALF_BOARD_SIZE 64
#define PIXELS_PER_CELL   4
#define BOARD_SCREEN_SIZE 512
const int SCREEN_WIDTH    = BOARD_SCREEN_SIZE ;
const int SCREEN_HEIGHT   = BOARD_SCREEN_SIZE ;
const int COLOR_DEPTH     = 8;
const int UPDATE_INTERVAL = 30;

Uint32 sdlColor[PaletteSize];

//-----------------------------------------------------------------------------
// GLOBALS
//-----------------------------------------------------------------------------
SDL_Surface *g_screenSurface = NULL;

Board *board = NULL;

//-----------------------------------------------------------------------------
// PROTOTYPES
//-----------------------------------------------------------------------------
int main(int argc, char *argv[]);
void init(void);
void shutDown(void);
void renderPixel(int x, int y, PaletteIndex pal);
Uint32 timeLeft(void);
void render(double*);

//-----------------------------------------------------------------------------
// Name: main()
// Desc: 
//-----------------------------------------------------------------------------
int main( int argc, char *argv[] )
{
  double targetUpdatesPerCell = 1., maxTimeInSeconds = .01, updateRate, minUpdateRate, renderRate;
  int iter = 0, births = 0, deaths = 0;

    init();

    int bDone = 0;

    while( !bDone )
    {
        SDL_Event event;

        while( SDL_PollEvent( &event ) )
        {
            if( event.type == SDL_QUIT )  
                bDone = 1;

            if( event.type == SDL_KEYDOWN )
            {
                if( event.key.keysym.sym == SDLK_ESCAPE ) 
                    bDone = 1;
            }
        }

	int oldDrifters = board->byType[1]->count;
	evolveBoard (board, targetUpdatesPerCell, maxTimeInSeconds, &updateRate, &minUpdateRate);
	int newDrifters = board->byType[1]->count;
	if (newDrifters > oldDrifters)
	  births += newDrifters - oldDrifters;
	else if (oldDrifters > newDrifters)
	  deaths += oldDrifters - newDrifters;
	render(&renderRate);

	if (++iter % (int) (1. + 1. / maxTimeInSeconds) == 0)
	  printf ("renderRate=%g targetUpdateRate=%g updateRate=%g minUpdateRate=%g boardFiringRate=%g updatesPerCell=%g birthRate=%g deathRate=%g\n", renderRate, targetUpdatesPerCell / maxTimeInSeconds, updateRate, minUpdateRate, boardFiringRate(board), board->updatesPerCell, (double) births / board->updatesPerCell, (double) deaths / board->updatesPerCell);
    }

    shutDown();

    return 0;
}

//-----------------------------------------------------------------------------
// Name: init()
// Desc: 
//-----------------------------------------------------------------------------
void init( void )
{
  int x, y;
  PaletteIndex pal;

    if( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
        printf( "Unable to init SDL: %s\n", SDL_GetError() );
        exit(1);
    }

    atexit( SDL_Quit );

    g_screenSurface = SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, COLOR_DEPTH, 
                                        SDL_HWSURFACE | SDL_DOUBLEBUF );

    if( g_screenSurface == NULL )
    {
        printf( "Unable to set video: %s\n", SDL_GetError() );
        exit(1);
    }

    //
    // Initialize...
    //

    board = newBoardFromXmlString("<xml><board><size>" QUOTEME(BOARD_SIZE) "</size>"
				  "<grammar><particle>"
				  //"<sync/><shuffle/>"
"<name>drifter</name><type>1</type><color><mask>3</mask><hexmul>80000</hexmul></color><color><mask>0</mask><hexinc>ffff</hexinc></color>"
"<rule><rate>.01</rate><test><loc><x>1</x></loc><val>0</val></test><exec><src></src><dest><x>1</x></dest></exec></rule>"
"<rule><test><loc><x>1</x></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><src><x>1</x></src><dest></dest><hexinc>49</hexinc><hexmask>ff9</hexmask></exec><exec><dest><x>1</x></dest><rshift>32</rshift><hexinc>10001</hexinc></exec></rule>"
"<rule><test><loc><x>-1</x></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><src><x>-1</x></src><dest></dest><hexinc>109</hexinc><hexmask>ff9</hexmask></exec><exec><dest><x>-1</x></dest><rshift>32</rshift><hexinc>10002</hexinc></exec></rule>"
"<rule><test><loc><y>1</y></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><src><y>1</y></src><dest></dest><hexinc>409</hexinc><hexmask>ff9</hexmask></exec><exec><dest><y>1</y></dest><rshift>32</rshift><hexinc>10003</hexinc></exec></rule>"
"<rule><test><loc><y>-1</y></loc><val>0</val><ignore>.05</ignore></test><exec><rshift>32</rshift></exec><exec><src><y>-1</y></src><dest></dest><hexinc>89</hexinc><hexmask>ff9</hexmask></exec><exec><dest><y>-1</y></dest><rshift>32</rshift><hexinc>10004</hexinc></exec></rule>"
"</particle></grammar><init><x>" QUOTEME(HALF_BOARD_SIZE) "</x><y>" QUOTEME(HALF_BOARD_SIZE) "</y><type>1</type></init></board></xml>");

    /* palette test */
    pal = 0;
    for (y = 0; y < BOARD_SIZE && pal <= PaletteMax; ++y)
      for (x = 0; x < BOARD_SIZE && pal <= PaletteMax; ++x)
	if (!readBoardParticle (board, x, y))
	  writeBoardStateUnguarded (board, x, y, pal++);

    /* palette lookup */
    for (pal = 0; pal <= PaletteMax; ++pal)
      sdlColor[pal] = SDL_MapRGB( g_screenSurface->format, board->palette.rgb[pal].r, board->palette.rgb[pal].g, board->palette.rgb[pal].b );
}

//-----------------------------------------------------------------------------
// Name: shutDown()
// Desc: 
//-----------------------------------------------------------------------------
void shutDown( void )
{
    SDL_FreeSurface( g_screenSurface );
    deleteBoard(board);
}

//-----------------------------------------------------------------------------
// Name: renderPixel()
// Desc: 
//-----------------------------------------------------------------------------
void renderPixel( int x, int y, PaletteIndex pal )
{
  Uint32 color = sdlColor[pal];

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

//-----------------------------------------------------------------------------
// Name: timeLeft()
// Desc: 
//-----------------------------------------------------------------------------
Uint32 timeLeft( void )
{
    static Uint32 timeToNextUpdate = 0;
    Uint32 currentTime;

    currentTime = SDL_GetTicks();

    if( timeToNextUpdate <= currentTime ) 
    {
        timeToNextUpdate = currentTime + UPDATE_INTERVAL;
        return 0;
    }

    return( timeToNextUpdate - currentTime );
}

//-----------------------------------------------------------------------------
// Name: render()
// Desc: 
//-----------------------------------------------------------------------------
void render( double* rate )
{
  clock_t start, end;

    SDL_Delay( timeLeft() );

    start = clock();

    SDL_FillRect( g_screenSurface, NULL, SDL_MapRGB( g_screenSurface->format, 0, 0, 0));

    //
    // Lock the screen's surface...
    //

    if( SDL_MUSTLOCK( g_screenSurface ) )
    {
        if( SDL_LockSurface(g_screenSurface) < 0 )
            return;
    }

    //
    // Plot each cell as a single pixel...
    //

    int x, y, i, j;
    for (x = 0; x < BOARD_SIZE; ++x)
      for (y = 0; y < BOARD_SIZE; ++y) {
	PaletteIndex pal = readBoardColor (board, x, y);
	for (i = 0; i < PIXELS_PER_CELL; ++i)
	  for (j = 0; j < PIXELS_PER_CELL; ++j)
	    renderPixel( PIXELS_PER_CELL*x+i, PIXELS_PER_CELL*y+j, pal );
      }

    //
    // Unlock the screen's surface...
    //

    if( SDL_MUSTLOCK( g_screenSurface ) )
        SDL_UnlockSurface( g_screenSurface );

    SDL_Flip( g_screenSurface );

    end = clock();
    if (rate)
      *rate = (double) CLOCKS_PER_SEC / (double) (end - start);
}
