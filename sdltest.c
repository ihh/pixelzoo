#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>

#include "xmlboard.h"

// stupid quoting bollix
#define QUOTEME_(x) #x
#define QUOTEME(x) QUOTEME_(x)

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
void renderPixel(int x, int y, Uint8 R, Uint8 G, Uint8 B);
Uint32 timeLeft(void);
void render(void);

//-----------------------------------------------------------------------------
// Name: main()
// Desc: 
//-----------------------------------------------------------------------------
int main( int argc, char *argv[] )
{
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

	evolveBoard (board, 1., .1, NULL, NULL);
        
        render();
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
"<grammar><particle><name>drifter</name><type>1</type><color>ff7f7f</color>"
"<rule><test><loc><x>1</x></loc><val>0</val><ignore>.05</ignore></test><exec><dest><x>1</x></dest></exec><exec><rshift>32</rshift><fail>.01</fail></exec></rule>"
"<rule><test><loc><x>-1</x></loc><val>0</val><ignore>.05</ignore></test><exec><dest><x>-1</x></dest></exec><exec><rshift>32</rshift></exec></rule>"
"<rule><test><loc><y>1</y></loc><val>0</val><ignore>.05</ignore></test><exec><dest><y>1</y></dest></exec><exec><rshift>32</rshift></exec></rule>"
"<rule><test><loc><y>-1</y></loc><val>0</val><ignore>.05</ignore></test><exec><dest><y>-1</y></dest></exec><exec><rshift>32</rshift></exec></rule>"
"</particle></grammar><init><x>" QUOTEME(HALF_BOARD_SIZE) "</x><y>" QUOTEME(HALF_BOARD_SIZE) "</y><val>1</val></init></board></xml>");
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
void renderPixel( int x, int y, Uint8 R, Uint8 G, Uint8 B )
{
    Uint32 color = SDL_MapRGB( g_screenSurface->format, R, G, B );

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
void render( void )
{
    SDL_Delay( timeLeft() );

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
	Particle* p = readBoardParticle (board, x, y);
	if (p)
	  for (i = 0; i < PIXELS_PER_CELL; ++i)
	    for (j = 0; j < PIXELS_PER_CELL; ++j)
	      renderPixel( PIXELS_PER_CELL*x+i, PIXELS_PER_CELL*y+j, p->color.r, p->color.g, p->color.b );
      }

    //
    // Unlock the screen's surface...
    //

    if( SDL_MUSTLOCK( g_screenSurface ) )
        SDL_UnlockSurface( g_screenSurface );

    SDL_Flip( g_screenSurface );
}
