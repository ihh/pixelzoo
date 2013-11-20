//
//  pixelzooDefs.h
//  pixelzoo
//
//  Created by Ian Holmes on 8/22/13.
//

#ifndef pixelzoo_PZDefs_h
#define pixelzoo_PZDefs_h

// PixelZoo server
#define SERVER_URL_PREFIX "http://localhost:3000"

// PixelZoo game constants
// #define GAME_XML_FILENAME "testgame"
#define GAME_XML_FILENAME "simple"
#define MAX_PROPORTION_TIME_EVOLVING  .9
#define REDRAWS_PER_SECOND 30   /* frame rate */
#define GAMELOOP_CALLS_PER_SECOND REDRAWS_PER_SECOND    /* for some reason, increasing this slows updates down; maybe need a separate thread? */

// dimensions (overhead view)
#define CONSOLE_HEIGHT  128
#define TOOLBAR_WIDTH   64
#define INITIAL_PIXELS_PER_CELL 4
#define MIN_PIXELS_PER_CELL 1
#define MAX_PIXELS_PER_CELL 4
#define MAGNIFIED_PIXELS_PER_CELL 32

// text (overhead view)
#define GAME_FONT "Helvetica"
#define CONSOLE_FONT_SIZE 5
#define CONSOLE_FONT_SPACING 1
#define CONSOLE_FONT_FADE 0.9
#define BALLOON_FONT_SIZE 10
#define BALLOON_BACKGROUND_OPACITY .5
#define TOOL_FONT_SIZE 9
#define TOOL_FONT_SPACING 0
#define TOOL_NAME_OPACITY .9
#define MAX_TOOL_SELECT_STROKE 4
#define TOOL_SELECT_PULSE_RATE .1
#define EXAMINE_FONT_SIZE 10
#define EXAMINE_FONT_SPACING 0
#define EXAMINE_BACKGROUND_OPACITY .9
#define EXAMINE_TEXT_DISPLACEMENT 64
#define EXAMINE_LABEL_BORDER 1

// other visuals (overhead view)
#define BOARD_BORDER_OPACITY .5
#define EXTRA_TOOLS_AT_TOP 1  /* Examine tool */
#define EXTRA_TOOLS_AT_BOTTOM 0
#define EXAMINE_TOOL_NAME "Examine"
#define EXAMINE_EMPTY_TEXT "nothing"
#define EXAMINE_CIRCLE_RADIUS 20

// isometric view
#define TOOL_ICON_WIDTH 64
#define TOOL_ICON_HEIGHT 64
#define TOOL_RESERVE_HEIGHT 4

#define TILE_SPRITE_HEIGHT 32  /* natural height for a (flat) tile sprite (a cube will be twice this height) */
#define MIN_TILE_HEIGHT_FOR_SPRITES 12  /* threshold for showing sprites instead of solid cubes */
#define MAX_NATURAL_TILE_HEIGHT 32 /* max scale at which board image will actually be rendered */
#define MAX_TILE_HEIGHT 32  /* max scale at which board image will be shown */

#define TUTORIAL_WORLD_NAME "Tutorial"

#endif
