//
//  pixelzooViewController.h
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//  Copyright University of California - Berkeley 2010. All rights reserved.
//

#import <UIKit/UIKit.h>

// PixelZoo includes
#include "xmlgame.h"

// PixelZoo game constants
// #define GAME_XML_FILENAME "testgame"
#define GAME_XML_FILENAME "simple"
#define MAX_PROPORTION_TIME_EVOLVING  .9
#define REDRAWS_PER_SECOND 30   /* frame rate */
#define GAMELOOP_CALLS_PER_SECOND REDRAWS_PER_SECOND    /* for some reason, increasing this slows updates down; maybe need a separate thread? */

// dimensions
#define CONSOLE_HEIGHT  128
#define TOOLBAR_WIDTH   64
#define INITIAL_PIXELS_PER_CELL 4
#define MIN_PIXELS_PER_CELL 1
#define MAX_PIXELS_PER_CELL 4
#define MAGNIFIED_PIXELS_PER_CELL 32

// text
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
#define EXAMINE_FONT_SIZE 20
#define EXAMINE_FONT_SPACING 0
#define EXAMINE_BACKGROUND_OPACITY .9
#define EXAMINE_TEXT_DISPLACEMENT 64
#define EXAMINE_LABEL_BORDER 1

// other visuals
#define BOARD_BORDER_OPACITY .5

#define EXTRA_TOOLS_AT_TOP 1  /* Examine tool */
#define EXTRA_TOOLS_AT_BOTTOM 1  /* Reset level */

#define EXAMINE_TOOL_NAME "Examine"
#define EXAMINE_EMPTY_TEXT "nothing"
#define EXAMINE_CIRCLE_RADIUS 20

#define RESET_TOOL_NAME "Restart"

// Game data
@interface pixelzooViewController : UIViewController {
	// PixelZoo game
	Game *game;
	CGPoint viewOrigin;
	CGFloat cellSize;  // pixels per cell
	
	// timers
	NSTimer *redrawTimer;
	NSTimer *evolveTimer;
	
	// UI
	int panning, zooming, examining;
	XYCoord examCoord;
	CGFloat cellSizeAtStartOfZoom;  // used when zooming
	CGPoint viewOriginAtStartOfZoom;  // used when zooming
	CGPoint viewOriginAtStartOfPan;  // used when panning
}

@property(readonly) Game *game;
@property(readonly) CGPoint viewOrigin;
@property(readonly) int examining;
@property(readonly) XYCoord examCoord;
@property(readonly) CGFloat cellSize;

- (void)loadGame;
- (void)deleteGame;
- (void)reloadGame;
- (void)startTimers;
- (void)stopTimers;
- (void)triggerRedraw;
- (void)callGameLoop;

// The following methods relate to the layout of the UI
// Some of them are called by View's drawRect method
- (CGFloat)cellSize;
- (CGFloat)maxCellSize;
- (CGFloat)minCellSize;
- (CGFloat)magCellSize;
- (CGPoint)maxViewOrigin;
- (CGRect)boardRect;
- (CGRect)bigBoardRect;
- (CGRect)toolboxRect;
- (CGRect)consoleRect;
- (CGPoint)consoleCentroid;
- (CGRect)consoleBoardRect;
- (CGRect)toolRect:(int)nTool;
- (CGRect)toolPartialRect:(int)nTool startingAt:(CGFloat)startFraction endingAt:(CGFloat)endFraction;

@end

