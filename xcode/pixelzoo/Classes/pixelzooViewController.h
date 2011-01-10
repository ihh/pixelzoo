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
#define GAME_XML_FILENAME "testgame"
#define MAX_PROPORTION_TIME_EVOLVING  .9
#define REDRAWS_PER_SECOND 60   /* frame rate */
#define GAMELOOP_CALLS_PER_SECOND REDRAWS_PER_SECOND

// visuals
#define GAME_CONSOLE_FONT_SIZE 10
#define GAME_CONSOLE_FONT_SPACING 1
#define GAME_CONSOLE_FONT "Helvetica"
#define GAME_CONSOLE_FONT_FADE 0.9

// Game data
@interface pixelzooViewController : UIViewController {
	// PixelZoo game
	Game *game;

	// timers
	NSTimer *redrawTimer;
	NSTimer *evolveTimer;

	// touch tracking
	CGPoint lastPoint;
	BOOL mouseSwiped;	
	int mouseMoved;
}

@property(readonly) Game *game;

- (void)startTimers;
- (void)triggerRedraw;
- (void)callGameLoop;

// The following methods relate to the layout of the UI
// Some of them are called by View's drawRect method
- (CGFloat)cellSize;
- (CGRect)boardRect;
- (CGRect)toolboxRect;
- (CGRect)toolRect:(int)nTool;
- (CGRect)toolPartialRect:(int)nTool startingAt:(CGFloat)startFraction endingAt:(CGFloat)endFraction;

@end

