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
#define GAMELOOP_CALLS_PER_SECOND (REDRAWS_PER_SECOND * 4)

// Game data
@interface pixelzooViewController : UIViewController {
	Game *game;
	
	NSTimer *redrawTimer;
	NSTimer *evolveTimer;

	UIColor *boardColor[PaletteMax];
	
	CGPoint lastPoint;
	BOOL mouseSwiped;	
	int mouseMoved;
}

@property(readonly) Game *game;
@property(readonly) UIColor **boardColor;

- (void)startTimers;
- (void)triggerRedraw;
- (void)callGameLoop;

@end

