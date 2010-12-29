//
//  pixelzooViewController.h
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//  Copyright University of California - Berkeley 2010. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "pixelzooView.h"

// PixelZoo includes
#include "xmlgame.h"

// PixelZoo game constants
#define GAME_XML_FILENAME "testgame"
#define MAX_PROPORTION_TIME_EVOLVING  .9

// Game data
@interface pixelzooViewController : UIViewController {
	Game *game;
	pixelzooView *gameView;
	
	NSTimer *redrawTimer;
	NSTimer *evolveTimer;
	
//	UIImageView *drawImage;

	CGPoint lastPoint;
	BOOL mouseSwiped;	
	int mouseMoved;
}

- (pixelzooView *)gameView;
- (void)startTimers;
- (void)triggerRedraw;
- (void)callGameLoop;

@end

