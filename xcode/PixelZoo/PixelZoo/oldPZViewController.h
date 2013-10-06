//
//  PZViewController.h
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//

#import <UIKit/UIKit.h>

// PixelZoo includes
#import "pixelzoo.h"

typedef struct XYCoord { int x, y; } XYCoord;

// Game data
@interface PZViewController : UIViewController {
@public
    // the game
    pzGame* game;
	
    // rendering
    CGFloat cellSize;

    // timers
	NSTimer *redrawTimer;
	NSTimer *evolveTimer;
	
	// UI
	int panning, zooming, examining;
    XYCoord examCoord;

@protected
	CGFloat cellSizeAtStartOfZoom;  // used when zooming
	CGPoint viewOriginAtStartOfZoom;  // used when zooming
	CGPoint viewOriginAtStartOfPan;  // used when panning
}

@property(readonly) pzGame *game;
@property(readonly) int examining;
@property(readonly) CGPoint viewOrigin;
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

