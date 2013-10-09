//
//  PZViewController.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "PZWorldDescriptor.h"

#import "pixelzoo.h"

typedef struct XYCoord { int x, y; } XYCoord;


@interface PZViewController : UIViewController <NSURLConnectionDelegate> {
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
    NSMutableData *lockData;
    NSHTTPURLResponse* httpLockResponse;

	CGFloat cellSizeAtStartOfZoom;  // used when zooming
	CGPoint viewOriginAtStartOfZoom;  // used when zooming
	CGPoint viewOriginAtStartOfPan;  // used when panning
}


@property (nonatomic, strong) PZWorldDescriptor *worldDescriptor;
@property (nonatomic, strong) IBOutlet UILabel *worldLabel;
@property (nonatomic, strong) IBOutlet UIView *worldView;

@property(readonly) pzGame *game;
@property(readonly) int examining;
@property(readonly) CGPoint viewOrigin;
@property(readonly) XYCoord examCoord;
@property(readonly) CGFloat cellSize;

@property (nonatomic, retain) NSURLConnection *lockConnection;

- (void)initGameFromXML:(NSString*)gameXMLString;
- (void)deleteGame;
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
