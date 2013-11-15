//
//  PZIsometricMapViewController.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZIsometricViewController.h"
#import "PZIsoMapScene.h"
#import "PZDefs.h"

@interface PZIsometricViewController ()

@end

@implementation PZIsometricViewController

@synthesize gameWrapper;
@synthesize worldDescriptor;
@synthesize lockDescriptor;

@synthesize skview;


- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];

	// Do any additional setup after loading the view.
    skview.showsDrawCount = YES;
    skview.showsNodeCount = YES;
    skview.showsFPS = YES;

    currentTileHeight = 1;
}

- (void)viewWillAppear:(BOOL)animated
{
    map = [[PZIsoMapScene alloc] initWithSize:CGSizeMake(skview.bounds.size.width,skview.bounds.size.height)];
    map.game = gameWrapper;
    [skview presentScene: map];
}

- (void) viewDidAppear:(BOOL)animated
{
    [self startTimers];
}

- (void)viewWillDisappear:(BOOL)animated {
	[self stopTimers];
    [gameWrapper postTurn];
    [super viewWillDisappear:animated];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

/* Timers: board updates & rendering */
-(void)startTimers
{
	double evolvePeriod = 1. / GAMELOOP_CALLS_PER_SECOND;
    evolveTimer = [NSTimer scheduledTimerWithTimeInterval:evolvePeriod target:self selector:@selector(callGameLoop) userInfo:self repeats:YES];
}

-(void)stopTimers
{
	[evolveTimer invalidate];
}

/* Board updates */
- (void)callGameLoop
{
    [gameWrapper updateGame];
    [map showMapWithOffset:currentViewOffset withTileHeight:currentTileHeight];
}


/* Gesture recognizers */
- (IBAction)handlePan:(UIPanGestureRecognizer *)recognizer {
    
	if (recognizer.state == UIGestureRecognizerStateBegan) {
		viewOffsetAtStartOfPan = currentViewOffset;
	}
	
	if (recognizer.state == UIGestureRecognizerStateBegan || recognizer.state == UIGestureRecognizerStateChanged) {
		CGPoint trans = [recognizer translationInView:self.view];
        
		CGPoint mvo = [self maxViewOffset];
		currentViewOffset.x = MAX (0, MIN (viewOffsetAtStartOfPan.x - trans.x, mvo.x));
		currentViewOffset.y = MAX (0, MIN (viewOffsetAtStartOfPan.y - trans.y, mvo.y));
		
		panning = 1;
		[gameWrapper untouchCell];
	} else {
		panning = 0;
	}

}

- (IBAction)handlePinch:(UIPinchGestureRecognizer *)recognizer {
	
	if (recognizer.state == UIGestureRecognizerStateBegan) {
		tileHeightAtStartOfZoom = currentTileHeight;
		viewOffsetAtStartOfZoom = currentViewOffset;
	}
    
	if (recognizer.state == UIGestureRecognizerStateBegan || recognizer.state == UIGestureRecognizerStateChanged) {
		CGFloat scale = MAX (0, [recognizer scale]);
		
		CGFloat minth = 1;
		CGFloat maxth = 16;  // hardwire for now
		currentTileHeight = MAX (minth, MIN (maxth, tileHeightAtStartOfZoom * scale));
		
		CGPoint mvo = [self maxViewOffset];
		currentViewOffset.x = MAX (-mvo.x, MIN (viewOffsetAtStartOfZoom.x * scale, mvo.x));
		currentViewOffset.y = MAX (-mvo.y, MIN (viewOffsetAtStartOfZoom.y * scale, mvo.y));
		
		zooming = 1;
		[gameWrapper untouchCell];
	} else {
		zooming = 0;
	}
    
    NSLog(@"Current tile height is %f\n",currentTileHeight);
}

- (CGPoint) maxViewOffset {
	CGPoint mvo;
	CGFloat th = currentTileHeight;
	CGFloat bs = [gameWrapper boardSize];
    CGFloat boardImgWidth = 2 * th * bs;
    CGFloat boardImgHeight = th * bs;
    CGSize frameSize = skview.frame.size;
	mvo.x = MAX (0, boardImgWidth - frameSize.width);
	mvo.y = MAX (0, boardImgHeight - frameSize.height);
	return mvo;
}



@end
