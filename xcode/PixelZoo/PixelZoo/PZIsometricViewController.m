//
//  PZIsometricViewController.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZIsometricViewController.h"
#import "PZIsometricScene.h"
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
//    currentTileHeight = 8;
}

- (void)viewWillAppear:(BOOL)animated
{
    // present a dummy scene so that the screen looks black when the view appears.
    // if we present the actual scene here, the SKView's coordinate system gets screwed up.
    SKScene *dummyScene = [[SKScene alloc] initWithSize:skview.frame.size];
    [skview presentScene: dummyScene];
    [super viewWillAppear:animated];
}

- (void) viewDidAppear:(BOOL)animated
{
    map = [[PZIsometricScene alloc] initWithSize:skview.frame.size forGame:gameWrapper withWorld:worldDescriptor withLock:lockDescriptor];
    [skview presentScene: map];
    [self startTimers];
    [super viewDidAppear:animated];
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
    NSInteger expiryTime = [lockDescriptor lockExpiryWait];
    if (expiryTime < 0) {
        [self.navigationController popViewControllerAnimated:YES];
    } else {
        [gameWrapper updateGame];
        [map showMapWithOffset:currentViewOffset withTileHeight:currentTileHeight];
    }
}


/* Gesture recognizers */
- (IBAction)handlePan:(UIPanGestureRecognizer *)recognizer {
    
    if ([self moveToolSelected]) {
        if (recognizer.state == UIGestureRecognizerStateBegan) {
            viewOffsetAtStartOfPan = currentViewOffset;
        }
        
        if (recognizer.state == UIGestureRecognizerStateBegan || recognizer.state == UIGestureRecognizerStateChanged) {
            CGPoint trans = [recognizer translationInView:self.view];
            
            CGPoint mvo = [self maxViewOffset];
            currentViewOffset.x = MAX (-mvo.x, MIN (mvo.x, viewOffsetAtStartOfPan.x - trans.x));
            currentViewOffset.y = MAX (-mvo.y, MIN (mvo.y, viewOffsetAtStartOfPan.y - trans.y));
            
            panning = 1;
            [gameWrapper untouchCell];
        } else {
            panning = 0;
        }
    }
}

- (IBAction)handlePinch:(UIPinchGestureRecognizer *)recognizer {
	
    if ([self moveToolSelected]) {
        if (recognizer.state == UIGestureRecognizerStateBegan) {
            tileHeightAtStartOfZoom = currentTileHeight;
            viewOffsetAtStartOfZoom = currentViewOffset;
        }
        
        if (recognizer.state == UIGestureRecognizerStateBegan || recognizer.state == UIGestureRecognizerStateChanged) {
            CGFloat scale = MAX (0, [recognizer scale]);
            
            CGFloat minth = 1;
            CGFloat maxth = MAX_TILE_HEIGHT;
            currentTileHeight = MAX (minth, MIN (maxth, tileHeightAtStartOfZoom * scale));
            scale = currentTileHeight / tileHeightAtStartOfZoom;
            
            CGPoint mvo = [self maxViewOffset];
            currentViewOffset.x = MAX (-mvo.x, MIN (mvo.x, viewOffsetAtStartOfZoom.x * scale));
            currentViewOffset.y = MAX (-mvo.y, MIN (mvo.y, viewOffsetAtStartOfZoom.y * scale));
            
            zooming = 1;
            [gameWrapper untouchCell];
        } else {
            zooming = 0;
        }
    }
}

// helpers

- (bool) moveToolSelected {
    return [gameWrapper selectedToolNumber] < 0;
}


- (CGPoint) maxViewOffset {
	CGPoint mvo;
	CGFloat th = currentTileHeight;
	CGFloat bs = [gameWrapper boardSize];
	CGFloat bd = [gameWrapper boardDepth];
    CGFloat boardImgWidth = 2 * th * bs;
    CGFloat boardImgHeight = th * bs + bd;
    CGSize frameSize = skview.frame.size;
	mvo.x = MAX (0, (boardImgWidth - frameSize.width)/2);
	mvo.y = MAX (0, (boardImgHeight - (frameSize.height - TOOL_ICON_HEIGHT))/2);
	return mvo;
}



-(CGPoint) touchLocation:(UITouch*)touch {
    CGPoint currentPoint = [touch locationInView:[self skview]];
    int bs = [gameWrapper boardSize];
    int renderTileHeight = 1;
    CGFloat px = currentPoint.x / (2*renderTileHeight) + bs/2;
    CGFloat py = bs/2 - currentPoint.y / renderTileHeight;
    return CGPointMake(px,py);
}



-(void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
	// single-touch
	if ([touches count] == 1) {
		UITouch *touch = [touches anyObject];
        CGPoint touchPoint = [touch locationInView:[self skview]];
        CGPoint mapPoint = [map locationInMapImage:touchPoint];
        [gameWrapper touchIsometricMapAt:mapPoint];
    }
    [super touchesBegan:touches withEvent:event];
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
    
	// single-touch
	if ([touches count] == 1) {
		UITouch *touch = [touches anyObject];
        CGPoint touchPoint = [touch locationInView:[self skview]];
        CGPoint mapPoint = [map locationInMapImage:touchPoint];
        [gameWrapper touchIsometricMapAt:mapPoint];
	}
    [super touchesMoved:touches withEvent:event];
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    [gameWrapper untouchCell];
    [super touchesEnded:touches withEvent:event];
}


@end
