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
}

- (void)viewWillAppear:(BOOL)animated
{
    map = [[PZIsometricScene alloc] initWithSize:skview.frame.size forGame:gameWrapper];
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
    [map showMapWithOffset:currentViewOffset withTileHeight:currentTileHeight forController:self];
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
            CGFloat maxth = 16;  // hardwire for now
            currentTileHeight = MAX (minth, MIN (maxth, tileHeightAtStartOfZoom * scale));
            
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
        CGPoint currentPoint = [touch locationInView:[self skview]];
        // seems like skview is getting resized. original center is green dot, new center (where map is centered) is yellow dot. according to frame, center is still at green dot. what we want is to translate (and probably scale) to get co-ordinates relative to yellow dot.
        // a quick & dirty fix is to initialize skview within viewDidAppear, rather than viewWillAppear. This looks horrible though.
        NSLog(@"touchesBegan: in skview (%f,%f)  center is (%f,%f)",currentPoint.x,currentPoint.y,[self skview].frame.size.width/2,[self skview].frame.size.height/2);
        CGPoint mapPoint = [map locationInMapImage:currentPoint];
//        NSLog(@"touchesBegan: (%f,%f) -> (%f,%f)",currentPoint.x,currentPoint.y,mapPoint.x,mapPoint.y);
    }
    [super touchesBegan:touches withEvent:event];
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
    
	// single-touch
	if ([touches count] == 1) {
		UITouch *touch = [touches anyObject];
        CGPoint currentPoint = [touch locationInView:[self skview]];
//        NSLog(@"touchesMoved=(%f,%f)",currentPoint.x,currentPoint.y);
	}
    [super touchesMoved:touches withEvent:event];
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    [super touchesEnded:touches withEvent:event];
}


@end
