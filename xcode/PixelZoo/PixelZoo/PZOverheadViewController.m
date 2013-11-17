//
//  PZOverheadViewController.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZOverheadViewController.h"
#import "PZDefs.h"
#import "PZOverheadView.h"

@interface PZOverheadViewController ()

@end

@implementation PZOverheadViewController

@synthesize gameWrapper;
@synthesize worldDescriptor;
@synthesize lockDescriptor;

@synthesize lockLabel;
@synthesize worldLabel;
@synthesize countLabel;

@synthesize worldView;

@synthesize examining;
@synthesize examCoord;
@synthesize viewOrigin;
@synthesize cellSize;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)startGame {
	// tell view about its controller (hacky, this; suspect there'd be a less object-model-violating way if I understood things better)
	[((PZOverheadView*) [self worldView]) setGameViewController:self];   // HACK HACK HACK
    
	// attach pan recognizer
	UIPanGestureRecognizer *panner = [[UIPanGestureRecognizer alloc] initWithTarget:self action:@selector(handlePanFrom:)];
    panner.minimumNumberOfTouches = 2;
    [self.view addGestureRecognizer:panner];
    
	// attach pinch recognizer
	UIPinchGestureRecognizer *zoomer = [[UIPinchGestureRecognizer alloc] initWithTarget:self action:@selector(handleZoomFrom:)];
    [self.view addGestureRecognizer:zoomer];
    
	// clear gesture flags
	panning = examining = zooming = 0;

	// initial cell size
    cellSize = INITIAL_PIXELS_PER_CELL;
    
	// start triggerRedraw & callGameLoop timers
	[self startTimers];
}

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view.
    worldLabel.text = [self.worldDescriptor name];
    [self startGame];
}

- (void)viewWillDisappear:(BOOL)animated {
	[self stopTimers];
    [gameWrapper postTurn];
    [super viewWillDisappear:animated];
}


/* Timers: board updates & rendering */
-(void)startTimers
{
	double renderPeriod = 1. / REDRAWS_PER_SECOND;
    redrawTimer = [NSTimer scheduledTimerWithTimeInterval:renderPeriod target:self selector:@selector(triggerRedraw) userInfo:self repeats:YES];
    
	double evolvePeriod = 1. / GAMELOOP_CALLS_PER_SECOND;
    evolveTimer = [NSTimer scheduledTimerWithTimeInterval:evolvePeriod target:self selector:@selector(callGameLoop) userInfo:self repeats:YES];
}

-(void)stopTimers
{
	[redrawTimer invalidate];
	[evolveTimer invalidate];
}


/* Board updates */
- (void)callGameLoop
{
    [gameWrapper updateGame];
}


/* Rendering */
- (void)triggerRedraw
{
    //	NSLog(@"triggerRedraw");
    NSInteger expiryTime = [lockDescriptor lockExpiryWait];
    if (expiryTime < 0) {
        [self.navigationController popViewControllerAnimated:YES];
    } else
        lockLabel.text = [NSString stringWithFormat:@"%d:%02d",(int)(expiryTime/60),(int)(expiryTime%60)];

    countLabel.text = [worldDescriptor userIsOwner]
    ? [NSString stringWithFormat:@"%d",[gameWrapper incumbentCount]]
    : [NSString stringWithFormat:@"%d/%d",[gameWrapper incumbentCount],[gameWrapper challengerCount]];
    
    [self.worldView setNeedsDisplay];
}


// boardRect method - returns the clipping rectangle of the board, in worldView coords
- (CGRect) boardRect {
	CGFloat boardWidth = self.worldView.frame.size.width - TOOLBAR_WIDTH;
	CGFloat boardHeight = self.worldView.frame.size.height - CONSOLE_HEIGHT;
	return CGRectMake(0, 0, boardWidth, boardHeight);
}

// bigBoardRect method - returns the rectangle that the full board would occupy, in worldView coords
- (CGRect) bigBoardRect {
	CGFloat boardSize = [gameWrapper boardSize] * [self cellSize];
	return CGRectMake(-viewOrigin.x, -viewOrigin.y, boardSize, boardSize);
}

// consoleCentroid method - returns the center point of the console, in worldView coords
- (CGPoint)consoleCentroid {
	CGRect cr = [self consoleRect];
	return CGPointMake (cr.origin.x + cr.size.width / 2,
                        cr.origin.y + cr.size.height / 2)  ;
}

// consoleBoardRect method - returns (in worldView coords) the rectangle that the full board would occupy, if it were being displayed in the console window at MAGNIFIED_PIXELS_PER_CELL
- (CGRect) consoleBoardRect {
	CGFloat magCellSize = [self magCellSize];
	CGFloat consoleBoardSize = [gameWrapper boardSize] * magCellSize;
	CGPoint cmid = [self consoleCentroid];
	// want origin + magCellSize*examCoord = consoleCentroid
	return CGRectMake (cmid.x - magCellSize * (.5 + (double) examCoord.x),
                       cmid.y - magCellSize * (.5 + (double) examCoord.y),
                       consoleBoardSize,
                       consoleBoardSize);
}

// magCellSize method - returns the size of a cell in the magnified console window.
- (CGFloat) magCellSize {
	return MAGNIFIED_PIXELS_PER_CELL;
}

// toolboxRect method - returns the drawing/clipping rectangle of entire toolbox
- (CGRect) toolboxRect {
	return CGRectMake(worldView.frame.size.width - TOOLBAR_WIDTH, 0, TOOLBAR_WIDTH, TOOLBAR_WIDTH * ([gameWrapper numberOfTools] + EXTRA_TOOLS_AT_TOP + EXTRA_TOOLS_AT_BOTTOM));
}

// consoleRect method - returns the drawing/clipping rectangle of text console
- (CGRect) consoleRect {
	return CGRectMake(0, worldView.frame.size.height - CONSOLE_HEIGHT, worldView.frame.size.width - TOOLBAR_WIDTH, CONSOLE_HEIGHT);
}

// toolRect method - returns the drawing rectangle of tool with given index
- (CGRect) toolRect:(int)nTool {
	return [self toolPartialRect:nTool startingAt:0 endingAt:1];
}

// toolRect method - returns the drawing sub-rectangle of tool, suitable for showing reserve level
- (CGRect)toolPartialRect:(int)nTool startingAt:(CGFloat)startFraction endingAt:(CGFloat)endFraction {
	CGFloat width = worldView.frame.size.width;
	CGFloat height = worldView.frame.size.height;
	
	CGFloat tw = TOOLBAR_WIDTH;
	CGFloat tx = width - tw;
	CGFloat th = MIN (tw, height / ([gameWrapper numberOfTools] + EXTRA_TOOLS_AT_TOP + EXTRA_TOOLS_AT_BOTTOM));

	return CGRectMake(tx + startFraction*tw, nTool*th, tw * (endFraction - startFraction), th);
}

/* Event handlers */
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    
	// single-touch
	if ([touches count] == 1 && !panning && !zooming) {
		UITouch *touch = [touches anyObject];
		
		CGPoint currentPoint = [touch locationInView:self.worldView];
        
		CGRect boardRect = [self boardRect];
		CGRect toolboxRect = [self toolboxRect];
        
		if (CGRectContainsPoint(boardRect, currentPoint)) {
			int x = (currentPoint.x + viewOrigin.x) / cellSize;
			int y = (currentPoint.y + viewOrigin.y) / cellSize;
			
			if ([gameWrapper selectedToolNumber] < 0) {
				examCoord.x = x;
				examCoord.y = y;
                
				examining = 1;
				[gameWrapper untouchCell];
                
			} else {
				[gameWrapper touchCellAtX:x y:y z:0];
			}
			
		} else if (CGRectContainsPoint(toolboxRect, currentPoint)) {
			[gameWrapper untouchCell];
			if (CGRectContainsPoint (toolboxRect, currentPoint)) {
				int nTool = 0;
				CGRect moveToolRect = [self toolRect:(nTool++)];
				// examine
				if (CGRectContainsPoint(moveToolRect, currentPoint)) {
					[gameWrapper unselectTool];
				} else {
					// Game tools
                    int nTools = [gameWrapper numberOfTools];
					int foundTool = false;
                    for (int nTool = 0; nTool < nTools; ++nTool) {
                        CGRect toolRect = [self toolRect:(nTool+1)];
                        if (CGRectContainsPoint(toolRect, currentPoint)) {
                            [gameWrapper selectTool:nTool];
                            foundTool = true;
                            break;
                        }
                    }
				}
			}
		}
	}
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
    
	// single-touch
	if ([touches count] == 1 && !panning && !zooming) {
		UITouch *touch = [touches anyObject];
		CGPoint currentPoint = [touch locationInView:self.worldView];
		
		CGRect boardRect = [self boardRect];
        
		if (CGRectContainsPoint(boardRect, currentPoint)) {
			int x = (currentPoint.x + viewOrigin.x) / (double) cellSize;
			int y = (currentPoint.y + viewOrigin.y) / (double) cellSize;
            
			if ([gameWrapper selectedToolNumber] < 0) {
				examCoord.x = x;
				examCoord.y = y;
				
			} else {
                [gameWrapper touchCellAtX:x y:y z:0];
			}
		}
	}
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    
	[gameWrapper untouchCell];
	examining = panning = zooming = 0;
}

- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event {
	[self touchesEnded:touches withEvent:event];
}


// pan
- (void)handlePanFrom:(UIPanGestureRecognizer *)recognizer {
	
	if (recognizer.state == UIGestureRecognizerStateBegan) {
		viewOriginAtStartOfPan = viewOrigin;
	}
	
	if (recognizer.state == UIGestureRecognizerStateBegan || recognizer.state == UIGestureRecognizerStateChanged) {
		CGPoint trans = [recognizer translationInView:self.view];
        
		CGPoint mvo = [self maxViewOrigin];
		viewOrigin.x = MAX (0, MIN (viewOriginAtStartOfPan.x - trans.x, mvo.x));
		viewOrigin.y = MAX (0, MIN (viewOriginAtStartOfPan.y - trans.y, mvo.y));
		
		panning = 1;
		[gameWrapper untouchCell];
	} else {
		panning = 0;
	}
    
}

// zoom
- (void)handleZoomFrom:(UIPinchGestureRecognizer *)recognizer {
	
	if (recognizer.state == UIGestureRecognizerStateBegan) {
		cellSizeAtStartOfZoom = cellSize;
		viewOriginAtStartOfZoom = viewOrigin;
	}
    
	if (recognizer.state == UIGestureRecognizerStateBegan || recognizer.state == UIGestureRecognizerStateChanged) {
		CGFloat scale = MAX (0, [recognizer scale]);
		
		CGFloat mincs = [self minCellSize];
		CGFloat maxcs = [self maxCellSize];
		cellSize = MAX (mincs, MIN (maxcs, cellSizeAtStartOfZoom * scale));
		
		CGPoint mvo = [self maxViewOrigin];
		viewOrigin.x = MAX (0, MIN (viewOriginAtStartOfZoom.x * scale, mvo.x));
		viewOrigin.y = MAX (0, MIN (viewOriginAtStartOfZoom.y * scale, mvo.y));
		
		zooming = 1;
		[gameWrapper untouchCell];
	} else {
		zooming = 0;
	}
	
}

- (CGFloat) minCellSize {
	CGRect boardRect = [self boardRect];
	CGFloat minDim = MIN (boardRect.size.width, boardRect.size.height);
	double minSize = minDim / (CGFloat) [gameWrapper boardSize];
    //	CGFloat maxDim = MAX (boardRect.size.width, boardRect.size.height);
    //	double minSize = maxDim / (CGFloat) self->game->board->size;
    //	int minIntSize = ceil((double) minSize);
    //	int minIntSize = (int) minSize;
	return MAX (MIN_PIXELS_PER_CELL, minSize);
}

- (CGFloat) maxCellSize {
	return MAX_PIXELS_PER_CELL;
}

- (CGPoint) maxViewOrigin {
	CGPoint mvo;
	CGFloat cs = [self cellSize];
	CGFloat boardSize = cs * [gameWrapper boardSize];
	CGRect boardRect = [self boardRect];
	mvo.x = MAX (0, boardSize - boardRect.size.width);
	mvo.y = MAX (0, boardSize - boardRect.size.height);
	return mvo;
}	


- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


@end
