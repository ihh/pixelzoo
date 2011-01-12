//
//  pixelzooViewController.m
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//  Copyright University of California - Berkeley 2010. All rights reserved.
//

#import "pixelzooViewController.h"
#import "pixelzooView.h"

@implementation pixelzooViewController

@synthesize game;
@synthesize viewOrigin;
@synthesize examCoord;
@synthesize examining;

/*
 // The designated initializer. Override to perform setup that is required before the view is loaded.
 - (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
 if ((self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil])) {
 // Custom initialization
 }
 return self;
 }
 */

/*
 // Override to allow orientations other than the default portrait orientation.
 - (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
 // Return YES for supported orientations
 return (interfaceOrientation == UIInterfaceOrientationPortrait);
 }
 */

// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
    [super viewDidLoad];
	NSLog(@"viewDidLoad");
	
	// Load the game XML
	NSString *gameFilePath = [[NSBundle mainBundle] pathForResource:@GAME_XML_FILENAME ofType:@"xml"];  
//	NSLog(@"gameFilePath=%@",gameFilePath);
	NSData *gameXMLData = [NSData dataWithContentsOfFile:gameFilePath];  
	if (gameXMLData) {  
		// initialize the game  
		NSString *gameXMLString = [[NSString alloc] initWithData:gameXMLData encoding:NSUTF8StringEncoding];
		game = newGameFromXmlString([gameXMLString UTF8String]);
		[gameXMLString release];
	} else {
		NSLog(@"Couldn't find game XML file");
	}

	if (game == NULL)
		NSLog(@"Couldn't get Game");

	// set viewOrigin and maxViewOrigin
	viewOrigin.x = viewOrigin.y = 0;
	CGFloat cs = [self cellSize];
	CGFloat boardSize = cs * self->game->board->size;
	CGRect boardRect = [self boardRect];
	maxViewOrigin.x = MAX (0, boardSize - boardRect.size.width);
	maxViewOrigin.y = MAX (0, boardSize - boardRect.size.height);
	
	// tell view about its controller (hacky, this; suspect there'd be a less object-model-violating way if I understood nibs/etc better)
	[(pixelzooView*) [self view] setController:self];   // HACK HACK HACK

	// attach pan recognizer
	UIPanGestureRecognizer *recognizer = [[UIPanGestureRecognizer alloc] initWithTarget:self action:@selector(handlePanFrom:)];
    recognizer.minimumNumberOfTouches = 2;
    [self.view addGestureRecognizer:recognizer];
	[recognizer release];

	panning = 0;
	examining = 0;
	
	// start triggerRedraw & callGameLoop timers
	[self startTimers];
}



/* Timers: board updates & rendering */
-(void)startTimers
{       
	double renderPeriod = 1. / REDRAWS_PER_SECOND;
    redrawTimer = [NSTimer scheduledTimerWithTimeInterval:renderPeriod target:self selector:@selector(triggerRedraw) userInfo:self repeats:YES];

	double evolvePeriod = 1. / GAMELOOP_CALLS_PER_SECOND;
    evolveTimer = [NSTimer scheduledTimerWithTimeInterval:evolvePeriod target:self selector:@selector(callGameLoop) userInfo:self repeats:YES];
}

/* Board updates */
- (void)callGameLoop
{   
	double targetUpdatesPerCell = game->updatesPerSecond / GAMELOOP_CALLS_PER_SECOND;
	double actualUpdatesPerCell, elapsedTime;
	int cellUpdates;
	gameLoop (game, targetUpdatesPerCell, MAX_PROPORTION_TIME_EVOLVING, &actualUpdatesPerCell, &cellUpdates, &elapsedTime);
	if (boardTopOverloadThreshold(game->board) < 1.)
		NSLog(@"updatesPerSecond:%g gameloopCallsPerSecond=%d targetUpdatesPerCell:%g actualUpdatesPerCell:%g firingRate:%g overloadThreshold:%g",game->updatesPerSecond,GAMELOOP_CALLS_PER_SECOND,targetUpdatesPerCell,actualUpdatesPerCell,boardFiringRate(game->board),boardTopOverloadThreshold(game->board));
}


/* Rendering */
- (void)triggerRedraw
{   
//	NSLog(@"triggerRedraw");
	[self.view setNeedsDisplay];
}


// cellSize method - returns the size of a cell.
- (CGFloat) cellSize {
	return PIXELS_PER_CELL;
}

// bigCellSize method - returns the size of a cell in the magnified console window.
- (CGFloat) bigCellSize {
	return MAGNIFIED_PIXELS_PER_CELL;
}

// boardRect method - returns the clipping rectangle of the board
- (CGRect) boardRect {
	CGFloat boardWidth = self.view.frame.size.width - TOOLBAR_WIDTH;
	CGFloat boardHeight = self.view.frame.size.height - CONSOLE_HEIGHT;
	return CGRectMake(0, 0, boardWidth, boardHeight);
}

// bigBoardRect method - returns the rectangle that the full board would occupy
- (CGRect) bigBoardRect {
	CGFloat boardSize = self->game->board->size * [self cellSize];
	return CGRectMake(-viewOrigin.x, -viewOrigin.y, boardSize, boardSize);
}

// consoleCentroid method - returns the center point of the console
- (CGPoint)consoleCentroid {
	CGRect cr = [self consoleRect];
	return CGPointMake (cr.origin.x + cr.size.width / 2,
						cr.origin.y + cr.size.height / 2);
}

// consoleBoardRect method - returns the rectangle that the full board would occupy, if it were being displayed in the console window at MAGNIFIED_PIXELS_PER_CELL
- (CGRect) consoleBoardRect {
	CGFloat bigCellSize = [self bigCellSize];
	CGFloat consoleBoardSize = self->game->board->size * bigCellSize;
	CGPoint cmid = [self consoleCentroid];
	// want origin + bigCellSize*examCoord = consoleCentroid
	return CGRectMake (cmid.x - bigCellSize * (.5 + (double) examCoord.x),
					   cmid.y - bigCellSize * (.5 + (double) examCoord.y),
					   consoleBoardSize,
					   consoleBoardSize);
}

// toolboxRect method - returns the drawing/clipping rectangle of entire toolbox
- (CGRect) toolboxRect {
	return CGRectMake(self.view.frame.size.width - TOOLBAR_WIDTH, 0, TOOLBAR_WIDTH, TOOLBAR_WIDTH * (numberOfToolsVisible(game) + 1));
}

// consoleRect method - returns the drawing/clipping rectangle of text console
- (CGRect) consoleRect {
	return CGRectMake(0, self.view.frame.size.height - CONSOLE_HEIGHT, self.view.frame.size.width - TOOLBAR_WIDTH, CONSOLE_HEIGHT);
}

// toolRect method - returns the drawing rectangle of tool with given index
- (CGRect) toolRect:(int)nTool {
	return [self toolPartialRect:nTool startingAt:0 endingAt:1];
}

// toolRect method - returns the drawing sub-rectangle of tool, suitable for showing reserve level
- (CGRect)toolPartialRect:(int)nTool startingAt:(CGFloat)startFraction endingAt:(CGFloat)endFraction {
	CGFloat width = self.view.frame.size.width;
	CGFloat height = self.view.frame.size.height;
	
	CGFloat tw = TOOLBAR_WIDTH;
	CGFloat tx = width - tw; 
	CGFloat th = MIN (tw, height / (numberOfToolsVisible(game) + 1));
	
	return CGRectMake(tx + startFraction*tw, nTool*th, tw * (endFraction - startFraction), th);	
}

/* Event handlers */
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    
	// single-touch
	if ([touches count] == 1 && !panning) {
		UITouch *touch = [touches anyObject];
		
		CGPoint currentPoint = [touch locationInView:self.view];

		CGRect boardRect = [self boardRect];
		CGRect toolboxRect = [self toolboxRect];

		if (CGRectContainsPoint(boardRect, currentPoint)) {
			CGFloat cellSize = [self cellSize];
			int x = (currentPoint.x + viewOrigin.x) / cellSize;
			int y = (currentPoint.y + viewOrigin.y) / cellSize;
			
			if (game->selectedTool == NULL) {
				examCoord.x = x;
				examCoord.y = y;

				examining = 1;
				game->toolActive = 0;

			} else {
				game->toolPos.x = x;
				game->toolPos.y = y;

				if (!game->toolActive)
					game->lastToolPos = game->toolPos;
			
				game->toolActive = 1;
			}
			
		} else {
			game->toolActive = 0;
			if (CGRectContainsPoint (toolboxRect, currentPoint)) {
				int nTool = 0;
				CGRect moveToolRect = [self toolRect:(nTool++)];
				if (CGRectContainsPoint(moveToolRect, currentPoint)) {
					game->selectedTool = NULL;
				} else {
					Stack *toolStack = RBTreeEnumerate (game->toolByName, NULL, NULL);
					StringMapNode *toolNode;
					while ((toolNode = StackPop(toolStack)) != NULL) {
						Tool *tool = toolNode->value;
						if (!tool->hidden) {
							CGRect toolRect = [self toolRect:(nTool++)];
							if (CGRectContainsPoint(toolRect, currentPoint)) {
								game->selectedTool = tool;
								break;
							}
						}
					}
					deleteStack (toolStack);
				}
			}
		}
	}
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {

	// single-touch
	if ([touches count] == 1 && !panning) {
		UITouch *touch = [touches anyObject];   
		CGPoint currentPoint = [touch locationInView:self.view];
		
		CGRect boardRect = [self boardRect];

		if (CGRectContainsPoint(boardRect, currentPoint)) {
			CGFloat cellSize = [self cellSize];
			int x = (currentPoint.x + viewOrigin.x) / cellSize;
			int y = (currentPoint.y + viewOrigin.y) / cellSize;

			if (game->selectedTool == NULL) {
				examCoord.x = x;
				examCoord.y = y;
				
			} else {
				game->toolPos.x = x;
				game->toolPos.y = y;
			}
		}
	}
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    
	game->toolActive = 0;
	examining = 0;
	panning = 0;
}

- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event {
	[self touchesEnded:touches withEvent:event];
}


// pan
- (void)handlePanFrom:(UIPanGestureRecognizer *)recognizer {
	
	if (recognizer.state == UIGestureRecognizerStateBegan || recognizer.state == UIGestureRecognizerStateChanged) {
		CGPoint trans = [recognizer translationInView:self.view];

		viewOrigin.x = MAX (0, MIN (viewOrigin.x - trans.x, maxViewOrigin.x));
		viewOrigin.y = MAX (0, MIN (viewOrigin.y - trans.y, maxViewOrigin.y));
		
		trans.x = trans.y = 0;
		[recognizer setTranslation:trans inView:self.view];

		panning = 1;
		game->toolActive = 0;
	} else {
		panning = 0;
	}

}


/* release, dealloc */

- (void)didReceiveMemoryWarning {
	// Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
	
	// Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload {
	// Release any retained subviews of the main view.
	// e.g. self.myOutlet = nil;
}


- (void)dealloc {
    [super dealloc];
}

@end
