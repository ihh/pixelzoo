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
	} else {
		NSLog(@"Couldn't find game XML file");
	}

	if (game == NULL)
		NSLog(@"Couldn't get Game");

	// tell view about its controller (hacky, this)
	[(pixelzooView*) [self view] setController:self];   // HACK HACK HACK

	// reset touch tracking vars
	mouseMoved = 0;
	
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


// cellSize method - calculates the size of a cell.
- (CGFloat) cellSize {
	CGFloat width = self.view.frame.size.width;
	CGFloat height = self.view.frame.size.height;
	
	CGFloat dim = MIN(width,height);
	CGFloat cs = (int) (dim / game->board->size);
	
	return cs;
}

// boardRect method - returns the drawing rectangle of the board
- (CGRect) boardRect {
	CGFloat cs = [self cellSize];
	return CGRectMake(0, 0, cs * game->board->size, cs * game->board->size);
}

// toolboxRect method - returns the drawing rectangle of entire toolbox
- (CGRect) toolboxRect {
	CGFloat width = self.view.frame.size.width;
	
	CGFloat cs = [self cellSize];
	CGFloat tx = cs * game->board->size; 
	CGFloat tw = width - tx;
	CGFloat th = tw;

	return CGRectMake(tx, 0, tw, th * numberOfToolsVisible(game));
}

// toolRect method - returns the drawing rectangle of tool with given index
- (CGRect) toolRect:(int)nTool {
	return [self toolPartialRect:nTool startingAt:0 endingAt:1];
}

// toolRect method - returns the drawing sub-rectangle of tool, suitable for showing reserve level
- (CGRect)toolPartialRect:(int)nTool startingAt:(CGFloat)startFraction endingAt:(CGFloat)endFraction {
	CGFloat width = self.view.frame.size.width;
	CGFloat height = self.view.frame.size.height;
	
	CGFloat cs = [self cellSize];
	CGFloat tx = cs * game->board->size; 
	CGFloat tw = width - tx;
	CGFloat th = MIN (tw, height / numberOfToolsVisible(game));
	
	return CGRectMake(tx + startFraction*tw, nTool*th, tw * (endFraction - startFraction), th);	
}

/* Event handlers */
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    
    mouseSwiped = NO;
    UITouch *touch = [touches anyObject];
    
    if ([touch tapCount] == 2) {
//        double-tap
        return;
    }
	
    lastPoint = [touch locationInView:self.view];

	CGRect boardRect = [self boardRect];
	CGRect toolboxRect = [self toolboxRect];

	if (CGRectContainsPoint(boardRect, lastPoint)) {
		CGFloat cellSize = [self cellSize];
		game->toolPos.x = lastPoint.x / cellSize;
		game->toolPos.y = lastPoint.y / cellSize;
		
		game->toolActive = 1;
	} else if (CGRectContainsPoint (toolboxRect, lastPoint)) {
		int nTool = 0;
		Stack *toolStack = RBTreeEnumerate (game->toolByName, NULL, NULL);
		StringMapNode *toolNode;
		while ((toolNode = StackPop(toolStack)) != NULL) {
			Tool *tool = toolNode->value;
			if (!tool->hidden) {
				CGRect toolRect = [self toolRect:nTool];
				if (CGRectContainsPoint(toolRect, lastPoint)) {
					game->selectedTool = tool;
					break;
				}
				++nTool;
			}
		}
		deleteStack (toolStack);
	}
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
    mouseSwiped = YES;
    
    UITouch *touch = [touches anyObject];   
    lastPoint = [touch locationInView:self.view];
	
	CGFloat cellSize = [self cellSize];
	game->toolPos.x = lastPoint.x / cellSize;
	game->toolPos.y = lastPoint.y / cellSize;
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    
    UITouch *touch = [touches anyObject];
    
    if ([touch tapCount] == 2) {
//        drawImage.image = nil;
        return;
    }
    
    if(!mouseSwiped) {
		/* tapped in place */
	}
	game->toolActive = 0;

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
