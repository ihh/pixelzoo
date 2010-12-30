//
//  pixelzooViewController.m
//  pixelzoo
//
//  Created by Ian Holmes on 12/28/10.
//  Copyright University of California - Berkeley 2010. All rights reserved.
//

#import "pixelzooViewController.h"

/* global constants */
#define REDRAWS_PER_SECOND 60   /* frame rate */


@implementation pixelzooViewController

@synthesize game;

-(UIColor**) boardColor {
	return boardColor;
}

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

	// create color palette
	int c;
	RGB *rgb =game->board->palette.rgb;
	for (c = 0; c < PaletteMax; ++c) {
		CGFloat r = (CGFloat) rgb->r / RGB_max;
		CGFloat g = (CGFloat) rgb->g / RGB_max;
		CGFloat b = (CGFloat) rgb->b / RGB_max;
		boardColor[c] = [UIColor colorWithRed:r green:g blue:b alpha:1];
	}
	
	// tell view about all our good stuff (hacky, this)
	[[self view] setController:self];
	
	mouseMoved = 0;

	[self startTimers];
}



/* Timers: board updates & rendering */
-(void)startTimers
{       
	double renderPeriod = 1. / REDRAWS_PER_SECOND;
    redrawTimer = [NSTimer scheduledTimerWithTimeInterval:renderPeriod target:self selector:@selector(triggerRedraw) userInfo:self repeats:YES];

	double evolvePeriod = 1 / game->updatesPerSecond;
    evolveTimer = [NSTimer scheduledTimerWithTimeInterval:evolvePeriod target:self selector:@selector(callGameLoop) userInfo:self repeats:YES];
}

/* Board updates */
- (void)callGameLoop
{   
	int updates;
	gameLoop (game, 1., MAX_PROPORTION_TIME_EVOLVING, NULL, &updates, NULL);
//	NSLog(@"%d updates",updates);
}


/* Rendering */
- (void)triggerRedraw
{   
//	NSLog(@"triggerRedraw");
	[self.view setNeedsDisplay];
}

/* Event handlers */
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    
    mouseSwiped = NO;
    UITouch *touch = [touches anyObject];
    
    if ([touch tapCount] == 2) {
//        drawImage.image = nil;
        return;
    }
	
    lastPoint = [touch locationInView:self.view];
    lastPoint.y -= 20;
	
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
    mouseSwiped = YES;
    
    UITouch *touch = [touches anyObject];   
    CGPoint currentPoint = [touch locationInView:self.view];
    currentPoint.y -= 20;
    
/*    
    UIGraphicsBeginImageContext(self.view.frame.size);
    [drawImage.image drawInRect:CGRectMake(0, 0, self.view.frame.size.width, self.view.frame.size.height)];
    CGContextSetLineCap(UIGraphicsGetCurrentContext(), kCGLineCapRound);
    CGContextSetLineWidth(UIGraphicsGetCurrentContext(), 5.0);
    CGContextSetRGBStrokeColor(UIGraphicsGetCurrentContext(), 1.0, 0.0, 0.0, 1.0);
    CGContextBeginPath(UIGraphicsGetCurrentContext());
    CGContextMoveToPoint(UIGraphicsGetCurrentContext(), lastPoint.x, lastPoint.y);
    CGContextAddLineToPoint(UIGraphicsGetCurrentContext(), currentPoint.x, currentPoint.y);
    CGContextStrokePath(UIGraphicsGetCurrentContext());
    drawImage.image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
 */  
 
    lastPoint = currentPoint;
	
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    
    UITouch *touch = [touches anyObject];
    
    if ([touch tapCount] == 2) {
//        drawImage.image = nil;
        return;
    }
    
    
    if(!mouseSwiped) {
/*
        UIGraphicsBeginImageContext(self.view.frame.size);
        [drawImage.image drawInRect:CGRectMake(0, 0, self.view.frame.size.width, self.view.frame.size.height)];
        CGContextSetLineCap(UIGraphicsGetCurrentContext(), kCGLineCapRound);
        CGContextSetLineWidth(UIGraphicsGetCurrentContext(), 5.0);
        CGContextSetRGBStrokeColor(UIGraphicsGetCurrentContext(), 1.0, 0.0, 0.0, 1.0);
        CGContextMoveToPoint(UIGraphicsGetCurrentContext(), lastPoint.x, lastPoint.y);
        CGContextAddLineToPoint(UIGraphicsGetCurrentContext(), lastPoint.x, lastPoint.y);
        CGContextStrokePath(UIGraphicsGetCurrentContext());
        CGContextFlush(UIGraphicsGetCurrentContext());
        drawImage.image = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
*/
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
