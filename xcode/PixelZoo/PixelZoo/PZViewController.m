//
//  PZViewController.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZViewController.h"
#import "PZDefs.h"
#import "PZView.h"

@interface PZViewController ()

@end

@implementation PZViewController

@synthesize worldDescriptor;
@synthesize worldLabel;
@synthesize worldView;

@synthesize game;
@synthesize examining;
@synthesize examCoord;
@synthesize viewOrigin;
@synthesize cellSize;

@synthesize lockConnection;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)initGameFromXML:(NSString*)gameXMLString {
    game = pzNewGameFromXmlString([gameXMLString UTF8String], false);

	// tell view about its controller (hacky, this; suspect there'd be a less object-model-violating way if I understood things better)
	[((PZView*) [self worldView]) setPzViewController:self];   // HACK HACK HACK
    
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

-(void)deleteGame {
	if (game)
		pzDeleteGame(game);
	game = NULL;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view.
    worldLabel.text = [worldDescriptor name];

    // POST a lock to SERVER_URL_PREFIX/world/WorldID/lock
    // http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/
    // Create the request.
    NSMutableURLRequest *request = [worldDescriptor getController:@"lock"];
    
    // Specify that it will be a POST request
    request.HTTPMethod = @"POST";
    
    // set header fields
    [request setValue:@"application/xml; charset=utf-8" forHTTPHeaderField:@"Content-Type"];
    
    // Convert data and set request's HTTPBody property
    NSString *toolsString = @"";// [worldDescriptor tools];
    NSData *requestBodyData = [toolsString dataUsingEncoding:NSUTF8StringEncoding];
    request.HTTPBody = requestBodyData;
    
    // Create url connection and fire request
    lockConnection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
}

/* Timers: board updates & rendering */
-(void)startTimers
{
	double renderPeriod = 1. / REDRAWS_PER_SECOND;
    redrawTimer = [NSTimer scheduledTimerWithTimeInterval:renderPeriod target:self selector:@selector(triggerRedraw) userInfo:self repeats:YES];
    
	double evolvePeriod = 1. / GAMELOOP_CALLS_PER_SECOND;
    evolveTimer = [NSTimer scheduledTimerWithTimeInterval:evolvePeriod target:self selector:@selector(callGameLoop) userInfo:self repeats:YES];
    
	// start the Game
	pzStartGame(game);
}

-(void)stopTimers
{
	[redrawTimer invalidate];
	[evolveTimer invalidate];
}


/* Board updates */
- (void)callGameLoop
{
    pzUpdateGame(game,GAMELOOP_CALLS_PER_SECOND,0);
}


/* Rendering */
- (void)triggerRedraw
{
    //	NSLog(@"triggerRedraw");
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
	CGFloat boardSize = pzGetBoardSize(self->game) * [self cellSize];
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
	CGFloat consoleBoardSize = pzGetBoardSize(self->game) * magCellSize;
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
	return CGRectMake(worldView.frame.size.width - TOOLBAR_WIDTH, 0, TOOLBAR_WIDTH, TOOLBAR_WIDTH * (pzGetNumberOfTools(game) + EXTRA_TOOLS_AT_TOP + EXTRA_TOOLS_AT_BOTTOM));
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
	CGFloat th = MIN (tw, height / (pzGetNumberOfTools(game) + EXTRA_TOOLS_AT_TOP + EXTRA_TOOLS_AT_BOTTOM));

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
			
			if (pzGetSelectedToolNumber(game) < 0) {
				examCoord.x = x;
				examCoord.y = y;
                
				examining = 1;
				pzUntouchCell(game);
                
			} else {
				pzTouchCell(game,x,y);
			}
			
		} else if (CGRectContainsPoint(toolboxRect, currentPoint)) {
			pzUntouchCell(game);
			if (CGRectContainsPoint (toolboxRect, currentPoint)) {
				int nTool = 0;
				CGRect moveToolRect = [self toolRect:(nTool++)];
				// examine
				if (CGRectContainsPoint(moveToolRect, currentPoint)) {
					pzUnselectTool(game);
				} else {
					// Game tools
                    int nTools = pzGetNumberOfTools(game);
					int foundTool = false;
                    for (int nTool = 0; nTool < nTools; ++nTool) {
                        CGRect toolRect = [self toolRect:(nTool+1)];
                        if (CGRectContainsPoint(toolRect, currentPoint)) {
                            pzSelectTool(game, nTool);
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
            
			if (pzGetSelectedToolNumber(game) < 0) {
				examCoord.x = x;
				examCoord.y = y;
				
			} else {
                pzTouchCell(game, x, y);
			}
		}
	}
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    
	pzUntouchCell(game);
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
		pzUntouchCell(game);
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
		pzUntouchCell(game);
	} else {
		zooming = 0;
	}
	
}

- (CGFloat) minCellSize {
	CGRect boardRect = [self boardRect];
	CGFloat minDim = MIN (boardRect.size.width, boardRect.size.height);
	double minSize = minDim / (CGFloat) pzGetBoardSize(self->game);
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
	CGFloat boardSize = cs * pzGetBoardSize(self->game);;
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

- (void)dealloc {
	[self stopTimers];
	[self deleteGame];
}

#pragma mark NSURLConnection Delegate Methods

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    // A response has been received, this is where we initialize the instance var
    // so that we can append data to it in the didReceiveData method
    // Furthermore, this method is called each time there is a redirect so reinitializing it
    // also serves to clear it
    lockData = [[NSMutableData alloc] init];
    httpLockResponse = (NSHTTPURLResponse*)response;
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    // Append the new data to the instance variable
    [lockData appendData:data];
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection
                  willCacheResponse:(NSCachedURLResponse*)cachedResponse {
    // Return nil to indicate not necessary to store a cached response for this connection
    return nil;
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection {
    // The request is complete and data has been received
    // We can parse the stuff in the instance variable now
    
    // if lock successfully POSTed, parse return body using GDataXMLDocument; use xpath to get <game>...</game>, also lock expiration time
    if ([httpLockResponse statusCode] == 201)  // 201 CREATED
    {
        // parse data using GDataXMLDocument, use xpath to extract <game>...</game> element
        NSError * error = nil;
        GDataXMLDocument *lockDoc = [[GDataXMLDocument alloc] initWithData:lockData
                                                                   options:0 error:&error];
        
        NSArray *gamesArray = [lockDoc nodesForXPath:@"//lock/world/game" error:nil];
        GDataXMLElement *gameElement = [gamesArray objectAtIndex:0];
        NSString *gameString = [gameElement XMLString];
        
        [self initGameFromXML:gameString];
        
        // TODO:
        // add another NSTimer for lock expiration, change "restart" to "quit"
        
        // the following end-of-turn logic needs to go in a common method called by "quit" & timeout:
        // call pzSaveBoardAsXmlString and POST to http://localhost:3000/world/WorldID/turn
        // again see GET/POST tutorial http://codewithchris.com/tutorial-how-to-use-ios-nsurlconnection-by-example/
        
    }
    
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    // The request has failed for some reason!
    // Check the error var
}

@end
