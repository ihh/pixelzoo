//
//  PZIsometricScene.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZIsometricScene.h"
#import "PZDefs.h"

#define boardIndex(SIZE,X,Y,Z) ((X) + (SIZE) * ((Y) + (SIZE) * (Z)))

@implementation PZIsometricScene

@synthesize game;
@synthesize worldDescriptor;
@synthesize lockDescriptor;

-(PZIsometricScene*) initWithSize:(CGSize)size forGame:(PZGameWrapper*)gameWrapper withWorld:(PZWorldDescriptor*)world withLock:(PZLockDescriptor*)lock forController:(UIViewController*)controller {
    self = [super initWithSize:size];
    game = gameWrapper;
    worldDescriptor = world;
    lockDescriptor = lock;
    viewController = controller;
    
    toolbar = [PZToolbarNode newToolbarNodeForGame:game withWidth:size.width];
    toolbar.position = CGPointMake(0, [toolbar height]);
    toolbar.zPosition = 1;
    [self addChild:toolbar];

    worldLabel = [SKLabelNode labelNodeWithFontNamed:@"Chalkboard"];
    worldLabel.verticalAlignmentMode = SKLabelVerticalAlignmentModeTop;
    worldLabel.horizontalAlignmentMode = SKLabelHorizontalAlignmentModeLeft;
    worldLabel.position = CGPointMake(0,size.height);
    worldLabel.text = worldDescriptor ? [worldDescriptor name] : @TUTORIAL_WORLD_NAME;
    worldLabel.zPosition = 1;
    [self addChild:worldLabel];
    
    countLabel = [SKLabelNode labelNodeWithFontNamed:@"Chalkboard"];
    countLabel.verticalAlignmentMode = SKLabelVerticalAlignmentModeTop;
    countLabel.horizontalAlignmentMode = SKLabelHorizontalAlignmentModeCenter;
    countLabel.position = CGPointMake(size.width / 2,size.height);
    countLabel.zPosition = 1;
    if (worldDescriptor)
        [self addChild:countLabel];

    lockLabel = [SKLabelNode labelNodeWithFontNamed:@"Chalkboard"];
    lockLabel.verticalAlignmentMode = SKLabelVerticalAlignmentModeTop;
    lockLabel.horizontalAlignmentMode = SKLabelHorizontalAlignmentModeRight;
    lockLabel.position = CGPointMake(size.width,size.height);
    lockLabel.zPosition = 1;
    if (lockDescriptor)
        [self addChild:lockLabel];
    

    currentTileHeight = 1;

    return self;
}

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)newTileHeight {
    CGRect frame = [[self view] frame];
    mapCenter = CGPointMake(frame.size.width/2 - offset.x,
                            frame.size.height/2 + offset.y);
    tileHeight = newTileHeight;

    if ([map parent])
        [map removeFromParent];
    map = nil;
    
    PZIsometricMapNode *node = [PZIsometricMapNode newMapForGame:game withVisibleRect:[self visibleMapRect] withTileHeight:newTileHeight];
    node.position = mapCenter;
    node.zPosition = 0;
    [self addChild:node];
    map = node;

    //	NSLog(@"triggerRedraw");
    NSInteger expiryTime = [lockDescriptor lockExpiryWait];
    lockLabel.text = [NSString stringWithFormat:@"%d:%02d",(int)(expiryTime/60),(int)(expiryTime%60)];
    
    countLabel.text = [worldDescriptor userIsOwner]
    ? [NSString stringWithFormat:@"%d",[game incumbentCount]]
    : [NSString stringWithFormat:@"%d/%d",[game incumbentCount],[game challengerCount]];
    
    [toolbar refresh];
}

-(CGRect) visibleMapRect {
    CGRect frame = [[self view] frame];
    CGPoint mapTopLeft = [self locationInMapImage:CGPointMake(0,0)];
    CGPoint mapBottomRight = [self locationInMapImage:CGPointMake(frame.size.width,frame.size.height)];
    return CGRectMake(mapTopLeft.x-2,mapTopLeft.y-1,mapBottomRight.x-mapTopLeft.x+4,mapBottomRight.y-mapTopLeft.y+2);
}


-(void)iterateOverIsometricRegion:(NSObject<PZBoardIterator>*)iter {
    CGRect mapFrame = [self visibleMapRect];
    [game iterateOverIsometricRegion:mapFrame withIterator:iter];
}

-(CGPoint) locationInMapImage:(CGPoint)locationInView {
    CGRect f = [self view].frame;
    int bs = [game boardSize];
    CGFloat px = (locationInView.x - mapCenter.x) / tileHeight + bs;
    CGFloat py = bs/2 - ((f.size.height - locationInView.y) - mapCenter.y) / tileHeight;
    return CGPointMake(px,py);
}

-(CGPoint) locationInSpriteView:(CGPoint)locationInMapImage {
    int bs = [game boardSize];
    CGFloat sx = locationInMapImage.x - bs*tileHeight + mapCenter.x;
    CGFloat sy = bs*tileHeight/2 - locationInMapImage.y + mapCenter.y;
    return CGPointMake(sx,sy);
}

-(void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
	// single-touch
	if ([touches count] == 1) {
		UITouch *touch = [touches anyObject];
        CGPoint touchPoint = [touch locationInView:[self view]];
        CGPoint mapPoint = [self locationInMapImage:touchPoint];
        [game touchIsometricMapAt:mapPoint];
    }
    [super touchesBegan:touches withEvent:event];
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
    
	// single-touch
	if ([touches count] == 1) {
		UITouch *touch = [touches anyObject];
        CGPoint touchPoint = [touch locationInView:[self view]];
        CGPoint mapPoint = [self locationInMapImage:touchPoint];
        [game touchIsometricMapAt:mapPoint];
	}
    [super touchesMoved:touches withEvent:event];
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    [game untouchCell];
    [super touchesEnded:touches withEvent:event];
}

/* Gesture recognizers */
- (void)handlePan:(UIPanGestureRecognizer *)recognizer {
    
    [game untouchCell];
    CGPoint loc = [recognizer locationInView:self.view];
    CGSize frameSize = [self.view frame].size;
    const bool
        inLeftMargin = loc.x < PAN_MARGIN,
        inRightMargin = loc.x >= frameSize.width - PAN_MARGIN,
        inTopMargin = loc.y < PAN_MARGIN,
        inBottomMargin = loc.y >= frameSize.height - PAN_MARGIN;
    switch (recognizer.state) {

        case UIGestureRecognizerStateBegan:
            viewOffsetAtStartOfPan = currentViewOffset;
            break;

        case UIGestureRecognizerStateChanged:
            if ([self moveToolSelected]) {
                CGPoint trans = [recognizer translationInView:self.view];
            
                currentViewOffset.x = viewOffsetAtStartOfPan.x - trans.x;
                currentViewOffset.y = viewOffsetAtStartOfPan.y - trans.y;

            } else if (inLeftMargin || inRightMargin || inTopMargin || inBottomMargin) {
                if (inLeftMargin)
                    currentViewOffset.x -= PAN_RATE;
                else if (inRightMargin)
                    currentViewOffset.x += PAN_RATE;

                if (inTopMargin)
                    currentViewOffset.y -= PAN_RATE;
                else if (inBottomMargin)
                    currentViewOffset.y += PAN_RATE;

            }
            break;

        default:
            break;
    }
    CGPoint mvo = [self maxViewOffset];
    currentViewOffset.x = MAX (-mvo.x, MIN (mvo.x, currentViewOffset.x));
    currentViewOffset.y = MAX (-mvo.y, MIN (mvo.y, currentViewOffset.y));
}

- (void)handlePinch:(UIPinchGestureRecognizer *)recognizer {
	
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
            [game untouchCell];
        } else {
            zooming = 0;
        }
    }
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
    [game postTurn];
}

/* Board updates */
- (void)callGameLoop
{
    NSInteger expiryTime = [lockDescriptor lockExpiryWait];
    if (expiryTime < 0) {
        [[viewController navigationController] popViewControllerAnimated:YES];
    } else {
        [game updateGame];
        [self showMapWithOffset:currentViewOffset withTileHeight:currentTileHeight];
    }
}

// helpers

- (bool) moveToolSelected {
    return [game selectedToolNumber] < 0;
}


- (CGPoint) maxViewOffset {
	CGPoint mvo;
	CGFloat th = currentTileHeight;
	CGFloat bs = [game boardSize];
	CGFloat bd = [game boardDepth];
    CGFloat boardImgWidth = 2 * th * bs;
    CGFloat boardImgHeight = th * bs + bd;
    CGSize frameSize = [self view].frame.size;
	mvo.x = MAX (0, (boardImgWidth - frameSize.width)/2);
	mvo.y = MAX (0, (boardImgHeight - (frameSize.height - TOOL_ICON_HEIGHT))/2);
	return mvo;
}



@end
