//
//  PZIsometricScene.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <SpriteKit/SpriteKit.h>
#import "PZGameWrapper.h"
#import "PZToolbarNode.h"
#import "PZIsometricMapNode.h"

@interface PZIsometricScene : SKScene {
    PZIsometricMapNode *map;
    CGPoint mapCenter;
    CGFloat tileHeight;
    PZToolbarNode *toolbar;
    SKLabelNode *lockLabel, *countLabel, *worldLabel;

    // timers
	NSTimer *evolveTimer;
    
    // zoom
    bool zooming;
    CGFloat currentTileHeight, tileHeightAtStartOfZoom;
    CGPoint viewOffsetAtStartOfZoom;
    
    // pan
    CGPoint currentViewOffset, viewOffsetAtStartOfPan;

    // UIViewController (solely so we can call the navigation controller & pop when turn ends)
    UIViewController *viewController;
}

@property PZGameWrapper* game;

@property (nonatomic, strong) PZWorldDescriptor *worldDescriptor;
@property (nonatomic, strong) PZLockDescriptor *lockDescriptor;

-(PZIsometricScene*) initWithSize:(CGSize)size forGame:(PZGameWrapper*)gameWrapper withWorld:(PZWorldDescriptor*)world withLock:(PZLockDescriptor*)lock forController:(UIViewController*)controller;

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight;

-(CGPoint) locationInMapImage:(CGPoint)locationInView;
-(CGPoint) locationInSpriteView:(CGPoint)locationInMapImage;
-(CGRect) visibleMapRect;

- (void)handlePan:(UIPanGestureRecognizer *)recognizer;
- (void)handlePinch:(UIPinchGestureRecognizer *)recognizer;

- (CGPoint) maxViewOffset;
- (bool) moveToolSelected;

-(void)startTimers;
-(void)stopTimers;
-(void)callGameLoop;


@end
