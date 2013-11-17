//
//  PZIsometricViewController.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <SpriteKit/SpriteKit.h>
#import "PZWorldDescriptor.h"
#import "PZGameWrapper.h"
#import "PZIsometricScene.h"

@interface PZIsometricViewController : UIViewController {
    // timers
	NSTimer *evolveTimer;

    // scenes
    PZIsometricScene* map;

    // zoom
    bool zooming;
    CGFloat currentTileHeight, tileHeightAtStartOfZoom;
    CGPoint viewOffsetAtStartOfZoom;

    // pan
    bool panning;
    CGPoint currentViewOffset, viewOffsetAtStartOfPan;
}

@property (nonatomic, strong) PZWorldDescriptor *worldDescriptor;
@property (nonatomic, strong) PZLockDescriptor *lockDescriptor;
@property (nonatomic, strong) PZGameWrapper *gameWrapper;
@property (weak, nonatomic) IBOutlet SKView *skview;

- (IBAction)handlePan:(UIPanGestureRecognizer *)recognizer;
- (IBAction)handlePinch:(UIPinchGestureRecognizer *)recognizer;

-(CGPoint) touchLocation:(UITouch*)touch;

- (CGPoint) maxViewOffset;

- (bool) moveToolSelected;

@end
