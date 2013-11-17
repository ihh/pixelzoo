//
//  PZIsometricMapNode.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZGameWrapper.h"

@interface PZIsometricMapNode : SKSpriteNode {
    UIViewController* controller;
    int renderTileHeight;
    CGPoint previousTouchLocation;  // hack, workaround for weird touchesMoved coordinate-change effect
}

@property PZGameWrapper* game;

+(PZIsometricMapNode*) newMapForGame:(PZGameWrapper*)game withOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight forController:(UIViewController*)controller;

-(CGPoint) touchLocation:(UITouch*)touch withCorrection:(bool)doCorrection;

-(CGPoint) locationInMapImage:(CGPoint)locationInParent;

@end
