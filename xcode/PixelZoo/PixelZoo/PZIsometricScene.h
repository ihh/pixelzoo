//
//  PZIsometricScene.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZGameWrapper.h"
#import "PZToolbarNode.h"
#import "PZIsometricMapNode.h"

@interface PZIsometricScene : SKScene {
    PZIsometricMapNode *map;
    PZToolbarNode *toolbar;
}

@property PZGameWrapper* game;

-(PZIsometricScene*) initWithSize:(CGSize)size forGame:(PZGameWrapper*)gameWrapper;

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight;

-(CGPoint) locationInMapImage:(CGPoint)locationInView;

@end
