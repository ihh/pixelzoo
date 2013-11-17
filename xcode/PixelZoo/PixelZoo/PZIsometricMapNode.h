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
    int renderTileHeight;
    CGPoint offset;
}

@property PZGameWrapper* game;

+(PZIsometricMapNode*) newMapForGame:(PZGameWrapper*)game withOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight;

-(CGPoint) locationInMapImage:(CGPoint)locationInParent;

@end
