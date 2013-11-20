//
//  PZIsometricMapNode.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZGameWrapper.h"

@interface PZIsometricMapNode : SKSpriteNode

@property PZGameWrapper* game;

+(PZIsometricMapNode*) newMapForGame:(PZGameWrapper*)game withVisibleRect:(CGRect)mapRect withTileHeight:(CGFloat)tileHeight;

@end
