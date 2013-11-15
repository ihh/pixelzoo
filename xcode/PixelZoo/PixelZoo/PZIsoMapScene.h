//
//  PZIsoMapScene.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZGameWrapper.h"

@interface PZIsoMapScene : SKScene

@property PZGameWrapper* game;

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight ;

@end
