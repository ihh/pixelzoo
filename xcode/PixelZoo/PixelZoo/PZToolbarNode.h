//
//  PZToolbarNode.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZGameWrapper.h"

@interface PZToolbarNode : SKNode

@property PZGameWrapper* game;
@property NSArray* tools;

+(PZToolbarNode*)newToolbarNodeForGame:(PZGameWrapper*)game withWidth:(CGFloat)width;

-(CGFloat)height;
-(void)refresh;


@end
