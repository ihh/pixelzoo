//
//  PZToolNode.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZGameWrapper.h"

@interface PZToolNode : SKSpriteNode {
   UIResponder* toolbar;
}

@property int toolnum;
@property PZGameWrapper* game;
@property SKSpriteNode* frame;
@property SKSpriteNode* reserve;

+(PZToolNode*)spriteNodeForToolNumber:(int)n forGame:(PZGameWrapper*)game forToolbar:(UIResponder*)toolbar;

-(void)refresh;

@end
