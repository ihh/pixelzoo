//
//  PZIsoMapScene.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZIsoMapScene.h"

@implementation PZIsoMapScene

@synthesize game;

-(void) showMap {
    CGImageRef boardImg = [game newIsometricBoardImage:1];
    SKTexture *texture = [SKTexture textureWithCGImage:boardImg];
    SKSpriteNode *node = [[SKSpriteNode alloc] initWithTexture:texture];
    SKView *skview = [self view];
    node.position = CGPointMake(skview.frame.size.width/2,
                            skview.frame.size.height/2);
    [self removeAllChildren];
    [self addChild:node];
    CGImageRelease(boardImg);
}

@end
