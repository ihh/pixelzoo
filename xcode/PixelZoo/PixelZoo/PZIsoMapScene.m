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

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight {
    int renderTileHeight = tileHeight >= 2 ? 2 : 1;
    CGImageRef boardImg = [game newIsometricBoardImage:renderTileHeight];
    SKTexture *texture = [SKTexture textureWithCGImage:boardImg];
    SKSpriteNode *node = [[SKSpriteNode alloc] initWithTexture:texture];
    SKView *skview = [self view];
    node.position = CGPointMake(skview.frame.size.width/2 - offset.x,
                            skview.frame.size.height/2 - offset.y);
    [node setScale:tileHeight / renderTileHeight];
    [self removeAllChildren];
    [self addChild:node];
    CGImageRelease(boardImg);
}

@end
