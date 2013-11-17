//
//  PZIsometricMapNode.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZIsometricMapNode.h"

@implementation PZIsometricMapNode

@synthesize game;

+(PZIsometricMapNode*) newMapForGame:(PZGameWrapper*)game withTileHeight:(CGFloat)tileHeight {
    int renderTileHeight = tileHeight >= 2 ? 2 : 1;
    CGImageRef boardImg = [game newIsometricBoardImage:renderTileHeight];
    SKTexture *texture = [SKTexture textureWithCGImage:boardImg];
    PZIsometricMapNode *node = [[PZIsometricMapNode alloc] initWithTexture:texture];
    CGImageRelease(boardImg);
    node.game = game;
    [node setScale:tileHeight / renderTileHeight];
    node.userInteractionEnabled = YES;
    return node;
}

@end
