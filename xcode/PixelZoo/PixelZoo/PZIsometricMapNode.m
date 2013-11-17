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

+(PZIsometricMapNode*) newMapForGame:(PZGameWrapper*)game withOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight {
    int renderTileHeight = tileHeight >= 2 ? 2 : 1;
    CGImageRef boardImg = [game newIsometricBoardImage:renderTileHeight];
    SKTexture *texture = [SKTexture textureWithCGImage:boardImg];
    PZIsometricMapNode *node = [[PZIsometricMapNode alloc] initWithTexture:texture];
    CGImageRelease(boardImg);
    node.game = game;
    node->offset = offset;
    node->renderTileHeight = renderTileHeight;
    [node setScale:tileHeight / renderTileHeight];
    node.userInteractionEnabled = YES;
    return node;
}

-(CGPoint) locationInMapImage:(CGPoint)locationInParent {
    int bs = [game boardSize];
    CGFloat px = (locationInParent.x - self.position.x) / (2 * self.xScale * renderTileHeight) + bs/2;
    CGFloat py = bs/2 - (locationInParent.y - self.position.y) / (self.yScale * renderTileHeight);
    return CGPointMake(px,py);
}

@end
