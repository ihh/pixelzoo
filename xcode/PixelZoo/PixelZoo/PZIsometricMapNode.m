//
//  PZIsometricMapNode.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/16/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZIsometricMapNode.h"
#import "PZDefs.h"

@implementation PZIsometricMapNode

@synthesize game;

+(PZIsometricMapNode*) newMapForGame:(PZGameWrapper*)game withTileHeight:(CGFloat)tileHeight {
    int renderTileHeight;
    PZIsometricMapNode *node;
    CGImageRef boardImg;

    
    // TODO: render only visible area
    // TODO: optimize newNaturalIsometricBoardImage (i.e. isometric bitmap)
    // TODO: preserve bitmap between calls, only render cells that change
    // TODO: for z>0, render shaded 3d blocks instead of tiles (or make this a configurable property of the Particle)
    // TODO: at higher zooms, render z=0 using a bitmap and z>0 using sprites
    // TODO: implement bitmap in ARM for max speed (using Galactic Dan pipelining trick)

    // TODO: try using CGContextDrawImage to place sprite images directly onto map, bypassing Sprite Kit
    
    // TODO: identify & eliminate "premature optimization" in above list
    // TODO: consider if isometric view is really worth the speed hit? (it IS cute)
    
    if (tileHeight < 2) {
        renderTileHeight = tileHeight >= 2 ? 2 : 1;
        boardImg = [game newIsometricBoardImage:renderTileHeight];
    } else {
        for (renderTileHeight = 1; renderTileHeight < MAX_NATURAL_TILE_HEIGHT && 2*renderTileHeight <= tileHeight; renderTileHeight *= 2) { }
        boardImg = [game newNaturalIsometricBoardImage:renderTileHeight];
    }
    
    SKTexture *texture = [SKTexture textureWithCGImage:boardImg];
    node = [[PZIsometricMapNode alloc] initWithTexture:texture];
    CGImageRelease(boardImg);
    node.game = game;
    [node setScale:tileHeight / renderTileHeight];
    node.userInteractionEnabled = YES;
    return node;
}

@end
