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

+(PZIsometricMapNode*) newMapForGame:(PZGameWrapper*)game withVisibleRect:(CGRect)mapRect withTileHeight:(CGFloat)tileHeight {
    int renderTileHeight;
    PZIsometricMapNode *node;
    CGImageRef boardImg;

    
    // TODO: optimize newNaturalIsometricBoardImage (i.e. isometric bitmap)
    // TODO: preserve bitmap between calls, only render cells that change
    // TODO: at higher zooms, render z=0 using a bitmap and z>0 using sprites
    // TODO: implement bitmap in ARM for max speed (using Galactic Dan pipelining trick)

    // TODO: try using CGContextDrawImage to place sprite images directly onto map, bypassing Sprite Kit
    
    // TODO: identify & eliminate "premature optimization" in above list
    // TODO: consider if isometric view is really worth the speed hit? (it IS cute)

    CGPoint origin = CGPointMake(0,0);
    if (tileHeight < 2) {
        renderTileHeight = tileHeight >= 2 ? 2 : 1;
        boardImg = [game newIsometricBoardImage:renderTileHeight];
    } else {
        for (renderTileHeight = 1; renderTileHeight < MAX_NATURAL_TILE_HEIGHT && 2*renderTileHeight <= tileHeight; renderTileHeight *= 2) { }
        boardImg = [game newNaturalIsometricBoardImageForMapRect:mapRect withTileHeight:renderTileHeight usingImages:(renderTileHeight >= MIN_TILE_HEIGHT_FOR_SPRITES) storingOriginIn:&origin];
    }
    
    SKTexture *texture = [SKTexture textureWithCGImage:boardImg];
    node = [[PZIsometricMapNode alloc] initWithTexture:texture];
    CGSize fullSize = [game isometricMapSize:renderTileHeight];
    node.anchorPoint = CGPointMake((origin.x + fullSize.width/2)/CGImageGetWidth(boardImg), 1 - (origin.y + fullSize.height/2)/CGImageGetHeight(boardImg));

    node.game = game;
    [node setScale:tileHeight / renderTileHeight];
    node.userInteractionEnabled = YES;

    CGImageRelease(boardImg);
    return node;
}

@end
