//
//  PZCellNode.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/17/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZCellNode.h"
#import "PZDefs.h"
#import "pixelzoo.h"

#define DefaultSpriteName @"Grass.png"

@implementation PZCellNode

@synthesize lastModified;
@synthesize lastUpdated;

+(PZCellNode*)newForGame:(PZGameWrapper*)gw x:(int)x y:(int)y z:(int)z {
    const int bs = [gw boardSize];
    const int bd = [gw boardDepth];
    // a bit hacky: reaches into PixelZoo guts
    const char* sprite = [gw cellSpriteAtX:x y:y z:z];
    PZCellNode *node = [[PZCellNode alloc] initWithImageNamed:sprite ? [NSString stringWithUTF8String:sprite] : DefaultSpriteName];
    node.lastModified = [gw cellLastModifiedTimeAtX:x y:y z:z];
    node.anchorPoint = CGPointMake(node.size.width / 2, TILE_SPRITE_HEIGHT / 2);
    CGPoint mapCoord = [gw isometricMapCoordAtX:x y:y z:z forTileHeight:TILE_SPRITE_HEIGHT];
    node.position = CGPointMake(mapCoord.x-TILE_SPRITE_HEIGHT*bs,mapCoord.y-TILE_SPRITE_HEIGHT*bs/2);
    node.zPosition = x + y + (z-bd)*2*bs;
    return node;
}

@end
