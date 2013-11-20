//
//  PZIsometricScene.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZIsometricScene.h"
#import "PZDefs.h"

#define boardIndex(SIZE,X,Y,Z) ((X) + (SIZE) * ((Y) + (SIZE) * (Z)))

@implementation PZIsometricScene

@synthesize game;

-(PZIsometricScene*) initWithSize:(CGSize)size forGame:(PZGameWrapper*)gameWrapper {
    self = [super initWithSize:size];
    game = gameWrapper;
    tileNode = [SKNode node];
    toolbar = [PZToolbarNode newToolbarNodeForGame:game withWidth:size.width];
    toolbar.position = CGPointMake(0, [toolbar height]);
    toolbar.zPosition = 1;
    [self addChild:toolbar];

    return self;
}

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)newTileHeight {
    CGRect frame = [[self view] frame];
    mapCenter = CGPointMake(frame.size.width/2 - offset.x,
                            frame.size.height/2 + offset.y);
    tileHeight = newTileHeight;

    if ([map parent])
        [map removeFromParent];
    map = nil;
    
    if ([tileNode parent])
        [tileNode removeFromParent];
    PZIsometricMapNode *node = [PZIsometricMapNode newMapForGame:game withVisibleRect:[self visibleMapRect] withTileHeight:newTileHeight];
    node.position = mapCenter;
    node.zPosition = 0;
    [self addChild:node];
    map = node;

    /*
    [self iterateOverIsometricRegion:self];
        [self flushStaleSprites:[game microticksPerSecond]];
        tileNode.position = mapCenter;
        tileNode.zPosition = 0;
        [tileNode setScale:tileHeight / TILE_SPRITE_HEIGHT];
        if (![tileNode parent])
            [self addChild:tileNode];
*/
     

    [toolbar refresh];
}

-(CGRect) visibleMapRect {
    CGRect frame = [[self view] frame];
    CGPoint mapTopLeft = [self locationInMapImage:CGPointMake(0,0)];
    CGPoint mapBottomRight = [self locationInMapImage:CGPointMake(frame.size.width,frame.size.height)];
    return CGRectMake(mapTopLeft.x-2,mapTopLeft.y-1,mapBottomRight.x-mapTopLeft.x+4,mapBottomRight.y-mapTopLeft.y+1);
}


-(void)iterateOverIsometricRegion:(NSObject<PZBoardIterator>*)iter {
    CGRect mapFrame = [self visibleMapRect];
    [game iterateOverIsometricRegion:mapFrame withIterator:iter];
}

-(CGPoint) locationInMapImage:(CGPoint)locationInView {
    CGRect f = [self view].frame;
    int bs = [game boardSize];
    CGFloat px = (locationInView.x - mapCenter.x) / tileHeight + bs;
    CGFloat py = bs/2 - ((f.size.height - locationInView.y) - mapCenter.y) / tileHeight;
    return CGPointMake(px,py);
}

-(CGPoint) locationInSpriteView:(CGPoint)locationInMapImage {
    int bs = [game boardSize];
    CGFloat sx = locationInMapImage.x - bs*tileHeight + mapCenter.x;
    CGFloat sy = bs*tileHeight/2 - locationInMapImage.y + mapCenter.y;
    return CGPointMake(sx,sy);
}


@end
