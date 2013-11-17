//
//  PZIsometricScene.m
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZIsometricScene.h"

@implementation PZIsometricScene

@synthesize game;

-(PZIsometricScene*) initWithSize:(CGSize)size forGame:(PZGameWrapper*)gameWrapper {
    self = [super initWithSize:size];
    game = gameWrapper;
    toolbar = [PZToolbarNode newToolbarNodeForGame:game withWidth:size.width];
    toolbar.position = CGPointMake(0, [toolbar height]);
    toolbar.zPosition = 1;
    [self addChild:toolbar];

    return self;
}

-(void)testMapCoord:(int)x y:(int)y z:(int)z {
    CGPoint p = [game isometricMapCoordAtX:x y:y z:z forTileHeight:tileHeight];
    CGPoint s = [self locationInSpriteView:p];
    NSLog(@"xyz=(%d,%d,%d) Map (%f,%f) Screen (%f,%f)",x,y,z,p.x,p.y,s.x,s.y);
}

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)newTileHeight {
    CGRect frame = [[self view] frame];
    mapCenter = CGPointMake(frame.size.width/2 - offset.x,
                            frame.size.height/2 + offset.y);
    tileHeight = newTileHeight;

    PZIsometricMapNode *node = [PZIsometricMapNode newMapForGame:game withTileHeight:newTileHeight];
    node.position = mapCenter;
    node.zPosition = 0;

    // debug hacks
    [self removeAllChildren];
    [self addChild:toolbar];

    /*
    [self testMapCoord:0 y:0 z:1];
    [self testMapCoord:0 y:128 z:1];
    [self testMapCoord:128 y:0 z:1];
    [self testMapCoord:128 y:128 z:1];
     */
    [self iterateOverIsometricRegion:self];

    if ([map parent])
        [map removeFromParent];
    [self addChild:node];
    map = node;
    [toolbar refresh];
}

-(void)iterateOverIsometricRegion:(NSObject<PZBoardIterator>*)iter {
    CGRect frame = [[self view] frame];
    CGPoint mapTopLeft = [self locationInMapImage:CGPointMake(0,0)];
    CGPoint mapBottomRight = [self locationInMapImage:CGPointMake(frame.size.width,frame.size.height)];
    CGRect mapFrame = CGRectMake(mapTopLeft.x,mapTopLeft.y,mapBottomRight.x-mapTopLeft.x,mapBottomRight.y-mapTopLeft.y);
//    NSLog(@"Map frame is (%f,%f) + (%f,%f)",mapFrame.origin.x,mapFrame.origin.y,mapFrame.size.width,mapFrame.size.height);
    [game iterateOverIsometricRegion:mapFrame withIterator:iter];
}


-(void)visitCellAtX:(int)x y:(int)y z:(int)z {
    if (x % 16 == 0 && y % 16 == 0) {
        SKSpriteNode *rect = [[SKSpriteNode alloc] initWithColor:[SKColor greenColor] size:CGSizeMake(4,4)];
        CGPoint mapCoord = [game isometricMapCoordAtX:x y:y z:z forTileHeight:tileHeight];
        CGPoint screenCoord = [self locationInSpriteView:mapCoord];
//    NSLog(@"Board (%d,%d,%d) -> mapImage (%f,%f) -> screen (%f,%f)",x,y,z,mapCoord.x,mapCoord.y,screenCoord.x,screenCoord.y);
        rect.position = screenCoord;
        rect.zPosition = 100;
        [self addChild:rect];
    }
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
