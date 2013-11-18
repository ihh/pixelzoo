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
    int bs = [game boardSize], bd = [game boardDepth];
    tileNode = [SKNode node];
    tileSprite = (PZCellNode*__strong*)calloc(bs*bs*bd, sizeof(PZCellNode*));
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
    
    if (tileHeight < TILE_SPRITE_HEIGHT / 4) {
        if ([tileNode parent])
            [tileNode removeFromParent];
        PZIsometricMapNode *node = [PZIsometricMapNode newMapForGame:game withTileHeight:newTileHeight];
        node.position = mapCenter;
        node.zPosition = 0;
        [self addChild:node];
        map = node;
    } else {
        [self iterateOverIsometricRegion:self];
        [self flushStaleSprites:[game microticksPerSecond]];
        tileNode.position = mapCenter;
        tileNode.zPosition = 0;
        [tileNode setScale:tileHeight / TILE_SPRITE_HEIGHT];
        if (![tileNode parent])
            [self addChild:tileNode];
//        NSLog(@"Updated tileNode position (%f,%f) scale %f",tileNode.position.x,tileNode.position.y,tileHeight/TILE_SPRITE_HEIGHT);
    }
    

    [toolbar refresh];
}

-(void)iterateOverIsometricRegion:(NSObject<PZBoardIterator>*)iter {
    CGRect frame = [[self view] frame];
    CGPoint mapTopLeft = [self locationInMapImage:CGPointMake(0,0)];
    CGPoint mapBottomRight = [self locationInMapImage:CGPointMake(frame.size.width,frame.size.height)];
    CGRect mapFrame = CGRectMake(mapTopLeft.x-2,mapTopLeft.y-1,mapBottomRight.x-mapTopLeft.x+4,mapBottomRight.y-mapTopLeft.y+1);
//    NSLog(@"Map frame is (%f,%f) + (%f,%f)",mapFrame.origin.x,mapFrame.origin.y,mapFrame.size.width,mapFrame.size.height);
    [game iterateOverIsometricRegion:mapFrame withIterator:iter];
}


-(void)visitCellAtX:(int)x y:(int)y z:(int)z {
    [self updateSpriteAtX:x y:y z:z];
    /*
    if (x % 16 == 0 && y % 16 == 0) {
        SKSpriteNode *rect = [[SKSpriteNode alloc] initWithColor:[SKColor greenColor] size:CGSizeMake(4,4)];
        CGPoint mapCoord = [game isometricMapCoordAtX:x y:y z:z forTileHeight:tileHeight];
        CGPoint screenCoord = [self locationInSpriteView:mapCoord];
//    NSLog(@"Board (%d,%d,%d) -> mapImage (%f,%f) -> screen (%f,%f)",x,y,z,mapCoord.x,mapCoord.y,screenCoord.x,screenCoord.y);
        rect.position = screenCoord;
        rect.zPosition = 100;
        [self addChild:rect];
    }
*/
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

-(PZCellNode*) getSprite:(int)i{
    return tileSprite[i];
}

-(void) updateSpriteAtX:(int)x y:(int)y z:(int)z {
    const int bs = [game boardSize];
    const int i = boardIndex(bs,x,y,z);
    PZCellNode *oldSprite = [self getSprite:i];
    if (oldSprite == nil || [oldSprite lastModified] < [game cellLastModifiedTimeAtX:x y:y z:z]) {
        if (oldSprite)
            [self destroySprite:i];
        PZCellNode *newSprite = [PZCellNode newForGame:game x:x y:y z:z];
        tileSprite[i] = newSprite;
        [tileNode addChild:newSprite];
//        NSLog(@"Sprite (%d,%d,%d) position (%f,%f)",x,y,z,newSprite.position.x,newSprite.position.y);
    }
    tileSprite[i].lastUpdated = [game boardClock];
}

-(void) destroySprite:(int)i {
    PZCellNode *oldSprite = tileSprite[i];
    [oldSprite removeFromParent];
    tileSprite[i] = nil;
}

-(void) flushStaleSprites:(long long)minimumAge {
    const int bs = [game boardSize];
    const int bd = [game boardDepth];
    long long minUpdateTime = [game boardClock] - minimumAge;
    for (int i = bs*bs*bd-1; i >= 0; --i) {
        PZCellNode* sprite = [self getSprite:i];
        if ([sprite lastUpdated] < minUpdateTime)
            [self destroySprite:i];
    }
    
}


@end
