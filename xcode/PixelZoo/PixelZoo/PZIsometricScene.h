//
//  PZIsometricScene.h
//  PixelZoo
//
//  Created by Ian Holmes on 11/15/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <SpriteKit/SpriteKit.h>
#import "PZGameWrapper.h"
#import "PZToolbarNode.h"
#import "PZIsometricMapNode.h"
#import "PZCellNode.h"

@interface PZIsometricScene : SKScene <PZBoardIterator> {
    PZIsometricMapNode *map;
    CGPoint mapCenter;
    CGFloat tileHeight;
    PZToolbarNode *toolbar;
    SKNode *tileNode;
    PZCellNode *__strong *tileSprite;
}

@property PZGameWrapper* game;

-(PZIsometricScene*) initWithSize:(CGSize)size forGame:(PZGameWrapper*)gameWrapper;

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight;

-(CGPoint) locationInMapImage:(CGPoint)locationInView;
-(CGPoint) locationInSpriteView:(CGPoint)locationInMapImage;
-(CGRect) visibleMapRect;

-(PZCellNode*) getSprite:(int)i;
-(void) destroySprite:(int)i;
-(void) flushStaleSprites:(long long)minimumAge;

-(void) updateSpriteAtX:(int)x y:(int)y z:(int)z;

@end
