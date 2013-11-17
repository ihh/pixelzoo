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

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight {
    PZIsometricMapNode *node = [PZIsometricMapNode newMapForGame:game withOffset:offset withTileHeight:tileHeight];
    SKView *skview = [self view];
    node.position = CGPointMake(skview.frame.size.width/2 - offset.x,
                                skview.frame.size.height/2 + offset.y);
    node.zPosition = 0;

    if ([map parent])
        [map removeFromParent];
    [self addChild:node];
    map = node;
    [toolbar refresh];
}

-(CGPoint) locationInMapImage:(CGPoint)locationInView {
    CGRect f = [self view].frame;
    return [map locationInMapImage:CGPointMake(locationInView.x,f.size.height-locationInView.y)];
}

@end
