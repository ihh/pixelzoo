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

    center = [[SKSpriteNode alloc] initWithColor:[SKColor greenColor] size:CGSizeMake(1,1)];
    center.position = CGPointMake(size.width/2,size.height/2);
    center.zPosition = 100;
    [self addChild:center];

    newCenter = [[SKSpriteNode alloc] initWithColor:[SKColor yellowColor] size:CGSizeMake(1,1)];
    newCenter.position = CGPointMake(size.width/2,size.height/2);
    newCenter.zPosition = 100;
    [self addChild:newCenter];

    return self;
}

-(void) showMapWithOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight forController:(UIViewController*)controller {
    PZIsometricMapNode *node = [PZIsometricMapNode newMapForGame:game withOffset:offset withTileHeight:tileHeight forController:controller];
    SKView *skview = [self view];
    node.position = CGPointMake(skview.frame.size.width/2 - offset.x,
                                skview.frame.size.height/2 + offset.y);
    node.zPosition = 0;

    newCenter.position = CGPointMake(skview.frame.size.width/2,skview.frame.size.height/2);

    
    if ([map parent])
        [map removeFromParent];
    [self addChild:node];
    map = node;
    [toolbar refresh];
}

-(CGPoint) locationInMapImage:(CGPoint)locationInView {
    CGRect f = [self view].frame;
//    NSLog(@"view.frame.origin=(%f,%f) view.frame.size=(%f,%f)",f.origin.x,f.origin.y,f.size.width,f.size.height);
    return [map locationInMapImage:locationInView];
}

@end
