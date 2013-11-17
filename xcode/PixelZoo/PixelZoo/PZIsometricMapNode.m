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

+(PZIsometricMapNode*) newMapForGame:(PZGameWrapper*)game withOffset:(CGPoint)offset withTileHeight:(CGFloat)tileHeight forController:(UIViewController*)controller {
    int renderTileHeight = tileHeight >= 2 ? 2 : 1;
    CGImageRef boardImg = [game newIsometricBoardImage:renderTileHeight];
    SKTexture *texture = [SKTexture textureWithCGImage:boardImg];
    PZIsometricMapNode *node = [[PZIsometricMapNode alloc] initWithTexture:texture];
    CGImageRelease(boardImg);
    node.game = game;
    node->controller = controller;
    node->renderTileHeight = renderTileHeight;
    [node setScale:tileHeight / renderTileHeight];
    node.userInteractionEnabled = YES;
    return node;
}

-(CGPoint) locationInMapImage:(CGPoint)locationInParent {
    CGPoint p;
    p.x = (locationInParent.x - self.position.x) / (self.xScale * renderTileHeight);
    p.y = (locationInParent.y - self.position.y) / (self.yScale * renderTileHeight);
//    NSLog(@"locationInParent=(%f,%f) position=(%f,%f) delta=(%f,%f)",locationInParent.x,locationInParent.y,self.position.x,self.position.y,locationInParent.x-self.position.x,locationInParent.y-self.position.y);
    return p;
}

-(CGPoint) touchLocation:(UITouch*)touch withCorrection:(bool)doCorrection {
    CGPoint currentPoint = [touch locationInNode:self];
    CGFloat x = currentPoint.x;
    CGFloat y = currentPoint.y;
    CGPoint lastPoint = [touch previousLocationInNode:self];
    if (doCorrection) {
        x = previousTouchLocation.x + (x - lastPoint.x);
        y = previousTouchLocation.y - (y - lastPoint.y);
    }
    previousTouchLocation.x = x;
    previousTouchLocation.y = y;
//    NSLog(@"Touch=(%f,%f) previous=(%f,%f) remapped=(%f,%f) correction=%d",currentPoint.x,currentPoint.y,lastPoint.x,lastPoint.y,x,y,doCorrection);
    int bs = [game boardSize];
    CGFloat px = x / (2*renderTileHeight) + bs/2;
    CGFloat py = bs/2 - y / renderTileHeight;
    return CGPointMake(px,py);
}

-(void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
	// single-touch
	if ([touches count] == 1) {
		UITouch *touch = [touches anyObject];
//        [game touchIsometricMapAt:[self touchLocation:touch withCorrection:NO]];
    }
    [super touchesBegan:touches withEvent:event];
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {

	// single-touch
	if ([touches count] == 1) {
		UITouch *touch = [touches anyObject];
//        [game touchIsometricMapAt:[self touchLocation:touch withCorrection:YES]];
	}
    [super touchesMoved:touches withEvent:event];
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    [game untouchCell];
    [super touchesEnded:touches withEvent:event];
}

- (UIResponder*) nextResponder {
    return controller;
}

@end
