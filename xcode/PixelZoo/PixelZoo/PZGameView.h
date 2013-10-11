//
//  PZGameView.h
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "PZGameViewController.h"

@interface PZGameView : UIView {
	unsigned char *bitmapData;
	int bytesPerRow;
	CGContextRef bitmapContext;
	int redraws;
}

@property (strong, nonatomic) PZGameViewController* gameViewController;


// helpers
- (CGSize) measureText:(char*)text withFont:(UIFont*)font withSpacing:(CGFloat)charSpacing;
- (void) renderTool:(int)nTool withContext:(CGContextRef)ctx withColor:(int)rgb withReserve:(CGFloat)reserve withName:(char*)name asSelected:(BOOL)selectFlag;
- (CGFloat) myInverse:(CGFloat)x;
- (void) setFill:(int)rgb withContext:(CGContextRef)ctx withFactor:(CGFloat)fade withOpacity:(CGFloat)opacity asInverse:(BOOL)inverseFlag;
- (void) setStroke:(int)rgb withContext:(CGContextRef)ctx withFactor:(CGFloat)fade withOpacity:(CGFloat)opacity asInverse:(BOOL)inverseFlag;


@end
