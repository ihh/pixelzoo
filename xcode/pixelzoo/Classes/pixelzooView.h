//
//  pixelzooView.h
//  pixelzoo
//
//  Created by Ian Holmes on 12/29/10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "pixelzooViewController.h"

// PixelZoo includes
#include "game.h"

@interface pixelzooView : UIView {
	pixelzooViewController *controller;
	unsigned char *bitmapData; 
	int bytesPerRow;
	CGContextRef bitmapContext;
	int redraws;
}

@property(readwrite,assign) pixelzooViewController *controller;

// helpers
- (CGSize) measureText:(char*)text withFont:(UIFont*)font withSpacing:(CGFloat)charSpacing;
- (void) renderTool:(int)nTool withContext:(CGContextRef)ctx withColor:(RGB*)rgb withReserve:(CGFloat)reserve withName:(char*)name asSelected:(BOOL)selectFlag;

@end
