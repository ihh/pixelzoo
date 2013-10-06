//
//  PZView.h
//  pixelzoo
//
//  Created by Ian Holmes on 12/29/10.
//

#import <UIKit/UIKit.h>

#import "PZViewController.h"

// PixelZoo includes
#include "pixelzoo.h"

@interface PZView : UIView {
	PZViewController *pzViewController;
	unsigned char *bitmapData; 
	int bytesPerRow;
	CGContextRef bitmapContext;
	int redraws;
}

@property (strong, nonatomic) PZViewController* pzViewController;


// helpers
- (CGSize) measureText:(char*)text withFont:(UIFont*)font withSpacing:(CGFloat)charSpacing;
- (void) renderTool:(int)nTool withContext:(CGContextRef)ctx withColor:(int)rgb withReserve:(CGFloat)reserve withName:(char*)name asSelected:(BOOL)selectFlag;
- (CGFloat) myInverse:(CGFloat)x;
- (void) setFill:(int)rgb withContext:(CGContextRef)ctx withFactor:(CGFloat)fade withOpacity:(CGFloat)opacity asInverse:(BOOL)inverseFlag;
- (void) setStroke:(int)rgb withContext:(CGContextRef)ctx withFactor:(CGFloat)fade withOpacity:(CGFloat)opacity asInverse:(BOOL)inverseFlag;

@end
