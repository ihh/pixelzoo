//
//  pixelzooView.m
//  pixelzoo
//
//  Created by Ian Holmes on 12/29/10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import "pixelzooView.h"


@implementation pixelzooView

@synthesize controller;

- (id)initWithFrame:(CGRect)frame {
    if ((self = [super initWithFrame:frame])) {
        // Initialization code
		NSLog(@"initWithFrame");

		redraws = 0;
    }
    return self;
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
*/
- (void)drawRect:(CGRect)rect
{   
//	NSLog(@"drawRect");

	++redraws;
	
	// get controller info
	Game *game = [controller game];
	CGFloat cellSize = [controller cellSize];
	
	// Get the graphics context and clear it
    CGContextRef ctx = UIGraphicsGetCurrentContext();
    CGContextClearRect(ctx, rect);

	// draw cells in specified area
	int x, y;
// Commented-out code uses palette CGColorRef's but doesn't work (maybe because they're initialized in Controller?)
	//	UIColor **boardColor = [controller boardColor];
	
	for (x = (int) rect.origin.x / cellSize; x * cellSize < (rect.origin.x + rect.size.width); ++x) {
		for (y = (int) rect.origin.y / cellSize; y * cellSize < (rect.origin.y + rect.size.height); ++y) {
			// Get cell color
			PaletteIndex cellColorIndex = readBoardColor(game->board, x, y);
			
// Commented-out code uses palette CGColorRef's but doesn't work (maybe because they're initialized in Controller?)
//			UIColor *cellColor = boardColor[cellColorIndex];
//			CGContextSetFillColorWithColor (ctx, cellColor.CGColor);

			// Look up RGB dynamically - doesn't seem to hurt performance much even though it's in inner draw loop
			RGB *rgb = &game->board->palette.rgb[cellColorIndex];
			CGContextSetRGBFillColor (ctx, (CGFloat)rgb->r/255, (CGFloat)rgb->g/255, (CGFloat)rgb->b/255, 1);

			// Fill rectangle
			CGContextFillRect(ctx, CGRectMake(x * cellSize, y * cellSize, cellSize, cellSize));
		}
	}
}

- (void)dealloc {
    [super dealloc];
}


@end
