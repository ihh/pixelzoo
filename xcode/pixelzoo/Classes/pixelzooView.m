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
	
	Game *game = [controller game];
	
	// get cell dimensions
	CGFloat width = self.frame.size.width;
	CGFloat height = self.frame.size.height;
	// NSLog(@"w=%f h=%f",width,height);
	CGFloat dim = MIN(width,height);
	CGFloat cellSize = dim / game->board->size;

	// Get the graphics context and clear it
    CGContextRef ctx = UIGraphicsGetCurrentContext();
    CGContextClearRect(ctx, rect);

	// draw cells in specified area
	int x, y;
	UIColor **boardColor = [controller boardColor];
	
	for (x = (int) rect.origin.x / cellSize; x * cellSize < (rect.origin.x + rect.size.width); ++x) {
		for (y = (int) rect.origin.y / cellSize; y * cellSize < (rect.origin.y + rect.size.height); ++y) {
			PaletteIndex cellColorIndex = readBoardColor(game->board, x, y);
			UIColor *cellColor = boardColor[cellColorIndex];
			//			CGContextSetFillColorWithColor (ctx, cellColor.CGColor);
			
			RGB *rgb = &game->board->palette.rgb[cellColorIndex];
			if (cellColorIndex>0)
				CGContextSetRGBFillColor (ctx, (CGFloat)rgb->r/255, (CGFloat)rgb->g/255, (CGFloat)rgb->b/255, 1);
			else
				CGContextSetRGBFillColor(ctx, redraws%2, 0, 1-(redraws%2), 1);

			CGContextFillRect(ctx, CGRectMake(x * cellSize, y * cellSize, cellSize, cellSize));
		}
	}
	// Draw a red solid square
//    CGContextSetRGBFillColor(ctx, 255, 0, 0, 1);
//    CGContextFillRect(ctx, CGRectMake(10, 10, 50, 50));
}



- (void)dealloc {
    [super dealloc];
}


@end
