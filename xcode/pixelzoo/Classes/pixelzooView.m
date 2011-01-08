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
	CGRect boardRect = [controller boardRect];
	
	// Get the graphics context and clear it
    CGContextRef ctx = UIGraphicsGetCurrentContext();
    CGContextClearRect(ctx, rect);

	// redraw board
	if (CGRectIntersectsRect (boardRect, rect)) {
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

	// redraw tools
	int nTool = 0;
	Stack *toolStack = RBTreeEnumerate (game->toolByName, NULL, NULL);
	StringMapNode *toolNode;
	while ((toolNode = StackPop(toolStack)) != NULL) {
		Tool *tool = toolNode->value;
		if (!tool->hidden) {
			CGRect toolRect = [controller toolRect:nTool];
			CGRect toolPartialRect = [controller toolPartialRect:nTool startingAt:0 endingAt:(tool->reserve / tool->maxReserve)];
			if (CGRectIntersectsRect(toolRect, rect)) {
				State toolState = tool->defaultBrushState;
				PaletteIndex toolColorIndex = getParticleColor (game->board->byType[StateType(toolState)], toolState);
				RGB *rgb = &game->board->palette.rgb[toolColorIndex];
				CGContextSetRGBFillColor (ctx, (CGFloat)rgb->r/255, (CGFloat)rgb->g/255, (CGFloat)rgb->b/255, 1);
				CGContextFillRect(ctx, toolPartialRect);
				if (tool == game->selectedTool) {
					CGContextSetRGBStrokeColor (ctx, 1.-(CGFloat)rgb->r/255, 1.-(CGFloat)rgb->g/255, 1.-(CGFloat)rgb->b/255, 1);
					CGContextStrokeRect(ctx, toolRect);
				}
			}
			++nTool;
		}
	}
	deleteStack (toolStack);

	// redraw console
	CGFloat cy = [self frame].size.height - 1;
	CGFloat fade = 1;
	for (int cl = ConsoleLines - 1; cl >= 0; --cl) {
		int ci = (cl + game->consoleLastLineIndex) % ConsoleLines;
		if (game->consoleText[ci]) {
			CGFloat ch = (CGFloat) game->consoleSize[ci] * GAME_CONSOLE_FONT_SIZE;
			if (cy - ch < boardRect.size.height)
				break;
			CGContextSelectFont (ctx,
								 GAME_CONSOLE_FONT,
								 ch,
								 kCGEncodingMacRoman);
			CGContextSetCharacterSpacing (ctx, GAME_CONSOLE_FONT_SPACING);
			CGContextSetTextDrawingMode (ctx, kCGTextFillStroke);
			
			RGB *rgb = &game->board->palette.rgb[game->consoleColor[ci]];
			CGContextSetRGBFillColor (ctx, fade * (CGFloat)rgb->r/255, fade * (CGFloat)rgb->g/255, fade * (CGFloat)rgb->b/255, 1);
			CGContextSetRGBStrokeColor (ctx, fade * (CGFloat)rgb->r/255, fade * (CGFloat)rgb->g/255, fade * (CGFloat)rgb->b/255, 1);
			// need to flip it upside down, ho hum
			CGAffineTransform transform = CGAffineTransformMake (1., 0., 0., -1., 0., 0.);
			CGContextSetTextMatrix (ctx, transform);
			// print
			CGContextShowTextAtPoint (ctx, 0, cy, game->consoleText[ci], strlen(game->consoleText[ci]));			
			// next line
			fade *= GAME_CONSOLE_FONT_FADE;
			cy -= ch;
		}
	}
	
}

- (void)dealloc {
    [super dealloc];
}


@end
