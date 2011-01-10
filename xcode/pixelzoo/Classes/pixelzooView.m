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
	bitmapData = NULL;
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
	int boardSize = game->board->size;
	CGFloat cellSize = [controller cellSize];
	CGRect boardRect = [controller boardRect];

	// create the bitmap context
	if (bitmapData == NULL) {
		CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
		bytesPerRow = 4 * boardSize * sizeof(unsigned char);
		bitmapData = malloc (bytesPerRow * boardSize);
		bitmapContext = CGBitmapContextCreate(bitmapData,
											  boardSize,
											  boardSize,
											  8,  // bits per component
											  bytesPerRow,
											  colorSpace,
											  kCGImageAlphaNoneSkipLast);
		
		CFRelease(colorSpace);
		Assert (bitmapData != NULL, "Couldn't alloc bitmapData");
	}
	
	// Get the graphics context and clear it
    CGContextRef ctx = UIGraphicsGetCurrentContext();
    CGContextClearRect(ctx, rect);

	// redraw board
	if (CGRectIntersectsRect (boardRect, rect)) {
		unsigned char *bitmapWritePtr = bitmapData;
		for (int y = boardSize - 1; y >= 0; --y) {   // quick hack/fix: reverse y-loop order to flip image vertically 
			for (int x = 0; x < boardSize; ++x) {
				PaletteIndex cellColorIndex = readBoardColor(game->board, x, y);
				RGB *rgb = &game->board->palette.rgb[cellColorIndex];
				*(bitmapWritePtr++) = rgb->r;
				*(bitmapWritePtr++) = rgb->g;
				*(bitmapWritePtr++) = rgb->b;
				++bitmapWritePtr;
			}
		}
		CGImageRef image = CGBitmapContextCreateImage(bitmapContext);
		CGContextDrawImage (ctx, boardRect, image);
		CFRelease (image);
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

	// need to flip all text upside down, ho hum
	CGAffineTransform transform = CGAffineTransformMake (1., 0., 0., -1., 0., 0.);
	CGContextSetTextMatrix (ctx, transform);

	// redraw console
	CGFloat cy = [self frame].size.height - 1;
	CGFloat fade = 1;
	for (int cl = ConsoleLines; cl > 0; --cl) {
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
			CGContextSetTextDrawingMode (ctx, kCGTextFill);
			
			RGB *rgb = &game->board->palette.rgb[game->consoleColor[ci]];
			CGContextSetRGBFillColor (ctx, fade * (CGFloat)rgb->r/255, fade * (CGFloat)rgb->g/255, fade * (CGFloat)rgb->b/255, 1);
			// print
			CGContextShowTextAtPoint (ctx, 0, cy, game->consoleText[ci], strlen(game->consoleText[ci]));			
			// next line
			fade *= GAME_CONSOLE_FONT_FADE;
			cy -= ch;
		}
	}

	// redraw speech balloons
	CGContextClipToRect (ctx, boardRect);
	for (void **ptr = game->board->balloon->begin; ptr != game->board->balloon->end; ++ptr) {
		Balloon *b = (Balloon*) *ptr;
		CGFloat cw = b->size + b->z;
		CGContextSelectFont (ctx,
							 GAME_CONSOLE_FONT,
							 (CGFloat) b->size * GAME_CONSOLE_FONT_SIZE,
							 kCGEncodingMacRoman);
		CGContextSetCharacterSpacing (ctx, (CGFloat) b->z);
		CGContextSetTextDrawingMode (ctx, kCGTextFill);
		
		RGB *rgb = &game->board->palette.rgb[b->color];
		CGContextSetRGBFillColor (ctx, (CGFloat)rgb->r/255, (CGFloat)rgb->g/255, (CGFloat)rgb->b/255, (CGFloat) b->opacity);

		// print
		int len = strlen(b->text);
		CGContextShowTextAtPoint (ctx, (cellSize * (double) b->x) - (double) len * cw / 2, (cellSize * (double) b->y) - b->size / 2, b->text, len);
	}
}

- (void)dealloc {
	if (bitmapData) {
		free (bitmapData);
		CFRelease (bitmapContext);
	}
	[super dealloc];
}


@end
