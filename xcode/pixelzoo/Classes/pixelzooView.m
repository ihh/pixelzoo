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
		bitmapData = NULL;
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
	int boardSize = game->board->size;
	CGFloat cellSize = [controller cellSize];
	CGRect boardRect = [controller boardRect];
	CGRect bigBoardRect = [controller bigBoardRect];
	CGRect consoleRect = [controller consoleRect];
	
	// create the bitmap context if necessary
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
		
		CGColorSpaceRelease(colorSpace);
		Assert (bitmapData != NULL, "Couldn't alloc bitmapData");
	}
	
	// Get the graphics context and clear it
    CGContextRef ctx = UIGraphicsGetCurrentContext();
    CGContextClearRect(ctx, rect);

	// need to flip all text upside down, ho hum
	CGAffineTransform transform = CGAffineTransformMake (1., 0., 0., -1., 0., 0.);
	CGContextSetTextMatrix (ctx, transform);
	
	// get font name
	NSString *fontName = [[NSString alloc] initWithUTF8String:GAME_FONT];
	
	// redraw board
	if (CGRectIntersectsRect (boardRect, rect)) {

		CGContextSaveGState (ctx);
		CGContextClipToRect (ctx, boardRect);

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
		CGContextDrawImage (ctx, bigBoardRect, image);
		CGImageRelease (image);
		
		// draw border
		CGContextSetRGBStrokeColor (ctx, 1, 1, 1, BOARD_BORDER_OPACITY);
		CGContextStrokeRect(ctx, boardRect);

		CGContextRestoreGState (ctx);
	}
	
	// redraw tools
	int nTool = 0;
	[self renderTool:(nTool++) withContext:ctx withColor:NULL withReserve:1 withName:EXAMINE_TOOL_NAME asSelected:(game->selectedTool == NULL)];
	Stack *toolStack = RBTreeEnumerate (game->toolByName, NULL, NULL);
	StringMapNode *toolNode;
	while ((toolNode = StackPop(toolStack)) != NULL) {
		Tool *tool = toolNode->value;
		if (!tool->hidden) {
			State toolState = tool->defaultBrushState;
			PaletteIndex toolColorIndex = getParticleColor (game->board->byType[StateType(toolState)], toolState);
			RGB *rgb = &game->board->palette.rgb[toolColorIndex];
			[self renderTool:(nTool++) withContext:ctx withColor:rgb withReserve:(tool->reserve / tool->maxReserve) withName:tool->name asSelected:(tool == game->selectedTool)];
		}
	}
	deleteStack (toolStack);

	// redraw console
	CGContextSaveGState (ctx);
	CGContextClipToRect (ctx, consoleRect);
	CGFloat cy = [self frame].size.height - 1;
	CGFloat fade = 1;
	for (int cl = ConsoleLines; cl > 0 && cy >= boardRect.size.height; --cl) {
		int ci = (cl + game->consoleLastLineIndex) % ConsoleLines;
		if (game->consoleText[ci]) {
			// measure text
			CGFloat charsize = game->consoleSize[ci] * CONSOLE_FONT_SIZE;
			UIFont *font = [UIFont fontWithName:fontName size:charsize];
			CGSize textSize = [self measureText:game->consoleText[ci] withFont:font withSpacing:CONSOLE_FONT_SPACING];
			
			CGFloat ch = textSize.height;
			CGContextSelectFont (ctx,
								 GAME_FONT,
								 ch,
								 kCGEncodingMacRoman);
			CGContextSetCharacterSpacing (ctx, CONSOLE_FONT_SPACING);
			CGContextSetTextDrawingMode (ctx, kCGTextFill);
			
			RGB *rgb = &game->board->palette.rgb[game->consoleColor[ci]];
			CGContextSetRGBFillColor (ctx, fade * (CGFloat)rgb->r/255, fade * (CGFloat)rgb->g/255, fade * (CGFloat)rgb->b/255, 1);
			// print
			CGContextShowTextAtPoint (ctx, 0, cy + [font descender], game->consoleText[ci], strlen(game->consoleText[ci]));			
			// next line
			fade *= CONSOLE_FONT_FADE;
			cy -= ch;
		}
	}
	CGContextRestoreGState (ctx);

	// redraw speech balloons
	CGContextSaveGState (ctx);
	CGContextClipToRect (ctx, boardRect);
	for (void **ptr = game->board->balloon->begin; ptr != game->board->balloon->end; ++ptr) {
		Balloon *b = (Balloon*) *ptr;
		int len = strlen(b->text);
		RGB *rgb = &game->board->palette.rgb[b->color];

		// compute font size
		CGFloat
			charsize = b->size * BALLOON_FONT_SIZE,
			charspacing = b->z;

		// measure text
		UIFont *font = [UIFont fontWithName:fontName size:charsize];
		CGSize textSize = [self measureText:b->text withFont:font withSpacing:charspacing];
		
		// calculate coords to center text on point
		CGPoint viewOrigin = [controller viewOrigin];
		CGFloat
			xpos = (cellSize * (CGFloat) b->x) - textSize.width / 2 - viewOrigin.x,
			ypos = (cellSize * (CGFloat) b->y) + textSize.height / 2 - viewOrigin.y;
		
		// print balloon
		CGContextSetRGBFillColor (ctx, 1.-(CGFloat)rgb->r/255, 1.-(CGFloat)rgb->g/255, 1.-(CGFloat)rgb->b/255, (CGFloat) b->opacity * BALLOON_BACKGROUND_OPACITY);
		CGContextFillRect(ctx, CGRectMake(xpos, ypos - textSize.height, textSize.width, textSize.height));

		// print text
		CGContextSelectFont (ctx,
							 GAME_FONT,
							 charsize,
							 kCGEncodingMacRoman);

		CGContextSetCharacterSpacing (ctx, charspacing);
		CGContextSetTextDrawingMode (ctx, kCGTextFill);

		CGContextSetRGBFillColor (ctx, (CGFloat)rgb->r/255, (CGFloat)rgb->g/255, (CGFloat)rgb->b/255, (CGFloat) b->opacity);
		CGContextShowTextAtPoint (ctx, xpos, ypos + [font descender], b->text, len);
	}
	CGContextRestoreGState (ctx);

	// draw name of currently touched pixel, if "examine" tool is being used
	if ([controller examining]) {
		XYCoord pos = [controller examCoord];
		State examState = readBoardState (game->board, pos.x, pos.y);
		Particle *particle = game->board->byType[StateType(examState)];
		if (particle) {
			PaletteIndex examColorIndex = getParticleColor (particle, examState);
			RGB *rgb = &game->board->palette.rgb[examColorIndex];
			UIFont *font = [UIFont fontWithName:fontName size:EXAMINE_FONT_SIZE];
			CGSize textSize = [self measureText:particle->name withFont:font withSpacing:EXAMINE_FONT_SPACING];
			CGContextSelectFont (ctx,
								 GAME_FONT,
								 EXAMINE_FONT_SIZE,
								 kCGEncodingMacRoman);
			
			CGContextSetCharacterSpacing (ctx, EXAMINE_FONT_SPACING);
			CGContextSetTextDrawingMode (ctx, kCGTextFill);

			CGPoint viewOrigin = [controller viewOrigin];
			CGFloat
				xmid = (cellSize * (CGFloat) pos.x) - textSize.width / 2 - viewOrigin.x,
				ymid = (cellSize * (CGFloat) pos.y) + textSize.height / 2 - viewOrigin.y;

			CGFloat xpos, ypos;
			xpos = MAX (boardRect.origin.x, xmid);
			if (ymid - EXAMINE_TEXT_DISPLACEMENT - textSize.height - 1 < boardRect.origin.y) {
				ypos = ymid + EXAMINE_TEXT_DISPLACEMENT;
			} else {
				ypos = ymid - EXAMINE_TEXT_DISPLACEMENT;
			}

			CGContextSetRGBFillColor (ctx, 1.-(CGFloat)rgb->r/255, 1.-(CGFloat)rgb->g/255, 1.-(CGFloat)rgb->b/255, (CGFloat) EXAMINE_BACKGROUND_OPACITY);
			CGContextFillRect(ctx, CGRectMake(xpos, ypos - textSize.height, textSize.width, textSize.height));

			CGContextSetRGBFillColor (ctx, (CGFloat)rgb->r/255, (CGFloat)rgb->g/255, (CGFloat)rgb->b/255, (CGFloat) 1.);
			CGContextShowTextAtPoint (ctx, xpos, ypos + [font descender], particle->name, strlen(particle->name));

			CGContextSetRGBStrokeColor (ctx, (CGFloat)rgb->r/255, (CGFloat)rgb->g/255, (CGFloat)rgb->b/255, (CGFloat) EXAMINE_BACKGROUND_OPACITY);
			CGContextStrokeRect(ctx, CGRectMake(xpos - 1, ypos - textSize.height - 1, textSize.width + 2, textSize.height + 2));
		}
	}
	
	// release font name
	[fontName release];
}

- (void)dealloc {
	if (bitmapData) {
		free (bitmapData);
		CFRelease (bitmapContext);
	}
	[super dealloc];
}

// helpers

- (void) renderTool:(int)nTool withContext:(CGContextRef)ctx withColor:(RGB*)rgb withReserve:(CGFloat)reserve withName:(char*)name asSelected:(BOOL)selectFlag {
	NSString *fontName = [[NSString alloc] initWithUTF8String:GAME_FONT];
	UIFont *font = [UIFont fontWithName:fontName size:TOOL_FONT_SIZE];
	[fontName release];
	CGContextSelectFont (ctx,
						 GAME_FONT,
						 TOOL_FONT_SIZE,
						 kCGEncodingMacRoman);
	CGContextSetCharacterSpacing (ctx, TOOL_FONT_SPACING);
	CGContextSetTextDrawingMode (ctx, kCGTextFill);

	CGRect toolRect = [controller toolRect:nTool];
	CGRect toolPartialRect = [controller toolPartialRect:nTool startingAt:0 endingAt:reserve];
	if (rgb != NULL)
		CGContextSetRGBFillColor (ctx, (CGFloat)rgb->r/255, (CGFloat)rgb->g/255, (CGFloat)rgb->b/255, 1);
	else
		CGContextSetRGBFillColor (ctx, 0, 0, 0, 1);
	CGContextFillRect(ctx, toolPartialRect);
	CGSize textSize = [self measureText:name withFont:font withSpacing:TOOL_FONT_SPACING];
	CGFloat
	toolx = toolRect.origin.x + toolRect.size.width/2 - textSize.width/2,
	tooly = toolRect.origin.y + toolRect.size.height/2 + textSize.height/2 + [font descender];
	CGContextShowTextAtPoint (ctx, toolx, tooly, name, strlen(name));			
	if (rgb)
		CGContextSetRGBFillColor (ctx, 1.-(CGFloat)rgb->r/255, 1.-(CGFloat)rgb->g/255, 1.-(CGFloat)rgb->b/255, TOOL_NAME_OPACITY);
	else
		CGContextSetRGBFillColor (ctx, 1, 1, 1, TOOL_NAME_OPACITY);
	CGContextShowTextAtPoint (ctx, toolx, tooly, name, strlen(name));			
	if (selectFlag) {
		if (rgb)
			CGContextSetRGBStrokeColor (ctx, 1.-(CGFloat)rgb->r/255, 1.-(CGFloat)rgb->g/255, 1.-(CGFloat)rgb->b/255, TOOL_NAME_OPACITY);
		else
			CGContextSetRGBStrokeColor (ctx, 1, 1, 1, TOOL_NAME_OPACITY);
		CGContextStrokeRect(ctx, toolRect);
	}
}

- (CGSize) measureText:(char*)text withFont:(UIFont*)font withSpacing:(CGFloat)charSpacing {
	NSString *str = [[NSString alloc] initWithUTF8String:text];
	CGSize textSize = [str sizeWithFont:font];
	textSize.width += ([str length] - 1) * charSpacing;
	[str release];
	return textSize;
}

@end
