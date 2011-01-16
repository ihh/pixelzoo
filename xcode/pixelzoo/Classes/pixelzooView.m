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
	CGRect consoleBoardRect = [controller consoleBoardRect];
	
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
	CGContextSaveGState (ctx);
	CGContextClipToRect (ctx, boardRect);

	// create board image
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

	// draw board image
	CGImageRef boardImage = CGBitmapContextCreateImage(bitmapContext);
	CGContextDrawImage (ctx, bigBoardRect, boardImage);
	
	// draw border
	CGContextSetRGBStrokeColor (ctx, 1, 1, 1, BOARD_BORDER_OPACITY);
	CGRect border = boardRect;
	CGFloat maxDim = boardSize * cellSize;
	border.size.width = MIN (maxDim, border.size.width);
	border.size.height = MIN (maxDim, border.size.height);
	CGContextStrokeRect(ctx, border);

	CGContextRestoreGState (ctx);
	
	// redraw tools
	int nTool = 0;
	[self renderTool:(nTool++) withContext:ctx withColor:NULL withReserve:1 withName:EXAMINE_TOOL_NAME asSelected:(game->selectedTool == NULL)];
	for (ListNode *toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next) {
		Tool *tool = toolNode->value;
		if (!tool->hidden) {
			State toolState = tool->defaultBrushState;
			PaletteIndex toolColorIndex = getParticleColor (game->board->byType[StateType(toolState)], toolState);
			RGB *rgb = &game->board->palette.rgb[toolColorIndex];
			[self renderTool:(nTool++) withContext:ctx withColor:rgb withReserve:(tool->reserve / tool->maxReserve) withName:tool->name asSelected:(tool == game->selectedTool)];
		}
	}
	[self renderTool:(nTool++) withContext:ctx withColor:NULL withReserve:1 withName:RESET_TOOL_NAME asSelected:0];

	// redraw console
	CGContextSaveGState (ctx);
	CGContextClipToRect (ctx, consoleRect);
	if ([controller examining]) {
		CGContextDrawImage (ctx, consoleBoardRect, boardImage);
	} else {
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
				[self setFill:rgb withContext:ctx withFactor:fade withOpacity:1 asInverse:0];
				// print
				CGContextShowTextAtPoint (ctx, 0, cy + [font descender], game->consoleText[ci], strlen(game->consoleText[ci]));			
				// next line
				fade *= CONSOLE_FONT_FADE;
				cy -= ch;
			}
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
		charsize = b->size * BALLOON_FONT_SIZE * cellSize / INITIAL_PIXELS_PER_CELL,
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
		[self setFill:rgb withContext:ctx withFactor:1 withOpacity:(b->opacity * BALLOON_BACKGROUND_OPACITY) asInverse:1];
		CGContextFillRect(ctx, CGRectMake(xpos, ypos - textSize.height, textSize.width, textSize.height));

		// print text
		CGContextSelectFont (ctx,
							 GAME_FONT,
							 charsize,
							 kCGEncodingMacRoman);

		CGContextSetCharacterSpacing (ctx, charspacing);
		CGContextSetTextDrawingMode (ctx, kCGTextFill);

		[self setFill:rgb withContext:ctx withFactor:1 withOpacity:b->opacity asInverse:0];
		CGContextShowTextAtPoint (ctx, xpos, ypos + [font descender], b->text, len);
	}
	CGContextRestoreGState (ctx);

	// redraw name of currently touched pixel, if "examine" tool is being used
	if ([controller examining]) {
		XYCoord pos = [controller examCoord];
		State examState = readBoardState (game->board, pos.x, pos.y);
		Particle *particle = game->board->byType[StateType(examState)];
		PaletteIndex examColorIndex = particle ? getParticleColor (particle, examState) : PaletteWhite;
		RGB *rgb = &game->board->palette.rgb[examColorIndex];
		UIFont *font = [UIFont fontWithName:fontName size:EXAMINE_FONT_SIZE];
		char *text = particle ? particle->name : EXAMINE_EMPTY_TEXT;

		char debugText[512];
		sprintf (debugText, "%s %llx", text, examState);
		text = debugText;
		
		CGSize textSize = [self measureText:text withFont:font withSpacing:EXAMINE_FONT_SPACING];
		CGContextSelectFont (ctx,
							 GAME_FONT,
							 EXAMINE_FONT_SIZE,
							 kCGEncodingMacRoman);
		
		CGContextSetCharacterSpacing (ctx, EXAMINE_FONT_SPACING);
		CGContextSetTextDrawingMode (ctx, kCGTextFill);

		CGPoint viewOrigin = [controller viewOrigin];
		CGFloat
			xcell = (cellSize * (.5 + (CGFloat) pos.x)) - viewOrigin.x,
			ycell = (cellSize * (.5 + (CGFloat) pos.y)) - viewOrigin.y,
			xmid = xcell - textSize.width / 2,
			ymid = ycell + textSize.height / 2;

		CGFloat xpos, ypos, xanchor, yanchor, xlabel, ylabel;
		if (ymid - EXAMINE_TEXT_DISPLACEMENT - textSize.height - EXAMINE_LABEL_BORDER < boardRect.origin.y) {
			ypos = ymid;
			ylabel = yanchor = ycell;
			if (xmid - textSize.width/2 - EXAMINE_TEXT_DISPLACEMENT - EXAMINE_LABEL_BORDER < boardRect.origin.x) {
				xpos = xmid + textSize.width/2 + EXAMINE_TEXT_DISPLACEMENT;
				xlabel = xpos - EXAMINE_LABEL_BORDER;
				xanchor = xcell + EXAMINE_CIRCLE_RADIUS;
			} else {
				xpos = xmid - textSize.width/2 - EXAMINE_TEXT_DISPLACEMENT;
				xlabel = xpos + textSize.width + EXAMINE_LABEL_BORDER;
				xanchor = xcell - EXAMINE_CIRCLE_RADIUS;
			}

		} else {
			xpos = MAX (boardRect.origin.x + EXAMINE_LABEL_BORDER, xmid);
			ypos = ymid - EXAMINE_TEXT_DISPLACEMENT;
			xlabel = xpos + textSize.width / 2;
			ylabel = ypos + EXAMINE_LABEL_BORDER;
			xanchor = xcell;
			yanchor = ycell - EXAMINE_CIRCLE_RADIUS;
		}

		// main board label
		[self setFill:rgb withContext:ctx withFactor:1 withOpacity:EXAMINE_BACKGROUND_OPACITY asInverse:1];
		CGContextFillRect(ctx, CGRectMake(xpos, ypos - textSize.height, textSize.width, textSize.height));

		[self setFill:rgb withContext:ctx withFactor:1 withOpacity:1 asInverse:0];
		CGContextShowTextAtPoint (ctx, xpos, ypos + [font descender], text, strlen(text));

		[self setStroke:rgb withContext:ctx withFactor:1 withOpacity:EXAMINE_BACKGROUND_OPACITY asInverse:0];
		CGContextStrokeRect(ctx, CGRectMake(xpos - EXAMINE_LABEL_BORDER, ypos - textSize.height - EXAMINE_LABEL_BORDER, textSize.width + 2*EXAMINE_LABEL_BORDER, textSize.height + 2*EXAMINE_LABEL_BORDER));

		// circle and connecting line
		CGContextBeginPath (ctx);
		CGContextMoveToPoint (ctx, xanchor, yanchor);
		CGContextAddLineToPoint (ctx, xlabel, ylabel);
		CGContextStrokePath (ctx);

		CGContextStrokeEllipseInRect (ctx, CGRectMake(xcell-EXAMINE_CIRCLE_RADIUS,ycell-EXAMINE_CIRCLE_RADIUS,2*EXAMINE_CIRCLE_RADIUS,2*EXAMINE_CIRCLE_RADIUS));

		// console label
		CGPoint cmid = [controller consoleCentroid];
		CGFloat
			clx = cmid.x - textSize.width/2,
			cly = cmid.y + textSize.height/2;
		
		[self setFill:rgb withContext:ctx withFactor:1 withOpacity:EXAMINE_BACKGROUND_OPACITY asInverse:1];
		CGContextFillRect(ctx, CGRectMake(clx, cly - textSize.height, textSize.width, textSize.height));
		
		[self setFill:rgb withContext:ctx withFactor:1 withOpacity:1 asInverse:0];
		CGContextShowTextAtPoint (ctx, clx, cly + [font descender], text, strlen(text));
		
		[self setStroke:rgb withContext:ctx withFactor:1 withOpacity:EXAMINE_BACKGROUND_OPACITY asInverse:0];
		CGContextStrokeRect(ctx, CGRectMake(clx - EXAMINE_LABEL_BORDER, cly - textSize.height - EXAMINE_LABEL_BORDER, textSize.width + 2*EXAMINE_LABEL_BORDER, textSize.height + 2*EXAMINE_LABEL_BORDER));
	}
	
	// release
	CGImageRelease (boardImage);
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
	[self setFill:rgb withContext:ctx withFactor:1 withOpacity:1 asInverse:0];
	CGContextFillRect(ctx, toolPartialRect);
	CGSize textSize = [self measureText:name withFont:font withSpacing:TOOL_FONT_SPACING];
	CGFloat
	toolx = toolRect.origin.x + toolRect.size.width/2 - textSize.width/2,
	tooly = toolRect.origin.y + toolRect.size.height/2 + textSize.height/2 + [font descender];
	CGContextShowTextAtPoint (ctx, toolx, tooly, name, strlen(name));			
	[self setFill:rgb withContext:ctx withFactor:1 withOpacity:TOOL_NAME_OPACITY asInverse:1];
	CGContextShowTextAtPoint (ctx, toolx, tooly, name, strlen(name));			
	if (selectFlag) {
		[self setStroke:rgb withContext:ctx withFactor:1 withOpacity:TOOL_NAME_OPACITY asInverse:1];
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

- (void) setStroke:(RGB*)rgb withContext:(CGContextRef)ctx withFactor:(CGFloat)fade withOpacity:(CGFloat)opacity asInverse:(BOOL)inverseFlag {
	if (inverseFlag) {
		if (rgb)
			CGContextSetRGBStrokeColor (ctx, 1.-fade*(CGFloat)rgb->r/255, 1.-fade*(CGFloat)rgb->g/255, 1.-fade*(CGFloat)rgb->b/255, opacity);
		else
			CGContextSetRGBStrokeColor (ctx, 1, 1, 1, opacity);
	} else {
		if (rgb)
			CGContextSetRGBStrokeColor (ctx, fade*(CGFloat)rgb->r/255, fade*(CGFloat)rgb->g/255, fade*(CGFloat)rgb->b/255, opacity);
		else
			CGContextSetRGBStrokeColor (ctx, 0, 0, 0, opacity);
	}
}

- (void) setFill:(RGB*)rgb withContext:(CGContextRef)ctx withFactor:(CGFloat)fade withOpacity:(CGFloat)opacity asInverse:(BOOL)inverseFlag {
	if (inverseFlag) {
		if (rgb)
			CGContextSetRGBFillColor (ctx, 1.-fade*(CGFloat)rgb->r/255, 1.-fade*(CGFloat)rgb->g/255, 1.-fade*(CGFloat)rgb->b/255, opacity);
		else
			CGContextSetRGBFillColor (ctx, 1, 1, 1, opacity);
	} else {
		if (rgb)
			CGContextSetRGBFillColor (ctx, fade*(CGFloat)rgb->r/255, fade*(CGFloat)rgb->g/255, fade*(CGFloat)rgb->b/255, opacity);
		else
			CGContextSetRGBFillColor (ctx, 0, 0, 0, opacity);
	}
}


@end
