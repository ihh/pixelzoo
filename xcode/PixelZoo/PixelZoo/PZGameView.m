//
//  PZGameView.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/6/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZGameView.h"
#import "PZDefs.h"

@implementation PZGameView

@synthesize gameViewController;

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}

// Only override drawRect: if you perform custom drawing.
- (void)drawRect:(CGRect)rect {
    //	NSLog(@"drawRect");
    
    ++redraws;
    
    // get controller info
    PZGameWrapper *gameWrapper = [gameViewController gameWrapper];
    if ([gameWrapper isInitialized]) {
        int boardSize = [gameWrapper boardSize];
        CGFloat cellSize = [gameViewController cellSize];
        CGRect boardRect = [gameViewController boardRect];
        CGRect bigBoardRect = [gameViewController bigBoardRect];
        CGRect consoleRect = [gameViewController consoleRect];
        CGRect consoleBoardRect = [gameViewController consoleBoardRect];
        

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
                                                  (CGBitmapInfo) kCGImageAlphaNoneSkipLast);
            
            CGColorSpaceRelease(colorSpace);
            pzAssert (bitmapData != NULL, "Couldn't alloc bitmapData");
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
                int rgb = [gameWrapper cellRgbAtX:x y:y z:0];
                *(bitmapWritePtr++) = pzGetRgbRed(rgb);
                *(bitmapWritePtr++) = pzGetRgbGreen(rgb);
                *(bitmapWritePtr++) = pzGetRgbBlue(rgb);
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
        [self renderTool:0 withContext:ctx withColor:0 withReserve:1 withName:EXAMINE_TOOL_NAME asSelected:([gameWrapper selectedToolNumber] < 0)];
        int nTools = [gameWrapper numberOfTools];
        for (int nTool = 0; nTool < nTools; ++nTool) {
            int rgb = [gameWrapper toolRgbByNumber:nTool];
            [self renderTool:(nTool+1) withContext:ctx withColor:rgb withReserve:[gameWrapper toolReserveByNumber:nTool] withName:((char*)[gameWrapper toolNameByNumber:nTool]) asSelected:(nTool == [gameWrapper selectedToolNumber])];
        }
        
        // redraw console
        CGContextSaveGState (ctx);
        CGContextClipToRect (ctx, consoleRect);
        if ([gameViewController examining]) {
            CGContextDrawImage (ctx, consoleBoardRect, boardImage);
        } else {
            CGFloat cy = [self frame].size.height - 1;
            CGFloat fade = 1;
            int lines = [gameWrapper numberOfConsoleLines];
            for (int cl = lines; cl > 0 && cy >= boardRect.size.height; --cl) {
                int ci = (cl + lines - 1) % lines;
                char* ctext = (char*) [gameWrapper consoleText:ci];
                if (ctext) {
                    // measure text
                    CGFloat charsize = CONSOLE_FONT_SIZE;  // ignores game->consoleSize[ci]
                    UIFont *font = [UIFont fontWithName:fontName size:charsize];
                    CGSize textSize = [self measureText:ctext withFont:font withSpacing:CONSOLE_FONT_SPACING];
                    
                    CGFloat ch = textSize.height;
                    CGContextSelectFont (ctx,
                                         GAME_FONT,
                                         ch,
                                         kCGEncodingMacRoman);
                    CGContextSetCharacterSpacing (ctx, CONSOLE_FONT_SPACING);
                    CGContextSetTextDrawingMode (ctx, kCGTextFill);
                    
                    int rgb = 0;  // ignores game->consoleColor[ci]
                    [self setFill:rgb withContext:ctx withFactor:fade withOpacity:1 asInverse:0];
                    // print
                    CGContextShowTextAtPoint (ctx, 0, cy + [font descender], ctext, strlen(ctext));
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
        int balloons = [gameWrapper numberOfBalloons];
        for (int balloon = 0; balloon < balloons; ++balloon) {
            pzBalloon b = [gameWrapper balloonNumber:balloon];
            char* text = (char*) pzGetBalloonText(b);
            int len = strlen(text);
            int rgb = [gameWrapper textRgbForBalloon:b];
            double x = pzGetBalloonXpos(b);
            double y = pzGetBalloonYpos(b);
            double csz = pzGetBalloonCharSize(b);
            double csp = pzGetBalloonCharSpacing(b);
            double opacity = pzGetBalloonOpacity(b);
            
            // compute font size
            CGFloat
            charsize = csz * BALLOON_FONT_SIZE * cellSize / INITIAL_PIXELS_PER_CELL,
            charspacing = csp;
            
            // measure text
            UIFont *font = [UIFont fontWithName:fontName size:charsize];
            CGSize textSize = [self measureText:text withFont:font withSpacing:charspacing];
            
            // calculate coords to center text on point
            CGPoint viewOrigin = [gameViewController viewOrigin];
            CGFloat
            xpos = (cellSize * (CGFloat) x) - textSize.width / 2 - viewOrigin.x,
            ypos = (cellSize * (CGFloat) y) + textSize.height / 2 - viewOrigin.y;
            
            // print balloon
            [self setFill:rgb withContext:ctx withFactor:1 withOpacity:(opacity * BALLOON_BACKGROUND_OPACITY) asInverse:1];
            CGContextFillRect(ctx, CGRectMake(xpos, ypos - textSize.height, textSize.width, textSize.height));
            
            // print text
            CGContextSelectFont (ctx,
                                 GAME_FONT,
                                 charsize,
                                 kCGEncodingMacRoman);
            
            CGContextSetCharacterSpacing (ctx, charspacing);
            CGContextSetTextDrawingMode (ctx, kCGTextFill);
            
            [self setFill:rgb withContext:ctx withFactor:1 withOpacity:opacity asInverse:0];
            CGContextShowTextAtPoint (ctx, xpos, ypos + [font descender], text, len);
        }
        CGContextRestoreGState (ctx);
        
        // redraw name of currently touched pixel, if "examine" tool is being used
        if ([gameViewController examining]) {
            XYCoord pos = [gameViewController examCoord];
            UIFont *font = [UIFont fontWithName:fontName size:EXAMINE_FONT_SIZE];
            char* text = (char*) [gameWrapper cellNameAtX:pos.x y:pos.y z:0];
            int rgb = [gameWrapper cellNameRgbAtX:pos.x y:pos.y z:0];
            if (!text) text = EXAMINE_EMPTY_TEXT;
            
            CGSize textSize = [self measureText:text withFont:font withSpacing:EXAMINE_FONT_SPACING];
            CGContextSelectFont (ctx,
                                 GAME_FONT,
                                 EXAMINE_FONT_SIZE,
                                 kCGEncodingMacRoman);
            
            CGContextSetCharacterSpacing (ctx, EXAMINE_FONT_SPACING);
            CGContextSetTextDrawingMode (ctx, kCGTextFill);
            
            CGPoint viewOrigin = [gameViewController viewOrigin];
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
            CGPoint cmid = [gameViewController consoleCentroid];
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
    }
}

- (void)dealloc {
	if (bitmapData) {
		free (bitmapData);
		CFRelease (bitmapContext);
	}
}

// helpers

- (void) renderTool:(int)nTool withContext:(CGContextRef)ctx withColor:(int)rgb withReserve:(CGFloat)reserve withName:(char*)name asSelected:(BOOL)selectFlag {
	NSString *fontName = [[NSString alloc] initWithUTF8String:GAME_FONT];
	UIFont *font = [UIFont fontWithName:fontName size:TOOL_FONT_SIZE];
	CGContextSelectFont (ctx,
						 GAME_FONT,
						 TOOL_FONT_SIZE,
						 kCGEncodingMacRoman);
	CGContextSetCharacterSpacing (ctx, TOOL_FONT_SPACING);
	CGContextSetTextDrawingMode (ctx, kCGTextFill);
    
	CGRect toolRect = [gameViewController toolRect:nTool];
	CGRect toolPartialRect = [gameViewController toolPartialRect:nTool startingAt:0 endingAt:reserve];
	[self setFill:rgb withContext:ctx withFactor:1 withOpacity:1 asInverse:0];
	CGContextFillRect(ctx, toolPartialRect);
	CGSize textSize = [self measureText:name withFont:font withSpacing:TOOL_FONT_SPACING];
	CGFloat
	toolx = toolRect.origin.x + toolRect.size.width/2 - textSize.width/2,
	tooly = toolRect.origin.y + toolRect.size.height/2 + textSize.height/2 + [font descender];
	[self setFill:rgb withContext:ctx withFactor:1 withOpacity:TOOL_NAME_OPACITY asInverse:1];
	CGContextShowTextAtPoint (ctx, toolx, tooly, name, strlen(name));
	if (selectFlag) {
		[self setStroke:rgb withContext:ctx withFactor:1 withOpacity:TOOL_NAME_OPACITY asInverse:1];
		int pulse = ((int) (TOOL_SELECT_PULSE_RATE * redraws)) % MAX_TOOL_SELECT_STROKE;
		toolRect.origin.x += pulse/2;
		toolRect.origin.y += pulse/2;
		toolRect.size.width -= pulse;
		toolRect.size.height -= pulse;
		CGContextStrokeRectWithWidth(ctx, toolRect, pulse);
	}
}

- (CGSize) measureText:(char*)text withFont:(UIFont*)font withSpacing:(CGFloat)charSpacing {
	NSString *str = [[NSString alloc] initWithUTF8String:text];
	CGSize textSize = [str sizeWithFont:font];
	textSize.width += ([str length] - 1) * charSpacing;
	return textSize;
}

- (CGFloat) myInverse:(CGFloat)x {
	if (x > .4 & x < .6) return x < .5 ? 1 : 0;
	return 1-x;
}

- (void) setStroke:(int)rgb withContext:(CGContextRef)ctx withFactor:(CGFloat)fade withOpacity:(CGFloat)opacity asInverse:(BOOL)inverseFlag {
	CGFloat r = 0, g = 0, b = 0;
	if (rgb) {
		r = fade*((CGFloat)pzGetRgbRed(rgb))/255;
		g = fade*((CGFloat)pzGetRgbBlue(rgb))/255;
		b = fade*((CGFloat)pzGetRgbBlue(rgb))/255;
	}
	CGFloat invr = [self myInverse:r], invg = [self myInverse:g], invb = [self myInverse:b];
	if (inverseFlag) {
		CGContextSetRGBStrokeColor (ctx, invr, invg, invb, opacity);
	} else {
		CGContextSetRGBStrokeColor (ctx, r, g, b, opacity);
	}
}

- (void) setFill:(int)rgb withContext:(CGContextRef)ctx withFactor:(CGFloat)fade withOpacity:(CGFloat)opacity asInverse:(BOOL)inverseFlag {
	CGFloat r = 0, g = 0, b = 0;
	if (rgb) {
		r = fade*((CGFloat)pzGetRgbRed(rgb))/255;
		g = fade*((CGFloat)pzGetRgbBlue(rgb))/255;
		b = fade*((CGFloat)pzGetRgbBlue(rgb))/255;
	}
	CGFloat invr = [self myInverse:r], invg = [self myInverse:g], invb = [self myInverse:b];
	if (inverseFlag) {
		CGContextSetRGBFillColor (ctx, invr, invg, invb, opacity);
	} else {
		CGContextSetRGBFillColor (ctx, r, g, b, opacity);
	}
}

@end
