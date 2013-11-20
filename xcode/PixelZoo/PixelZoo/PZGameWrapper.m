//
//  PZGameWrapper.m
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "PZGameWrapper.h"
#import "PZAppDelegate.h"
#import "PZDefs.h"
#import "Base64.h"

@implementation PZGameWrapper

@synthesize lockDescriptor;
@synthesize worldDescriptor;

@synthesize game;
@synthesize lastSavedBoardClock;

-(void)initGameFromXMLString:(NSString*)xmlString {
    game = pzNewGameFromXmlString([xmlString UTF8String], false);
    lastSavedBoardClock = pzBoardClock(game);
    textureCache = [[NSMutableDictionary alloc] init];
	pzStartGame(game);
}

-(void)initGameFromLock:(PZLockDescriptor*)lock {
    worldDescriptor = [lock worldDescriptor];
    lockDescriptor = lock;
    [self initGameFromXMLString:[[lock gameXMLElement] XMLString]];
}

-(bool)isInitialized {
    return game != NULL;
}

-(void)dealloc {
    if (game != NULL)
        pzDeleteGame(game);
}

-(unsigned char*) allocBoardBitmap {
    int bytesPerRow = 4 * [self boardSize] * sizeof(unsigned char);
    unsigned char* bitmapData = malloc (bytesPerRow * [self boardSize]);
    return bitmapData;
}

-(CGImageRef) newBoardImageForZ:(int)z {
    unsigned char* bitmap = [self allocBoardBitmap];
    CGImageRef boardImg = [self newBoardImageForZ:z withBitmap:bitmap];
    free(bitmap);
    return boardImg;
}

-(CGImageRef) newBoardImageForZ:(int)z withBitmap:(unsigned char*)bitmapData {
    int boardSize = [self boardSize];
    int bytesPerRow = 4 * boardSize * sizeof(unsigned char);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef bitmapContext = CGBitmapContextCreate(bitmapData,
                                                       boardSize,
                                                       boardSize,
                                                       8,  // bits per component
                                                       bytesPerRow,
                                                       colorSpace,
                                                       (CGBitmapInfo) kCGImageAlphaPremultipliedLast);
    
    // create board image
    unsigned int *bitmapWritePtr = (unsigned int*) bitmapData;
    for (int y = boardSize - 1; y >= 0; --y) {   // quick hack/fix: reverse y-loop order to flip image vertically
        for (int x = 0; x < boardSize; ++x) {
            int pzRgb = [self cellRgbAtX:x y:y z:z];
            unsigned int rgb = pzGetRgbRed(pzRgb) | (pzGetRgbGreen(pzRgb) << 8) | (pzGetRgbBlue(pzRgb) << 16);
            if (rgb || z == 0)
                rgb = rgb | 0xff000000;  // black cells above the bottom layer are transparent... a bit hacky
            *(bitmapWritePtr++) = rgb;
        }
    }
    
    // draw board image
    CGImageRef boardImg = CGBitmapContextCreateImage(bitmapContext);
    CGContextRelease(bitmapContext);
    CGColorSpaceRelease(colorSpace);
    return boardImg;

}

- (CGImageRef)newIsometricBoardImage:(CGFloat)tileHeight {
    int boardSize = [self boardSize];
    int boardDepth = [self boardDepth];
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef bmContext = CGBitmapContextCreate(NULL,
                                                   2*tileHeight*boardSize,
                                                   tileHeight*(boardSize+boardDepth),
                                                   8,
                                                   0,
                                                   colorSpace,
                                                   (CGBitmapInfo) kCGImageAlphaPremultipliedFirst);
    
    CGAffineTransform trans = CGAffineTransformMake(tileHeight,-tileHeight/2,-tileHeight,-tileHeight/2,tileHeight*boardSize,tileHeight*boardSize);
    CGContextConcatCTM(bmContext, trans);
    
    CGContextSetAllowsAntialiasing(bmContext, FALSE);
    CGContextSetInterpolationQuality(bmContext, kCGInterpolationNone);
    CGColorSpaceRelease(colorSpace);
    
    for (int z = 0; z < boardDepth; ++z) {
        int zOffset = -z;
        CGImageRef boardImg = [self newBoardImageForZ:z];
        CGContextDrawImage(bmContext, CGRectMake(zOffset,
                                                 zOffset,
                                                 boardSize + zOffset,
                                                 boardSize + zOffset),
                           boardImg);
        
        CGImageRelease(boardImg);
        
        /* Commented-out code should put a green dot at the top and a blue dot at the bottom:
         
         UIColor * greenColor = [UIColor colorWithRed:0.0 green:1.0 blue:0.0 alpha:1.0];
         CGContextSetFillColorWithColor(bmContext, greenColor.CGColor);
         CGContextFillRect(bmContext,CGRectMake(-1, -1, 1, 1));
         
         UIColor * blueColor = [UIColor colorWithRed:0.0 green:0.0 blue:1.0 alpha:1.0];
         CGContextSetFillColorWithColor(bmContext, blueColor.CGColor);
         CGContextFillRect(bmContext,CGRectMake(boardSize-1, boardSize-1, 1, 1));
         */
    }
    
    CGImageRef isoBoardImg = CGBitmapContextCreateImage(bmContext);
    CFRelease(bmContext);
    
    return isoBoardImg;
}


-(CGSize)isometricMapSize:(CGFloat)tileHeight {
    int boardSize = [self boardSize];
    int boardDepth = [self boardDepth];
    int boardImgWidth = 2 * tileHeight * boardSize;
    int boardImgHeight = tileHeight * (boardSize + boardDepth);
    return CGSizeMake(boardImgWidth, boardImgHeight);
}

-(CGImageRef)newNaturalIsometricBoardImage:(CGFloat)tileHeight {
    int boardSize = [self boardSize];
    int boardDepth = [self boardDepth];
    int boardImgWidth = 2 * tileHeight * boardSize;
    int boardImgHeight = tileHeight * (boardSize + boardDepth);
    int bytesPerRow = boardImgWidth * 4 * sizeof(unsigned char);
    unsigned char* bitmapData = calloc (boardImgWidth * boardImgHeight, 4*sizeof(unsigned char));
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef bitmapContext = CGBitmapContextCreate(bitmapData,
                                                       boardImgWidth,
                                                       boardImgHeight,
                                                       8,  // bits per component
                                                       bytesPerRow,
                                                       colorSpace,
                                                       (CGBitmapInfo) kCGImageAlphaPremultipliedLast);
    
    // create board image
    unsigned int *bitmapWritePtr = (unsigned int*) bitmapData;
// #define bitmapWriteLoc(X,Y) *(bitmapWritePtr + (Y)*boardImgWidth + (X))
#define bitmapWriteLoc(X,Y) *(X >= 0 && X < boardImgWidth && Y >= 0 && Y < boardImgHeight ? bitmapWritePtr + (Y)*boardImgWidth + (X) : NULL)
    for (int bz = 0; bz < boardDepth; ++bz) {
        for (int bx = 0; bx < boardSize; ++bx) {
            for (int by = 0; by < boardSize; ++by) {
                int pxMid = tileHeight * (bx - by + boardSize);
                int pyTop = tileHeight * (bx + by - 2*bz + 2*boardDepth) / 2;
                int pyBottom = pyTop + tileHeight - 1;

                int pzRgb = [self cellRgbAtX:bx y:by z:bz];
                int r = pzGetRgbRed(pzRgb), g = pzGetRgbGreen(pzRgb), b = pzGetRgbBlue(pzRgb);
                unsigned int rgb = r | (g<<8) | (b<<16);

                // black cells above the bottom layer are transparent... a bit hacky
                if (pzRgb || bz == 0) {
                    rgb |= 0xff000000;
                    for (int py = 0, pxMax = 1; py < tileHeight/2; ++py, pxMax += 2) {
                        for (int px = 0; px < pxMax; ++px) {
                            bitmapWriteLoc(pxMid+px,pyTop+py) = rgb;
                            bitmapWriteLoc(pxMid-px-1,pyTop+py) = rgb;
                            bitmapWriteLoc(pxMid+px,pyBottom-py) = rgb;
                            bitmapWriteLoc(pxMid-px-1,pyBottom-py) = rgb;
                        }
                    }
                }
            }
        }
    }
    
    // draw board image
    CGImageRef boardImg = CGBitmapContextCreateImage(bitmapContext);
    CGContextRelease(bitmapContext);
    CGColorSpaceRelease(colorSpace);

    free(bitmapData);
    return boardImg;
}

- (CGImageRef)newNaturalIsometricBoardImageForMapRect:(CGRect)rect withTileHeight:(CGFloat)tileHeight storingOriginIn:(CGPoint*)originRet {
    const int boardSize = [self boardSize];
    const int boardDepth = [self boardDepth];
    
    const CGFloat
    a = rect.origin.x,
    b = rect.origin.y,
    c = rect.origin.x + rect.size.width,
    d = rect.origin.y + rect.size.height;

    const int boardImgWidth = tileHeight * (rect.size.width + 4);  // 2 tiles horizontal padding
    const int boardImgHeight = tileHeight * (rect.size.height + 4);  // 4 tiles vertical padding
    const int pxOrig = tileHeight * (a - 1);
    const int pyOrig = tileHeight * (b - 1);
    originRet->x = -pxOrig;
    originRet->y = -pyOrig;
    const int bytesPerRow = boardImgWidth * 4 * sizeof(unsigned char);
    unsigned char* bitmapData = calloc (boardImgWidth * boardImgHeight, 4*sizeof(unsigned char));
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef bitmapContext = CGBitmapContextCreate(bitmapData,
                                                       boardImgWidth,
                                                       boardImgHeight,
                                                       8,  // bits per component
                                                       bytesPerRow,
                                                       colorSpace,
                                                       (CGBitmapInfo) kCGImageAlphaPremultipliedLast);
    
    // create board image
    unsigned int *bitmapWritePtr = (unsigned int*) bitmapData;
// #define bitmapWriteLoc(X,Y) *(bitmapWritePtr + (Y)*boardImgWidth + (X))
#define bitmapWriteLoc(X,Y) *(X >= 0 && X < boardImgWidth && Y >= 0 && Y < boardImgHeight ? bitmapWritePtr + (Y)*boardImgWidth + (X) : NULL)
    
    const CGFloat bx_minus_by_min = MAX(-boardSize,a - boardSize), bx_minus_by_max = MIN(boardSize,c - boardSize);
    for (int bz = 0; bz < boardDepth; ++bz) {
        const CGFloat bx_plus_by_min = MAX(0,2*(b-boardDepth+1+bz)), bx_plus_by_max = MIN(2*boardSize,2*(d-boardDepth+1+bz)+.5);
        for (int bx_plus_by = bx_plus_by_min; bx_plus_by <= bx_plus_by_max; ++bx_plus_by) {
            const int bx_max = MIN(boardSize-1,bx_plus_by);
            for (int bx = MAX(bx_plus_by-boardSize,0); bx <= bx_max; ++bx) {
                const int by = bx_plus_by - bx;
                if (by >= 0 && by < boardSize && bx-by >= bx_minus_by_min && bx-by <= bx_minus_by_max) {
                    int pxMid = tileHeight * (bx - by + boardSize) - pxOrig;
                    int pyTop = tileHeight * (bx + by - 2*bz + 2*boardDepth) / 2 - pyOrig;
                    int pyBottom = pyTop + tileHeight - 1;
                    
                    int pzRgb = [self cellRgbAtX:bx y:by z:bz];
                    int r = pzGetRgbRed(pzRgb), g = pzGetRgbGreen(pzRgb), b = pzGetRgbBlue(pzRgb);
                    unsigned int rgb = r | (g<<8) | (b<<16);
                    
                    // black cells above the bottom layer are transparent... a bit hacky
                    if (pzRgb || bz == 0) {
                        rgb |= 0xff000000;
                        for (int py = 0, pxMax = 1; py < tileHeight/2; ++py, pxMax += 2) {
                            for (int px = 0; px < pxMax; ++px) {
                                bitmapWriteLoc(pxMid+px,pyTop+py) = rgb;
                                bitmapWriteLoc(pxMid-px-1,pyTop+py) = rgb;
                                bitmapWriteLoc(pxMid+px,pyBottom-py) = rgb;
                                bitmapWriteLoc(pxMid-px-1,pyBottom-py) = rgb;
                            }
                        }
                    }
                }
            }
        }
    }


    // draw board image
    CGImageRef boardImg = CGBitmapContextCreateImage(bitmapContext);
    CGContextRelease(bitmapContext);
    CGColorSpaceRelease(colorSpace);
    
    free(bitmapData);
    return boardImg;
    
}

-(void)touchIsometricMapAt:(CGPoint)p {
    int tn = pzGetSelectedToolNumber(game);
    CGFloat bs = [self boardSize];
    int bd = [self boardDepth];
    int bz = pzGetToolZ (pzGetToolByNumber (game, tn));
    CGFloat px = (p.x - bs) / 2;
    CGFloat py = p.y - (bd - 1 - bz);
    int bx = px + py + .5;
    int by = py - px + .5;
    [self touchCellAtX:bx y:by z:bz];
}

-(void)iterateOverIsometricRegion:(CGRect)rect withIterator:(NSObject<PZBoardIterator>*)iter {
    // with reference to the variables defined in touchIsometricMapAt:
    // if (p.x,p.y) bounded by (a,b) [top left] and (c,d) [bottom right]
    // then a < p.x < c,  b < p.y < d
    // now, p.x = 2(px+bs)   and   p.y = py+(bd-1-z)
    // so (a-bs)/2 < px < (c-bs)/2,   b-(bd-1-bz) < py < d-(bd-1-bz)
    // but px=(bx-by)/2 and py=(bx+by)/2
    // so a-bs < (bx-by) < c-bs,   2(b-bd+1+bz) < (bx+by) < 2(d-bd+1+bz)

    const int bs = [self boardSize];
    const int bd = [self boardDepth];

    const CGFloat
            a = rect.origin.x,
            b = rect.origin.y,
            c = rect.origin.x + rect.size.width,
            d = rect.origin.y + rect.size.height;

    const CGFloat bx_minus_by_min = MAX(-bs,a - bs), bx_minus_by_max = MIN(bs,c - bs);
    for (int bz = 0; bz < bd; ++bz) {
        const CGFloat bx_plus_by_min = MAX(0,2*(b-bd+1+bz)), bx_plus_by_max = MIN(2*bs,2*(d-bd+1+bz)+.5);
        for (int bx_plus_by = bx_plus_by_min; bx_plus_by <= bx_plus_by_max; ++bx_plus_by) {
            const int bx_max = MIN(bs-1,bx_plus_by);
            for (int bx = MAX(bx_plus_by-bs,0); bx <= bx_max; ++bx) {
                const int by = bx_plus_by - bx;
                if (by >= 0 && by < bs && bx-by >= bx_minus_by_min && bx-by <= bx_minus_by_max)
                    [iter visitCellAtX:bx y:by z:bz];
            }
        }
    }
}

-(CGPoint)isometricMapCoordAtX:(int)x y:(int)y z:(int)z forTileHeight:(CGFloat)tileHeight {
    int bs = [self boardSize];
    int bd = [self boardDepth];
    
    return CGPointMake(tileHeight*(x-y+bs),tileHeight*(((double)(x+y))/2+bd-z-1));
}

-(SKTexture*)textureWithName:(NSString*)name {
    SKTexture* texture = [textureCache valueForKey:name];
    if (!texture) {
        texture = [SKTexture textureWithImageNamed:name];
        texture.usesMipmaps = YES;
        [textureCache setValue:texture forKey:name];
    }
    return texture;
}


-(void)postTurn {
    // cancel any previous POST in progress
    if (turnConnection) {
        [turnConnection cancel];
        turnConnection = nil;
    }
    
    // POST a turn to SERVER_URL_PREFIX/world/WorldID/turn
    // Create turn XML
    const char* turnCString = pzSaveBoardAndMoveAsXmlString(game);
    NSString *turnString = [[NSString alloc] initWithUTF8String:turnCString];
    free ((char*) turnCString);

    // Create the request.
    NSMutableURLRequest *request = [worldDescriptor postRequest:@"turn" withContent:turnString];
    
    // Create url connection and fire request
    turnConnection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
}

-(void)postTurnAndDeleteLock {
    deleteLockWhenTurnPosted = true;
    [self postTurn];
}

-(bool)turnSaved {
    return [self isInitialized] && lastSavedBoardClock == [self boardClock];
}

-(void)updateGame {
    pzUpdateGame(game,GAMELOOP_CALLS_PER_SECOND,0);
}

-(int)boardSize {
    return pzGetBoardSize(game);
}

-(int)boardDepth{
    return pzGetBoardDepth(game);
}

-(long long)boardClock {
    return pzBoardClock(game);
}

-(double)microticksPerSecond {
    return pzBoardMicroticksPerSecond(game);
}

-(int)numberOfTools {
    return pzGetNumberOfTools(game);
}

-(void)selectTool:(int)n {
    pzSelectTool(game, n);
}

-(int)selectedToolNumber {
    return pzGetSelectedToolNumber(game);
}

-(void)unselectTool {
    pzUnselectTool(game);
}

-(void)untouchCell {
    pzUntouchCell(game);
}

-(void)touchCellAtX:(int)x y:(int)y z:(int)z {
    pzTouchCell(game,x,y);
}

-(int)cellRgbAtX:(int)x y:(int)y z:(int)z {
    return pzGetCellRgb(game, x, y, z);
}

-(const char*)cellNameAtX:(int)x y:(int)y z:(int)z {
    return pzGetCellName(game, x, y, z);
}

-(int)cellNameRgbAtX:(int)x y:(int)y z:(int)z {
    return pzGetCellNameRgb(game, x, y, z);
}

-(long long)cellLastModifiedTimeAtX:(int)x y:(int)y z:(int)z {
    return pzGetCellLastModifiedTime(game, x, y, z);
}

-(const char*)cellSpriteAtX:(int)x y:(int)y z:(int)z {
    return pzGetCellSprite(game, x, y, z);
}

-(int)toolRgbByNumber:(int)n {
    return pzGetToolRgb(game,pzGetToolByNumber(game,n));
}

-(const char*)toolNameByNumber:(int)n {
    return pzGetToolName(pzGetToolByNumber(game,n));
}

-(const char*)toolIconByNumber:(int)n {
    return pzGetToolIcon(pzGetToolByNumber(game,n));
}

-(CGFloat)toolReserveByNumber:(int)n {
    return (CGFloat) pzGetToolReserveLevel(pzGetToolByNumber(game, n));
}

-(int)numberOfConsoleLines {
    return pzGetNumberOfConsoleLines(game);
}

-(const char*)consoleText:(int)line {
    return pzGetConsoleText(game, line);
}

-(int)numberOfBalloons {
    return pzGetNumberOfBalloons(game);
}

-(pzBalloon)balloonNumber:(int)n {
    return pzGetBalloonByNumber(game,n);
}

-(int)textRgbForBalloon:(pzBalloon)b {
    return pzGetBalloonTextRgb(game,b);
}

-(int)incumbentCount { return pzIncumbentCount(game); }
-(int)challengerCount { return pzChallengerCount(game); }


#pragma mark NSURLConnectionDelegate methods


- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    NSHTTPURLResponse *httpLockResponse = (NSHTTPURLResponse*)response;
    if ([httpLockResponse statusCode] == 204) {  // 204 CREATED
        lastSavedBoardClock = pzBoardClock(game);
        if (deleteLockWhenTurnPosted)
            [lockDescriptor deleteLock];
    }
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    // nothing to see here
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection
                  willCacheResponse:(NSCachedURLResponse*)cachedResponse {
    // Return nil to indicate not necessary to store a cached response for this connection
    return nil;
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection {
    // nothing to see here
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    // The request has failed for some reason!
    // Check the error var
}


@end
