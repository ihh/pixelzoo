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

-(CGImageRef) newBoardImage {
    unsigned char* bitmap = [self allocBoardBitmap];
    CGImageRef boardImg = [self newBoardImage:bitmap];
    free(bitmap);
    return boardImg;
}

-(CGImageRef) newBoardImage:(unsigned char*)bitmapData {
    // create the bitmap context if necessary
    int boardSize = [self boardSize];
    int bytesPerRow = 4 * boardSize * sizeof(unsigned char);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef bitmapContext = CGBitmapContextCreate(bitmapData,
                                                       boardSize,
                                                       boardSize,
                                                       8,  // bits per component
                                                       bytesPerRow,
                                                       colorSpace,
                                                       (CGBitmapInfo) kCGImageAlphaNoneSkipLast);
    
    // create board image
    unsigned char *bitmapWritePtr = bitmapData;
    for (int y = boardSize - 1; y >= 0; --y) {   // quick hack/fix: reverse y-loop order to flip image vertically
        for (int x = 0; x < boardSize; ++x) {
            int rgb = [self cellRgbAtX:x y:y z:0];
            *(bitmapWritePtr++) = pzGetRgbRed(rgb);
            *(bitmapWritePtr++) = pzGetRgbGreen(rgb);
            *(bitmapWritePtr++) = pzGetRgbBlue(rgb);
            ++bitmapWritePtr;
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
    CGImageRef boardImg = [self newBoardImage];
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef bmContext = CGBitmapContextCreate(NULL,
                                                   2*tileHeight*boardSize,
                                                   tileHeight*boardSize,
                                                   8,
                                                   0,
                                                   colorSpace,
                                                   (CGBitmapInfo) kCGImageAlphaPremultipliedFirst);
    
    CGAffineTransform trans = CGAffineTransformMake(tileHeight,tileHeight/2,-tileHeight,tileHeight/2,tileHeight*boardSize,0);
    CGContextConcatCTM(bmContext, trans);
    
    CGContextSetAllowsAntialiasing(bmContext, FALSE);
    CGContextSetInterpolationQuality(bmContext, kCGInterpolationNone);
    CGColorSpaceRelease(colorSpace);
    CGContextDrawImage(bmContext, CGRectMake(0, 0,
                                             boardSize,
                                             boardSize),
                       boardImg);
    
    CGImageRef rotatedImage = CGBitmapContextCreateImage(bmContext);
    CFRelease(bmContext);
    CGImageRelease(boardImg);
    
    return rotatedImage;
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

-(long long)boardClock {
    return pzBoardClock(game);
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
