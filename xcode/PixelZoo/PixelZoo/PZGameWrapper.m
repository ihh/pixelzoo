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

@synthesize game;
@synthesize worldDescriptor;

-(void)initGameFromXMLElement:(GDataXMLElement*)element forWorld:(PZWorldDescriptor*)world {
    game = pzNewGameFromXmlString([[element XMLString] UTF8String], false);
    worldDescriptor = world;
	pzStartGame(game);
}

-(bool)isInitialized {
    return game != NULL;
}

-(void)dealloc {
    if (game != NULL)
        pzDeleteGame(game);
}

-(void)postTurn {
    // check for previous POST in progress
    if (turnConnection) {
        [turnConnection cancel];
        turnConnection = nil;
    }
    
    // POST a turn to SERVER_URL_PREFIX/world/WorldID/turn
    // Create turn XML
    const char* turnCString = pzSaveBoardAndEndGoalStatusAsXmlString(game);
    NSString *turnString = [[NSString alloc] initWithUTF8String:turnCString];
    free ((char*) turnCString);

    // Create the request.
    NSMutableURLRequest *request = [worldDescriptor authenticatedPostRequest:@"turn" withContent:turnString];
    
    // Create url connection and fire request
    turnConnection = [[NSURLConnection alloc] initWithRequest:request delegate:self];
}

-(void)updateGame {
    pzUpdateGame(game,GAMELOOP_CALLS_PER_SECOND,0);
}

-(int)boardSize {
    return pzGetBoardSize(game);
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

-(void)touchCellAtX:(int)x y:(int)y {
    pzTouchCell(game,x,y);
}

-(int)cellRgbAtX:(int)x y:(int)y {
    return pzGetCellRgb(game, x, y);
}

-(const char*)cellNameAtX:(int)x y:(int)y {
    return pzGetCellName(game, x, y);
}

-(int)cellNameRgbAtX:(int)x y:(int)y {
    return pzGetCellNameRgb(game, x, y);
}

-(int)toolRgbByNumber:(int)n {
    return pzGetToolRgb(game,pzGetToolByNumber(game,n));
}

-(const char*)toolNameByNumber:(int)n {
    return pzGetToolName(pzGetToolByNumber(game,n));
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


// TODO: NSURLConnectionDelegate methods

@end
