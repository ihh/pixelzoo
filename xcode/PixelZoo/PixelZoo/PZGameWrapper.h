//
//  PZGameWrapper.h
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "pixelzoo.h"
#import "GDataXMLNode.h"
#import "PZWorldDescriptor.h"

@interface PZGameWrapper : NSObject <NSURLConnectionDelegate> {
    NSURLConnection* turnConnection;
}

@property (nonatomic) pzGame *game;
@property (strong, nonatomic) PZWorldDescriptor *worldDescriptor;

-(void)initGameFromXMLElement:(GDataXMLElement*)element forWorld:(PZWorldDescriptor*)world;
-(bool)isInitialized;

-(void)updateGame;
-(int)boardSize;

-(int)numberOfTools;
-(void)selectTool:(int)n;
-(void)unselectTool;
-(int)selectedToolNumber;

-(void)untouchCell;
-(void)touchCellAtX:(int)x y:(int)y;

-(int)cellRgbAtX:(int)x y:(int)y;
-(const char*)cellNameAtX:(int)x y:(int)y;
-(int)cellNameRgbAtX:(int)x y:(int)y;

-(int)toolRgbByNumber:(int)n;
-(const char*)toolNameByNumber:(int)n;
-(CGFloat)toolReserveByNumber:(int)n;

-(int)numberOfConsoleLines;
-(const char*)consoleText:(int)line;

-(int)numberOfBalloons;
-(pzBalloon)balloonNumber:(int)n;
-(int)textRgbForBalloon:(pzBalloon)b;

-(void)postTurn;


@end
