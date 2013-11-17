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
#import "PZLockDescriptor.h"
#import "PZBoardIterator.h"


@interface PZGameWrapper : NSObject <NSURLConnectionDelegate> {
    NSURLConnection* turnConnection;
    bool deleteLockWhenTurnPosted;
}

@property (strong, nonatomic) PZWorldDescriptor *worldDescriptor;
@property (strong, nonatomic) PZLockDescriptor *lockDescriptor;

@property (nonatomic) pzGame *game;
@property (nonatomic) long long lastSavedBoardClock;

// initialization

-(void)initGameFromXMLString:(NSString*)xmlString;
-(void)initGameFromLock:(PZLockDescriptor*)lock;
-(bool)isInitialized;

// drawing map
-(unsigned char*) allocBoardBitmap;
-(CGImageRef) newBoardImageForZ:(int)z withBitmap:(unsigned char*)bitmapData;
-(CGImageRef) newBoardImageForZ:(int)z;

-(CGImageRef)newIsometricBoardImage:(CGFloat)tileHeight;

-(void)touchIsometricMapAt:(CGPoint)xy;
-(CGPoint)isometricMapCoordAtX:(int)x y:(int)y z:(int)z forTileHeight:(CGFloat)tileHeight;
-(void)iterateOverIsometricRegion:(CGRect)rect withIterator:(NSObject<PZBoardIterator>*)iter;

// saving turns

-(void)postTurn;
-(void)postTurnAndDeleteLock;

-(bool)turnSaved;

// pixelzoo.h wrapper functions

-(void)updateGame;
-(int)boardSize;
-(int)boardDepth;
-(long long)boardClock;

-(int)numberOfTools;
-(void)selectTool:(int)n;
-(void)unselectTool;
-(int)selectedToolNumber;

-(void)untouchCell;
-(void)touchCellAtX:(int)x y:(int)y z:(int)z;

-(int)cellRgbAtX:(int)x y:(int)y z:(int)z;
-(const char*)cellNameAtX:(int)x y:(int)y z:(int)z;
-(int)cellNameRgbAtX:(int)x y:(int)y z:(int)z;

-(int)toolRgbByNumber:(int)n;
-(const char*)toolNameByNumber:(int)n;
-(const char*)toolIconByNumber:(int)n;
-(CGFloat)toolReserveByNumber:(int)n;

-(int)numberOfConsoleLines;
-(const char*)consoleText:(int)line;

-(int)numberOfBalloons;
-(pzBalloon)balloonNumber:(int)n;
-(int)textRgbForBalloon:(pzBalloon)b;

-(int)incumbentCount;
-(int)challengerCount;

@end
