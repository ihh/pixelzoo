//
//  PZGameWrapper.h
//  pixelzoo
//
//  Created by Ian Holmes on 10/10/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <SpriteKit/SpriteKit.h>

#import "pixelzoo.h"
#import "GDataXMLNode.h"
#import "PZWorldDescriptor.h"
#import "PZLockDescriptor.h"
#import "PZBoardIterator.h"

// extern int fastCube16(int* writePtr, int bytesPerRow, int topFaceRGB, int leftFaceRGB, int rightFaceRGB);

@interface PZGameWrapper : NSObject <NSURLConnectionDelegate> {
    // caching sprite textures
    NSMutableDictionary *imageCache;
    // saving the turn
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

// overhead-view map
-(unsigned char*) allocBoardBitmap;
-(CGImageRef) newBoardImageForZ:(int)z withBitmap:(unsigned char*)bitmapData;
-(CGImageRef) newBoardImageForZ:(int)z;

// isometric-view map
-(CGSize)isometricMapSize:(CGFloat)tileHeight;

-(CGImageRef)newIsometricBoardImage:(CGFloat)tileHeight;
-(CGImageRef)newNaturalIsometricBoardImageForMapRect:(CGRect)rect withTileHeight:(CGFloat)tileHeight usingImages:(bool)useImages storingOriginIn:(CGPoint*)origin;

-(void)touchIsometricMapAt:(CGPoint)xy;
-(CGPoint)isometricMapCoordAtX:(int)x y:(int)y z:(int)z forTileHeight:(CGFloat)tileHeight;

-(void)iterateOverIsometricRegion:(CGRect)mapImageRect withIterator:(NSObject<PZBoardIterator>*)iter;

// image cache
-(CGImageRef)imageWithName:(NSString*)name;

// saving turns

-(void)postTurn;
-(void)postTurnAndDeleteLock;

-(bool)turnSaved;

// pixelzoo.h wrapper functions

-(void)updateGame;
-(int)boardSize;
-(int)boardDepth;
-(long long)boardClock;
-(double)microticksPerSecond;

-(int)numberOfTools;
-(void)selectTool:(int)n;
-(void)unselectTool;
-(int)selectedToolNumber;

-(void)untouchCell;
-(void)touchCellAtX:(int)x y:(int)y z:(int)z;

-(int)cellRgbAtX:(int)x y:(int)y z:(int)z;
-(const char*)cellNameAtX:(int)x y:(int)y z:(int)z;
-(int)cellNameRgbAtX:(int)x y:(int)y z:(int)z;

-(const char*)cellSpriteAtX:(int)x y:(int)y z:(int)z;

-(int)toolRgbByNumber:(int)n;
-(const char*)toolNameByNumber:(int)n;
-(const char*)toolIconByNumber:(int)n;
-(CGFloat)toolReserveByNumber:(int)n;

-(int)incumbentCount;
-(int)challengerCount;

@end
